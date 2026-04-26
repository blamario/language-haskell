{-# Language FlexibleContexts, FlexibleInstances, ImportQualifiedPost,
             MultiParamTypeClasses, NamedFieldPuns, OverloadedStrings,
             ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators #-}

-- | Type system, OutsideIn(X) formulated as an attribute grammar

module Language.Haskell.TypeSystem (checkExpression{-, checkDeclaration, checkModule-}) where

import Data.Either.Validation (Validation(Failure, Success), validationToEither)
import Data.Functor.Identity (Identity(Identity, runIdentity))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Semigroup (Sum(Sum))
import Data.Text qualified as Text
import Data.ZipNonEmpty (ZipNonEmpty(ZipNonEmpty))
import Rank2 qualified
import Transformation qualified
import Transformation (Transformation)
import Transformation.AG qualified as AG
import Transformation.AG.Generics qualified as AG (Auto(Auto), Synthesizer(synthesis), passDown)
import Transformation.Deep qualified as Deep
import Transformation.Full qualified as Full
import Transformation.Rank2 qualified

import Language.Haskell.Extensions.Abstract qualified as Abstract
import Language.Haskell.Binder qualified as Binder
import Language.Haskell.Reserializer qualified as Reserializer
import Language.Haskell.Extensions as Extensions (Extension, includedByDefault)
import Language.Haskell.Extensions.AST qualified as AST

checkExpression :: (Abstract.Haskell l,
                    Abstract.Name l ~ AST.Name l,
                    Abstract.QualifiedName l ~ AST.QualifiedName l,
                    Abstract.TypeVarBinding l ~ AST.TypeVarBinding l,
                    Abstract.Type l ~ AST.Type l,
                    Abstract.Context l ~ AST.Context l,
                    Abstract.Expression l ~ AST.Expression l,
                    Abstract.Pattern l ~ AST.Pattern l)
                => ConstraintHandler l con
                -> Map (AST.QualifiedName l) (AST.Type l l Identity Identity)
                -> Wrap l pos s (AST.Expression l l (Wrap l pos s) (Wrap l pos s))
                -> Either (TypeErrors l pos con) (AST.Type l l Identity Identity)
checkExpression handler bindings e =
  fmap fst $ validationToEither $ AG.syn $ (AG.Knit (TypeCheck handler) Full.<$> e) Rank2.$ AG.Inherited env
  where env = TypeEnv{
          bindings,
          freshVarPrefix = "",
          constraints = empty handler}

-- | Transformation for checking and inference of types. The @con@ parameter is for constraints.
data TypeCheck l pos s con = TypeCheck (ConstraintHandler l con)

-- | Record of functions for handling constraints
data ConstraintHandler l con = ConstraintHandler{
  fromContext :: AST.Context l l Identity Identity -> con,
  replaceVar :: AST.Name l -> AST.Name l -> con -> con,
  simplify :: con  -- ^ given constraints to rely on
           -> con  -- ^ wanted constraints to simplify
           -> (con, TypeEnv l Identity con),
  unify :: AST.Type l l Identity Identity -> AST.Type l l Identity Identity -> con,
  union :: con -> con -> con,
  empty :: con}

-- | The type environment maps variables to their types
data TypeEnv l f con = TypeEnv {
  freshVarPrefix :: String,
  bindings :: Map (AST.QualifiedName l) (AST.Type l l f f),
  constraints :: con}

data TypeError l con
  = TypeMismatch (AST.Type l l Identity Identity) (AST.Type l l Identity Identity)
  | TypeAmbiguity con
  | DuplicatePatternVariables [AST.Name l]

type TypeErrors l pos con = NonEmpty (pos, TypeError l con)

type Wrap l pos s = Reserializer.Wrapped pos s

instance AG.Attribution (TypeCheck l pos s con) where
  type Origin (TypeCheck l pos s con) = Wrap l pos s
  unwrap _ (_, x) = x

type instance AG.Atts (AG.Inherited (TypeCheck l pos s con)) g = InhAtts l pos s con g
type instance AG.Atts (AG.Synthesized (TypeCheck l pos s con)) g = SynAtts l pos s con g

type family InhAtts l pos s con (g :: (Type -> Type) -> (Type -> Type) -> Type) where
  InhAtts l pos s con _ = TypeEnv l Identity con

type family SynAtts l pos s con g where
  SynAtts l pos s con (AST.Expression l l) = Validation (TypeErrors l pos con) (AST.Type l l Identity Identity, con)
  SynAtts l pos s con (AST.Pattern l l) =
    Validation (TypeErrors l pos con) (AST.Name l, Map (AST.Name l) (AST.Type l l Identity Identity), con)
  SynAtts l pos s con _ = Validation (TypeErrors l pos con) ()

instance (Abstract.Haskell l,
          Abstract.Name l ~ AST.Name l,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.TypeVarBinding l ~ AST.TypeVarBinding l,
          Abstract.Type l ~ AST.Type l,
          Abstract.Context l ~ AST.Context l,
          Abstract.Expression l ~ AST.Expression l,
          Abstract.Pattern l ~ AST.Pattern l) =>
         AG.At (TypeCheck l pos s con) (AST.Expression l l) where
  attribution
    (TypeCheck ConstraintHandler{fromContext, replaceVar})
    (_, AST.ReferenceExpression var) (AG.Inherited env, _) =
    case Map.lookup var (bindings env) of
      Just t -> (AG.Synthesized $ Success $ concrete t, AST.ReferenceExpression var)
    where concrete (AST.ForallType vars (Identity (AST.ConstrainedType (Identity context) (Identity body)))) =
            foldr (replace . runIdentity) (body, fromContext context) vars
          replace (AST.ExplicitlyKindedTypeVariable _ name _) typeCon = replaceFresh replaceVar env name typeCon
          replace (AST.ImplicitlyKindedTypeVariable _ name) typeCon = replaceFresh replaceVar env name typeCon
          replace AST.WildcardTypeBinding typeCon = typeCon
          replace AST.ExplicitlyKindedWildcardTypeBinding{} typeCon = typeCon
  attribution
    (TypeCheck ConstraintHandler{unify, union})
    (_, AST.ApplyExpression{}) (AG.Inherited env, AST.ApplyExpression (AG.Synthesized tc1) (AG.Synthesized tc2)) =
    (AG.Synthesized $ liftA2 apply tc1 tc2,
     AST.ApplyExpression (AG.Inherited $ forkFresh 'f' env) (AG.Inherited $ forkFresh 'a' env))
    where apply :: (AST.Type l l Identity Identity, con)
                -> (AST.Type l l Identity Identity, con)
                -> (AST.Type l l Identity Identity, con)
          apply (t1, c1) (t2, c2) =
            (var, c1 `union` c2 `union` unify t1 (AST.FunctionType (Identity t2) (Identity var)))
          var = AST.TypeVariable (freshTV env)
  attribution
    (TypeCheck ConstraintHandler{union})
    ((start, _, end), AST.LambdaExpression patterns _)
    (AG.Inherited env, AST.LambdaExpression patSyns (AG.Synthesized body)) =
    (AG.Synthesized
     $ if null patVarDuplicates
       then Failure $ pure (start, DuplicatePatternVariables patVarDuplicates)
       else foldr (liftA2 abstract . AG.syn) body patSyns,
     AST.LambdaExpression (AG.Inherited <$> patEnvs) (AG.Inherited bodyEnv))
    where abstract :: (AST.Name l, Map (AST.Name l) (AST.Type l l Identity Identity), con)
                   -> (AST.Type l l Identity Identity, con)
                   -> (AST.Type l l Identity Identity, con)
          abstract (patVar, _, patCon) (rhsType, rhsCon) =
            (AST.FunctionType (Identity $ AST.TypeVariable patVar) (Identity rhsType), union patCon rhsCon)
          patEnvs = flip forkFresh env <$> (ZipNonEmpty ('a' :| ['b' ..]) <* patterns)
          bodyEnv =
            forkFresh 'x' env{bindings= bindings env <> Map.mapKeysMonotonic Binder.unqualifiedName patVarBindings}
          patVarBindings = foldMap (\(AG.Synthesized (Success (_, varBindings, _))) -> varBindings) patSyns
          patVarDuplicates =
            Map.keys $ Map.filter (> Sum 1)
            $ foldMap (\(AG.Synthesized (Success (_, varBindings, _))) -> Sum 1 <$ varBindings) patSyns

replaceTypeVar :: AST.Name l -> AST.Name l -> AST.Type l l Identity Identity -> AST.Type l l Identity Identity
replaceTypeVar old new = undefined

replaceFresh :: (AST.Name l -> AST.Name l -> con -> con)
             -> TypeEnv l f con
             -> AST.Name l
             -> (AST.Type l l Identity Identity, con)
             -> (AST.Type l l Identity Identity, con)
replaceFresh replaceConVar TypeEnv{freshVarPrefix} name@(AST.Name n) (t, con) =
  (replaceTypeVar name name' t, replaceConVar name name' con)
  where name' = AST.Name (Text.pack freshVarPrefix <> "_" <> n)

forkFresh :: Char -> TypeEnv l f con -> TypeEnv l f con
forkFresh c env@TypeEnv{freshVarPrefix} = env{freshVarPrefix= c:freshVarPrefix}

freshTV :: Abstract.Haskell l => TypeEnv l f con -> Abstract.Name l
freshTV TypeEnv{freshVarPrefix} = Abstract.name (Text.pack freshVarPrefix)

