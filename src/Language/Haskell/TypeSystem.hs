{-# Language DataKinds, FlexibleContexts, FlexibleInstances, ImportQualifiedPost, LambdaCase,
             MultiParamTypeClasses, NamedFieldPuns, OverloadedRecordDot, OverloadedStrings,
             ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeFamilies, TypeOperators,
             UndecidableInstances #-}

-- | Type system, OutsideIn(X) formulated as an attribute grammar

module Language.Haskell.TypeSystem (
  checkExpression{-, checkDeclaration, checkModule-},
  TypeErrors, DefaultConstraints, defaultConstraintHandler) where

import Control.Applicative (ZipList(ZipList))
import Data.Either.Validation (Validation(Failure, Success), validationToEither)
import Data.Foldable (fold, toList)
import Data.Functor ((<&>))
import Data.Functor.Compose (Compose(Compose, getCompose))
import Data.Functor.Identity (Identity(Identity, runIdentity))
import Data.Kind (Type)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty((:|)), nonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (Ap(Ap), Endo(Endo), Sum(Sum))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
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
import Language.Haskell.Extensions as Extensions (Extension(OverloadedStrings, RebindableSyntax))
import Language.Haskell.Extensions.AST qualified as AST

checkExpression :: (Abstract.Haskell l,
                    Abstract.Name l ~ AST.Name l,
                    Abstract.ModuleName l ~ AST.ModuleName l,
                    Abstract.QualifiedName l ~ AST.QualifiedName l,
                    Abstract.TypeVarBinding l ~ AST.TypeVarBinding l,
                    Abstract.Type l ~ AST.Type l,
                    Abstract.Kind l ~ AST.Type l,
                    Abstract.Context l ~ AST.Context l,
                    Abstract.Expression l ~ AST.Expression l,
                    Abstract.FieldBinding l ~ AST.FieldBinding l,
                    Abstract.Value l ~ AST.Value l,
                    Abstract.CaseAlternative l ~ AST.CaseAlternative l,
                    Abstract.LambdaCasesAlternative l ~ AST.LambdaCasesAlternative l,
                    Abstract.Statement l ~ AST.Statement l,
                    Abstract.EquationLHS l ~ AST.EquationLHS l,
                    Abstract.EquationRHS l ~ AST.EquationRHS l,
                    Abstract.GuardedExpression l ~ AST.GuardedExpression l,
                    Abstract.Declaration l ~ AST.Declaration l,
                    Abstract.FieldDeclaration l ~ AST.FieldDeclaration l,
                    Abstract.TypeLHS l ~ AST.TypeLHS l,
                    Abstract.ClassInstanceLHS l ~ AST.ClassInstanceLHS l,
                    Abstract.PatternLHS l ~ AST.PatternLHS l,
                    Abstract.PatternEquationLHS l ~ AST.PatternEquationLHS l,
                    Abstract.PatternEquationClause l ~ AST.PatternEquationClause l,
                    Abstract.FunctionalDependency l ~ AST.FunctionalDependency l,
                    Abstract.DerivingClause l ~ AST.DerivingClause l,
                    Abstract.DerivingStrategy l ~ AST.DerivingStrategy l,
                    Abstract.DataConstructor l ~ AST.DataConstructor l,
                    Abstract.GADTConstructor l ~ AST.GADTConstructor l,
                    Abstract.Constructor l ~ AST.Constructor l,
                    Abstract.Pattern l ~ AST.Pattern l,
                    Abstract.FieldPattern l ~ AST.FieldPattern l)
                => ConstraintHandler l con
                -> Map Extension Bool
                -> Map (AST.QualifiedName l) (AST.Type l l Identity Identity)
                -> Wrap l pos s (AST.Expression l l (Wrap l pos s) (Wrap l pos s))
                -> Either (TypeErrors l pos con) (AST.Type l l Identity Identity)
checkExpression constrain extensions bindings e =
  fmap constrainType $ validationToEither $ AG.syn $ (transformation Full.<$> e) Rank2.$ AG.Inherited env
  where env = TypeEnv{
          bindings,
          freshVarPrefix = "a",
          constraints = constrain.empty}
        transformation = AG.Knit TypeCheck{constrain, extensions}
        constrainType (t, con) = AST.ConstrainedType (Identity $ fst $ constrain.toContext con) (Identity t)

-- | Transformation for checking and inference of types. The @con@ parameter is for constraints.
data TypeCheck l pos s con = TypeCheck{
  constrain :: ConstraintHandler l con,
  extensions :: Map Extension Bool}

-- | Record of functions for handling constraints
data ConstraintHandler l con = ConstraintHandler{
  fromContext :: AST.Context l l Identity Identity -> con,
  toContext :: con -> (AST.Context l l Identity Identity, con),
  replaceVar :: AST.Name l -> AST.Name l -> con -> con,
  simplify :: con  -- ^ given constraints to rely on
           -> con  -- ^ wanted constraints to simplify
           -> (con, Map (AST.Name l) (AST.Type l l Identity Identity)),
  unify :: AST.Type l l Identity Identity -> AST.Type l l Identity Identity -> con,
  union :: con -> con -> con,
  empty :: con}

data DefaultConstraints l = DefaultConstraints{
  equations :: [(AST.Type l l Identity Identity, AST.Type l l Identity Identity)],
  classes :: Map (AST.QualifiedName l) [AST.Type l l Identity Identity]}

deriving instance (Show (AST.QualifiedName l), Show (AST.Type l l Identity Identity)) => Show (DefaultConstraints l)
instance Semigroup (DefaultConstraints AST.Language) where
  (<>) = defaultConstraintHandler.union
instance Monoid (DefaultConstraints AST.Language) where
  mempty = defaultConstraintHandler.empty

defaultConstraintHandler :: ConstraintHandler AST.Language (DefaultConstraints AST.Language)
defaultConstraintHandler = ConstraintHandler{
  fromContext = \case
      AST.ClassConstraint name (Identity arg) -> DefaultConstraints{equations= [], classes= Map.singleton name [arg]}
      AST.Constraints cons -> foldMap defaultConstraintHandler.fromContext (Compose cons)
      AST.NoContext -> defaultConstraintHandler.empty,
  toContext = \DefaultConstraints{equations, classes}->
      case [AST.TypeEquality (Identity l) (Identity r) | (l, r) <- equations]
           <> [AST.ClassConstraint name (Identity arg) | (name, [arg]) <- Map.toList classes]
      of [] -> (AST.NoContext, defaultConstraintHandler.empty)
         cons -> (AST.Constraints (ZipList $ Identity <$> cons), defaultConstraintHandler.empty),
  replaceVar = \from to DefaultConstraints{equations, classes} ->
      let replaceInType = \case
            AST.TypeVariable name
              | name == from -> AST.TypeVariable to
              | otherwise -> AST.TypeVariable name
            AST.FunctionType l r -> AST.FunctionType (replaceInType <$> l) (replaceInType <$> r)
            AST.ListType t -> AST.ListType (replaceInType <$> t)
            AST.StrictType t -> AST.StrictType (replaceInType <$> t)
            AST.TupleType fields -> AST.TupleType (getCompose $ replaceInType <$> Compose fields)
            AST.TypeApplication l r -> AST.TypeApplication (replaceInType <$> l) (replaceInType <$> r)
            t -> t
      in DefaultConstraints{
        equations = equations <&> \(l, r)-> (replaceInType l, replaceInType r),
        classes = getCompose $ replaceInType <$> Compose classes},
  -- TODO: actually simplify wanted, report contradictions
  simplify = \given wanted-> (given <> wanted, Map.empty),
  unify = \a b -> DefaultConstraints{equations= [(a, b)], classes= mempty},
  union = \l r-> DefaultConstraints{
      equations= l.equations <> r.equations,
      classes= Map.unionWith (<>) l.classes r.classes},
  empty = DefaultConstraints{equations= [], classes= Map.empty}}

-- | The type environment maps variables to their types
data TypeEnv l f con = TypeEnv{
  freshVarPrefix :: String,
  bindings :: Map (AST.QualifiedName l) (AST.Type l l f f),
  constraints :: con}

data TypeError l con
  = TypeMismatch (AST.Type l l Identity Identity) (AST.Type l l Identity Identity)
  | TypeAmbiguity con
  | DuplicatePatternVariables (NonEmpty (AST.Name l))
  | UnknownTypeVariable (AST.QualifiedName l)

deriving instance (Show (AST.Type l l Identity Identity), Show (AST.Name l), Show con) => Show (TypeError l con)

type TypeErrors l pos con = NonEmpty (pos, TypeError l con)

type Wrap l pos s = Reserializer.Wrapped pos s

instance AG.Attribution (TypeCheck l pos s con) where
  type Origin (TypeCheck l pos s con) = Wrap l pos s
  unwrap _ (_, x) = x

type instance AG.Atts (AG.Inherited (TypeCheck l pos s con)) g = InhAtts l pos s con g
type instance AG.Atts (AG.Synthesized (TypeCheck l pos s con)) g =
  Validation (TypeErrors l pos con) (SynAtts l pos s con g)

type family InhAtts l pos s con (g :: (Type -> Type) -> (Type -> Type) -> Type) where
  InhAtts l pos s con (AST.DataConstructor l l) = (AST.Type l l Identity Identity, TypeEnv l Identity con)
  InhAtts l pos s con (AST.GuardedExpression l l) = (StatementConstraintBuilder l con, TypeEnv l Identity con)
  InhAtts l pos s con (AST.Statement l l) = (StatementConstraintBuilder l con, TypeEnv l Identity con)
  InhAtts l pos s con _ = TypeEnv l Identity con

type family SynAtts l pos s con g where
  SynAtts l pos s con (AST.Expression l l) = (AST.Type l l Identity Identity, con)
  SynAtts l pos s con (AST.Value l l) = (AST.Type l l Identity Identity, con)
  SynAtts l pos s con (AST.FieldBinding l l) = (AST.QualifiedName l, AST.Type l l Identity Identity, con)
  SynAtts l pos s con (AST.Constructor l l) = (AST.Type l l Identity Identity, con)
  SynAtts l pos s con (AST.Pattern l l) = (AST.Name l, Map (AST.Name l) (AST.Type l l Identity Identity), con)
  SynAtts l pos s con (AST.PatternEquationLHS l l) = (Map (AST.Name l) (AST.Type l l Identity Identity), con)
  SynAtts l pos s con (AST.PatternEquationClause l l) = (Map (AST.Name l) (AST.Type l l Identity Identity), con)
  SynAtts l pos s con (AST.FieldPattern l l) =
    (AST.QualifiedName l, Map (AST.Name l) (AST.Type l l Identity Identity), con)
  SynAtts l pos s con (AST.GuardedExpression l l) = (AST.Type l l Identity Identity, con)
  SynAtts l pos s con (AST.LambdaCasesAlternative l l) =
    ([AST.Name l], Map (AST.Name l) (AST.Type l l Identity Identity), AST.Type l l Identity Identity, con)
  SynAtts l pos s con (AST.CaseAlternative l l) = (AST.Type l l Identity Identity, AST.Type l l Identity Identity, con)
  SynAtts l pos s con (AST.Statement l l) = (Map (AST.Name l) (AST.Type l l Identity Identity), con)
  SynAtts l pos s con (AST.EquationLHS l l) = (AST.Name l, Map (AST.Name l) (AST.Type l l Identity Identity), con)
  SynAtts l pos s con (AST.EquationRHS l l) = (AST.Type l l Identity Identity, con)
  SynAtts l pos s con (AST.Declaration l l) = (Map (AST.Name l) (AST.Type l l Identity Identity), con)
  SynAtts l pos s con (AST.FieldDeclaration l l) = Map (AST.Name l) (AST.Type l l Identity Identity)
  SynAtts l pos s con (AST.DataConstructor l l) = (AST.Name l, AST.Type l l Identity Identity)
  SynAtts l pos s con (AST.GADTConstructor l l) = Map (AST.Name l) (AST.Type l l Identity Identity)
  SynAtts l pos s con (AST.Type l l) = AST.Type l l Identity Identity
  SynAtts l pos s con (AST.TypeVarBinding l l) = AST.TypeVarBinding l l Identity Identity
  SynAtts l pos s con (AST.Context l l) = (AST.Context l l Identity Identity, con)
  SynAtts l pos s con _ = ()

type StatementConstraintBuilder l con =
  TypeEnv l Identity con -> Maybe (AST.Type l l Identity Identity) -> AST.Type l l Identity Identity -> con

instance (Abstract.Haskell l,
          Abstract.Name l ~ AST.Name l,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.TypeVarBinding l ~ AST.TypeVarBinding l,
          Abstract.EquationLHS l ~ AST.EquationLHS l,
          Abstract.EquationRHS l ~ AST.EquationRHS l,
          Abstract.Declaration l ~ AST.Declaration l,
          Abstract.Type l ~ AST.Type l) =>
         AG.At (TypeCheck l pos s con) (AST.Declaration l l) where
  attribution
    TypeCheck{constrain}
    (_, AST.EquationDeclaration _ _ wheres)
    (AG.Inherited env, AST.EquationDeclaration (AG.Synthesized lhsSyn) (AG.Synthesized rhsSyn) whereSyns)
    =
    (AG.Synthesized $ combineSyn <$> lhsSyn <*> rhsSyn <*> traverse AG.syn whereSyns,
     AST.EquationDeclaration (AG.Inherited lhsEnv) (AG.Inherited rhsEnv) whereEnvs)
    where combineSyn (lhsName, lhsEnv, lhsCon) (rhsType, rhsCon) whereTC =
            (Map.insert lhsName rhsType lhsEnv,
             foldr (constrain.union . snd) (constrain.union lhsCon rhsCon) whereTC)
          lhsEnv = forkFresh 'x' env
          lhsBindings = foldMap (\(_, env, _)-> env) lhsSyn
          rhsEnv = forkFresh 'y' $ extendWith (lhsBindings <> foldMap (foldMap fst . AG.syn) whereSyns) env
          whereEnvs = AG.Inherited . (`forkFresh` extendWith lhsBindings env)
                      <$> (ZipList ['a' ..] <* wheres)

instance (Abstract.Haskell l,
          Abstract.Name l ~ AST.Name l,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.TypeVarBinding l ~ AST.TypeVarBinding l,
          Abstract.Type l ~ AST.Type l,
          Abstract.Context l ~ AST.Context l) =>
         AG.At (TypeCheck l pos s con) (AST.TypeLHS l l) where
  attribution TypeCheck{constrain} (_, AST.SimpleTypeLHS name vars) (AG.Inherited env, _) =
    (AG.Synthesized $ Success (), AST.SimpleTypeLHS name vars)

instance (Abstract.Haskell l,
          Abstract.Name l ~ AST.Name l,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.Type l ~ AST.Type l) =>
         AG.At (TypeCheck l pos s con) (AST.ClassInstanceLHS l l) where
  attribution TypeCheck{} (_, AST.TypeClassInstanceLHS name _) (AG.Inherited env, AST.TypeClassInstanceLHS _ typeSyn) =
    (AG.Synthesized $ Success (), AST.TypeClassInstanceLHS name (AG.Inherited env))

instance (Abstract.Haskell l,
          Abstract.Name l ~ AST.Name l,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.Type l ~ AST.Type l) =>
         AG.At (TypeCheck l pos s con) (AST.PatternLHS l l) where
  attribution TypeCheck{} (_, AST.PrefixPatternLHS name args) _ =
    (AG.Synthesized $ Success (), AST.PrefixPatternLHS name args)

instance (Abstract.Haskell l,
          Abstract.Name l ~ AST.Name l,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.Pattern l ~ AST.Pattern l) =>
         AG.At (TypeCheck l pos s con) (AST.PatternEquationLHS l l) where
  attribution
    TypeCheck{constrain}
    (_, AST.PrefixPatternEquationLHS name args)
    (AG.Inherited env, AST.PrefixPatternEquationLHS _ argSyns)
    =
    (AG.Synthesized $ foldr combine (mempty, constrain.empty) <$> traverse AG.syn argSyns,
     AST.PrefixPatternEquationLHS name (AG.Inherited env <$ args))
    where combine (_, bindings1, con1) (bindings, con) = (bindings <> bindings1, constrain.union con1 con)

instance (Abstract.Haskell l,
          Abstract.Name l ~ AST.Name l,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.PatternEquationLHS l ~ AST.PatternEquationLHS l,
          Abstract.EquationRHS l ~ AST.EquationRHS l,
          Abstract.Declaration l ~ AST.Declaration l,
          Abstract.Type l ~ AST.Type l) =>
         AG.At (TypeCheck l pos s con) (AST.PatternEquationClause l l) where
  attribution
    TypeCheck{constrain}
    (_, AST.PatternEquationClause sup lhs rhs wheres)
    (AG.Inherited env, AST.PatternEquationClause _ (AG.Synthesized lhsSyn) (AG.Synthesized rhsSyn) whereSyns)
    =
    (AG.Synthesized $ combineSyn <$> lhsSyn <*> rhsSyn <*> traverse AG.syn whereSyns,
     AST.PatternEquationClause sup (AG.Inherited lhsEnv) (AG.Inherited rhsEnv) whereEnvs)
    where combineSyn ({-lhsName,-} lhsEnv, lhsCon) (rhsType, rhsCon) whereTC =
            ({-Map.insert lhsName rhsType-} lhsEnv,
             foldr (constrain.union . snd) (constrain.union lhsCon rhsCon) whereTC)
          lhsEnv = forkFresh 'x' env
          lhsBindings = foldMap (\({-_,-} env, _)-> env) lhsSyn
          rhsEnv = forkFresh 'y' $ extendWith (lhsBindings <> foldMap (foldMap fst . AG.syn) whereSyns) env
          whereEnvs = AG.Inherited . (`forkFresh` extendWith lhsBindings env)
                      <$> (ZipList ['a' ..] <* wheres)

instance (Abstract.Haskell l,
          Abstract.Name l ~ AST.Name l,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.TypeVarBinding l ~ AST.TypeVarBinding l,
          Abstract.Type l ~ AST.Type l,
          Abstract.Context l ~ AST.Context l,
          Abstract.Pattern l ~ AST.Pattern l) =>
         AG.At (TypeCheck l pos s con) (AST.EquationLHS l l) where
  attribution TypeCheck{constrain} (_, AST.VariableLHS name) (AG.Inherited env, _) =
    (AG.Synthesized $ Success (name, Map.singleton name (AST.TypeVariable $ freshTV env), constrain.empty),
     AST.VariableLHS name)

instance (Abstract.Haskell l,
          Abstract.Name l ~ AST.Name l) =>
         AG.At (TypeCheck l pos s con) (AST.FunctionalDependency l l) where
  attribution TypeCheck{constrain} (_, AST.FunctionalDependency lhs rhs) (AG.Inherited env, _) =
    (AG.Synthesized $ Success (), AST.FunctionalDependency lhs rhs)

instance (Abstract.Haskell l,
          Abstract.Name l ~ AST.Name l,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.TypeVarBinding l ~ AST.TypeVarBinding l,
          Abstract.Type l ~ AST.Type l) =>
         AG.At (TypeCheck l pos s con) (AST.FieldDeclaration l l) where
  attribution TypeCheck{} (_, AST.ConstructorFields names ty) (AG.Inherited env, AST.ConstructorFields _ tySyn) =
    (AG.Synthesized $ flip Map.fromSet (Set.fromList $ toList names) . const <$> AG.syn tySyn,
     AST.ConstructorFields names $ AG.Inherited env)

instance (Abstract.Haskell l,
          Abstract.Name l ~ AST.Name l) =>
         AG.At (TypeCheck l pos s con) (AST.DerivingClause l l) where
  attribution TypeCheck{} (_, AST.SimpleDerive name) _ = (AG.Synthesized $ Success (), AST.SimpleDerive name)

instance (Abstract.Haskell l,
          Abstract.Name l ~ AST.Name l,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.Type l ~ AST.Type l) =>
         AG.At (TypeCheck l pos s con) (AST.DerivingStrategy l l) where
  attribution TypeCheck{} (_, AST.Default) _ = (AG.Synthesized $ Success (), AST.Default)

instance (Abstract.Haskell l,
          Abstract.Name l ~ AST.Name l,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.TypeVarBinding l ~ AST.TypeVarBinding l,
          Abstract.Type l ~ AST.Type l,
          Abstract.Context l ~ AST.Context l,
          Abstract.Expression l ~ AST.Expression l,
          Abstract.GuardedExpression l ~ AST.GuardedExpression l) =>
         AG.At (TypeCheck l pos s con) (AST.EquationRHS l l) where
  attribution TypeCheck{constrain} (_, AST.NormalRHS{}) (AG.Inherited env, AST.NormalRHS (AG.Synthesized bodySyn)) =
    (AG.Synthesized bodySyn, AST.NormalRHS $ AG.Inherited env)
  attribution
    TypeCheck{constrain, extensions}
    (_, AST.GuardedRHS guardeds)
    (AG.Inherited env, AST.GuardedRHS guardedSyns)
    =
    (AG.Synthesized $ collapse <$> traverse AG.syn guardedSyns,
     AST.GuardedRHS
     $ AG.Inherited . (,) constrainToBool <$> liftA2 forkFresh (ZipNonEmpty $ 'a' :| ['b'..]) (env <$ guardeds))
     where
       collapse (ZipNonEmpty ((t1, con1) :| tyCons)) =
         (t1, foldr (\(ty, con) cons-> constrain.union (constrain.unify t1 ty) $ constrain.union con cons) con1 tyCons)
       constrainToBool _env mlt rt = constrain.unify (fromMaybe (preludeType extensions "Bool") mlt) rt

instance (Abstract.Haskell l,
          Abstract.Name l ~ AST.Name l,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.Statement l ~ AST.Statement l,
          Abstract.Expression l ~ AST.Expression l) =>
         AG.At (TypeCheck l pos s con) (AST.GuardedExpression l l) where
  attribution
    TypeCheck{constrain}
    (_, AST.GuardedExpression guards _)
    (AG.Inherited (statConBuilder, env), AST.GuardedExpression (ZipList guardSyns) (AG.Synthesized bodySyn))
    =
    (AG.Synthesized $ combine <$> bodySyn <*> guardsCon,
     AST.GuardedExpression
       (AG.Inherited . (,) statConBuilder <$> liftA2 forkFresh (ZipList ['a' ..]) (ZipList guardEnvs))
       (AG.Inherited $ forkFresh 'x' bodyEnv))
    where combine (bodyType, bodyCon) (Endo appGuardsCon) = (bodyType, appGuardsCon bodyCon)
          Ap guardsCon = foldMap (Ap . fmap (Endo . constrain.union . snd) . AG.syn) guardSyns
          bodyEnv :| guardEnvs = NonEmpty.reverse $ NonEmpty.scanl carry env guardSyns
          carry prevEnv (AG.Synthesized (Success (guardBindings, _))) = extendWith guardBindings prevEnv
          carry prevEnv _ = prevEnv

instance (Abstract.Haskell l,
          Abstract.Name l ~ AST.Name l,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.Declaration l ~ AST.Declaration l,
          Abstract.Pattern l ~ AST.Pattern l,
          Abstract.Expression l ~ AST.Expression l) =>
         AG.At (TypeCheck l pos s con) (AST.Statement l l) where
  attribution TypeCheck{constrain, extensions} (_, AST.ExpressionStatement _)
    (AG.Inherited (buildCon, env), AST.ExpressionStatement (AG.Synthesized bodySyn))
    =
    (AG.Synthesized $ addConstraints <$> bodySyn, AST.ExpressionStatement (AG.Inherited env))
    where addConstraints (t, con) = (mempty, constrain.union con $ buildCon env Nothing t)

instance (Abstract.Haskell l,
          Abstract.Name l ~ AST.Name l,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.CaseAlternative l ~ AST.CaseAlternative l,
          Abstract.Constructor l ~ AST.Constructor l,
          Abstract.Context l ~ AST.Context l,
          Abstract.GuardedExpression l ~ AST.GuardedExpression l,
          Abstract.Expression l ~ AST.Expression l,
          Abstract.Value l ~ AST.Value l,
          Abstract.Pattern l ~ AST.Pattern l,
          Abstract.Type l ~ AST.Type l,
          Abstract.TypeVarBinding l ~ AST.TypeVarBinding l) =>
         AG.At (TypeCheck l pos s con) (AST.Expression l l) where
  attribution TypeCheck{constrain} (i, AST.ReferenceExpression var) (AG.Inherited env, _) =
    (AG.Synthesized $ placeError i $ referenceAttribution constrain env var, AST.ReferenceExpression var)
  attribution TypeCheck{} (i, AST.LiteralExpression{}) (AG.Inherited env, AST.LiteralExpression valueSyn) =
    (AG.Synthesized $ AG.syn valueSyn, AST.LiteralExpression $ AG.Inherited env)
  attribution
    TypeCheck{constrain=ConstraintHandler{unify, union}}
    (_, AST.ApplyExpression{})
    (AG.Inherited env, AST.ApplyExpression (AG.Synthesized tc1) (AG.Synthesized tc2))
    =
    (AG.Synthesized $ liftA2 apply tc1 tc2,
     AST.ApplyExpression (AG.Inherited $ forkFresh 'f' env) (AG.Inherited $ forkFresh 'a' env))
    where apply :: (AST.Type l l Identity Identity, con)
                -> (AST.Type l l Identity Identity, con)
                -> (AST.Type l l Identity Identity, con)
          apply (t1, c1) (t2, c2) =
            (var, c1 `union` c2 `union` unify t1 (AST.FunctionType (Identity t2) (Identity var)))
          var = AST.TypeVariable (freshTV env)
  attribution
    TypeCheck{constrain}
    (i, AST.CaseExpression _ cases)
    (AG.Inherited env,
     AST.CaseExpression (AG.Synthesized scrutineeSyn) caseSyns) =
    (AG.Synthesized $ (,) caseType <$> con,
     AST.CaseExpression
      (AG.Inherited $ forkFresh 's' env)
      (AG.Inherited . flip forkFresh env <$> (ZipList ['a' ..] <* cases)))
    where caseType = AST.TypeVariable $ freshTV env
          con = liftA2 constrain.union (snd <$> scrutineeSyn) $
            foldr constrain.union constrain.empty <$> traverse caseConstraints caseSyns
          caseConstraints (AG.Synthesized caseSyn) =
            forA2 scrutineeSyn caseSyn $ \(scrutineeType, _) (lhsTy, rhsTy, caseCon)->
              constrain.union (constrain.unify scrutineeType lhsTy)
              $ constrain.union (constrain.unify caseType rhsTy) caseCon
  attribution
    TypeCheck{constrain, extensions}
    (i, AST.ConditionalExpression{})
    (AG.Inherited env,
     AST.ConditionalExpression (AG.Synthesized condSyn) (AG.Synthesized trueSyn) (AG.Synthesized falseSyn)) =
    (AG.Synthesized $ (,) . fst <$> trueSyn <*> con,
     AST.ConditionalExpression
      (AG.Inherited $ forkFresh 'c' env)
      (AG.Inherited $ forkFresh 't' env)
      (AG.Inherited $ forkFresh 'f' env))
    where con = foldr constrain.union constrain.empty <$> sequenceA [
            constrain.unify (preludeType extensions "Bool") . fst <$> condSyn,
            liftA2 constrain.unify (fst <$> trueSyn) (fst <$> falseSyn),
            snd <$> condSyn,
            snd <$> trueSyn,
            snd <$> falseSyn]
  attribution TypeCheck{} (i, AST.ConstructorExpression{}) (AG.Inherited env, AST.ConstructorExpression consSyn) =
    (AG.Synthesized $ AG.syn consSyn, AST.ConstructorExpression $ AG.Inherited env)
  attribution
    TypeCheck{constrain, extensions}
    (_, AST.DoExpression{})
    (AG.Inherited env, AST.DoExpression (AG.Synthesized bodySyn))
    = (AG.Synthesized $ constrainBodyType <$> bodySyn, AST.DoExpression $ AG.Inherited (statementCon, env))
    where
      constrainBodyType (ty, con) =
        (ty,
         constrain.union con
         $ constrain.union (constrain.unify ty $ AST.TypeApplication mt $ Identity AST.TypeWildcard)
         $ constrain.fromContext (AST.ClassConstraint (preludeName extensions "Monad") mt))
      statementCon _ mlt rt = constrain.unify rt $ AST.TypeApplication mt $ Identity $ fromMaybe AST.TypeWildcard mlt
      mt = Identity $ AST.TypeVariable $ freshTV (forkFresh 'm' env)
  attribution
    TypeCheck{constrain}
    ((start, _, end), AST.LambdaExpression patterns _)
    (AG.Inherited env, AST.LambdaExpression patSyns (AG.Synthesized body))
    =
    (AG.Synthesized
     $ case nonEmpty patVarDuplicates
       of Just duplicates -> Failure $ pure (start, DuplicatePatternVariables duplicates)
          Nothing -> foldr (liftA2 abstract . AG.syn) body patSyns,
     AST.LambdaExpression (AG.Inherited <$> patEnvs) (AG.Inherited bodyEnv))
    where abstract :: (AST.Name l, Map (AST.Name l) (AST.Type l l Identity Identity), con)
                   -> (AST.Type l l Identity Identity, con)
                   -> (AST.Type l l Identity Identity, con)
          abstract (patVar, patBindings, patCon) (rhsType, rhsCon) =
            (AST.FunctionType (Identity $ patBindings Map.! patVar) (Identity rhsType),
             constrain.union patCon rhsCon)
          patEnvs = flip forkFresh env <$> (ZipNonEmpty ('a' :| ['b' ..]) <* patterns)
          bodyEnv = forkFresh 'x' $ extendWith patVarBindings env
          patVarBindings = foldMap (\(AG.Synthesized (Success (_, varBindings, _))) -> varBindings) patSyns
          patVarDuplicates =
            Map.keys $ Map.filter (> Sum 1)
            $ foldMap (\(AG.Synthesized (Success (_, varBindings, _))) -> Sum 1 <$ varBindings) patSyns
  attribution
    TypeCheck{constrain}
    (_, AST.TupleExpression items)
    (AG.Inherited env, AST.TupleExpression itemSyns)
    =
    (AG.Synthesized
     $ (,) . AST.TupleType . (Identity . fst <$>)
       <$> itemTypeCons
       <*> (foldr (constrain.union . snd) constrain.empty <$> itemTypeCons),
     AST.TupleExpression (AG.Inherited . flip forkFresh env <$> (ZipNonEmpty ('a' :| ['b' ..]) <* items)))
    where
      itemTypeCons = traverse AG.syn itemSyns
  attribution
    TypeCheck{constrain}
    (_, AST.TypedExpression{})
    (AG.Inherited env, AST.TypedExpression (AG.Synthesized eSyn) (AG.Synthesized tSyn))
    =
    (AG.Synthesized $ (,) <$> tSyn <*> (constrain.union . snd <$> eSyn <*> (constrain.unify . fst <$> eSyn <*> tSyn)),
     AST.TypedExpression (AG.Inherited $ forkFresh 'e' env) (AG.Inherited $ forkFresh 't' env))

instance (Abstract.Haskell l,
          Abstract.Name l ~ AST.Name l,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.Expression l ~ AST.Expression l) =>
         AG.At (TypeCheck l pos s con) (AST.FieldBinding l l) where
  attribution TypeCheck{} (_, AST.FieldBinding name _) (AG.Inherited env, AST.FieldBinding _ (AG.Synthesized valSyn)) =
    (AG.Synthesized $ valSyn <&> \(t, con)-> (name, t, con),
     AST.FieldBinding name $ AG.Inherited env)

instance (Abstract.Haskell l,
          Abstract.Name l ~ AST.Name l,
          Abstract.ModuleName l ~ AST.ModuleName l,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.Constructor l ~ AST.Constructor l,
          Abstract.Type l ~ AST.Type l) =>
         AG.At (TypeCheck l pos s con) (AST.Value l l) where
  attribution TypeCheck{constrain, extensions} (_, AST.IntegerLiteral n) (AG.Inherited env, _) =
    (AG.Synthesized $ Success (ty, con), AST.IntegerLiteral n)
    where ty = AST.TypeVariable $ freshTV env
          con = constrain.fromContext $ AST.ClassConstraint (preludeName extensions "Num") (Identity ty)
  attribution TypeCheck{constrain, extensions} (_, AST.FloatingLiteral r) (AG.Inherited env, _) =
    (AG.Synthesized $ Success (ty, con), AST.FloatingLiteral r)
    where ty = AST.TypeVariable $ freshTV env
          con = constrain.fromContext $ AST.ClassConstraint (preludeName extensions "Fractional") (Identity ty)
  attribution TypeCheck{constrain, extensions} (_, AST.CharLiteral c) _ =
    (AG.Synthesized $ Success (preludeType extensions "Char", constrain.empty), AST.CharLiteral c)
  attribution TypeCheck{constrain, extensions} (_, AST.StringLiteral s) (AG.Inherited env, _) =
    (AG.Synthesized $ Success (ty, con), AST.StringLiteral s)
    where (ty, con)
            | Map.findWithDefault False Extensions.OverloadedStrings extensions
            = (AST.TypeVariable $ freshTV env,
               constrain.fromContext $ AST.ClassConstraint (preludeName extensions "IsString") (Identity ty))
            | otherwise = (preludeType extensions "String", constrain.empty)

instance (Abstract.Haskell l,
          Abstract.Name l ~ AST.Name l,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.TypeVarBinding l ~ AST.TypeVarBinding l,
          Abstract.Type l ~ AST.Type l,
          Abstract.Context l ~ AST.Context l,
          Abstract.Expression l ~ AST.Expression l,
          Abstract.Pattern l ~ AST.Pattern l) =>
         AG.At (TypeCheck l pos s con) (AST.Pattern l l) where
  attribution TypeCheck{constrain} (_, AST.VariablePattern name) (AG.Inherited env, _) =
    (AG.Synthesized $ Success (name, Map.singleton name (AST.TypeVariable $ freshTV env), constrain.empty),
     AST.VariablePattern name)

instance (Abstract.Haskell l,
          Abstract.Name l ~ AST.Name l,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.Pattern l ~ AST.Pattern l) =>
         AG.At (TypeCheck l pos s con) (AST.FieldPattern l l) where
  attribution TypeCheck{} (_, AST.FieldPattern name _) (AG.Inherited env, AST.FieldPattern _ patSyn) =
    (AG.Synthesized $ replaceName <$> AG.syn patSyn, AST.FieldPattern name $ AG.Inherited env)
    where replaceName (_, bindings, con) = (name, bindings, con)

instance (Abstract.Haskell l,
          Abstract.Name l ~ AST.Name l,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.TypeVarBinding l ~ AST.TypeVarBinding l,
          Abstract.Type l ~ AST.Type l,
          Abstract.Context l ~ AST.Context l,
          Abstract.Expression l ~ AST.Expression l,
          Abstract.Pattern l ~ AST.Pattern l) =>
         AG.At (TypeCheck l pos s con) (AST.Constructor l l) where
  attribution TypeCheck{constrain} (i, AST.ConstructorReference name) (AG.Inherited env, _) =
    (AG.Synthesized $ placeError i $ referenceAttribution constrain env name, AST.ConstructorReference name)

instance (Abstract.Haskell l,
          Abstract.Name l ~ AST.Name l,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.TypeVarBinding l ~ AST.TypeVarBinding l,
          Abstract.Type l ~ AST.Type l,
          Abstract.Context l ~ AST.Context l,
          Abstract.Expression l ~ AST.Expression l,
          Abstract.Pattern l ~ AST.Pattern l) =>
         AG.At (TypeCheck l pos s con) (AST.DataConstructor l l) where
  attribution TypeCheck{} (_, AST.Constructor name types) (AG.Inherited (resultType, env), AST.Constructor _ typeSyns) =
    (AG.Synthesized $ (,) name <$> constructorType, AST.Constructor name $ AG.Inherited env <$ types)
    where constructorType =
            foldr (\argType-> AST.FunctionType (Identity argType) . Identity) resultType
            <$> traverse AG.syn typeSyns

instance (Abstract.Haskell l,
          Abstract.Name l ~ AST.Name l,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.TypeVarBinding l ~ AST.TypeVarBinding l,
          Abstract.Type l ~ AST.Type l,
          Abstract.Context l ~ AST.Context l,
          Abstract.Expression l ~ AST.Expression l,
          Abstract.Pattern l ~ AST.Pattern l) =>
         AG.At (TypeCheck l pos s con) (AST.GADTConstructor l l) where
  attribution TypeCheck{constrain}
    (_, AST.GADTConstructors names vars context t)
    (AG.Inherited env, AST.GADTConstructors _ varSyns contextSyn typeSyn)
    =
    (AG.Synthesized $ flip Map.fromSet (Set.fromList $ toList names) . const <$> constructorType,
     AST.GADTConstructors names (AG.Inherited env <$ vars) (AG.Inherited env) (AG.Inherited env))
    where
      constructorType = AST.ForallType
                        <$> (traverse (fmap Identity . AG.syn) varSyns)
                        <*> (Identity
                             <$> (AST.ConstrainedType
                                  <$> (Identity . fst <$> AG.syn contextSyn)
                                  <*> (Identity <$> AG.syn typeSyn)))

instance (Abstract.Haskell l,
          Abstract.Name l ~ AST.Name l,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.TypeVarBinding l ~ AST.TypeVarBinding l,
          Abstract.Type l ~ AST.Type l,
          Abstract.Context l ~ AST.Context l,
          Abstract.Expression l ~ AST.Expression l,
          Abstract.Constructor l ~ AST.Constructor l,
          Abstract.Pattern l ~ AST.Pattern l) =>
         AG.At (TypeCheck l pos s con) (AST.Type l l) where
  attribution TypeCheck{} (_, AST.ConstructorType{}) (AG.Inherited env, AST.ConstructorType conSyn) =
    (AG.Synthesized $ fst <$> AG.syn conSyn, AST.ConstructorType $ AG.Inherited env)
  attribution TypeCheck{} (_, AST.FunctionConstructorType) _ =
    (AG.Synthesized $ Success AST.FunctionConstructorType, AST.FunctionConstructorType)
  attribution TypeCheck{} (_, AST.FunctionType{}) (env, AST.FunctionType lSyn rSyn) =
    (AG.Synthesized $ AST.FunctionType . Identity <$> AG.syn lSyn <*> (Identity <$> AG.syn rSyn),
     AST.FunctionType env env)
  attribution TypeCheck{constrain} (_, AST.TypeVariable name) (AG.Inherited env, _) =
    (AG.Synthesized $ case referenceAttribution constrain env (Binder.unqualifiedName name) of
        Success (t, con) -> Success t
        Failure _ -> Success $ AST.TypeVariable name,
     AST.TypeVariable name)

instance (Abstract.Haskell l,
          Abstract.Name l ~ AST.Name l,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.Type l ~ AST.Type l,
          Abstract.Kind l ~ AST.Type l,
          Abstract.Context l ~ AST.Context l) =>
         AG.At (TypeCheck l pos s con) (AST.TypeVarBinding l l) where
  attribution TypeCheck{} (_, AST.ImplicitlyKindedTypeVariable inf name) _ =
    (AG.Synthesized $ Success (AST.ImplicitlyKindedTypeVariable inf name), AST.ImplicitlyKindedTypeVariable inf name)

instance (Abstract.Haskell l,
          Abstract.Name l ~ AST.Name l,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.Type l ~ AST.Type l,
          Abstract.Context l ~ AST.Context l) =>
         AG.At (TypeCheck l pos s con) (AST.Context l l) where
  attribution TypeCheck{constrain} (_, AST.NoContext) _ =
    (AG.Synthesized $ Success (AST.NoContext, constrain.empty), AST.NoContext)

instance (Abstract.Haskell l,
          Abstract.Name l ~ AST.Name l,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.EquationRHS l ~ AST.EquationRHS l,
          Abstract.Pattern l ~ AST.Pattern l) =>
         AG.At (TypeCheck l pos s con) (AST.LambdaCasesAlternative l l) where
  attribution TypeCheck{constrain}
    (input, AST.LambdaCasesAlternative sup args body)
    (AG.Inherited env, AST.LambdaCasesAlternative _ argSyns (AG.Synthesized bodySyn))
    =
    (AG.Synthesized $ combine <$> bodySyn <*> argsSyn <* argBindingsGuard,
     AST.LambdaCasesAlternative sup argEnvs (AG.Inherited bodyEnv))
    where
      argsSyn = traverse AG.syn argSyns
      combine (bodyType, bodyCon) = foldr collect ([], mempty, bodyType, bodyCon)
      collect (patName, bindings, patCon) (patNames, bindingses, bodyType, con) =
        (patName : patNames, bindings <> bindingses, bodyType, constrain.union patCon con)
      argEnvs = AG.Inherited . (`forkFresh` env) <$> (ZipList ['a' ..] <* args)
      bodyEnv = forkFresh 'x'  $ extendWith argBindings env
      (conflicts, argBindings) = foldr addBindings mempty (Compose argsSyn)
      addBindings (_, bindings, _) (conflicts, allBindings) =
        (Map.intersection bindings allBindings `Map.union` conflicts, Map.union bindings allBindings)
      argBindingsGuard =
        maybe (Success ()) (placeError input . Failure . DuplicatePatternVariables) $ nonEmpty (Map.keys conflicts)

instance (Abstract.Haskell l,
          Abstract.Name l ~ AST.Name l,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.TypeVarBinding l ~ AST.TypeVarBinding l,
          Abstract.Type l ~ AST.Type l,
          Abstract.Context l ~ AST.Context l,
          Abstract.Expression l ~ AST.Expression l,
          Abstract.Pattern l ~ AST.Pattern l,
          Abstract.EquationRHS l ~ AST.EquationRHS l,
          Abstract.Declaration l ~ AST.Declaration l) =>
         AG.At (TypeCheck l pos s con) (AST.CaseAlternative l l) where
  attribution TypeCheck{constrain}
    (_, AST.CaseAlternative _ _ wheres)
    (AG.Inherited env, AST.CaseAlternative (AG.Synthesized lhsSyn) (AG.Synthesized rhsSyn) whereSyns)
    =
    (AG.Synthesized $ combineSyn <$> lhsSyn <*> rhsSyn <*> traverse AG.syn whereSyns,
     AST.CaseAlternative (AG.Inherited lhsEnv) (AG.Inherited rhsEnv) whereEnvs)
    where combineSyn (lhsName, lhsEnv, lhsCon) (rhsType, rhsCon) whereTC =
            (lhsEnv Map.! lhsName, rhsType, foldr (constrain.union . snd) (constrain.union lhsCon rhsCon) whereTC)
          lhsEnv = forkFresh 'x' env
          lhsBindings = foldMap (\(_, env, _)-> env) lhsSyn
          rhsEnv = forkFresh 'y' $ extendWith (lhsBindings <> foldMap (foldMap fst . AG.syn) whereSyns) env
          whereEnvs = AG.Inherited . (`forkFresh` extendWith lhsBindings env)
                      <$> (ZipList ['a' ..] <* wheres)

referenceAttribution :: (Abstract.Name l ~ AST.Name l,
                         Abstract.TypeVarBinding l ~ AST.TypeVarBinding l,
                         Abstract.Type l ~ AST.Type l,
                         Abstract.Context l ~ AST.Context l)
                     => ConstraintHandler l con
                     -> TypeEnv l Identity con
                     -> AST.QualifiedName l
                     -> Validation (TypeError l con) (AST.Type l l Identity Identity, con)
referenceAttribution ConstraintHandler{empty, fromContext, replaceVar} env name =
    case Map.lookup name (bindings env) of
      Just t -> Success $ concrete t
      Nothing -> Failure $ UnknownTypeVariable name
    where concrete (AST.ForallType vars (Identity (AST.ConstrainedType (Identity context) (Identity body)))) =
            foldr (replace . runIdentity) (body, fromContext context) vars
          concrete t = (t, empty)
          replace (AST.ExplicitlyKindedTypeVariable _ name _) typeCon = replaceFresh replaceVar env name typeCon
          replace (AST.ImplicitlyKindedTypeVariable _ name) typeCon = replaceFresh replaceVar env name typeCon
          replace AST.WildcardTypeBinding typeCon = typeCon
          replace AST.ExplicitlyKindedWildcardTypeBinding{} typeCon = typeCon

placeError :: (pos, s, pos) -> Validation (TypeError l con) a -> Validation (TypeErrors l pos con) a
placeError (pos, _, _) (Failure err) = Failure $ (pos, err) :| []
placeError _ (Success a) = Success a

extendWith :: (Abstract.Haskell l, Abstract.Name l ~ AST.Name l, Abstract.QualifiedName l ~ AST.QualifiedName l)
           => Map (AST.Name l) (AST.Type l l Identity Identity) -> TypeEnv l Identity con -> TypeEnv l Identity con
extendWith localEnv env@TypeEnv{bindings} =
  env{bindings = bindings <> Map.mapKeysMonotonic Binder.unqualifiedName localEnv}

replaceTypeVar :: AST.Name l -> AST.Name l -> AST.Type l l Identity Identity -> AST.Type l l Identity Identity
replaceTypeVar old new = undefined

preludeType :: Abstract.Haskell l => Map Extension Bool -> Text -> AST.Type l l Identity Identity
preludeType extensions = AST.ConstructorType . Identity . Abstract.constructorReference . preludeName extensions

preludeName :: Abstract.Haskell l => Map Extension Bool -> Text -> Abstract.QualifiedName l
preludeName extensions =
  (if Map.findWithDefault False Extensions.RebindableSyntax extensions then Binder.unqualifiedName
   else Abstract.qualifiedName (Just Binder.preludeName))
  . Abstract.name

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

forA2 :: Applicative f  => f a -> f b -> (a -> b -> c) -> f c
forA2 a b f = liftA2 f a b
