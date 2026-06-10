{-# Language DataKinds, DuplicateRecordFields, FlexibleContexts, FlexibleInstances, ImportQualifiedPost, LambdaCase,
             MultiParamTypeClasses, NamedFieldPuns, NoFieldSelectors, OverloadedRecordDot, OverloadedStrings,
             ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeFamilies, TypeOperators,
             UndecidableInstances #-}

-- | Type system, OutsideIn(X) formulated as an attribute grammar

module Language.Haskell.TypeSystem (
  checkExpression, checkModule,
  TypeMap(..), LocalTypeMap(..), TypeErrors, DefaultConstraints, defaultConstraintHandler) where

import Control.Applicative (ZipList(ZipList), liftA3)
import Data.Bifunctor (bimap, first)
import Data.Coerce (coerce)
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
import Data.Semigroup.Union (UnionWith(..))
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

import Language.Haskell.Binder (Binder(Binder))
import Language.Haskell.Binder qualified as Binder
import Language.Haskell.Extensions.Abstract qualified as Abstract
import Language.Haskell.Reserializer qualified as Reserializer
import Language.Haskell.Extensions as Extensions (Extension(OverloadedStrings, RebindableSyntax))
import Language.Haskell.Extensions.AST qualified as AST
import Language.Haskell.TypeSystem.Constraints (
  ConstraintHandler(..), DefaultConstraints, defaultConstraintHandler, TypeError(..), TypeErrors, TypeOrError(..))

checkModule :: (Abstract.Haskell l,
                Binder.BindingMembers l,
                Abstract.Name l ~ AST.Name l,
                Abstract.ModuleName l ~ AST.ModuleName l,
                Abstract.QualifiedName l ~ AST.QualifiedName l,
                Abstract.Module l ~ AST.Module l,
                Abstract.Export l ~ AST.Export l,
                Abstract.Import l ~ AST.Import l,
                Abstract.ImportSpecification l ~ AST.ImportSpecification l,
                Abstract.ImportItem l ~ AST.ImportItem l,
                Abstract.Declaration l ~ AST.Declaration l,
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
            => ConstraintHandler l pos con
            -> Map Extension Bool
            -> Binder.ModuleEnvironment l
            -> Binder.Environment l
            -> Map (AST.ModuleName l) (LocalTypeMap l Identity con)
            -> TypeMap l Identity con
            -> Wrap l pos s (AST.Module l l (Wrap l pos s) (Wrap l pos s))
            -> (Either (TypeErrors l pos con) (LocalTypeMap l Identity con), Binder.LocalEnvironment l)
checkModule constrain extensions binderModuleEnv binderEnv moduleBindings bindings m =
  bimap validationToEither snd $ AG.syn
  $ (transformation Full.<$> m) Rank2.$ AG.Inherited ((moduleBindings, env), (extensions, binderEnv))
  where env = TypeEnv{
          bindings,
          freshVarPrefix = "",
          constraints = constrain.empty}
        transformation = AG.Knit (TypeCheck{constrain, extensions}, AG.Auto $ Binder binderModuleEnv)

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
                => ConstraintHandler l pos con
                -> Map Extension Bool
                -> Map (AST.QualifiedName l) (AST.Type l l Identity Identity)
                -> Map (AST.QualifiedName l) (AST.Type l l Identity Identity)
                -> Wrap l pos s (AST.Expression l l (Wrap l pos s) (Wrap l pos s))
                -> Either (TypeErrors l pos con) (AST.Type l l Identity Identity)
checkExpression constrain extensions typeBindings valueBindings e =
  fmap constrainType $ validationToEither $ AG.syn $ (transformation Full.<$> e) Rank2.$ AG.Inherited env
  where env = TypeEnv{
          bindings = TypeMap{
              typeBindings,
              valueBindings = Success <$> valueBindings},
          freshVarPrefix = "a",
          constraints = constrain.empty}
        transformation = AG.Knit TypeCheck{constrain, extensions}
        constrainType (t, con) = AST.ConstrainedType (Identity $ fst $ constrain.toContext con) (Identity t)

-- | Transformation for checking and inference of types. The @con@ parameter is for constraints.
data TypeCheck l pos s con = TypeCheck{
  constrain :: ConstraintHandler l pos con,
  extensions :: Map Extension Bool}

data TypeMap l f con = TypeMap{
  typeBindings :: Map (AST.QualifiedName l) (AST.Type l l f f),
  valueBindings :: Map (AST.QualifiedName l) (Validation (TypeError l con) (AST.Type l l f f))}

instance Semigroup (TypeMap l f con) where
  m1 <> m2 = TypeMap{
    typeBindings = m1.typeBindings <> m2.typeBindings,
    valueBindings = m1.valueBindings <> m2.valueBindings}

instance Monoid (TypeMap l f con) where
  mempty = TypeMap{
    typeBindings = mempty,
    valueBindings = mempty}

data LocalTypeMap l f con = LocalTypeMap{
  typeBindings :: Map (AST.Name l) (AST.Type l l f f),
  valueBindings :: Map (AST.Name l) (Validation (TypeError l con) (AST.Type l l f f))}

instance Semigroup (LocalTypeMap l f con) where
  m1 <> m2 = LocalTypeMap{
    typeBindings = m1.typeBindings <> m2.typeBindings,
    valueBindings = m1.valueBindings <> m2.valueBindings}

instance Monoid (LocalTypeMap l f con) where
  mempty = LocalTypeMap{
    typeBindings = mempty,
    valueBindings = mempty}

-- | The type environment maps variables to their types
data TypeEnv l f con = TypeEnv{
  freshVarPrefix :: String,
  bindings :: TypeMap l f con,
  constraints :: con}

type Wrap l pos s = Reserializer.Wrapped pos s

-- | Pair of local type and value bindings
type LocalBindings l con = (Map (AST.Name l) (AST.Type l l Identity Identity),
                            Map (AST.Name l) (Validation (TypeError l con) (AST.Type l l Identity Identity)))

instance AG.Attribution (TypeCheck l pos s con) where
  type Origin (TypeCheck l pos s con) = Wrap l pos s

type instance AG.Atts (AG.Inherited (TypeCheck l pos s con)) g = InhAtts l pos s con g
type instance AG.Atts (AG.Synthesized (TypeCheck l pos s con)) g = SynAtts l pos s con g

type family InhAtts l pos s con (g :: (Type -> Type) -> (Type -> Type) -> Type) where
  InhAtts l pos s con (AST.Module l l) = (Map (AST.ModuleName l) (LocalTypeMap l Identity con), TypeEnv l Identity con)
  InhAtts l pos s con (AST.Import l l) = Map (AST.ModuleName l) (LocalTypeMap l Identity con)
  InhAtts l pos s con (AST.ImportSpecification l l) = ()
  InhAtts l pos s con (AST.ImportItem l l) = ()
  InhAtts l pos s con (AST.DataConstructor l l) = (AST.Type l l Identity Identity, TypeEnv l Identity con)
  InhAtts l pos s con (AST.GuardedExpression l l) = (StatementConstraintBuilder l pos con, TypeEnv l Identity con)
  InhAtts l pos s con (AST.Statement l l) = (StatementConstraintBuilder l pos con, TypeEnv l Identity con)
  InhAtts l pos s con _ = TypeEnv l Identity con

type family SynAtts l pos s con g where
  SynAtts l pos s con (AST.Module l l) = Validation (TypeErrors l pos con) (LocalTypeMap l Identity con)
  SynAtts l pos s con (AST.Export l l) = LocalTypeMap l Identity con
  SynAtts l pos s con (AST.Import l l) = TypeMap l Identity con
  SynAtts l pos s con (AST.ImportSpecification l l) = ()
  SynAtts l pos s con (AST.ImportItem l l) = ()
  SynAtts l pos s con (AST.Expression l l) = Validation (TypeErrors l pos con) (AST.Type l l Identity Identity, con)
  SynAtts l pos s con (AST.Value l l) = Validation (TypeErrors l pos con) (AST.Type l l Identity Identity, con)
  SynAtts l pos s con (AST.FieldBinding l l) =
    Validation (TypeErrors l pos con) (AST.QualifiedName l, AST.Type l l Identity Identity, con)
  SynAtts l pos s con (AST.Constructor l l) = Validation (TypeErrors l pos con) (AST.Type l l Identity Identity, con)
  SynAtts l pos s con (AST.Pattern l l) = (AST.Name l, LocalBindings l con, con)
  SynAtts l pos s con (AST.PatternEquationLHS l l) = (LocalBindings l con, con)
  SynAtts l pos s con (AST.PatternEquationClause l l) = Validation (TypeErrors l pos con) (LocalBindings l con, con)
  SynAtts l pos s con (AST.FieldPattern l l) = (AST.QualifiedName l, LocalBindings l con, con)
  SynAtts l pos s con (AST.GuardedExpression l l) =
    Validation (TypeErrors l pos con) (AST.Type l l Identity Identity, con)
  SynAtts l pos s con (AST.LambdaCasesAlternative l l) =
    Validation (TypeErrors l pos con) ([AST.Name l], LocalBindings l con, AST.Type l l Identity Identity, con)
  SynAtts l pos s con (AST.CaseAlternative l l) =
    Validation (TypeErrors l pos con) (AST.Type l l Identity Identity, AST.Type l l Identity Identity, con)
  SynAtts l pos s con (AST.Statement l l) = Validation (TypeErrors l pos con) (LocalBindings l con, con)
  SynAtts l pos s con (AST.EquationLHS l l) = (AST.Name l, LocalBindings l con, con)
  SynAtts l pos s con (AST.EquationRHS l l) = Validation (TypeErrors l pos con) (AST.Type l l Identity Identity, con)
  SynAtts l pos s con (AST.Declaration l l) = (LocalBindings l con, con)
  SynAtts l pos s con (AST.FieldDeclaration l l) = LocalBindings l con
  SynAtts l pos s con (AST.DataConstructor l l) =
    Validation (TypeErrors l pos con) (AST.Name l, AST.Type l l Identity Identity)
  SynAtts l pos s con (AST.GADTConstructor l l) = LocalBindings l con
  SynAtts l pos s con (AST.Type l l) = Validation (TypeErrors l pos con) (AST.Type l l Identity Identity)
  SynAtts l pos s con (AST.TypeVarBinding l l) =
    Validation (TypeErrors l pos con) (AST.TypeVarBinding l l Identity Identity)
  SynAtts l pos s con (AST.Context l l) = Validation (TypeErrors l pos con) (AST.Context l l Identity Identity, con)
  SynAtts l pos s con _ = Validation (TypeErrors l pos con) ()

type StatementConstraintBuilder l pos con =
  TypeEnv l Identity con -> Maybe (AST.Type l l Identity Identity) -> AST.Type l l Identity Identity -> con

instance {-# OVERLAPS #-} (
  Abstract.Haskell l, Binder.BindingMembers l, Abstract.Name l ~ AST.Name l,
  Abstract.ModuleName l ~ AST.ModuleName l,
  Abstract.QualifiedName l ~ AST.QualifiedName l
  ) => AG.At (TypeCheck l pos s con, AG.Auto (Binder l (Wrap l pos s))) (AST.Export l l) where
  attribution (TypeCheck{}, b) node@(_, x) (AG.Inherited (TypeEnv{bindings}, binds), expSyn) =
    (AG.Synthesized (Map.foldMapWithKey replicate binderExports, binderSyn), error "AST.Export node has no children")
    where (AG.Synthesized binderSyn@(UnionWith binderExports, _), _) =
            AG.attribution b node (AG.Inherited binds, AG.Synthesized . snd . AG.syn Rank2.<$> expSyn)
          replicate name Binder.TypeBinding{} = LocalTypeMap{
            typeBindings = foldMap (Map.singleton $ Binder.baseName name) (Map.lookup name bindings.typeBindings),
            valueBindings = mempty}
          replicate name Binder.ValueBinding{} = LocalTypeMap{
            typeBindings = mempty,
            valueBindings = foldMap (Map.singleton $ Binder.baseName name) (Map.lookup name bindings.valueBindings)}
          replicate name Binder.PatternBinding{} = LocalTypeMap{
            typeBindings = mempty,
            valueBindings = foldMap (Map.singleton $ Binder.baseName name) (Map.lookup name bindings.valueBindings)}
          replicate name _ = LocalTypeMap{
            typeBindings = foldMap (Map.singleton $ Binder.baseName name) (Map.lookup name bindings.typeBindings),
            valueBindings = foldMap (Map.singleton $ Binder.baseName name) (Map.lookup name bindings.valueBindings)}

instance {-# OVERLAPS #-} (
  Abstract.Haskell l,
  Abstract.Name l ~ AST.Name l, Abstract.ModuleName l ~ AST.ModuleName l,
  Abstract.QualifiedName l ~ AST.QualifiedName l,
  Abstract.ImportSpecification l ~ AST.ImportSpecification l
  ) => AG.At (TypeCheck l pos s con, AG.Auto (Binder l (Wrap l pos s))) (AST.Import l l) where
  attribution (t1, t2) x@(_, AST.Import safe qualified package name alias spec) (AG.Inherited (i1, i2), s) =
    (AG.Synthesized $ crossSyn s1 s2, Rank2.liftA2 pairInh i1' i2')
    where (AG.Synthesized s1, i1') = AG.attribution t1 x (AG.Inherited i1, AG.Synthesized . fst . AG.syn Rank2.<$> s)
          (AG.Synthesized s2, i2') = AG.attribution t2 x (AG.Inherited i2, AG.Synthesized . snd . AG.syn Rank2.<$> s)
          pairInh (AG.Inherited inh1) (AG.Inherited inh2) = AG.Inherited (inh1, inh2)
          crossSyn _ bsyn@((isPrelude, binderEnv), _) = (intersect (Map.findWithDefault mempty name i1) binderEnv, bsyn)
          intersect LocalTypeMap{typeBindings, valueBindings} (UnionWith boundEnv) =
            Map.foldMapWithKey importBinding boundEnv
            where importBinding name Binder.TypeBinding{} = TypeMap{
                    typeBindings= foldMap (Map.singleton name) (Map.lookup (Binder.baseName name) typeBindings),
                    valueBindings= mempty}
                  importBinding name Binder.ValueBinding{} = TypeMap{
                    typeBindings= mempty,
                    valueBindings= foldMap (Map.singleton name) (Map.lookup (Binder.baseName name) valueBindings)}
                  importBinding name Binder.PatternBinding{} = TypeMap{
                    typeBindings= mempty,
                    valueBindings= foldMap (Map.singleton name) (Map.lookup (Binder.baseName name) valueBindings)}
                  importBinding name _ = TypeMap{
                    typeBindings= foldMap (Map.singleton name) (Map.lookup (Binder.baseName name) typeBindings),
                    valueBindings= foldMap (Map.singleton name) (Map.lookup (Binder.baseName name) valueBindings)}

instance (Abstract.Haskell l,
          Abstract.Name l ~ AST.Name l,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.Module l ~ AST.Module l,
          Abstract.Export l ~ AST.Export l,
          Abstract.Import l ~ AST.Import l,
          Abstract.Declaration l ~ AST.Declaration l,
          Abstract.Context l ~ AST.Context l,
          Abstract.Type l ~ AST.Type l) =>
         AG.At (TypeCheck l pos s con) (AST.Module l l) where
  attribution
    TypeCheck{constrain}
    (_, AST.AnonymousModule imports declarations)
    (AG.Inherited env, AST.AnonymousModule impSyns bodySyns)
    =
    (AG.Synthesized $ Success $ LocalTypeMap mempty
     $ Map.mapKeysMonotonic Binder.baseName (Map.filterWithKey (const . (== mainName)) topEnv.bindings.valueBindings),
     AST.AnonymousModule
      (AG.Inherited (fst env) <$ imports)
      (AG.Inherited topEnv <$ declarations))
    where topEnv = TypeEnv{
            bindings = foldMap AG.syn impSyns <> foldMap (solve . AG.syn) bodySyns,
            freshVarPrefix = mempty,
            constraints = constrain.empty}
          solve ((ts, vs), con) = TypeMap{
            typeBindings = Map.mapKeysMonotonic Abstract.unqualifiedName (constrainType <$> ts),
            valueBindings = Map.mapKeysMonotonic Abstract.unqualifiedName ((constrainType <$>) <$> vs)}
            where constrainType t = AST.ConstrainedType (Identity $ fst $ constrain.toContext con) (Identity t)
          mainName = Abstract.qualifiedName Nothing (Abstract.name "main")
  attribution
    TypeCheck{constrain}
    (_, AST.NamedModule name exports imports declarations)
    (AG.Inherited env, AST.NamedModule _ expSyns impSyns bodySyns)
    =
    (AG.Synthesized $ Success $ foldMap AG.syn $ Compose expSyns,
     AST.NamedModule name
      (getCompose $ AG.Inherited topEnv <$ Compose exports)
      (AG.Inherited (fst env) <$ imports)
      (AG.Inherited topEnv <$ declarations))
    where topEnv = TypeEnv{
            bindings = foldMap AG.syn impSyns <> foldMap (solve . AG.syn) bodySyns,
            freshVarPrefix = mempty,
            constraints = constrain.empty}
          solve ((ts, vs), con) = TypeMap{
            typeBindings = Map.mapKeysMonotonic Abstract.unqualifiedName (constrainType <$> ts),
            valueBindings = Map.mapKeysMonotonic Abstract.unqualifiedName ((constrainType <$>) <$> vs)}
            where constrainType t = AST.ConstrainedType (Identity $ fst $ constrain.toContext con) (Identity t)
          mainName = Abstract.qualifiedName @l Nothing (Abstract.name "main")
  attribution
    TypeCheck{constrain}
    (_, AST.ExtendedModule extensions _)
    (AG.Inherited env, AST.ExtendedModule _ bodySyn)
    =
    (bodySyn, AST.ExtendedModule extensions $ AG.Inherited env)

instance (Abstract.Haskell l, Abstract.Name l ~ AST.Name l,
          Abstract.ImportSpecification l ~ AST.ImportSpecification l) =>
         AG.At (TypeCheck l pos s con) (AST.Import l l) where
  attribution TypeCheck{} (_, AST.Import safe qualified package name alias spec) (AG.Inherited env, _) =
    -- The actual imports are done by the pair instance above
    (AG.Synthesized mempty, AST.Import safe qualified package name alias $ AG.Inherited mempty <$ spec)

instance (Abstract.Haskell l, Abstract.Name l ~ AST.Name l, Abstract.ImportItem l ~ AST.ImportItem l) =>
  AG.At (TypeCheck l pos s con) (AST.ImportSpecification l l) where
  attribution TypeCheck{} (_, AST.ImportSpecification hiding items) (AG.Inherited env, _) =
    (AG.Synthesized (), AST.ImportSpecification hiding $ AG.Inherited env <$ items)

instance (Abstract.Haskell l, Abstract.Name l ~ AST.Name l) => AG.At (TypeCheck l pos s con) (AST.ImportItem l l) where
  attribution TypeCheck{} (_, node) (AG.Inherited env, _) = (AG.Synthesized (), coerce node)

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
    (AG.Inherited env,
     AST.EquationDeclaration (AG.Synthesized lhsSyn) (AG.Synthesized rhsSyn) whereSyns)
    =
    (AG.Synthesized eqSyn,
     AST.EquationDeclaration (AG.Inherited lhsEnv) (AG.Inherited rhsEnv) whereEnvs)
    where eqSyn =
            ((typeBindings, valueBindings),
              conconcat constrain
              $ [lhsCon, rhsCon, constrain.assign lhsName rhsTypeOrError] ++ whereCons)
          ~(rhsTypeOrError, rhsCon) = case rhsSyn of
            Success (t, c) -> (ProperType t, c)
            Failure err -> (ErrorType err, constrain.empty)
          (lhsName, lhsBindings@(typeBindings, valueBindings), lhsCon) = lhsSyn
          ZipList whereCons = snd . AG.syn <$> whereSyns
          lhsEnv = forkFresh 'l' env
          rhsEnv = forkFresh 'r' $ extendWith (lhsBindings <> foldMap (fst . AG.syn) whereSyns) env
          whereEnvs = AG.Inherited . (`forkFresh` extendWith lhsBindings (forkFresh 'w' env))
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
    (AG.Synthesized $ foldr combine (mempty, constrain.empty) (AG.syn <$> argSyns),
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
    (AG.Inherited env,
     AST.PatternEquationClause _ (AG.Synthesized (lhsBindings, lhsCon)) (AG.Synthesized rhsSyn) whereSyns)
    =
    (AG.Synthesized $ combineSyn <$> rhsSyn <*> pure (AG.syn <$> whereSyns),
     AST.PatternEquationClause sup (AG.Inherited lhsEnv) (AG.Inherited rhsEnv) whereEnvs)
    where combineSyn (rhsType, rhsCon) whereTC =
            ({-Map.insert lhsName rhsType-} lhsBindings,
             foldr (constrain.union . snd) (constrain.union lhsCon rhsCon) whereTC)
          lhsEnv = forkFresh 'x' env
          rhsEnv = forkFresh 'y' $ extendWith (lhsBindings <> foldMap (fst . AG.syn) whereSyns) env
          whereEnvs = AG.Inherited . (`forkFresh` extendWith lhsBindings env)
                      <$> (ZipList ['a' ..] <* wheres)

instance (Abstract.Haskell l,
          Abstract.Name l ~ AST.Name l,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.TypeVarBinding l ~ AST.TypeVarBinding l,
          Abstract.Context l ~ AST.Context l,
          Abstract.EquationLHS l ~ AST.EquationLHS l,
          Abstract.Pattern l ~ AST.Pattern l) =>
         AG.At (TypeCheck l pos s con) (AST.EquationLHS l l) where
  attribution TypeCheck{constrain} (_, AST.VariableLHS name) (AG.Inherited env, _) =
    (AG.Synthesized (tv, (Map.singleton tv varType, Map.singleton name $ Success varType), constrain.empty),
     AST.VariableLHS name)
    where tv = freshTV env
          varType = AST.TypeVariable tv
  attribution TypeCheck{constrain} (_, AST.PatternLHS{}) (AG.Inherited env, AST.PatternLHS (AG.Synthesized patSyn)) =
    (AG.Synthesized patSyn, AST.PatternLHS $ AG.Inherited env)
  attribution
    TypeCheck{constrain}
    (_, AST.PrefixLHS _ args)
    (AG.Inherited env, AST.PrefixLHS (AG.Synthesized funSyn) argSyns)
    =
    (AG.Synthesized $ collect funSyn (AG.syn <$> argSyns),
     AST.PrefixLHS (AG.Inherited $ forkFresh 'p' env) argEnvs)
    where
      argEnvs = AG.Inherited . flip forkFresh env <$> (ZipNonEmpty ('a' :| ['b'..])) <* args
      collect (name, prefixBindings, prefixCon) argSyns'
        | let (_, argBindings, argCons) = unzip3 $ toList argSyns'
        = (tv,
           mconcat $ (Map.singleton tv varType, Map.singleton name $ Success varType) : prefixBindings : argBindings,
           conconcat constrain $ prefixCon : argCons)
      tv = freshTV env
      varType = AST.TypeVariable tv
  attribution
    TypeCheck{constrain}
    (_, AST.InfixLHS{})
    (AG.Inherited env, AST.InfixLHS (AG.Synthesized lSyn) name (AG.Synthesized rSyn))
    =
    (AG.Synthesized $ combine lSyn rSyn,
     AST.InfixLHS (AG.Inherited $ forkFresh 'l' env) name (AG.Inherited $ forkFresh 'r' env))
    where
      combine (_, lBind, lCon) (_, rBind, rCon) =
        (tv,
         (Map.singleton tv varType, Map.singleton name $ Success varType) <> lBind <> rBind,
         constrain.union lCon rCon)
      tv = freshTV env
      varType = AST.TypeVariable tv

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
    (AG.Synthesized $ (,) mempty $ Map.fromSet (const $ firstError $ AG.syn tySyn) (Set.fromList $ toList names),
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
          Abstract.Declaration l ~ AST.Declaration l,
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
            conconcat constrain <$> traverse caseConstraints caseSyns
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
    where con = conconcat constrain <$> sequenceA [
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
    (_, AST.InfixExpression{})
    (AG.Inherited env, AST.InfixExpression (AG.Synthesized lSyn) (AG.Synthesized opSyn) (AG.Synthesized rSyn))
    =
    (AG.Synthesized $ liftA3 apply lSyn opSyn rSyn,
     AST.InfixExpression
       (AG.Inherited $ forkFresh 'l' env)
       (AG.Inherited $ forkFresh 'o' env)
       (AG.Inherited $ forkFresh 'r' env))
    where
      apply (lT, lCon) (opT, opCon) (rT, rCon) =
        (resultType,
         conconcat constrain [
            constrain.unify opT (AST.FunctionType (Identity lT) $ Identity
                                 $ AST.FunctionType (Identity rT) (Identity resultType)),
            lCon, opCon, rCon])
      resultType = AST.TypeVariable $ freshTV env
  attribution
    TypeCheck{constrain}
    (i, AST.LeftSectionExpression{})
    (AG.Inherited env, AST.LeftSectionExpression (AG.Synthesized argSyn) op)
    =
    (AG.Synthesized $ liftA2 apply opLookup argSyn, AST.LeftSectionExpression (AG.Inherited $ forkFresh 'l' env) op)
    where
      apply opT (argT, argCon) =
        (resultType,
         constrain.union argCon $ constrain.unify opT $ AST.FunctionType (Identity argT) (Identity resultType))
      opLookup = placeError i $ maybe (Failure $ UnknownValue op) id (Map.lookup op env.bindings.valueBindings)
      resultType = AST.TypeVariable $ freshTV env
  attribution
    TypeCheck{constrain}
    (i, AST.RightSectionExpression{})
    (AG.Inherited env, AST.RightSectionExpression op (AG.Synthesized argSyn))
    =
    (AG.Synthesized $ liftA2 apply opLookup argSyn, AST.RightSectionExpression op (AG.Inherited $ forkFresh 'r' env))
    where
      apply opT (argT, argCon) =
        (resultType,
         constrain.union argCon $ constrain.unify opT
         $ AST.FunctionType (Identity leftArgType) $ Identity
         $ AST.FunctionType (Identity argT) (Identity resultType))
      opLookup = placeError i $ maybe (Failure $ UnknownValue op) id (Map.lookup op env.bindings.valueBindings)
      resultType = AST.TypeVariable $ freshTV $ forkFresh 'x' env
      leftArgType = AST.TypeVariable $ freshTV $ forkFresh 'l' env
  attribution
    TypeCheck{constrain}
    ((start, _, end), AST.LambdaExpression patterns _)
    (AG.Inherited env, AST.LambdaExpression patSyns (AG.Synthesized bodySyn))
    =
    (AG.Synthesized
     $ case nonEmpty patVarDuplicates
       of Just duplicates -> Failure $ pure (start, DuplicatePatternVariables duplicates)
          Nothing -> flip (foldr (abstract . AG.syn)) patSyns <$> bodySyn,
     AST.LambdaExpression (AG.Inherited <$> patEnvs) (AG.Inherited bodyEnv))
    where
      abstract :: (AST.Name l, LocalBindings l con, con)
               -> (AST.Type l l Identity Identity, con)
               -> (AST.Type l l Identity Identity, con)
      abstract (patVar, (typeBindings, _), patCon) (rhsType, rhsCon) =
        (AST.FunctionType (Identity $ typeBindings Map.! patVar) (Identity rhsType),
         constrain.union patCon rhsCon)
      patEnvs = flip forkFresh env <$> (ZipNonEmpty ('a' :| ['b' ..]) <* patterns)
      bodyEnv = forkFresh 'x' $ extendWith patVarBindings env
      patVarBindings = foldMap (\(AG.Synthesized (_, varBindings, _)) -> varBindings) patSyns
      patVarDuplicates =
        Map.keys $ Map.filter (> Sum 1)
        $ foldMap (\(AG.Synthesized (_, (_, varBindings), _)) -> Sum 1 <$ varBindings) patSyns
  attribution
    TypeCheck{constrain}
    ((start, _, end), AST.LetExpression bindings _)
    (AG.Inherited env, AST.LetExpression bindSyns (AG.Synthesized bodySyn))
    =
    (AG.Synthesized $ complete (snd . AG.syn <$> bindSyns) <$> bodySyn,
     AST.LetExpression (AG.Inherited <$> bindEnvs) (AG.Inherited bodyEnv))
    where
      complete (ZipList bindCons) (bodyType, bodyCon) = (bodyType, conconcat constrain $ bodyCon : bindCons)
      boundEnv = foldMap fst $ AG.syn <$> bindSyns
      env' = extendWith boundEnv env
      bindEnvs = flip forkFresh env' <$> (ZipList ['a' ..] <* bindings)
      bodyEnv = forkFresh 'x' env'
  attribution TypeCheck{constrain} (i, AST.ListExpression items) (AG.Inherited env, AST.ListExpression itemSyns) =
    (AG.Synthesized unified, AST.ListExpression $ AG.Inherited <$> itemEnvs)
    where
      itemEnvs = flip forkFresh env <$> (ZipList ['a' ..] <* items)
      itemType = AST.TypeVariable $ freshTV env
      itemTypeCons = traverse AG.syn itemSyns
      constrainItem (t, con) = constrain.union con $ constrain.unify t itemType
      unified = (,) (AST.ListType $ Identity itemType) . conconcat constrain . (constrainItem <$>) <$> itemTypeCons
  attribution TypeCheck{} (_, AST.LiteralExpression{}) (AG.Inherited env, AST.LiteralExpression valueSyn) =
    (AG.Synthesized $ AG.syn valueSyn, AST.LiteralExpression $ AG.Inherited env)
  attribution TypeCheck{constrain} (i, AST.ReferenceExpression var) (AG.Inherited env, _) =
    (AG.Synthesized $ placeError i $ valueReferenceAttribution constrain env var, AST.ReferenceExpression var)
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
    (AG.Synthesized (tv, (Map.singleton tv varType, Map.singleton name $ Success varType), constrain.empty),
     AST.VariablePattern name)
    where tv = freshTV env
          varType = AST.TypeVariable tv

instance (Abstract.Haskell l,
          Abstract.Name l ~ AST.Name l,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.Pattern l ~ AST.Pattern l) =>
         AG.At (TypeCheck l pos s con) (AST.FieldPattern l l) where
  attribution TypeCheck{} (_, AST.FieldPattern name _) (AG.Inherited env, AST.FieldPattern _ patSyn) =
    (AG.Synthesized $ replaceName $ AG.syn patSyn, AST.FieldPattern name $ AG.Inherited env)
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
    (AG.Synthesized $ placeError i $ valueReferenceAttribution constrain env name, AST.ConstructorReference name)
  attribution TypeCheck{constrain} (i, AST.EmptyListConstructor) (AG.Inherited env, _) =
    (AG.Synthesized $ Success (AST.ListType $ Identity $ AST.TypeVariable $ freshTV env, constrain.empty),
     AST.EmptyListConstructor)

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
    (AG.Synthesized $ (,) mempty $ Map.fromSet (const $ firstError constructorType) (Set.fromList $ toList names),
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
    (AG.Synthesized $ case typeReferenceAttribution constrain env (Abstract.unqualifiedName name) of
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
    (AG.Synthesized $ combine <$> bodySyn <*> Success argsSyn <* argBindingsGuard,
     AST.LambdaCasesAlternative sup argEnvs (AG.Inherited bodyEnv))
    where
      argsSyn = AG.syn <$> argSyns
      combine (bodyType, bodyCon) = foldr collect ([], mempty, bodyType, bodyCon)
      collect (patName, bindings, patCon) (patNames, bindingses, bodyType, con) =
        (patName : patNames, bindings <> bindingses, bodyType, constrain.union patCon con)
      argEnvs = AG.Inherited . (`forkFresh` env) <$> (ZipList ['a' ..] <* args)
      bodyEnv = forkFresh 'x'  $ extendWith argBindings env
      (conflicts, argBindings) = foldr addBindings mempty argsSyn
      addBindings (_, (typeBindings, valueBindings), _) (conflicts, (allTypeBindings, allValueBindings)) =
        (Map.intersection valueBindings allValueBindings `Map.union` conflicts,
         (Map.union typeBindings allTypeBindings, Map.union valueBindings allValueBindings))
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
    (AG.Synthesized $ combineSyn (AG.syn <$> whereSyns) lhsSyn <$> rhsSyn,
     AST.CaseAlternative (AG.Inherited lhsEnv) (AG.Inherited rhsEnv) whereEnvs)
    where combineSyn whereTC (lhsName, (lhsTypeVars, _), lhsCon) (rhsType, rhsCon) =
            (lhsTypeVars Map.! lhsName, rhsType, foldr (constrain.union . snd) (constrain.union lhsCon rhsCon) whereTC)
          lhsEnv = forkFresh 'x' env
          lhsBindings = (\(_, env, _)-> env) lhsSyn
          rhsEnv = forkFresh 'y' $ extendWith (lhsBindings <> foldMap (fst . AG.syn) whereSyns) env
          whereEnvs = AG.Inherited . (`forkFresh` extendWith lhsBindings env)
                      <$> (ZipList ['a' ..] <* wheres)

typeReferenceAttribution constrain = referenceAttribution False constrain
valueReferenceAttribution constrain = referenceAttribution True constrain
referenceAttribution :: (Abstract.Name l ~ AST.Name l,
                         Abstract.TypeVarBinding l ~ AST.TypeVarBinding l,
                         Abstract.Type l ~ AST.Type l,
                         Abstract.Context l ~ AST.Context l)
                     => Bool
                     -> ConstraintHandler l pos con
                     -> TypeEnv l Identity con
                     -> AST.QualifiedName l
                     -> Validation (TypeError l con) (AST.Type l l Identity Identity, con)
referenceAttribution isValue ConstraintHandler{empty, fromContext, replaceVar} env name
  | isValue =
    case Map.lookup name env.bindings.valueBindings of
      Just (Success t) -> Success $ concrete t
      Just Failure{} -> Failure $ UntypedValue name
      Nothing -> Failure $ UnknownValue name
  | otherwise =
    case Map.lookup name env.bindings.typeBindings of
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

firstError :: Validation (TypeErrors l pos con) a -> Validation (TypeError l con) a
firstError (Failure ((_, err) :| _)) = Failure err
firstError (Success a) = Success a


extendWith :: (Abstract.Haskell l, Abstract.Name l ~ AST.Name l, Abstract.QualifiedName l ~ AST.QualifiedName l)
           => LocalBindings l con -> TypeEnv l Identity con -> TypeEnv l Identity con
extendWith (types, values) env@TypeEnv{bindings = TypeMap{typeBindings, valueBindings}} =
  env{bindings = TypeMap{typeBindings = typeBindings <> Map.mapKeysMonotonic Abstract.unqualifiedName types,
                         valueBindings = valueBindings <> Map.mapKeysMonotonic Abstract.unqualifiedName values}}

replaceTypeVar :: AST.Name l -> AST.Name l -> AST.Type l l Identity Identity -> AST.Type l l Identity Identity
replaceTypeVar old new = undefined

preludeType :: Abstract.Haskell l => Map Extension Bool -> Text -> AST.Type l l Identity Identity
preludeType extensions = AST.ConstructorType . Identity . Abstract.constructorReference . preludeName extensions

preludeName :: Abstract.Haskell l => Map Extension Bool -> Text -> Abstract.QualifiedName l
preludeName extensions =
  (if Map.findWithDefault False Extensions.RebindableSyntax extensions then Abstract.unqualifiedName
   else Abstract.qualifiedName (Just Abstract.preludeName))
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

conconcat :: Foldable f => ConstraintHandler l pos con -> f con -> con
conconcat constrain = foldr constrain.union constrain.empty

forA2 :: Applicative f  => f a -> f b -> (a -> b -> c) -> f c
forA2 a b f = liftA2 f a b
