{-# Language DuplicateRecordFields, FlexibleContexts, FlexibleInstances, ImportQualifiedPost, LambdaCase,
             MultiParamTypeClasses, NamedFieldPuns, NoFieldSelectors, OverloadedRecordDot, OverloadedStrings,
             ScopedTypeVariables, StandaloneDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}

-- | The X part of OutsideIn(X), the constraints and their handler

module Language.Haskell.TypeSystem.Constraints (
  ConstraintCollection(..), DefaultConstraints, TypeError(..), TypeErrors,
  resolveTypeVariables, splitFromType) where

import Control.Applicative (ZipList(ZipList))
import Data.Bifunctor (first)
import Data.Foldable (fold, toList)
import Data.Functor ((<&>))
import Data.Functor.Compose (Compose(Compose, getCompose))
import Data.Functor.Identity (Identity(Identity))
import Data.List.NonEmpty (NonEmpty)
import Data.DisjointMap qualified as DJMap
import Data.DisjointMap (DisjointMap)
import Data.Map.Strict qualified as Map
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Set qualified as Set
import Transformation.Deep qualified as Deep
import Language.Haskell.Extensions.AST qualified as AST
import Language.Haskell.Abstract qualified as Abstract
import Language.Haskell.TypeSystem.Transformations (FreeVariableFold, freeVariables)

-- | Record of functions for handling constraints
class Monoid con => ConstraintCollection con where
  type Language con
  type Position con
  display :: con -> String
  fromContext :: AST.Context (Language con) (Language con) Identity Identity -> con
  toContext :: con -> (AST.Context (Language con) (Language con) Identity Identity, con)
  replaceVar :: AST.Name (Language con) -> AST.Name (Language con) -> con -> con
  filterRelevant :: Set (AST.Name (Language con)) -> con -> con
  canonicalNameMap :: con -> Map (AST.Name (Language con)) (AST.Name (Language con))
  renameTypeVariables :: Map (AST.Name (Language con)) (AST.Name (Language con)) -> con -> con
  simplify :: con  -- ^ global constraints
           -> con  -- ^ given constraints to rely on
           -> con  -- ^ wanted constraints to simplify
           -> (con, Map (AST.Name (Language con)) (AST.Type (Language con) (Language con) Identity Identity))
  unify :: AST.Type (Language con) (Language con) Identity Identity
        -> AST.Type (Language con) (Language con) Identity Identity
        -> con
  assignType :: AST.Name (Language con) -> AST.Type (Language con) (Language con) Identity Identity -> con
  assignError :: AST.Name (Language con) -> TypeErrors (Language con) (Position con) con -> con
  errors :: con -> [(Position con, TypeError (Language con) con)]

data TypeError l con
  = TypeMismatch (AST.Type l l Identity Identity) (AST.Type l l Identity Identity)
  | TypeAmbiguity con
  | DuplicatePatternVariables (NonEmpty (AST.Name l))
  | UndeclaredContext (AST.Name l) (AST.Context l l Identity Identity)
  | UnknownTypeVariable (AST.QualifiedName l)
  | UnknownValue (AST.QualifiedName l)
  | UntypedValue (AST.QualifiedName l)

deriving instance (Show (AST.Context l l Identity Identity),
                   Show (AST.Type l l Identity Identity), Show con) => Show (TypeError l con)

type TypeErrors l pos con = NonEmpty (pos, TypeError l con)

data DefaultConstraints l pos = DefaultConstraints{
  assignments :: DJMap.DisjointMap (AST.Name l) [AST.Type l l Identity Identity],
  equations :: [(AST.Type l l Identity Identity, AST.Type l l Identity Identity)],
  errors :: Map (AST.Name l) (TypeErrors l pos (DefaultConstraints l pos)),
  classes :: Map (AST.QualifiedName l) [AST.Type l l Identity Identity]}

deriving instance (Show (AST.Context l l Identity Identity),
                   Show (AST.Type l l Identity Identity), Show pos) => Show (DefaultConstraints l pos)

instance Semigroup (DefaultConstraints l pos) where
  x <> y = DefaultConstraints{
    assignments= x.assignments <> y.assignments,
    classes= Map.unionWith (<>) x.classes y.classes,
    equations = x.equations <> y.equations,
    errors = x.errors <> y.errors}

instance Monoid (DefaultConstraints l pos) where
  mempty = DefaultConstraints{assignments= DJMap.empty, equations= [], classes= Map.empty, errors= Map.empty}

instance (Show pos,
          Show (AST.Context l l Identity Identity),
          Show (AST.Type l l Identity Identity),
          Abstract.Name l ~ AST.Name l,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.Context l ~ AST.Context l,
          Abstract.Type l ~ AST.Type l,
          Deep.Foldable (FreeVariableFold l Identity) (AST.Type l l)) =>
         ConstraintCollection (DefaultConstraints l pos) where
  type Language (DefaultConstraints l pos) = l
  type Position (DefaultConstraints l pos) = pos
  display = show
  fromContext = \case
      AST.ClassConstraint name (Identity arg) -> mempty{classes= Map.singleton name [arg]}
      AST.Constraints cons -> foldMap fromContext (Compose cons)
      AST.NoContext -> mempty
  toContext = \DefaultConstraints{assignments, equations, classes}->
      case [AST.TypeEquality (Identity l) (Identity r)
           | (vars, types) <- DJMap.toLists assignments,
             let allTypes = map AST.TypeVariable vars ++ types
                 (ls, rs) = splitAt 1 allTypes,
             l <- ls,
             r <- rs]
           <> [AST.TypeEquality (Identity l) (Identity r) | (l, r) <- equations]
           <> [AST.ClassConstraint name (Identity arg) | (name, args) <- Map.toList classes, arg <- args]
      of [] -> (AST.NoContext, mempty)
         cons -> (AST.Constraints (ZipList $ Identity <$> cons), mempty)
  replaceVar = \from to DefaultConstraints{assignments, errors, equations, classes} ->
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
        assignments = getCompose $ replaceInType <$> Compose assignments,
        errors = errors,
        equations = equations <&> \(l, r)-> (replaceInType l, replaceInType r),
        classes = getCompose $ replaceInType <$> Compose classes}
  filterRelevant = filterDefaultRelevant
  canonicalNameMap = canonicalDefaultNameMap
  renameTypeVariables = renameTypeVariablesDefault
  -- TODO: actually simplify wanted, report contradictions
  simplify = \_ given wanted->
    (given <> wanted{assignments= mempty}, DJMap.foldlWithKeys' resolveAssignments mempty wanted.assignments)
  unify (AST.TypeVariable name) t = assignType name t
  unify t (AST.TypeVariable name) = assignType name t
  unify a b = mempty{equations= [(a, b)]}
  assignType v1 (AST.TypeVariable v2) = mempty{assignments= DJMap.union v1 v2 DJMap.empty}
  assignType var t = mempty{assignments= DJMap.singleton var [t]}
  assignError var err = mempty{errors= Map.singleton var err}
  errors DefaultConstraints{errors} = foldMap toList errors

canonicalDefaultNameMap :: DefaultConstraints l pos -> Map (AST.Name l) (AST.Name l)
canonicalDefaultNameMap con = foldMap (canonicalMap . fst) $ DJMap.toLists con.assignments
  where canonicalMap names@(name:_)
          | Just canonical <- DJMap.representative name con.assignments
          = Map.fromList [(n, canonical) | n <- names]

renameTypeVariablesDefault :: (Abstract.Name l ~ AST.Name l, Abstract.Type l ~ AST.Type l)
                           => Map (AST.Name l) (AST.Name l) -> DefaultConstraints l pos -> DefaultConstraints l pos
renameTypeVariablesDefault renamings con = DefaultConstraints{
  assignments = fold $ DJMap.fromSets $ renameAssignments <$> DJMap.toLists con.assignments,
  errors = con.errors,
  equations = con.equations <&> \(l, r)-> (renameInType l, renameInType r),
  classes = getCompose $ renameInType <$> Compose con.classes}
  where
    renameAssignments (name:_, types) = (foldMap Set.singleton $ Map.lookup name renamings, renameInType <$> types)
    renameInType = resolveTypeVariables typeRenamings
    typeRenamings = AST.TypeVariable <$> renamings

resolveAssignments :: Map (AST.Name l) (AST.Type l l Identity Identity)
                   -> Set (AST.Name l)
                   -> [AST.Type l l Identity Identity]
                   -> Map (AST.Name l) (AST.Type l l Identity Identity)
resolveAssignments assigned names types = case toList types of
  [] -> assigned
  [t] -> foldr (`Map.insert` t) assigned names

resolveTypeVariables :: (Abstract.Name l ~ AST.Name l, Abstract.Type l ~ AST.Type l)
                     => Map (AST.Name l) (AST.Type l l Identity Identity)
                     -> AST.Type l l Identity Identity
                     -> AST.Type l l Identity Identity
resolveTypeVariables bindings t@(AST.TypeVariable name) =
  maybe t (resolveTypeVariables bindings) (Map.lookup name bindings)
resolveTypeVariables bindings (AST.FunctionType l r) =
  AST.FunctionType (resolveTypeVariables bindings <$> l) (resolveTypeVariables bindings <$> r)
resolveTypeVariables _ t = t

splitFromType :: (Abstract.Context l ~ AST.Context l, Abstract.Type l ~ AST.Type l,
                  ConstraintCollection con, Language con ~ l)
              => AST.Type l l Identity Identity -> (con, AST.Type l l Identity Identity)
splitFromType (AST.ConstrainedType (Identity context) (Identity t)) = first (fromContext context <>) (splitFromType t)
splitFromType t = (mempty, t)

filterDefaultRelevant :: (Abstract.Name l ~ AST.Name l,
                          Deep.Foldable (FreeVariableFold l Identity) (AST.Type l l))
                      => Set (AST.Name l) -> DefaultConstraints l pos -> DefaultConstraints l pos
filterDefaultRelevant vars con = DefaultConstraints{
  assignments = fold $ DJMap.fromSets $ filter relevantAssignment $ DJMap.toSets con.assignments,
  errors = con.errors,
  equations = filter relevantEquation $ con.equations,
  classes = Map.filter relevantClassParams $ con.classes}
  where
    relevantAssignment (keys, tys) = hasRelevant keys || any isRelevantType tys
    relevantEquation (l, r) = isRelevantType l || isRelevantType r
    relevantClassParams = any isRelevantType
    isRelevantType = hasRelevant . freeVariables
    hasRelevant = not . Set.disjoint vars

constraintRelatedTypeVarSetClosure :: (Abstract.Name l ~ AST.Name l,
                                       Deep.Foldable (FreeVariableFold l Identity) (AST.Type l l))
                                   => DefaultConstraints l pos -> Set (AST.Name l) -> Set (AST.Name l)
constraintRelatedTypeVarSetClosure DefaultConstraints{assignments, equations} =
  relatedSetClosure $ foldr unifyEquationVars (foldMap freeVariables <$> assignments) equations
  where
    unifyEquationVars :: (Abstract.Name l ~ AST.Name l,
                          Deep.Foldable (FreeVariableFold l Identity) (AST.Type l l))
                      => (AST.Type l l Identity Identity, AST.Type l l Identity Identity)
                      -> DisjointMap (AST.Name l) (Set (AST.Name l))
                      -> DisjointMap (AST.Name l) (Set (AST.Name l))
    unifyEquationVars (l, r) djmap =
      foldr (uncurry DJMap.union) djmap $ freeVariables l `Set.cartesianProduct` freeVariables r

relatedSetClosure :: Ord a => DisjointMap a (Set a) -> Set a -> Set a
relatedSetClosure related set = foldMap (`DJMap.lookup` disjointMapClosure related) set

disjointMapClosure :: Ord a => DisjointMap a (Set a) -> DisjointMap a (Set a)
disjointMapClosure relations =
  if all (Set.null . fst) front then relations else disjointMapClosure (snd <$> front) where
    front = fold $ DJMap.fromSets $ next <$> DJMap.toSets relations
    next (keys, related) = let additional = foldMap (`DJMap.lookup` relations) related in
      (keys, (additional Set.\\ related, additional `Set.union` related))
