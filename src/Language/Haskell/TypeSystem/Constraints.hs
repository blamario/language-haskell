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
import Data.Functor.Identity (Identity(Identity), runIdentity)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.DisjointMap qualified as DJMap
import Data.DisjointMap (DisjointMap)
import Data.Map.Strict qualified as Map
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Set qualified as Set
import Transformation.Full qualified as Full
import Language.Haskell.Extensions.AST qualified as AST
import Language.Haskell.Abstract qualified as Abstract
import Language.Haskell.Extensions.Abstract qualified as Abstract (TypeVarBinding)
import Language.Haskell.TypeSystem.Transformations (
  FreeVariableFold, TypeVariableSubstitution, freeVariables, substituteTypeVariables)

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
  -- | Solves the wanted constraints, relying on the given ones. Returns the constraints left unsolved, the bindings
  -- of type variables to the types unification has determined, and the contradictions that make the wanted
  -- constraints unsatisfiable.
  simplify :: con  -- ^ global constraints
           -> con  -- ^ given constraints to rely on
           -> con  -- ^ wanted constraints to simplify
           -> (con,
               Map (AST.Name (Language con)) (AST.Type (Language con) (Language con) Identity Identity),
               [TypeError (Language con) con])
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
          Eq (AST.Type l l Identity Identity),
          Abstract.Name l ~ AST.Name l,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.Context l ~ AST.Context l,
          Abstract.Type l ~ AST.Type l,
          Abstract.TypeVarBinding l ~ AST.TypeVarBinding l,
          Full.Foldable (FreeVariableFold l Identity) (AST.Type l l),
          Full.Functor (TypeVariableSubstitution l) (AST.Type l l)) =>
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
      let replaceInType = resolveTypeVariables (Map.singleton from $ AST.TypeVariable to)
      in DefaultConstraints{
        assignments = getCompose $ replaceInType <$> Compose assignments,
        errors = errors,
        equations = equations <&> \(l, r)-> (replaceInType l, replaceInType r),
        classes = getCompose $ replaceInType <$> Compose classes}
  filterRelevant = filterDefaultRelevant
  canonicalNameMap = canonicalDefaultNameMap
  renameTypeVariables = renameTypeVariablesDefault
  simplify = \_global given wanted->
    let (bindings, mismatches) = solveConstraints (given <> wanted)
        substituted = List.nub . map (resolveTypeVariables bindings)
        -- a wanted class constraint is discharged by a given one for the same type; there is no instance nor
        -- superclass reasoning yet
        discharge wantedArgs givenArgs = Just (filter (`notElem` givenArgs) wantedArgs)
        residual = mempty{
          classes = Map.filter (not . null)
                    $ Map.differenceWith discharge (substituted <$> wanted.classes) (substituted <$> given.classes),
          errors = (given <> wanted).errors}
    in (residual, bindings, mismatches)
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
        canonicalMap _ = mempty

renameTypeVariablesDefault :: (Abstract.Name l ~ AST.Name l, Abstract.Type l ~ AST.Type l,
                               Abstract.TypeVarBinding l ~ AST.TypeVarBinding l,
                               Full.Functor (TypeVariableSubstitution l) (AST.Type l l))
                           => Map (AST.Name l) (AST.Name l) -> DefaultConstraints l pos -> DefaultConstraints l pos
renameTypeVariablesDefault renamings con = DefaultConstraints{
  assignments = fold $ DJMap.fromSets $ renameAssignments <$> DJMap.toLists con.assignments,
  errors = con.errors,
  equations = con.equations <&> \(l, r)-> (renameInType l, renameInType r),
  classes = getCompose $ renameInType <$> Compose con.classes}
  where
    -- the name list is non-empty
    renameAssignments (name:_, types) =
      (Set.singleton $ Map.findWithDefault name name renamings, renameInType <$> types)
    renameInType = resolveTypeVariables typeRenamings
    typeRenamings = AST.TypeVariable <$> renamings

-- | Unifies all the equality constraints, both the type variable assignments and the type equations, and returns
-- the bindings of type variables to types together with the contradictions it ran into. A contradiction is either a
-- clash of two different type constructors or a variable bound to a type containing the variable itself, /i.e./ a
-- failure of the occurs check.
--
-- The returned bindings are not idempotent, as one bound variable may be bound to a type mentioning another;
-- 'resolveTypeVariables' follows the chains.
solveConstraints :: forall l pos. (Abstract.Name l ~ AST.Name l, Abstract.Type l ~ AST.Type l,
                                   Abstract.TypeVarBinding l ~ AST.TypeVarBinding l,
                                   Eq (AST.Type l l Identity Identity), Eq (Abstract.QualifiedName l),
                                   Full.Foldable (FreeVariableFold l Identity) (AST.Type l l),
                                   Full.Functor (TypeVariableSubstitution l) (AST.Type l l))
                 => DefaultConstraints l pos
                 -> (Map (AST.Name l) (AST.Type l l Identity Identity), [TypeError l (DefaultConstraints l pos)])
solveConstraints con = unifyAll mempty (assignedEquations <> con.equations) []
  where
    -- every variable of an equivalence class is equal to its first variable, as is every type assigned to the class
    assignedEquations =
      [(AST.TypeVariable var, t)
      | (var:vars, types) <- DJMap.toLists con.assignments,
        t <- (AST.TypeVariable <$> vars) <> types]
    -- the signature keeps the type variables tied to the ones above instead of generalizing this local binding,
    -- which would make the constraints above inapplicable to it
    unifyAll :: Map (AST.Name l) (AST.Type l l Identity Identity)
             -> [(AST.Type l l Identity Identity, AST.Type l l Identity Identity)]
             -> [TypeError l (DefaultConstraints l pos)]
             -> (Map (AST.Name l) (AST.Type l l Identity Identity), [TypeError l (DefaultConstraints l pos)])
    unifyAll bindings [] errs = (bindings, reverse errs)
    unifyAll bindings ((left, right) : rest) errs = case (l, r) of
      _ | l == r -> unifyAll bindings rest errs
      -- of two variables the lesser name survives, so that the outcome does not depend on the order the
      -- constraints were collected in; the fresh variable names grow with the depth of the node they belong to
      (AST.TypeVariable lName, AST.TypeVariable rName)
        | lName < rName -> unifyAll (Map.insert rName l bindings) rest errs
        | otherwise -> unifyAll (Map.insert lName r bindings) rest errs
      (AST.TypeVariable name, _) | not (occurs name r) -> unifyAll (Map.insert name r bindings) rest errs
      (_, AST.TypeVariable name) | not (occurs name l) -> unifyAll (Map.insert name l bindings) rest errs
      (AST.FunctionType (Identity l1) (Identity l2), AST.FunctionType (Identity r1) (Identity r2)) ->
        unifyAll bindings ((l1, r1) : (l2, r2) : rest) errs
      (AST.LinearFunctionType (Identity l1) (Identity l2), AST.LinearFunctionType (Identity r1) (Identity r2)) ->
        unifyAll bindings ((l1, r1) : (l2, r2) : rest) errs
      (AST.TypeApplication (Identity l1) (Identity l2), AST.TypeApplication (Identity r1) (Identity r2)) ->
        unifyAll bindings ((l1, r1) : (l2, r2) : rest) errs
      (AST.InfixTypeApplication (Identity l1) lName (Identity l2),
       AST.InfixTypeApplication (Identity r1) rName (Identity r2))
        | lName == rName -> unifyAll bindings ((l1, r1) : (l2, r2) : rest) errs
      (AST.ListType (Identity l1), AST.ListType (Identity r1)) -> unifyAll bindings ((l1, r1) : rest) errs
      (AST.StrictType (Identity l1), AST.StrictType (Identity r1)) -> unifyAll bindings ((l1, r1) : rest) errs
      (AST.TupleType ls, AST.TupleType rs) -> unifyAllOf (toList ls) (toList rs)
      (AST.UnboxedTupleType _ ls, AST.UnboxedTupleType _ rs) -> unifyAllOf (toList ls) (toList rs)
      (AST.UnboxedSumType _ ls, AST.UnboxedSumType _ rs) -> unifyAllOf (toList ls) (toList rs)
      _ -> mismatch
      where l = resolveTypeVariables bindings left
            r = resolveTypeVariables bindings right
            occurs :: AST.Name l -> AST.Type l l Identity Identity -> Bool
            occurs name t = name `Set.member` freeVariables t
            mismatch = unifyAll bindings rest (TypeMismatch l r : errs)
            unifyAllOf ls rs
              | length ls == length rs = unifyAll bindings (zip (runIdentity <$> ls) (runIdentity <$> rs) <> rest) errs
              | otherwise = mismatch

-- | Replaces the free occurrences of the bound type variables throughout the given type.
resolveTypeVariables :: (Abstract.Name l ~ AST.Name l, Abstract.Type l ~ AST.Type l,
                         Abstract.TypeVarBinding l ~ AST.TypeVarBinding l,
                         Full.Functor (TypeVariableSubstitution l) (AST.Type l l))
                     => Map (AST.Name l) (AST.Type l l Identity Identity)
                     -> AST.Type l l Identity Identity
                     -> AST.Type l l Identity Identity
resolveTypeVariables = substituteTypeVariables

splitFromType :: (Abstract.Context l ~ AST.Context l, Abstract.Type l ~ AST.Type l,
                  ConstraintCollection con, Language con ~ l)
              => AST.Type l l Identity Identity -> (con, AST.Type l l Identity Identity)
splitFromType (AST.ConstrainedType (Identity context) (Identity t)) = first (fromContext context <>) (splitFromType t)
splitFromType t = (mempty, t)

filterDefaultRelevant :: (Abstract.Name l ~ AST.Name l,
                          Full.Foldable (FreeVariableFold l Identity) (AST.Type l l))
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
    hasRelevant = not . Set.disjoint vars'
    vars' = Set.union vars $ foldMap partsOf vars
    partsOf var = foldMap freeVariables $ DJMap.lookup var con.assignments

constraintRelatedTypeVarSetClosure :: (Abstract.Name l ~ AST.Name l,
                                       Full.Foldable (FreeVariableFold l Identity) (AST.Type l l))
                                   => DefaultConstraints l pos -> Set (AST.Name l) -> Set (AST.Name l)
constraintRelatedTypeVarSetClosure DefaultConstraints{assignments, equations} =
  relatedSetClosure $ foldr unifyEquationVars (foldMap freeVariables <$> assignments) equations
  where
    unifyEquationVars :: (Abstract.Name l ~ AST.Name l,
                          Full.Foldable (FreeVariableFold l Identity) (AST.Type l l))
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
