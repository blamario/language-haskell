{-# Language DuplicateRecordFields, FlexibleContexts, FlexibleInstances, ImportQualifiedPost, LambdaCase,
             MultiParamTypeClasses, NamedFieldPuns, NoFieldSelectors, OverloadedRecordDot, OverloadedStrings,
             ScopedTypeVariables, StandaloneDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}

-- | The X part of OutsideIn(X), the constraints and their handler

module Language.Haskell.TypeSystem.Constraints (
  ConstraintCollection(..), DefaultConstraints, TypeError(..), TypeErrors,
  canonicalRenaming, splitFromType) where

import Data.Bifunctor (first)
import Data.Foldable (fold, toList)
import Data.Functor ((<&>))
import Data.Functor.Compose (Compose(Compose, getCompose))
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.DisjointMap qualified as DJMap
import Data.DisjointMap (DisjointMap)
import Data.Map.Strict qualified as Map
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Language.Haskell.Extensions.AST qualified as AST
import Language.Haskell.Abstract qualified as Abstract
import Language.Haskell.TypeSystem.Type (
  Ty(..), TyConstraint(..), freeVariableList, freeVariables, substituteTypeVariables)

-- | Record of functions for handling constraints
class Monoid con => ConstraintCollection con where
  type Language con
  type Position con
  display :: con -> String
  fromContext :: [TyConstraint (Language con)] -> con
  toContext :: con -> ([TyConstraint (Language con)], con)
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
           -> (con, Map (AST.Name (Language con)) (Ty (Language con)), [TypeError (Language con) con])
  unify :: Ty (Language con) -> Ty (Language con) -> con
  assignType :: AST.Name (Language con) -> Ty (Language con) -> con
  assignError :: AST.Name (Language con) -> TypeErrors (Language con) (Position con) con -> con
  errors :: con -> [(Position con, TypeError (Language con) con)]

data TypeError l con
  = TypeMismatch (Ty l) (Ty l)
  | TypeAmbiguity con
  | DuplicatePatternVariables (NonEmpty (AST.Name l))
  | UndeclaredContext (AST.Name l) [TyConstraint l]
  | UnknownTypeVariable (AST.QualifiedName l)
  | UnknownValue (AST.QualifiedName l)
  | UntypedValue (AST.QualifiedName l)
  -- | a type the checker has no representation for, /e.g./ an unboxed tuple in a signature
  | UnsupportedType

deriving instance (Show (Abstract.Name l), Show (Abstract.QualifiedName l), Show con) => Show (TypeError l con)

type TypeErrors l pos con = NonEmpty (pos, TypeError l con)

data DefaultConstraints l pos = DefaultConstraints{
  assignments :: DJMap.DisjointMap (AST.Name l) [Ty l],
  equations :: [(Ty l, Ty l)],
  errors :: Map (AST.Name l) (TypeErrors l pos (DefaultConstraints l pos)),
  classes :: Map (AST.QualifiedName l) [Ty l]}

deriving instance (Show (Abstract.Name l), Show (Abstract.QualifiedName l), Show pos) =>
                  Show (DefaultConstraints l pos)

instance Ord (AST.QualifiedName l) => Semigroup (DefaultConstraints l pos) where
  x <> y = DefaultConstraints{
    assignments= x.assignments <> y.assignments,
    classes= Map.unionWith (<>) x.classes y.classes,
    equations = x.equations <> y.equations,
    errors = x.errors <> y.errors}

instance Ord (AST.QualifiedName l) => Monoid (DefaultConstraints l pos) where
  mempty = DefaultConstraints{assignments= DJMap.empty, equations= [], classes= Map.empty, errors= Map.empty}

instance (Show pos,
          Show (Abstract.Name l),
          Show (Abstract.QualifiedName l),
          Abstract.Name l ~ AST.Name l,
          Abstract.QualifiedName l ~ AST.QualifiedName l) =>
         ConstraintCollection (DefaultConstraints l pos) where
  type Language (DefaultConstraints l pos) = l
  type Position (DefaultConstraints l pos) = pos
  display = show
  fromContext = foldMap $ \case
      ClassConstraint name arg -> mempty{classes= Map.singleton name [arg]}
      TypeEquality left right -> mempty{equations= [(left, right)]}
  toContext = \DefaultConstraints{assignments, equations, classes}->
      ([TypeEquality l r
       | (vars, types) <- DJMap.toLists assignments,
         let allTypes = map TypeVariable vars ++ types
             (ls, rs) = splitAt 1 allTypes,
         l <- ls,
         r <- rs]
       <> [TypeEquality l r | (l, r) <- equations]
       <> [ClassConstraint name arg | (name, args) <- Map.toList classes, arg <- args],
       mempty)
  replaceVar = \from to DefaultConstraints{assignments, errors, equations, classes} ->
      let replaceInType = substituteTypeVariables (Map.singleton from $ TypeVariable to)
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
        substituted = List.nub . map (substituteTypeVariables bindings)
        -- a wanted class constraint is discharged by a given one for the same type; there is no instance nor
        -- superclass reasoning yet
        discharge wantedArgs givenArgs = Just (filter (`notElem` givenArgs) wantedArgs)
        residual = mempty{
          classes = Map.filter (not . null)
                    $ Map.differenceWith discharge (substituted <$> wanted.classes) (substituted <$> given.classes),
          errors = (given <> wanted).errors}
    in (residual, bindings, mismatches)
  unify (TypeVariable name) t = assignType name t
  unify t (TypeVariable name) = assignType name t
  unify a b = mempty{equations= [(a, b)]}
  assignType v1 (TypeVariable v2) = mempty{assignments= DJMap.union v1 v2 DJMap.empty}
  assignType var t = mempty{assignments= DJMap.singleton var [t]}
  assignError var err = mempty{errors= Map.singleton var err}
  errors DefaultConstraints{errors} = foldMap toList errors

canonicalDefaultNameMap :: (Abstract.Name l ~ AST.Name l, Ord (AST.Name l)) => DefaultConstraints l pos -> Map (AST.Name l) (AST.Name l)
canonicalDefaultNameMap con = foldMap (canonicalMap . fst) $ DJMap.toLists con.assignments
  where canonicalMap names@(name:_)
          | Just canonical <- DJMap.representative name con.assignments
          = Map.fromList [(n, canonical) | n <- names]
        canonicalMap _ = mempty

renameTypeVariablesDefault :: (Abstract.Name l ~ AST.Name l, Ord (AST.Name l), Ord (AST.QualifiedName l))
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
    renameInType = substituteTypeVariables typeRenamings
    typeRenamings = TypeVariable <$> renamings

-- | Unifies all the equality constraints, both the type variable assignments and the type equations, and returns
-- the bindings of type variables to types together with the contradictions it ran into. A contradiction is either a
-- clash of two different type constructors or a variable bound to a type containing the variable itself, /i.e./ a
-- failure of the occurs check.
--
-- The returned bindings are not idempotent, as one bound variable may be bound to a type mentioning another;
-- 'substituteTypeVariables' follows the chains.
solveConstraints :: forall l pos. (Abstract.Name l ~ AST.Name l, Abstract.QualifiedName l ~ AST.QualifiedName l,
                                   Ord (AST.Name l), Eq (AST.QualifiedName l))
                 => DefaultConstraints l pos
                 -> (Map (AST.Name l) (Ty l), [TypeError l (DefaultConstraints l pos)])
solveConstraints con = unifyAll mempty (assignedEquations <> con.equations) []
  where
    -- every variable of an equivalence class is equal to its first variable, as is every type assigned to the class
    assignedEquations =
      [(TypeVariable var, t)
      | (var:vars, types) <- DJMap.toLists con.assignments,
        t <- (TypeVariable <$> vars) <> types]
    -- the signature keeps the type variables tied to the ones above instead of generalizing this local binding,
    -- which would make the constraints above inapplicable to it
    unifyAll :: Map (AST.Name l) (Ty l)
             -> [(Ty l, Ty l)]
             -> [TypeError l (DefaultConstraints l pos)]
             -> (Map (AST.Name l) (Ty l), [TypeError l (DefaultConstraints l pos)])
    unifyAll bindings [] errs = (bindings, reverse errs)
    unifyAll bindings ((left, right) : rest) errs = case (l, r) of
      _ | l == r -> unifyAll bindings rest errs
      -- a wildcard stands for a type nobody is interested in, so it agrees with every type
      (WildcardType, _) -> unifyAll bindings rest errs
      (_, WildcardType) -> unifyAll bindings rest errs
      -- of two variables the lesser name survives, so that the outcome does not depend on the order the
      -- constraints were collected in; the fresh variable names grow with the depth of the node they belong to
      (TypeVariable lName, TypeVariable rName)
        | lName < rName -> unifyAll (Map.insert rName l bindings) rest errs
        | otherwise -> unifyAll (Map.insert lName r bindings) rest errs
      (TypeVariable name, _) | not (occurs name r) -> unifyAll (Map.insert name r bindings) rest errs
      (_, TypeVariable name) | not (occurs name l) -> unifyAll (Map.insert name l bindings) rest errs
      (FunctionType l1 l2, FunctionType r1 r2) -> unifyAll bindings ((l1, r1) : (l2, r2) : rest) errs
      (ApplyType l1 l2, ApplyType r1 r2) -> unifyAll bindings ((l1, r1) : (l2, r2) : rest) errs
      (ListType l1, ListType r1) -> unifyAll bindings ((l1, r1) : rest) errs
      (StrictType l1, StrictType r1) -> unifyAll bindings ((l1, r1) : rest) errs
      (TupleType ls, TupleType rs)
        | length ls == length rs -> unifyAll bindings (zip (toList ls) (toList rs) <> rest) errs
      _ -> mismatch
      where l = substituteTypeVariables bindings left
            r = substituteTypeVariables bindings right
            occurs :: AST.Name l -> Ty l -> Bool
            occurs name t = name `Set.member` freeVariables t
            mismatch = unifyAll bindings rest (TypeMismatch l r : errs)

-- | Assigns a short name to every type variable free in the given type, in the order of their appearance: the first
-- variable becomes @a@, the second @b@, and so on past @z@ with @a1@, @b1@, and the rest. Identity renamings are
-- left out of the returned map, so its domain and codomain are distinct.
canonicalRenaming :: (Abstract.Name l ~ AST.Name l, Eq (AST.Name l)) => Ty l -> Map (AST.Name l) (AST.Name l)
canonicalRenaming t = Map.fromList (assign [] free)
  where assign _ [] = []
        assign assigned (var : rest) = case [name | name <- shortNames, name `notElem` assigned,
                                                    name == var || name `notElem` free] of
          name : _
            | name == var -> assign (name : assigned) rest
            | otherwise -> (var, name) : assign (name : assigned) rest
          [] -> []
        free = List.nub (freeVariableList t)
        shortNames = [AST.Name (Text.pack $ letter : suffix)
                     | suffix <- "" : map show [1 :: Int ..], letter <- ['a' .. 'z']]

-- | Separates the constraints a quantified type carries from the type they constrain
splitFromType :: (ConstraintCollection con, Language con ~ l) => Ty l -> (con, Ty l)
splitFromType (ConstrainedType context t) = first (fromContext context <>) (splitFromType t)
splitFromType t = (mempty, t)

filterDefaultRelevant :: (Abstract.Name l ~ AST.Name l, Ord (AST.Name l), Ord (AST.QualifiedName l))
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

constraintRelatedTypeVarSetClosure :: (Abstract.Name l ~ AST.Name l, Ord (AST.Name l))
                                   => DefaultConstraints l pos -> Set (AST.Name l) -> Set (AST.Name l)
constraintRelatedTypeVarSetClosure DefaultConstraints{assignments, equations} =
  relatedSetClosure $ foldr unifyEquationVars (foldMap freeVariables <$> assignments) equations
  where
    unifyEquationVars :: (Abstract.Name l ~ AST.Name l, Ord (AST.Name l))
                      => (Ty l, Ty l)
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
