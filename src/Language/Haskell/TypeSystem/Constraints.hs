{-# Language FlexibleContexts, FlexibleInstances, ImportQualifiedPost, LambdaCase,
             NamedFieldPuns, NoFieldSelectors, OverloadedRecordDot, OverloadedStrings,
             ScopedTypeVariables, StandaloneDeriving, TypeOperators, UndecidableInstances #-}

-- | The X part of OutsideIn(X), the constraints and their handler

module Language.Haskell.TypeSystem.Constraints (
  ConstraintHandler(..), DefaultConstraints, defaultConstraintHandler, TypeError(..), TypeErrors, TypeOrError(..)) where

import Control.Applicative (ZipList(ZipList))
import Data.Functor ((<&>))
import Data.Functor.Compose (Compose(Compose, getCompose))
import Data.Functor.Identity (Identity(Identity))
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict qualified as Map
import Data.Map.Strict (Map)
import Language.Haskell.Extensions.AST qualified as AST

-- | Record of functions for handling constraints
data ConstraintHandler l pos con = ConstraintHandler{
  display :: con -> String,
  fromContext :: AST.Context l l Identity Identity -> con,
  toContext :: con -> (AST.Context l l Identity Identity, con),
  replaceVar :: AST.Name l -> AST.Name l -> con -> con,
  simplify :: con  -- ^ given constraints to rely on
           -> con  -- ^ wanted constraints to simplify
           -> (con, Map (AST.Name l) (AST.Type l l Identity Identity)),
  unify :: AST.Type l l Identity Identity -> AST.Type l l Identity Identity -> con,
  assign :: AST.Name l -> TypeOrError l pos con -> con,
  union :: con -> con -> con,
  empty :: con}

data TypeError l con
  = TypeMismatch (AST.Type l l Identity Identity) (AST.Type l l Identity Identity)
  | TypeAmbiguity con
  | DuplicatePatternVariables (NonEmpty (AST.Name l))
  | UnknownTypeVariable (AST.QualifiedName l)
  | UnknownValue (AST.QualifiedName l)
  | UntypedValue (AST.QualifiedName l)

data TypeOrError l pos con
  = ProperType (AST.Type l l Identity Identity)
  | ErrorType (TypeErrors l pos con)

deriving instance (Show (AST.Type l l Identity Identity), Show con) => Show (TypeError l con)

type TypeErrors l pos con = NonEmpty (pos, TypeError l con)

data DefaultConstraints l pos = DefaultConstraints{
  equations :: [(AST.Type l l Identity Identity, AST.Type l l Identity Identity)],
  errors :: Map (AST.Name l) (TypeErrors l pos (DefaultConstraints l pos)),
  classes :: Map (AST.QualifiedName l) [AST.Type l l Identity Identity]}

deriving instance (Show (AST.Type l l Identity Identity), Show pos) =>
  Show (DefaultConstraints l pos)
instance Show pos => Semigroup (DefaultConstraints AST.Language pos) where
  (<>) = defaultConstraintHandler.union
instance Show pos => Monoid (DefaultConstraints AST.Language pos) where
  mempty = defaultConstraintHandler.empty

defaultConstraintHandler :: Show pos => ConstraintHandler AST.Language pos (DefaultConstraints AST.Language pos)
defaultConstraintHandler = ConstraintHandler{
  display = show,
  fromContext = \case
      AST.ClassConstraint name (Identity arg) -> defaultConstraintHandler.empty{classes= Map.singleton name [arg]}
      AST.Constraints cons -> foldMap defaultConstraintHandler.fromContext (Compose cons)
      AST.NoContext -> defaultConstraintHandler.empty,
  toContext = \DefaultConstraints{equations, classes}->
      case [AST.TypeEquality (Identity l) (Identity r) | (l, r) <- equations]
           <> [AST.ClassConstraint name (Identity arg) | (name, args) <- Map.toList classes, arg <- args]
      of [] -> (AST.NoContext, defaultConstraintHandler.empty)
         cons -> (AST.Constraints (ZipList $ Identity <$> cons), defaultConstraintHandler.empty),
  replaceVar = \from to DefaultConstraints{errors, equations, classes} ->
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
        errors = errors,
        equations = equations <&> \(l, r)-> (replaceInType l, replaceInType r),
        classes = getCompose $ replaceInType <$> Compose classes},
  -- TODO: actually simplify wanted, report contradictions
  simplify = \given wanted-> (given <> wanted, Map.empty),
  unify = \a b -> DefaultConstraints{equations= [(a, b)], errors= mempty, classes= mempty},
  assign = \var terr-> case terr of
      ProperType t -> DefaultConstraints{
        equations= [(AST.TypeVariable var, t)], classes= mempty, errors= mempty}
      ErrorType err -> DefaultConstraints{equations= mempty, classes= mempty, errors= Map.singleton var err},
  union = \l r-> DefaultConstraints{
      equations= l.equations <> r.equations,
      errors= l.errors <> r.errors,
      classes= Map.unionWith (<>) l.classes r.classes},
  empty = DefaultConstraints{equations= [], classes= Map.empty, errors= Map.empty}}

