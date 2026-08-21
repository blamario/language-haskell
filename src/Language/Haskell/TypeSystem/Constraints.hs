{-# Language DuplicateRecordFields, FlexibleContexts, FlexibleInstances, ImportQualifiedPost, LambdaCase,
             MultiParamTypeClasses, NamedFieldPuns, NoFieldSelectors, OverloadedRecordDot, OverloadedStrings,
             ScopedTypeVariables, StandaloneDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}

-- | The X part of OutsideIn(X), the constraints and their handler

module Language.Haskell.TypeSystem.Constraints (
  ConstraintCollection(..), DefaultConstraints, TypeError(..), TypeErrors, TypeOrError(..),
  resolveTypeVariables, splitFromType) where

import Control.Applicative (ZipList(ZipList))
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.Functor.Compose (Compose(Compose, getCompose))
import Data.Functor.Identity (Identity(Identity))
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict qualified as Map
import Data.Map.Strict (Map)
import Language.Haskell.Extensions.AST qualified as AST
import Language.Haskell.Abstract qualified as Abstract

-- | Record of functions for handling constraints
class Monoid con => ConstraintCollection con where
  type Language con
  type Position con
  display :: con -> String
  fromContext :: AST.Context (Language con) (Language con) Identity Identity -> con
  toContext :: con -> (AST.Context (Language con) (Language con) Identity Identity, con)
  replaceVar :: AST.Name (Language con) -> AST.Name (Language con) -> con -> con
  simplify :: con  -- ^ global constraints
           -> con  -- ^ given constraints to rely on
           -> con  -- ^ wanted constraints to simplify
           -> (con, Map (AST.Name (Language con)) (AST.Type (Language con) (Language con) Identity Identity))
  unify :: AST.Type (Language con) (Language con) Identity Identity
        -> AST.Type (Language con) (Language con) Identity Identity
        -> con
  assign :: AST.Name (Language con) -> TypeOrError (Language con) (Position con) con -> con
  errors :: con -> [(Position con, TypeError (Language con) con)]

data TypeError l con
  = TypeMismatch (AST.Type l l Identity Identity) (AST.Type l l Identity Identity)
  | TypeAmbiguity con
  | DuplicatePatternVariables (NonEmpty (AST.Name l))
  | UndeclaredContext (AST.Context l l Identity Identity)
  | UnknownTypeVariable (AST.QualifiedName l)
  | UnknownValue (AST.QualifiedName l)
  | UntypedValue (AST.QualifiedName l)

data TypeOrError l pos con
  = ProperType (AST.Type l l Identity Identity)
  | ErrorType (TypeErrors l pos con)

deriving instance (Show (AST.Context l l Identity Identity),
                   Show (AST.Type l l Identity Identity), Show con) => Show (TypeError l con)

type TypeErrors l pos con = NonEmpty (pos, TypeError l con)

data DefaultConstraints l pos = DefaultConstraints{
  equations :: [(AST.Type l l Identity Identity, AST.Type l l Identity Identity)],
  errors :: Map (AST.Name l) (TypeErrors l pos (DefaultConstraints l pos)),
  classes :: Map (AST.QualifiedName l) [AST.Type l l Identity Identity]}

deriving instance (Show (AST.Context l l Identity Identity),
                   Show (AST.Type l l Identity Identity), Show pos) => Show (DefaultConstraints l pos)

instance Semigroup (DefaultConstraints l pos) where
  x <> y = DefaultConstraints{
    classes= Map.unionWith (<>) x.classes y.classes,
    equations = x.equations <> y.equations,
    errors = x.errors <> y.errors}

instance Monoid (DefaultConstraints l pos) where
  mempty = DefaultConstraints{equations= [], classes= Map.empty, errors= Map.empty}

instance Show pos => ConstraintCollection (DefaultConstraints AST.Language pos) where
  type Language (DefaultConstraints AST.Language pos) = AST.Language
  type Position (DefaultConstraints AST.Language pos) = pos
  display = show
  fromContext = \case
      AST.ClassConstraint name (Identity arg) -> mempty{classes= Map.singleton name [arg]}
      AST.Constraints cons -> foldMap fromContext (Compose cons)
      AST.NoContext -> mempty
  toContext = \DefaultConstraints{equations, classes}->
      case [AST.TypeEquality (Identity l) (Identity r) | (l, r) <- equations]
           <> [AST.ClassConstraint name (Identity arg) | (name, args) <- Map.toList classes, arg <- args]
      of [] -> (AST.NoContext, mempty)
         cons -> (AST.Constraints (ZipList $ Identity <$> cons), mempty)
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
        classes = getCompose $ replaceInType <$> Compose classes}
  -- TODO: actually simplify wanted, report contradictions
  simplify = \_ given wanted-> (given <> wanted, Map.empty)
  unify = \a b -> DefaultConstraints{equations= [(a, b)], errors= mempty, classes= mempty}
  assign = \var terr-> case terr of
      ProperType t -> DefaultConstraints{
        equations= [(AST.TypeVariable var, t)], classes= mempty, errors= mempty}
      ErrorType err -> DefaultConstraints{equations= mempty, classes= mempty, errors= Map.singleton var err}
  errors DefaultConstraints{errors} = foldMap toList errors

resolveTypeVariables :: Map (AST.Name l) (AST.Type l l Identity Identity)
                     -> AST.Type l l Identity Identity
                     -> AST.Type l l Identity Identity
resolveTypeVariables bindings t = t

splitFromType :: (Abstract.Context l ~ AST.Context l, Abstract.Type l ~ AST.Type l,
                  ConstraintCollection con, Language con ~ l)
              => AST.Type l l Identity Identity -> (con, AST.Type l l Identity Identity)
splitFromType (AST.ConstrainedType (Identity context) (Identity t)) = first (fromContext context <>) (splitFromType t)
splitFromType t = (mempty, t)
  
