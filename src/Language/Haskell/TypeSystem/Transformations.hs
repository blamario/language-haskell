{-# Language DuplicateRecordFields, FlexibleContexts, FlexibleInstances, ImportQualifiedPost,
             KindSignatures, MultiParamTypeClasses, OverloadedStrings,
             ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}

-- | The X part of OutsideIn(X), the constraints and their handler

module Language.Haskell.TypeSystem.Transformations (freeVariables) where

import Data.Foldable (toList)
import Data.Functor.Compose (Compose(Compose, getCompose))
import Data.Functor.Const (Const(Const, getConst))
import Data.Functor.Identity (Identity(Identity))
import Data.Kind (Type)
import Data.Set (Set)
import Data.Set qualified as Set

import Transformation qualified
import Transformation (Transformation, At)
import Transformation.Deep qualified as Deep
import Transformation.Full qualified as Full

import Language.Haskell.Extensions.AST qualified as AST
import Language.Haskell.Extensions.Abstract qualified as Abstract

data FreeVariableFold l (f :: Type -> Type) = FreeVariableFold

instance Transformation (FreeVariableFold l f) where
  type Domain (FreeVariableFold l f) = f
  type Codomain (FreeVariableFold l f) = Const (Set (Abstract.Name l))

freeVariables :: (Foldable f, Ord (Abstract.Name l),
                  Full.Foldable (Full.Outward (FreeVariableFold l f)) (Abstract.Constructor l l),
                  Full.Foldable (Full.Outward (FreeVariableFold l f)) (Abstract.Context l l),
                  Full.Foldable (Full.Outward (FreeVariableFold l f)) (Abstract.FieldDeclaration l l),
                  Full.Foldable (Full.Outward (FreeVariableFold l f)) (Abstract.Kind l l),
                  Full.Foldable (Full.Outward (FreeVariableFold l f)) (Abstract.Type l l),
                  Full.Foldable (Full.Outward (FreeVariableFold l f)) (Abstract.TypeVarBinding l l))
              => AST.Type l l f f -> Set (Abstract.Name l)
freeVariables = Deep.foldMap (Full.Outward FreeVariableFold) 

instance (Foldable f, Ord (Abstract.Name l)) =>
  FreeVariableFold l f `At` AST.Type l l f f where
  _ $ x = foldMap collectFrom x where
    collectFrom (AST.TypeVariable x) = Const (Set.singleton x)
    collectFrom _ = mempty

instance Ord (Abstract.Name l) => FreeVariableFold l f `At` AST.Context l l f f where
  _ $ x = mempty

instance Ord (Abstract.Name l) => FreeVariableFold l f `At` AST.Constructor l l f f where
  _ $ x = mempty

instance Ord (Abstract.Name l) => FreeVariableFold l f `At` AST.FieldDeclaration l l f f where
  _ $ x = mempty

instance (Foldable f, Ord (Abstract.Name l)) =>
  FreeVariableFold l f `At` AST.TypeVarBinding l l f f where
  _ $ x = foldMap collectFrom x where
    collectFrom (AST.ExplicitlyKindedTypeVariable _ x _) = Const (Set.singleton x)
    collectFrom (AST.ImplicitlyKindedTypeVariable _ x) = Const (Set.singleton x)
    collectFrom _ = mempty
