{-# Language DuplicateRecordFields, FlexibleContexts, FlexibleInstances, ImportQualifiedPost,
             KindSignatures, MultiParamTypeClasses, OverloadedStrings,
             ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}

-- | Folds and traversals of type expressions

module Language.Haskell.TypeSystem.Transformations (
  freeVariables, FreeVariableFold,
  substituteTypeVariables, TypeVariableSubstitution) where

import Data.Foldable (toList)
import Data.Functor.Compose (Compose(Compose, getCompose))
import Data.Functor.Const (Const(Const, getConst))
import Data.Functor.Identity (Identity(Identity), runIdentity)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
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

freeVariables :: (Applicative f, Foldable f, Ord (Abstract.Name l),
                   Full.Foldable (FreeVariableFold l f) (AST.Type l l))
              => AST.Type l l f f -> Set (Abstract.Name l)
freeVariables = Full.foldMap FreeVariableFold . pure

instance Full.Foldable (FreeVariableFold l f) (AST.Constructor l l) where
  foldMap _ _ = mempty

instance (Foldable f, Ord (Abstract.Name l),
          Full.Foldable (FreeVariableFold l f) (Abstract.Context l l),
          Full.Foldable (FreeVariableFold l f) (Abstract.Type l l)) =>
         Full.Foldable (FreeVariableFold l f) (AST.Context l l) where
  foldMap = Full.foldMapUpDefault

instance (Foldable f, Ord (Abstract.Name l),
          Full.Foldable (FreeVariableFold l f) (Abstract.Type l l)) =>
         Full.Foldable (FreeVariableFold l f) (AST.FieldDeclaration l l) where
  foldMap = Full.foldMapUpDefault

instance (Foldable f, Ord (Abstract.Name l),
          Full.Foldable (FreeVariableFold l f) (Abstract.Kind l l)) =>
         Full.Foldable (FreeVariableFold l f) (AST.TypeVarBinding l l) where
  foldMap = Full.foldMapUpDefault

instance (Foldable f, Ord (Abstract.Name l),
          Full.Foldable (FreeVariableFold l f) (Abstract.Constructor l l),
          Full.Foldable (FreeVariableFold l f) (Abstract.Context l l),
          Full.Foldable (FreeVariableFold l f) (Abstract.FieldDeclaration l l),
          Full.Foldable (FreeVariableFold l f) (Abstract.Kind l l),
          Full.Foldable (FreeVariableFold l f) (Abstract.Type l l),
          Full.Foldable (FreeVariableFold l f) (Abstract.TypeVarBinding l l)) =>
         Full.Foldable (FreeVariableFold l f) (AST.Type l l) where
  foldMap t x = foldMap collectFrom x where
    collectFrom (AST.ForallType bindings ty) = Full.foldMap t ty Set.\\ foldMap (Full.foldMap t) bindings
    collectFrom _ = Full.foldMapUpDefault t x

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

-- | Substitution of types for the free occurrences of type variables
newtype TypeVariableSubstitution l =
  TypeVariableSubstitution (Map (Abstract.Name l) (AST.Type l l Identity Identity))

instance Transformation (TypeVariableSubstitution l) where
  type Domain (TypeVariableSubstitution l) = Identity
  type Codomain (TypeVariableSubstitution l) = Identity

-- | Replaces every /free/ occurrence of a type variable bound by the given map with the type it is bound to, and then
-- substitutes inside the replacement as well. A variable is never expanded within its own replacement, so a cyclic
-- map leaves the variable in place instead of diverging.
substituteTypeVariables :: (Ord (Abstract.Name l), Full.Functor (TypeVariableSubstitution l) (AST.Type l l))
                        => Map (Abstract.Name l) (AST.Type l l Identity Identity)
                        -> AST.Type l l Identity Identity
                        -> AST.Type l l Identity Identity
substituteTypeVariables substitution =
  runIdentity . (TypeVariableSubstitution substitution Full.<$>) . Identity

instance (Ord (Abstract.Name l),
          Abstract.TypeVarBinding l ~ AST.TypeVarBinding l,
          Deep.Functor (TypeVariableSubstitution l) (AST.Type l l),
          Full.Functor (TypeVariableSubstitution l) (Abstract.Type l l),
          Full.Functor (TypeVariableSubstitution l) (AST.TypeVarBinding l l)) =>
         Full.Functor (TypeVariableSubstitution l) (AST.Type l l) where
  t <$> Identity node = Identity (substituteUnbound t node)

-- | Substitutes in the given node, excluding from the substitution the type variables the node itself binds.
substituteUnbound :: (Ord (Abstract.Name l),
                      Abstract.TypeVarBinding l ~ AST.TypeVarBinding l,
                      Deep.Functor (TypeVariableSubstitution l) (AST.Type l l),
                      Full.Functor (TypeVariableSubstitution l) (AST.Type l l),
                      Full.Functor (TypeVariableSubstitution l) (Abstract.Type l l),
                      Full.Functor (TypeVariableSubstitution l) (AST.TypeVarBinding l l))
                  => TypeVariableSubstitution l
                  -> AST.Type l l Identity Identity
                  -> AST.Type l l Identity Identity
substituteUnbound t@(TypeVariableSubstitution substitution) node = case node of
  AST.ForallType bindings body -> underBinders AST.ForallType bindings body
  AST.VisibleDependentType bindings body -> underBinders AST.VisibleDependentType bindings body
  _ -> runIdentity (Full.mapUpDefault t $ Identity node)
  where underBinders construct bindings body = construct ((unbound Full.<$>) <$> bindings) (unbound Full.<$> body)
          where unbound =
                  TypeVariableSubstitution (substitution `Map.withoutKeys` foldMap boundTypeVariables bindings)

boundTypeVariables :: (Ord (Abstract.Name l), Abstract.TypeVarBinding l ~ AST.TypeVarBinding l)
                   => Identity (Abstract.TypeVarBinding l l Identity Identity) -> Set (Abstract.Name l)
boundTypeVariables (Identity (AST.ExplicitlyKindedTypeVariable _ name _)) = Set.singleton name
boundTypeVariables (Identity (AST.ImplicitlyKindedTypeVariable _ name)) = Set.singleton name
boundTypeVariables _ = mempty

instance (Ord (Abstract.Name l), Full.Functor (TypeVariableSubstitution l) (AST.Type l l)) =>
  TypeVariableSubstitution l `At` AST.Type l l Identity Identity where
  TypeVariableSubstitution substitution $ Identity node = Identity (replace node)
    where replace t@(AST.TypeVariable name) =
            maybe t (substituteTypeVariables $ Map.delete name substitution) (Map.lookup name substitution)
          replace t = t

instance Full.Functor (TypeVariableSubstitution l) (AST.Constructor l l) where
  _ <$> x = x

instance (Full.Functor (TypeVariableSubstitution l) (Abstract.Context l l),
          Full.Functor (TypeVariableSubstitution l) (Abstract.Type l l)) =>
         Full.Functor (TypeVariableSubstitution l) (AST.Context l l) where
  (<$>) = Full.mapUpDefault

instance Full.Functor (TypeVariableSubstitution l) (Abstract.Type l l) =>
         Full.Functor (TypeVariableSubstitution l) (AST.FieldDeclaration l l) where
  (<$>) = Full.mapUpDefault

instance Full.Functor (TypeVariableSubstitution l) (Abstract.Kind l l) =>
         Full.Functor (TypeVariableSubstitution l) (AST.TypeVarBinding l l) where
  (<$>) = Full.mapUpDefault

instance TypeVariableSubstitution l `At` AST.Context l l Identity Identity where
  _ $ x = x

instance TypeVariableSubstitution l `At` AST.FieldDeclaration l l Identity Identity where
  _ $ x = x

instance TypeVariableSubstitution l `At` AST.TypeVarBinding l l Identity Identity where
  _ $ x = x
