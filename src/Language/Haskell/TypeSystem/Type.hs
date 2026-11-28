{-# Language FlexibleContexts, FlexibleInstances, ImportQualifiedPost, LambdaCase, OverloadedStrings,
             ScopedTypeVariables, StandaloneDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}

-- | The type definitions for the type system, more convenient than the AST 'AST.Type' of the surface language.
--
-- A 'Ty' is converted from the AST by 'fromSyntax' as the checker walks a signature, and back by 'toSyntax'
-- once a type has been inferred and is handed out to be printed.

module Language.Haskell.TypeSystem.Type (
  Ty(..), TyConstraint(..), TyVarBinding(..),
  -- * Conversion to and from the type syntax
  fromSyntax, toSyntax, contextToSyntax,
  -- * Traversals
  freeVariables, freeVariableList, substituteTypeVariables, boundVariables) where

import Control.Applicative (ZipList(ZipList))
import Data.Foldable (toList)
import Data.Functor.Compose (Compose (Compose, getCompose))
import Data.Functor.Identity (Identity(Identity, runIdentity))
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.ZipNonEmpty (ZipNonEmpty(ZipNonEmpty))

import Language.Haskell.Abstract qualified as Abstract
import Language.Haskell.Extensions.Abstract qualified as Abstract (Kind, TypeVarBinding)
import Language.Haskell.Extensions.AST qualified as AST

-- | A type, as the type system understands it
data Ty l =
   TypeVariable (Abstract.Name l)
   | ConstructorType (Abstract.QualifiedName l)
   | ApplyType (Ty l) (Ty l)
   | FunctionType (Ty l) (Ty l)
   | ListType (Ty l)
   | TupleType (NonEmpty (Ty l))
   | UnitType
   | RecordType (Abstract.QualifiedName l) [(Abstract.Name l, Ty l)]
   | StrictType (Ty l)                       -- ^ a strict field's type, @!t@
   | ForallType [TyVarBinding l] (Ty l)
   | ConstrainedType [TyConstraint l] (Ty l) -- ^ a type with constrained context, @con => t@ 
   | WildcardType                            -- ^ a type of no interest, @_@
   | GroundKind                              -- ^ the kind of an ordinary value type
   | FunctionConstructor                     -- ^ the function type constructor, @(->)@

-- | A single type constraint to build up the type context
data TyConstraint l =
   ClassConstraint (Abstract.QualifiedName l) (Ty l)
   | TypeEquality (Ty l) (Ty l)

-- | A type variable bound by a @forall@, with its kind if declared and whether it is inferred
data TyVarBinding l = TyVarBinding Bool (Abstract.Name l) (Maybe (Ty l))

deriving instance (Eq (Abstract.Name l), Eq (Abstract.QualifiedName l)) => Eq (Ty l)
deriving instance (Eq (Abstract.Name l), Eq (Abstract.QualifiedName l)) => Eq (TyConstraint l)
deriving instance (Eq (Abstract.Name l), Eq (Abstract.QualifiedName l)) => Eq (TyVarBinding l)
deriving instance (Show (Abstract.Name l), Show (Abstract.QualifiedName l)) => Show (Ty l)
deriving instance (Show (Abstract.Name l), Show (Abstract.QualifiedName l)) => Show (TyConstraint l)
deriving instance (Show (Abstract.Name l), Show (Abstract.QualifiedName l)) => Show (TyVarBinding l)

-- | Converts the surface type syntax to the type system's type.
fromSyntax :: (Abstract.Name l ~ AST.Name l, Abstract.QualifiedName l ~ AST.QualifiedName l,
               Abstract.Type l ~ AST.Type l, Abstract.Kind l ~ AST.Type l,
               Abstract.Context l ~ AST.Context l, Abstract.Constructor l ~ AST.Constructor l,
               Abstract.TypeVarBinding l ~ AST.TypeVarBinding l,
               Show (AST.Constructor l l Identity Identity),
               Show (AST.Context l l Identity Identity),
               Show (AST.TypeVarBinding l l Identity Identity),
               Show (AST.Type l l Identity Identity))
           => AST.Type l l Identity Identity -> Ty l
fromSyntax = \case
  AST.TypeVariable name -> TypeVariable name
  AST.ConstructorType (Identity con) -> constructorFromSyntax con
  AST.FunctionType (Identity from) (Identity to) -> FunctionType (fromSyntax from) (fromSyntax to)
  AST.LinearFunctionType (Identity from) (Identity to) -> FunctionType (fromSyntax from) (fromSyntax to)
  AST.TypeApplication (Identity f) (Identity arg) -> ApplyType (fromSyntax f) (fromSyntax arg)
  AST.InfixTypeApplication (Identity left) op (Identity right)
    -> ConstructorType op `ApplyType` fromSyntax left `ApplyType` fromSyntax right
  AST.ListType (Identity item) -> ListType $ fromSyntax item
  AST.TupleType (ZipNonEmpty items) -> TupleType (fromSyntax . runIdentity <$> items)
  AST.StrictType (Identity t) -> StrictType $ fromSyntax t
  AST.LazyType _ (Identity t) -> fromSyntax t
  AST.ForallType bindings (Identity body)
    -> ForallType (bindingFromSyntax . runIdentity <$> toList bindings) (fromSyntax body)
  AST.ConstrainedType (Identity context) (Identity body)
    -> ConstrainedType (contextFromSyntax context) (fromSyntax body)
  AST.TypeWildcard -> WildcardType
  AST.GroundTypeKind -> GroundKind
  AST.FunctionConstructorType -> FunctionConstructor
  AST.TypeKind (Identity t) -> fromSyntax t
  AST.KindedType (Identity t) _kind -> fromSyntax t
  t -> error ("Unsupported type " <> show t)

constructorFromSyntax :: (Abstract.QualifiedName l ~ AST.QualifiedName l,
                      Show (AST.Constructor l l Identity Identity))
                      => AST.Constructor l l Identity Identity -> Ty l
constructorFromSyntax = \case
  AST.ConstructorReference name -> ConstructorType name
  AST.UnitConstructor -> UnitType
  con -> error ("Unsupported constructor " <> show con)

bindingFromSyntax :: (Abstract.Name l ~ AST.Name l, Abstract.QualifiedName l ~ AST.QualifiedName l,
                      Abstract.Type l ~ AST.Type l, Abstract.Kind l ~ AST.Type l,
                      Abstract.Context l ~ AST.Context l, Abstract.Constructor l ~ AST.Constructor l,
                      Abstract.TypeVarBinding l ~ AST.TypeVarBinding l,
                      Show (AST.Constructor l l Identity Identity),
                      Show (AST.Context l l Identity Identity),
                      Show (AST.TypeVarBinding l l Identity Identity),
                      Show (AST.Type l l Identity Identity))
                  => AST.TypeVarBinding l l Identity Identity -> TyVarBinding l
bindingFromSyntax = \case
  AST.ImplicitlyKindedTypeVariable inferred name -> TyVarBinding inferred name Nothing
  AST.ExplicitlyKindedTypeVariable inferred name (Identity kind)
    -> TyVarBinding inferred name $ Just $ fromSyntax kind
  binding -> error ("Unsupported binding " <> show binding)

contextFromSyntax :: (Abstract.Name l ~ AST.Name l, Abstract.QualifiedName l ~ AST.QualifiedName l,
                      Abstract.Type l ~ AST.Type l, Abstract.Kind l ~ AST.Type l,
                      Abstract.Context l ~ AST.Context l, Abstract.Constructor l ~ AST.Constructor l,
                      Abstract.TypeVarBinding l ~ AST.TypeVarBinding l,
                      Show (AST.Constructor l l Identity Identity),
                      Show (AST.Context l l Identity Identity),
                      Show (AST.TypeVarBinding l l Identity Identity),
                      Show (AST.Type l l Identity Identity))
                  => AST.Context l l Identity Identity -> [TyConstraint l]
contextFromSyntax = \case
  AST.NoContext -> []
  AST.ClassConstraint name (Identity arg) -> [ClassConstraint name $ fromSyntax arg]
  AST.TypeEquality (Identity left) (Identity right) -> [TypeEquality (fromSyntax left) (fromSyntax right)]
  AST.Constraints cons -> foldMap (contextFromSyntax . runIdentity) (toList cons)
  context -> error ("Unsupported context " <> show context)

-- | Converts a type back into the syntax, so that it can be printed the way the parser would have read it
toSyntax :: (Abstract.Haskell l, Abstract.Name l ~ AST.Name l, Abstract.QualifiedName l ~ AST.QualifiedName l,
             Abstract.Type l ~ AST.Type l, Abstract.Kind l ~ AST.Type l, Abstract.Context l ~ AST.Context l,
             Abstract.Constructor l ~ AST.Constructor l, Abstract.TypeVarBinding l ~ AST.TypeVarBinding l)
         => Ty l -> AST.Type l l Identity Identity
toSyntax = \case
  TypeVariable name -> AST.TypeVariable name
  ConstructorType name -> constructorType name
  ApplyType f arg -> AST.TypeApplication (Identity $ toSyntax f) (Identity $ toSyntax arg)
  FunctionType from to -> AST.FunctionType (Identity $ toSyntax from) (Identity $ toSyntax to)
  ListType item -> AST.ListType (Identity $ toSyntax item)
  TupleType items -> AST.TupleType (ZipNonEmpty $ Identity . toSyntax <$> items)
  UnitType -> AST.ConstructorType (Identity AST.UnitConstructor)
  RecordType name _fields -> constructorType name
  StrictType t -> AST.StrictType (Identity $ toSyntax t)
  ForallType bindings body ->
    AST.ForallType (ZipList $ Identity . bindingToSyntax <$> bindings) (Identity $ toSyntax body)
  ConstrainedType constraints body ->
    AST.ConstrainedType (Identity $ contextToSyntax constraints) (Identity $ toSyntax body)
  WildcardType -> AST.TypeWildcard
  GroundKind -> AST.GroundTypeKind
  FunctionConstructor -> AST.FunctionConstructorType
  where constructorType = AST.ConstructorType . Identity . AST.ConstructorReference

bindingToSyntax :: (Abstract.Haskell l, Abstract.Name l ~ AST.Name l,
                    Abstract.QualifiedName l ~ AST.QualifiedName l, Abstract.Type l ~ AST.Type l,
                    Abstract.Kind l ~ AST.Type l,
                    Abstract.Context l ~ AST.Context l, Abstract.Constructor l ~ AST.Constructor l,
                    Abstract.TypeVarBinding l ~ AST.TypeVarBinding l)
                => TyVarBinding l -> AST.TypeVarBinding l l Identity Identity
bindingToSyntax (TyVarBinding inferred name Nothing) = AST.ImplicitlyKindedTypeVariable inferred name
bindingToSyntax (TyVarBinding inferred name (Just kind)) =
  AST.ExplicitlyKindedTypeVariable inferred name (Identity $ toSyntax kind)

-- | Converts a context back into the syntax
contextToSyntax :: (Abstract.Haskell l, Abstract.Name l ~ AST.Name l,
                    Abstract.QualifiedName l ~ AST.QualifiedName l, Abstract.Type l ~ AST.Type l,
                    Abstract.Kind l ~ AST.Type l,
                    Abstract.Context l ~ AST.Context l, Abstract.Constructor l ~ AST.Constructor l,
                    Abstract.TypeVarBinding l ~ AST.TypeVarBinding l)
                => [TyConstraint l] -> AST.Context l l Identity Identity
contextToSyntax [] = AST.NoContext
contextToSyntax constraints = AST.Constraints (ZipList $ Identity . constraintToSyntax <$> constraints)
  where constraintToSyntax (ClassConstraint name arg) = AST.ClassConstraint name (Identity $ toSyntax arg)
        constraintToSyntax (TypeEquality left right) =
          AST.TypeEquality (Identity $ toSyntax left) (Identity $ toSyntax right)

-- | The type variables free in the given type
freeVariables :: Ord (Abstract.Name l) => Ty l -> Set (Abstract.Name l)
freeVariables = Set.fromList . freeVariableList

-- | The type variables free in the given type, in the order of their first appearance and with repetitions
freeVariableList :: Eq (Abstract.Name l) => Ty l -> [Abstract.Name l]
freeVariableList = \case
  TypeVariable name -> [name]
  ConstructorType{} -> []
  ApplyType f arg -> freeVariableList f <> freeVariableList arg
  FunctionType from to -> freeVariableList from <> freeVariableList to
  ListType item -> freeVariableList item
  TupleType items -> foldMap freeVariableList items
  UnitType -> []
  RecordType _ fields -> foldMap freeVariableList (Compose fields)
  StrictType t -> freeVariableList t
  ForallType bindings body ->
    filter (`notElem` boundVariables bindings) (foldMap bindingKindVariables bindings <> freeVariableList body)
  ConstrainedType constraints body -> foldMap constraintVariables constraints <> freeVariableList body
  WildcardType -> []
  GroundKind -> []
  FunctionConstructor -> []
  where constraintVariables (ClassConstraint _ arg) = freeVariableList arg
        constraintVariables (TypeEquality left right) = freeVariableList left <> freeVariableList right
        bindingKindVariables (TyVarBinding _ _ kind) = foldMap freeVariableList kind

-- | The type variables the given @forall@ bindings bind
boundVariables :: [TyVarBinding l] -> [Abstract.Name l]
boundVariables bindings = [name | TyVarBinding _ name _ <- bindings]

-- | Replaces every /free/ occurrence of a type variable bound by the given map with the type it is bound to, and
-- then substitutes inside the replacement as well. A variable is never expanded within its own replacement, so a
-- cyclic map leaves the variable in place instead of diverging.
substituteTypeVariables :: Ord (Abstract.Name l) => Map (Abstract.Name l) (Ty l) -> Ty l -> Ty l
substituteTypeVariables substitution = substitute
  where substitute t@(TypeVariable name) =
          maybe t (substituteTypeVariables $ Map.delete name substitution) (Map.lookup name substitution)
        substitute (ApplyType f arg) = ApplyType (substitute f) (substitute arg)
        substitute (FunctionType from to) = FunctionType (substitute from) (substitute to)
        substitute (ListType item) = ListType (substitute item)
        substitute (TupleType items) = TupleType (substitute <$> items)
        substitute (RecordType name fields) = RecordType name (getCompose $ substitute <$> Compose fields)
        substitute (StrictType t) = StrictType (substitute t)
        substitute (ForallType bindings body) =
          -- the bound variables shadow the substitution inside the quantified type
          ForallType (substituteBinding <$> bindings) (substituteTypeVariables unshadowed body)
          where unshadowed = substitution `Map.withoutKeys` Set.fromList (boundVariables bindings)
                substituteBinding (TyVarBinding inferred name kind) =
                  TyVarBinding inferred name (substituteTypeVariables unshadowed <$> kind)
        substitute (ConstrainedType constraints body) =
          ConstrainedType (substituteConstraint <$> constraints) (substitute body)
          where substituteConstraint (ClassConstraint name arg) = ClassConstraint name (substitute arg)
                substituteConstraint (TypeEquality left right) = TypeEquality (substitute left) (substitute right)
        substitute t@ConstructorType{} = t
        substitute t@UnitType = t
        substitute t@WildcardType = t
        substitute t@GroundKind = t
        substitute t@FunctionConstructor = t
