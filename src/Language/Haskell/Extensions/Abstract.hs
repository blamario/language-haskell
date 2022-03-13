{-# Language ConstraintKinds, FlexibleContexts, KindSignatures, TypeFamilies, TypeFamilyDependencies #-}
module Language.Haskell.Extensions.Abstract (ExtendedHaskell(..),
                                             DeeplyFunctor, DeeplyFoldable, DeeplyTraversable,
                                             module Language.Haskell.Abstract) where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import qualified Transformation.Deep as Deep

import Language.Haskell.Abstract hiding (DeeplyFunctor, DeeplyFoldable, DeeplyTraversable)
import qualified Language.Haskell.Abstract as Report

class Haskell λ => ExtendedHaskell λ where
   type GADTConstructor λ = (x :: * -> (* -> *) -> (* -> *) -> *) | x -> λ
   type Kind λ = (x :: * -> (* -> *) -> (* -> *) -> *) | x -> λ
   type TypeVarBinding λ = (x :: * -> (* -> *) -> (* -> *) -> *) | x -> λ
   type ModuleMember λ = x | x -> λ
   hashLiteral :: Value λ l d s -> Value λ l d s
   mdoExpression :: s (GuardedExpression l l d d) -> Expression λ l d s
   parallelListComprehension :: s (Expression l l d d)
                             -> NonEmpty (s (Statement l l d d))
                             -> NonEmpty (s (Statement l l d d))
                             -> [NonEmpty (s (Statement l l d d))]
                             -> Expression λ l d s
   tupleSectionExpression :: NonEmpty (Maybe (s (Expression l l d d))) -> Expression λ l d s
   lambdaCaseExpression :: [s (CaseAlternative l l d d)] -> Expression λ l d s
   multiWayIfExpression :: [s (GuardedExpression l l d d)] -> Expression λ l d s
   recursiveStatement :: [s (Statement l l d d)] -> Statement λ l d s
   safeImportDeclaration :: Bool -> ModuleName λ -> Maybe (ModuleName λ)
                         -> Maybe (s (ImportSpecification l l d d))
                         -> Import λ l d s
   packageQualifiedImportDeclaration :: Bool -> Text -> ModuleName λ -> Maybe (ModuleName λ)
                                     -> Maybe (s (ImportSpecification l l d d))
                                     -> Import λ l d s
   safePackageQualifiedImportDeclaration :: Bool -> Text -> ModuleName λ -> Maybe (ModuleName λ)
                                         -> Maybe (s (ImportSpecification l l d d))
                                         -> Import λ l d s
   infixTypeApplication :: s (Type l l d d) -> QualifiedName λ -> s (Type l l d d) -> Type λ l d s
   simpleInfixTypeLHSApplication :: TypeVarBinding λ l d s -> Name λ -> TypeVarBinding λ l d s -> TypeLHS λ l d s
   simpleTypeLHSApplication :: s (TypeLHS l l d d) -> TypeVarBinding λ l d s -> TypeLHS λ l d s
   simpleKindedTypeLHS :: Name λ -> [TypeVarBinding λ l d s] -> TypeLHS λ l d s
   existentialConstructor :: [TypeVarBinding λ l d s] -> s (Context l l d d) -> s (DataConstructor l l d d)
                          -> DataConstructor λ l d s
   explicitlyScopedInstanceDeclaration :: [TypeVarBinding λ l d s]
                                       -> s (Context l l d d)
                                       -> s (ClassInstanceLHS l l d d)
                                       -> [s (Declaration l l d d)]
                                       -> Declaration λ l d s
   forallType :: [TypeVarBinding λ l d s] -> s (Context l l d d) -> s (Type l l d d) -> Type λ l d s
   kindedType :: s (Type l l d d) -> s (Kind l l d d) -> Type λ l d s
   typeWildcard :: Type λ l d s
   explicitlyKindedTypeVariable :: Name λ -> s (Kind l l d d) -> TypeVarBinding λ l d s
   implicitlyKindedTypeVariable :: Name λ -> TypeVarBinding λ l d s
   kindVariable :: Name λ -> Kind λ l d s
   constructorKind :: s (Constructor l l d d) -> Kind λ l d s
   kindApplication :: s (Kind l l d d) -> s (Kind l l d d) -> Kind λ l d s
   infixKindApplication :: s (Kind l l d d) -> QualifiedName λ -> s (Kind l l d d) -> Kind λ l d s
   functionKind :: s (Kind l l d d) -> s (Kind l l d d) -> Kind λ l d s
   forallKind :: [TypeVarBinding λ l d s] -> s (Kind l l d d) -> Kind λ l d s
   groundTypeKind :: Kind λ l d s
   kindedDataDeclaration :: s (Context l l d d) -> s (TypeLHS l l d d) -> s (Kind l l d d)
                         -> [s (DataConstructor l l d d)] -> [s (DerivingClause l l d d)] -> Declaration λ l d s
   kindedNewtypeDeclaration :: s (Context l l d d) -> s (TypeLHS l l d d) -> s (Kind l l d d)
                            -> s (DataConstructor l l d d) -> [s (DerivingClause l l d d)] -> Declaration λ l d s
   gadtDeclaration :: s (TypeLHS l l d d) -> Maybe (s (Kind l l d d)) -> [s (GADTConstructor l l d d)]
                   -> [s (DerivingClause l l d d)] -> Declaration λ l d s
   gadtNewtypeDeclaration :: s (TypeLHS l l d d) -> Maybe (s (Kind l l d d)) -> s (GADTConstructor l l d d)
                          -> [s (DerivingClause l l d d)] -> Declaration λ l d s
   gadtConstructors :: NonEmpty (Name λ) -> [TypeVarBinding λ l d s] -> s (Context l l d d) -> s (Type l l d d)
                   -> GADTConstructor λ l d s
   recordFunctionType :: [s (FieldDeclaration l l d d)] -> s (Type l l d d) -> Type λ l d s

   dataFamilyDeclaration :: s (TypeLHS l l d d) -> Maybe (s (Kind l l d d)) -> Declaration λ l d s
   openTypeFamilyDeclaration :: s (TypeLHS l l d d) -> Maybe (s (Kind l l d d)) -> Declaration λ l d s
   closedTypeFamilyDeclaration :: s (TypeLHS l l d d) -> Maybe (s (Kind l l d d)) -> [s (Declaration l l d d)]
                               -> Declaration λ l d s
   injectiveOpenTypeFamilyDeclaration :: s (TypeLHS l l d d) -> TypeVarBinding λ l d s
                                      -> Maybe (Name λ, NonEmpty (Name λ)) -> Declaration λ l d s
   injectiveClosedTypeFamilyDeclaration :: s (TypeLHS l l d d) -> TypeVarBinding λ l d s
                                        -> Maybe (Name λ, NonEmpty (Name λ)) -> [s (Declaration l l d d)]
                                        -> Declaration λ l d s
   dataFamilyInstance :: [TypeVarBinding λ l d s] -> s (Context l l d d) -> s (ClassInstanceLHS l l d d)
                      -> Maybe (s (Kind l l d d)) -> [s (DataConstructor l l d d)] -> [s (DerivingClause l l d d)]
                      -> Declaration λ l d s
   newtypeFamilyInstance :: [TypeVarBinding λ l d s] -> s (Context l l d d) -> s (ClassInstanceLHS l l d d)
                         -> Maybe (s (Kind l l d d)) -> s (DataConstructor l l d d) -> [s (DerivingClause l l d d)]
                         -> Declaration λ l d s
   gadtDataFamilyInstance :: [TypeVarBinding λ l d s] -> s (ClassInstanceLHS l l d d) -> Maybe (s (Kind l l d d))
                          -> [s (GADTConstructor l l d d)] -> [s (DerivingClause l l d d)] -> Declaration λ l d s
   gadtNewtypeFamilyInstance :: [TypeVarBinding λ l d s] -> s (ClassInstanceLHS l l d d) -> Maybe (s (Kind l l d d))
                             -> s (GADTConstructor l l d d) -> [s (DerivingClause l l d d)] -> Declaration λ l d s
   typeFamilyInstance :: [TypeVarBinding λ l d s] -> s (ClassInstanceLHS l l d d) -> s (Type l l d d)
                      -> Declaration λ l d s
   classReferenceInstanceLHS :: QualifiedName λ -> ClassInstanceLHS λ l d s
   infixTypeClassInstanceLHS :: s (Type l l d d) -> QualifiedName λ -> s (Type l l d d) -> ClassInstanceLHS λ l d s
   classInstanceLHSApplication :: s (ClassInstanceLHS l l d d) -> s (Type l l d d) -> ClassInstanceLHS λ l d s

   typeEqualityConstraint :: s (Type l l d d) -> s (Type l l d d) -> Context λ l d s
   multiParameterClassConstraint :: QualifiedName λ -> [s (Type l l d d)] -> Context λ l d s
   infixConstraint :: s (Type l l d d) -> QualifiedName λ -> s (Type l l d d) -> Context λ l d s
   constraintType :: s (Context l l d d) -> Type λ l d s

   explicitlyNamespacedMemberList :: [ModuleMember λ] -> Members λ
   defaultMember, patternMember, typeMember :: Name λ -> ModuleMember λ

   promotedConstructorType :: s (Constructor l l d d) -> Type λ l d s
   promotedTupleType :: NonEmpty (s (Type l l d d)) -> Type λ l d s
   promotedListType :: [s (Type l l d d)] -> Type λ l d s
   promotedIntegerLiteral :: Integer -> Type λ l d s
   promotedCharLiteral :: Char -> Type λ l d s
   promotedStringLiteral :: Text -> Type λ l d s
   promotedInfixTypeApplication :: s (Type l l d d) -> QualifiedName λ -> s (Type l l d d) -> Type λ l d s
   tupleKind :: NonEmpty (s (Kind l l d d)) -> Kind λ l d s
   listKind :: s (Kind l l d d) -> Kind λ l d s
   typeRepresentationKind :: s (Type l l d d) -> Kind λ l d s

type DeeplyFunctor t l = (Deep.Functor t (GADTConstructor l l), Deep.Functor t (Kind l l),
                          Deep.Functor t (TypeVarBinding l l), Report.DeeplyFunctor t l)
type DeeplyFoldable t l = (Deep.Foldable t (GADTConstructor l l), Deep.Foldable t (Kind l l),
                           Deep.Foldable t (TypeVarBinding l l), Report.DeeplyFoldable t l)
type DeeplyTraversable t l = (Deep.Traversable t (GADTConstructor l l), Deep.Traversable t (Kind l l),
                              Deep.Traversable t (TypeVarBinding l l), Report.DeeplyTraversable t l)
