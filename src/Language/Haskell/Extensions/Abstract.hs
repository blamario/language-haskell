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
   simpleInfixTypeLHSApplication :: Name λ -> Name λ -> Name λ -> TypeLHS λ l d s
   simpleTypeLHSApplication :: s (TypeLHS l l d d) -> Name λ -> TypeLHS λ l d s
   kindedSimpleTypeLHSApplication :: s (TypeLHS l l d d) -> Name λ -> s (Kind l l d d) -> TypeLHS λ l d s
   existentialConstructor :: [TypeVarBinding λ l d s] -> s (Context l l d d) -> s (DataConstructor l l d d)
                          -> DataConstructor λ l d s
   explicitlyScopedInstanceDeclaration :: NonEmpty (TypeVarBinding λ l d s)
                                       -> s (Context l l d d)
                                       -> s (ClassInstanceLHS l l d d)
                                       -> [s (Declaration l l d d)]
                                       -> Declaration λ l d s
   forallType :: [TypeVarBinding λ l d s] -> s (Context l l d d) -> s (Type l l d d) -> Type λ l d s
   boundTypeVariable :: TypeVarBinding λ l d s -> Type λ l d s
   explicitlyKindedTypeVariable :: Name λ -> s (Kind l l d d) -> TypeVarBinding λ l d s
   implicitlyKindedTypeVariable :: Name λ -> TypeVarBinding λ l d s
   kindVariable :: Name λ -> Kind λ l d s
   constructorKind :: s (Constructor l l d d) -> Kind λ l d s
   kindApplication :: s (Kind l l d d) -> s (Kind l l d d) -> Kind λ l d s
   functionKind :: s (Kind l l d d) -> s (Kind l l d d) -> Kind λ l d s
   groundTypeKind :: Kind λ l d s
   kindedDataDeclaration :: s (Context l l d d) -> s (TypeLHS l l d d) -> s (Kind l l d d)
                         -> [s (DataConstructor l l d d)] -> [s (DerivingClause l l d d)] -> Declaration λ l d s
   gadtDeclaration :: s (TypeLHS l l d d) -> Maybe (s (Kind l l d d)) -> [s (GADTConstructor l l d d)]
                   -> [s (DerivingClause l l d d)] -> Declaration λ l d s
   gadtConstructors :: NonEmpty (Name λ) -> [TypeVarBinding λ l d s] -> s (Context l l d d) -> s (Type l l d d)
                   -> GADTConstructor λ l d s
   recordFunctionType :: [s (FieldDeclaration l l d d)] -> s (Type l l d d) -> Type λ l d s

type DeeplyFunctor t l = (Deep.Functor t (GADTConstructor l l), Deep.Functor t (Kind l l),
                          Deep.Functor t (TypeVarBinding l l), Report.DeeplyFunctor t l)
type DeeplyFoldable t l = (Deep.Foldable t (GADTConstructor l l), Deep.Foldable t (Kind l l),
                           Deep.Foldable t (TypeVarBinding l l), Report.DeeplyFoldable t l)
type DeeplyTraversable t l = (Deep.Traversable t (GADTConstructor l l), Deep.Traversable t (Kind l l),
                              Deep.Traversable t (TypeVarBinding l l), Report.DeeplyTraversable t l)
