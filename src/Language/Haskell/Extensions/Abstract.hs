{-# Language ConstraintKinds, DataKinds, FlexibleContexts, KindSignatures, MultiParamTypeClasses,
             PolyKinds, RankNTypes, TypeFamilies, TypeFamilyDependencies, TypeOperators,
             UndecidableInstances, UndecidableSuperClasses #-}
module Language.Haskell.Extensions.Abstract (
   ExtendedHaskell(..),
   ExtendedWith (build),
   ExtendedWithAllOf,
   Construct (RecordWildCardConstruction, wildcardRecordExpression', wildcardRecordPattern',
              MagicHashConstruction, hashLiteral',
              NamedFieldPunsConstruction, punnedFieldBinding', punnedFieldPattern',
              RecursiveDoConstruction, mdoExpression', recursiveStatement',
              ParallelListComprehensionConstruction, parallelListComprehension',
              TupleSectionConstruction, tupleSectionExpression'),
   ExtensionsSupportedBy, SupportFor, Supports, SupportsNo, SupportsAllOf,
   DeeplyFunctor, DeeplyFoldable, DeeplyTraversable,
   module Language.Haskell.Abstract) where

import qualified Data.Kind as Kind
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Type.Bool (If)
import Data.Void (Void)
import GHC.TypeError (ErrorMessage ((:<>:)))
import qualified GHC.TypeError as TypeError
import qualified Transformation.Deep as Deep

import Language.Haskell.Abstract hiding (DeeplyFunctor, DeeplyFoldable, DeeplyTraversable)
import qualified Language.Haskell.Abstract as Report
import Language.Haskell.Extensions (Extension)
import qualified Language.Haskell.Extensions as Extensions


data family Construct (e :: Extension) :: TreeNodeKind

type family ExtensionsSupportedBy λ :: [Extension]

type family Elem (t :: k) (ts :: [k]) :: Bool where
   Elem t (t ': _) = True
   Elem t (_ ': ts) = Elem t ts
   Elem _ '[] = 'False

type SupportFor (e :: Extension) (l :: Kind.Type) = If (Elem e (ExtensionsSupportedBy l)) () Void

type Supports e l = SupportFor e l ~ ()

type SupportsNo e l = SupportFor e l ~ Void

type family SupportsAllOf (es :: [Extension]) l :: Kind.Constraint where
   SupportsAllOf '[] _ = ()
   SupportsAllOf (e ': es) l = (Supports e l, SupportsAllOf es l)

class Supports e λ => ExtendedWith e λ where
   build :: Construct e λ l d s

type family ExtendedWithAllOf (es :: [Extension]) l :: Kind.Constraint where
   ExtendedWithAllOf '[] _ = ()
   ExtendedWithAllOf (e ': es) l = (ExtendedWith e l, ExtendedWithAllOf es l)

-- * 'Construct' instances for language extensions

data instance Construct 'Extensions.RecordWildCards λ l d s = RecordWildCardConstruction {
   wildcardRecordExpression' :: QualifiedName λ -> [s (FieldBinding l l d d)] -> Expression λ l d s,
   wildcardRecordPattern' :: QualifiedName λ -> [s (FieldPattern l l d d)] -> Pattern λ l d s}

data instance Construct 'Extensions.NamedFieldPuns λ l d s = NamedFieldPunsConstruction {
   punnedFieldBinding' :: QualifiedName λ -> FieldBinding λ l d s,
   punnedFieldPattern' :: QualifiedName λ -> FieldPattern λ l d s}

data instance Construct 'Extensions.MagicHash λ l d s = MagicHashConstruction {
   hashLiteral' :: Value λ l d s -> Value λ l d s}

data instance Construct 'Extensions.RecursiveDo λ l d s = RecursiveDoConstruction {
   mdoExpression' :: s (GuardedExpression l l d d) -> Expression λ l d s,
   recursiveStatement' :: [s (Statement l l d d)] -> Statement λ l d s}

data instance Construct 'Extensions.ParallelListComprehensions λ l d s = ParallelListComprehensionConstruction {
   parallelListComprehension' :: s (Expression l l d d)
                              -> NonEmpty (s (Statement l l d d))
                              -> NonEmpty (s (Statement l l d d))
                              -> [NonEmpty (s (Statement l l d d))]
                              -> Expression λ l d s}

data instance Construct 'Extensions.TupleSections λ l d s = TupleSectionConstruction {
   tupleSectionExpression' :: NonEmpty (Maybe (s (Expression l l d d))) -> Expression λ l d s}

class (Haskell λ,
       ExtendedWithAllOf ['Extensions.MagicHash, 'Extensions.ParallelListComprehensions, 'Extensions.NamedFieldPuns,
                          'Extensions.RecordWildCards, 'Extensions.RecursiveDo, 'Extensions.TupleSections] λ) =>
      ExtendedHaskell λ where
   type GADTConstructor λ = (x :: TreeNodeSubKind) | x -> λ
   type Kind λ = (x :: TreeNodeSubKind) | x -> λ
   type TypeVarBinding λ = (x :: TreeNodeSubKind) | x -> λ
   type ModuleMember λ = x | x -> λ
   type TypeRole λ = x | x -> λ
   hashLiteral :: Value λ l d s -> Value λ l d s
   hashLiteral = hashLiteral' build
   mdoExpression :: s (GuardedExpression l l d d) -> Expression λ l d s
   mdoExpression = mdoExpression' build
   parallelListComprehension :: s (Expression l l d d)
                             -> NonEmpty (s (Statement l l d d))
                             -> NonEmpty (s (Statement l l d d))
                             -> [NonEmpty (s (Statement l l d d))]
                             -> Expression λ l d s
   parallelListComprehension = parallelListComprehension' build
   tupleSectionExpression :: NonEmpty (Maybe (s (Expression l l d d))) -> Expression λ l d s
   tupleSectionExpression = tupleSectionExpression' build

   lambdaCaseExpression :: [s (CaseAlternative l l d d)] -> Expression λ l d s
   multiWayIfExpression :: [s (GuardedExpression l l d d)] -> Expression λ l d s
   overloadedLabel :: Text -> Expression λ l d s
   getField :: s (Expression l l d d) -> Name λ -> Expression λ l d s
   fieldProjection :: NonEmpty (Name λ) -> Expression λ l d s
   wildcardRecordExpression :: QualifiedName λ -> [s (FieldBinding l l d d)] -> Expression λ l d s
   wildcardRecordExpression = wildcardRecordExpression' build
   recursiveStatement :: [s (Statement l l d d)] -> Statement λ l d s
   recursiveStatement = recursiveStatement' build
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
   forallType :: [TypeVarBinding λ l d s] -> s (Type l l d d) -> Type λ l d s
   constrainedType :: s (Context l l d d) -> s (Type l l d d) -> Type λ l d s
   kindedType :: s (Type l l d d) -> s (Kind l l d d) -> Type λ l d s
   typeWildcard :: Type λ l d s
   groundType :: Type λ l d s

   explicitlyKindedTypeVariable :: Name λ -> s (Kind l l d d) -> TypeVarBinding λ l d s
   implicitlyKindedTypeVariable :: Name λ -> TypeVarBinding λ l d s
   inferredTypeVariable :: Name λ -> TypeVarBinding λ l d s
   inferredExplicitlyKindedTypeVariable :: Name λ -> s (Kind l l d d) -> TypeVarBinding λ l d s

   kindVariable :: Name λ -> Kind λ l d s
   constructorKind :: s (Constructor l l d d) -> Kind λ l d s
   kindApplication :: s (Kind l l d d) -> s (Kind l l d d) -> Kind λ l d s
   infixKindApplication :: s (Kind l l d d) -> QualifiedName λ -> s (Kind l l d d) -> Kind λ l d s
   functionKind :: s (Kind l l d d) -> s (Kind l l d d) -> Kind λ l d s
   forallKind :: [TypeVarBinding λ l d s] -> s (Kind l l d d) -> Kind λ l d s
   groundTypeKind :: Kind λ l d s
   typeKind :: s (Type l l d d) -> Kind λ l d s
   typeRoleDeclaration :: QualifiedName λ -> [TypeRole λ] -> Declaration λ l d s
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
   multiplicityFunctionType :: s (Type l l d d) -> s (Type l l d d) -> s (Type l l d d) -> Type λ l d s
   linearFunctionType :: s (Type l l d d) -> s (Type l l d d) -> Type λ l d s

   punnedFieldBinding :: QualifiedName λ -> FieldBinding λ l d s
   punnedFieldPattern :: QualifiedName λ -> FieldPattern λ l d s
   punnedFieldBinding = punnedFieldBinding' build
   punnedFieldPattern = punnedFieldPattern' build

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
   kindSignature :: Name λ -> s (Context l l d d) -> s (Kind l l d d) -> Declaration λ l d s

   typeEqualityConstraint :: s (Type l l d d) -> s (Type l l d d) -> Context λ l d s
   multiParameterClassConstraint :: QualifiedName λ -> [s (Type l l d d)] -> Context λ l d s
   infixConstraint :: s (Type l l d d) -> QualifiedName λ -> s (Type l l d d) -> Context λ l d s
   constraintType :: s (Context l l d d) -> Type λ l d s

   explicitlyNamespacedMemberList :: [ModuleMember λ] -> Members λ
   defaultMember, patternMember, typeMember :: Name λ -> ModuleMember λ

   inferredRole :: TypeRole λ
   nominalRole :: TypeRole λ
   representationalRole :: TypeRole λ
   phantomRole :: TypeRole λ

   promotedConstructorType :: s (Constructor l l d d) -> Type λ l d s
   promotedTupleType :: NonEmpty (s (Type l l d d)) -> Type λ l d s
   promotedListType :: [s (Type l l d d)] -> Type λ l d s
   promotedIntegerLiteral :: Integer -> Type λ l d s
   promotedCharLiteral :: Char -> Type λ l d s
   promotedStringLiteral :: Text -> Type λ l d s
   promotedInfixTypeApplication :: s (Type l l d d) -> QualifiedName λ -> s (Type l l d d) -> Type λ l d s
   visibleDependentType :: [TypeVarBinding λ l d s] -> s (Type l l d d) -> Type λ l d s
   tupleKind :: NonEmpty (s (Kind l l d d)) -> Kind λ l d s
   listKind :: s (Kind l l d d) -> Kind λ l d s
   typeRepresentationKind :: s (Type l l d d) -> Kind λ l d s

   visibleTypeApplication :: s (Expression l l d d) -> s (Type l l d d) -> Expression λ l d s
   visibleKindApplication :: s (Type l l d d) -> s (Kind l l d d) -> Type λ l d s
   visibleKindKindApplication :: s (Kind l l d d) -> s (Kind l l d d) -> Kind λ l d s
   constructorPatternWithTypeApplications :: s (Constructor l l d d) -> [s (Type l l d d)] -> [s (Pattern l l d d)]
                                          -> Pattern λ l d s
   wildcardRecordPattern :: QualifiedName λ -> [s (FieldPattern l l d d)] -> Pattern λ l d s
   wildcardRecordPattern = wildcardRecordPattern' build
   classInstanceLHSKindApplication :: s (ClassInstanceLHS l l d d) -> s (Kind l l d d) -> ClassInstanceLHS λ l d s

type DeeplyFunctor t l = (Deep.Functor t (GADTConstructor l l), Deep.Functor t (Kind l l),
                          Deep.Functor t (TypeVarBinding l l), Report.DeeplyFunctor t l)
type DeeplyFoldable t l = (Deep.Foldable t (GADTConstructor l l), Deep.Foldable t (Kind l l),
                           Deep.Foldable t (TypeVarBinding l l), Report.DeeplyFoldable t l)
type DeeplyTraversable t l = (Deep.Traversable t (GADTConstructor l l), Deep.Traversable t (Kind l l),
                              Deep.Traversable t (TypeVarBinding l l), Report.DeeplyTraversable t l)
