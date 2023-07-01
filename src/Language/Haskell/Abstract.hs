{-# Language ConstraintKinds, FlexibleContexts, KindSignatures, TypeFamilies, TypeFamilyDependencies #-}

module Language.Haskell.Abstract where

import qualified Data.Kind as Kind
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import qualified Rank2
import qualified Transformation.Deep as Deep

import Language.Haskell.Extensions (ExtensionSwitch)

type Language = Kind.Type
type NodeWrap = Kind.Type -> Kind.Type
type TreeNodeKind = Language -> TreeNodeSubKind
type TreeNodeSubKind = Language -> NodeWrap -> NodeWrap -> Kind.Type

class Haskell λ where
   type Module λ = (x :: TreeNodeSubKind) | x -> λ
   type Declaration λ = (x :: TreeNodeSubKind) | x -> λ
   type Expression λ = (x :: TreeNodeSubKind) | x -> λ
   type Type λ = (x :: TreeNodeSubKind) | x -> λ

   type EquationLHS λ = (x :: TreeNodeSubKind) | x -> λ
   type EquationRHS λ = (x :: TreeNodeSubKind) | x -> λ
   type GuardedExpression λ = (x :: TreeNodeSubKind) | x -> λ
   type Pattern λ = (x :: TreeNodeSubKind) | x -> λ
   type Statement λ = (x :: TreeNodeSubKind) | x -> λ
   type ClassInstanceLHS λ = (x :: TreeNodeSubKind) | x -> λ
   type TypeLHS λ = (x :: TreeNodeSubKind) | x -> λ

   type Import λ = (x :: TreeNodeSubKind) | x -> λ
   type ImportSpecification λ = (x :: TreeNodeSubKind) | x -> λ
   type ImportItem λ = (x :: TreeNodeSubKind) | x -> λ
   type Export λ = (x :: TreeNodeSubKind) | x -> λ

   type Context λ = (x :: TreeNodeSubKind) | x -> λ
   type DataConstructor λ = (x :: TreeNodeSubKind) | x -> λ
   type DerivingClause λ = (x :: TreeNodeSubKind) | x -> λ
   type FieldDeclaration λ = (x :: TreeNodeSubKind) | x -> λ
   type FieldBinding λ = (x :: TreeNodeSubKind) | x -> λ
   type FieldPattern λ = (x :: TreeNodeSubKind) | x -> λ
   type CaseAlternative λ = (x :: TreeNodeSubKind) | x -> λ

   type Constructor λ = (x :: TreeNodeSubKind) | x -> λ
   type Value λ = (x :: TreeNodeSubKind) | x -> λ

   type CallingConvention λ = x | x -> λ
   type CallSafety λ = x | x -> λ
   type Associativity λ = x | x -> λ
   type Members λ = x | x -> λ
   type Name λ = x | x -> λ
   type ModuleName λ = x | x -> λ
   type QualifiedName λ = x | x -> λ

   anonymousModule :: [s (Import l l d d)] -> [s (Declaration l l d d)] -> Module λ l d s
   namedModule :: ModuleName λ -> Maybe [s (Export l l d d)] -> [s (Import l l d d)] -> [s (Declaration l l d d)]
               -> Module λ l d s
   withLanguagePragma :: [ExtensionSwitch] -> s (Module l l d d) -> Module λ l d s

   exportClassOrType :: QualifiedName λ -> Maybe (Members λ) -> Export λ l d s
   exportVar :: QualifiedName λ -> Export λ l d s
   reExportModule :: ModuleName λ -> Export λ l d s

   importDeclaration :: Bool -> ModuleName λ -> Maybe (ModuleName λ) -> Maybe (s (ImportSpecification l l d d))
                     -> Import λ l d s
   excludedImports, includedImports :: [s (ImportItem l l d d)] -> ImportSpecification λ l d s
   importClassOrType :: Name λ -> Maybe (Members λ) -> ImportItem λ l d s
   importVar :: Name λ -> ImportItem λ l d s

   allMembers :: Members λ
   memberList :: [Name λ] -> Members λ

   classDeclaration :: s (Context l l d d) -> s (TypeLHS l l d d) -> [s (Declaration l l d d)] -> Declaration λ l d s
   dataDeclaration :: s (Context l l d d) -> s (TypeLHS l l d d) -> [s (DataConstructor l l d d)]
                   -> [s (DerivingClause l l d d)] -> Declaration λ l d s
   defaultDeclaration :: [s (Type l l d d)] -> Declaration λ l d s
   equationDeclaration :: s (EquationLHS l l d d) -> s (EquationRHS l l d d) -> [s (Declaration l l d d)]
                       -> Declaration λ l d s
   fixityDeclaration :: Associativity λ -> Maybe Int -> NonEmpty (Name λ) -> Declaration λ l d s
   foreignExport :: CallingConvention λ -> Maybe Text -> Name λ -> s (Type l l d d) -> Declaration λ l d s
   foreignImport :: CallingConvention λ -> Maybe (CallSafety λ) -> Maybe Text -> Name λ -> s (Type l l d d)
                 -> Declaration λ l d s
   instanceDeclaration :: s (Context l l d d) -> s (ClassInstanceLHS l l d d) -> [s (Declaration l l d d)]
                       -> Declaration λ l d s
   newtypeDeclaration :: s (Context l l d d) -> s (TypeLHS l l d d) -> s (DataConstructor l l d d)
                      -> [s (DerivingClause l l d d)] -> Declaration λ l d s
   typeSynonymDeclaration :: s (TypeLHS l l d d) -> s (Type l l d d) -> Declaration λ l d s
   typeSignature :: NonEmpty (Name λ) -> s (Context l l d d) -> s (Type l l d d) -> Declaration λ l d s

   applyExpression :: s (Expression l l d d) -> s (Expression l l d d) -> Expression λ l d s
   conditionalExpression :: s (Expression l l d d) -> s (Expression l l d d) -> s (Expression l l d d)
                         -> Expression λ l d s
   constructorExpression :: s (Constructor l l d d) -> Expression λ l d s
   caseExpression :: s (Expression l l d d) -> [s (CaseAlternative l l d d)] -> Expression λ l d s
   doExpression :: s (GuardedExpression l l d d) -> Expression λ l d s
   infixExpression :: s (Expression l l d d) -> s (Expression l l d d) -> s (Expression l l d d) -> Expression λ l d s
   leftSectionExpression :: s (Expression l l d d) -> QualifiedName λ -> Expression λ l d s
   lambdaExpression :: [s (Pattern l l d d)] -> s (Expression l l d d) -> Expression λ l d s
   letExpression :: [s (Declaration l l d d)] -> s (Expression l l d d) -> Expression λ l d s
   listComprehension :: s (Expression l l d d) -> NonEmpty (s (Statement l l d d)) -> Expression λ l d s
   listExpression :: [s (Expression l l d d)] -> Expression λ l d s
   literalExpression :: s (Value l l d d) -> Expression λ l d s
   negate :: Expression λ l d s
   recordExpression :: s (Expression l l d d) -> [s (FieldBinding l l d d)] -> Expression λ l d s
   referenceExpression :: QualifiedName λ -> Expression λ l d d
   rightSectionExpression :: QualifiedName λ -> s (Expression l l d d) -> Expression λ l d s
   sequenceExpression :: s (Expression l l d d) -> Maybe (s (Expression l l d d)) -> Maybe (s (Expression l l d d))
                      -> Expression λ l d s
   tupleExpression :: NonEmpty (s (Expression l l d d)) -> Expression λ l d s
   typedExpression :: s (Expression l l d d) -> s (Type l l d d) -> Expression λ l d s

   asPattern :: Name λ -> s (Pattern l l d d) -> Pattern λ l d s
   constructorPattern :: s (Constructor l l d d) -> [s (Pattern l l d d)] -> Pattern λ l d s
   infixPattern :: s (Pattern l l d d) -> QualifiedName λ -> s (Pattern l l d d) -> Pattern λ l d s
   irrefutablePattern :: s (Pattern l l d d) -> Pattern λ l d s
   listPattern :: [s (Pattern l l d d)] -> Pattern λ l d s
   literalPattern :: s (Value l l d d) -> Pattern λ l d s
   recordPattern :: QualifiedName λ -> [s (FieldPattern l l d d)] -> Pattern λ l d s
   tuplePattern :: NonEmpty (s (Pattern l l d d)) -> Pattern λ l d s
   variablePattern :: Name λ -> Pattern λ l d s
   wildcardPattern :: Pattern λ l d s

   constructorType :: s (Constructor l l d d) -> Type λ l d s
   functionConstructorType :: Type λ l d s
   functionType :: s (Type l l d d) -> s (Type l l d d) -> Type λ l d s
   listType :: s (Type l l d d) -> Type λ l d s
   strictType :: s (Type l l d d) -> Type λ l d s
   tupleType :: NonEmpty (s (Type l l d d)) -> Type λ l d s
   typeApplication :: s (Type l l d d) -> s (Type l l d d) -> Type λ l d s
   typeVariable :: Name λ -> Type λ l d s

   constructorReference :: QualifiedName λ -> Constructor λ l d s
   emptyListConstructor :: Constructor λ l d s
   tupleConstructor :: Int -> Constructor λ l d s
   unitConstructor :: Constructor λ l d s

   constructor :: Name λ -> [s (Type l l d d)] -> DataConstructor λ l d s
   recordConstructor :: Name λ -> [s (FieldDeclaration l l d d)] -> DataConstructor λ l d s
   constructorFields :: NonEmpty (Name λ) -> s (Type l l d d) -> FieldDeclaration λ l d s

   fieldBinding :: QualifiedName λ -> s (Expression l l d d) -> FieldBinding λ l d s
   fieldPattern :: QualifiedName λ -> s (Pattern l l d d) -> FieldPattern λ l d s

   simpleDerive :: QualifiedName λ -> DerivingClause λ l d s

   typeClassInstanceLHS :: QualifiedName λ -> s (Type l l d d) -> ClassInstanceLHS λ l d s
   simpleTypeLHS :: Name λ -> [Name λ] -> TypeLHS λ l d s

   prefixLHS :: s (EquationLHS l l d d) -> NonEmpty (s (Pattern l l d d)) -> EquationLHS λ l d s
   infixLHS :: s (Pattern l l d d) -> Name λ -> s (Pattern l l d d) -> EquationLHS λ l d s
   patternLHS :: s (Pattern l l d d) -> EquationLHS λ l d s
   variableLHS :: Name λ -> EquationLHS λ l d s

   caseAlternative :: s (Pattern l l d d) -> s (EquationRHS l l d d) -> [s (Declaration l l d d)]
                   -> CaseAlternative λ l d s

   guardedRHS :: NonEmpty (s (GuardedExpression l l d d)) -> EquationRHS λ l d s
   normalRHS :: s (Expression l l d d) -> EquationRHS λ l d s

   guardedExpression :: [s (Statement l l d d)] -> s (Expression  l l d d) -> GuardedExpression λ l d s

   simpleConstraint :: QualifiedName λ -> Name λ -> Context λ l d s
   classConstraint :: QualifiedName λ -> s (Type l l d d) -> Context λ l d s
   constraints :: [s (Context l l d d)] -> Context λ l d s
   noContext :: Context λ l d s

   bindStatement :: s (Pattern l l d d) -> s (Expression l l d d) -> Statement λ l d s
   expressionStatement :: s (Expression l l d d) -> Statement λ l d s
   letStatement :: [s (Declaration l l d d)] -> Statement λ l d s

   charLiteral :: Char -> Value λ l d s
   floatingLiteral :: Rational -> Value λ l d s
   integerLiteral :: Integer -> Value λ l d s
   stringLiteral :: Text -> Value λ l d s

   name :: Text -> Name λ
   moduleName :: NonEmpty (Name λ) -> ModuleName λ
   qualifiedName :: Maybe (ModuleName λ) -> Name λ -> QualifiedName λ

   nonAssociative, leftAssociative, rightAssociative :: Associativity λ

   cCall, cppCall, dotNetCall, jvmCall, stdCall :: CallingConvention λ
   safeCall, unsafeCall :: CallSafety λ

type DeeplyFunctor t l = (Deep.Functor t (Module l l),
                          Deep.Functor t (Declaration l l),
                          Deep.Functor t (Expression l l),
                          Deep.Functor t (Type l l),
                          Deep.Functor t (EquationLHS l l),
                          Deep.Functor t (EquationRHS l l),
                          Deep.Functor t (GuardedExpression l l),
                          Deep.Functor t (Pattern l l),
                          Deep.Functor t (Statement l l),
                          Deep.Functor t (ClassInstanceLHS l l),
                          Deep.Functor t (TypeLHS l l),
                          Deep.Functor t (Import l l),
                          Deep.Functor t (ImportSpecification l l),
                          Deep.Functor t (ImportItem l l),
                          Deep.Functor t (Export l l),
                          Deep.Functor t (Context l l),
                          Deep.Functor t (DataConstructor l l),
                          Deep.Functor t (DerivingClause l l),
                          Deep.Functor t (FieldDeclaration l l),
                          Deep.Functor t (FieldBinding l l),
                          Deep.Functor t (FieldPattern l l),
                          Deep.Functor t (CaseAlternative l l),
                          Deep.Functor t (Constructor l l),
                          Deep.Functor t (Value l l))

type DeeplyFoldable t l = (Deep.Foldable t (Module l l),
                           Deep.Foldable t (Declaration l l),
                           Deep.Foldable t (Expression l l),
                           Deep.Foldable t (Type l l),
                           Deep.Foldable t (EquationLHS l l),
                           Deep.Foldable t (EquationRHS l l),
                           Deep.Foldable t (GuardedExpression l l),
                           Deep.Foldable t (Pattern l l),
                           Deep.Foldable t (Statement l l),
                           Deep.Foldable t (ClassInstanceLHS l l),
                           Deep.Foldable t (TypeLHS l l),
                           Deep.Foldable t (Import l l),
                           Deep.Foldable t (ImportSpecification l l),
                           Deep.Foldable t (ImportItem l l),
                           Deep.Foldable t (Export l l),
                           Deep.Foldable t (Context l l),
                           Deep.Foldable t (DataConstructor l l),
                           Deep.Foldable t (DerivingClause l l),
                           Deep.Foldable t (FieldDeclaration l l),
                           Deep.Foldable t (FieldBinding l l),
                           Deep.Foldable t (FieldPattern l l),
                           Deep.Foldable t (CaseAlternative l l),
                           Deep.Foldable t (Constructor l l),
                           Deep.Foldable t (Value l l))

type DeeplyTraversable t l = (Deep.Traversable t (Module l l),
                              Deep.Traversable t (Declaration l l),
                              Deep.Traversable t (Expression l l),
                              Deep.Traversable t (Type l l),
                              Deep.Traversable t (EquationLHS l l),
                              Deep.Traversable t (EquationRHS l l),
                              Deep.Traversable t (GuardedExpression l l),
                              Deep.Traversable t (Pattern l l),
                              Deep.Traversable t (Statement l l),
                              Deep.Traversable t (ClassInstanceLHS l l),
                              Deep.Traversable t (TypeLHS l l),
                              Deep.Traversable t (Import l l),
                              Deep.Traversable t (ImportSpecification l l),
                              Deep.Traversable t (ImportItem l l),
                              Deep.Traversable t (Export l l),
                              Deep.Traversable t (Context l l),
                              Deep.Traversable t (DataConstructor l l),
                              Deep.Traversable t (DerivingClause l l),
                              Deep.Traversable t (FieldDeclaration l l),
                              Deep.Traversable t (FieldBinding l l),
                              Deep.Traversable t (FieldPattern l l),
                              Deep.Traversable t (CaseAlternative l l),
                              Deep.Traversable t (Constructor l l),
                              Deep.Traversable t (Value l l))

type Rank2lyFunctor l f = (Rank2.Functor (Module l l f),
                           Rank2.Functor (Declaration l l f),
                           Rank2.Functor (Expression l l f),
                           Rank2.Functor (Type l l f),
                           Rank2.Functor (EquationLHS l l f),
                           Rank2.Functor (EquationRHS l l f),
                           Rank2.Functor (GuardedExpression l l f),
                           Rank2.Functor (Pattern l l f),
                           Rank2.Functor (Statement l l f),
                           Rank2.Functor (ClassInstanceLHS l l f),
                           Rank2.Functor (TypeLHS l l f),
                           Rank2.Functor (Import l l f),
                           Rank2.Functor (ImportSpecification l l f),
                           Rank2.Functor (ImportItem l l f),
                           Rank2.Functor (Export l l f),
                           Rank2.Functor (Context l l f),
                           Rank2.Functor (DataConstructor l l f),
                           Rank2.Functor (DerivingClause l l f),
                           Rank2.Functor (FieldDeclaration l l f),
                           Rank2.Functor (FieldBinding l l f),
                           Rank2.Functor (FieldPattern l l f),
                           Rank2.Functor (CaseAlternative l l f),
                           Rank2.Functor (Constructor l l f),
                           Rank2.Functor (Value l l f))

type Rank2lyFoldable l f = (Rank2.Foldable (Module l l f),
                            Rank2.Foldable (Declaration l l f),
                            Rank2.Foldable (Expression l l f),
                            Rank2.Foldable (Type l l f),
                            Rank2.Foldable (EquationLHS l l f),
                            Rank2.Foldable (EquationRHS l l f),
                            Rank2.Foldable (GuardedExpression l l f),
                            Rank2.Foldable (Pattern l l f),
                            Rank2.Foldable (Statement l l f),
                            Rank2.Foldable (ClassInstanceLHS l l f),
                            Rank2.Foldable (TypeLHS l l f),
                            Rank2.Foldable (Import l l f),
                            Rank2.Foldable (ImportSpecification l l f),
                            Rank2.Foldable (ImportItem l l f),
                            Rank2.Foldable (Export l l f),
                            Rank2.Foldable (Context l l f),
                            Rank2.Foldable (DataConstructor l l f),
                            Rank2.Foldable (DerivingClause l l f),
                            Rank2.Foldable (FieldDeclaration l l f),
                            Rank2.Foldable (FieldBinding l l f),
                            Rank2.Foldable (FieldPattern l l f),
                            Rank2.Foldable (CaseAlternative l l f),
                            Rank2.Foldable (Constructor l l f),
                            Rank2.Foldable (Value l l f))

type Rank2lyTraversable l f = (Rank2.Traversable (Module l l f),
                               Rank2.Traversable (Declaration l l f),
                               Rank2.Traversable (Expression l l f),
                               Rank2.Traversable (Type l l f),
                               Rank2.Traversable (EquationLHS l l f),
                               Rank2.Traversable (EquationRHS l l f),
                               Rank2.Traversable (GuardedExpression l l f),
                               Rank2.Traversable (Pattern l l f),
                               Rank2.Traversable (Statement l l f),
                               Rank2.Traversable (ClassInstanceLHS l l f),
                               Rank2.Traversable (TypeLHS l l f),
                               Rank2.Traversable (Import l l f),
                               Rank2.Traversable (ImportSpecification l l f),
                               Rank2.Traversable (ImportItem l l f),
                               Rank2.Traversable (Export l l f),
                               Rank2.Traversable (Context l l f),
                               Rank2.Traversable (DataConstructor l l f),
                               Rank2.Traversable (DerivingClause l l f),
                               Rank2.Traversable (FieldDeclaration l l f),
                               Rank2.Traversable (FieldBinding l l f),
                               Rank2.Traversable (FieldPattern l l f),
                               Rank2.Traversable (CaseAlternative l l f),
                               Rank2.Traversable (Constructor l l f),
                               Rank2.Traversable (Value l l f))
