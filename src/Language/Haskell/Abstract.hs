{-# Language KindSignatures, TypeFamilies, TypeFamilyDependencies #-}

module Language.Haskell.Abstract where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

class Haskell λ where
   type Module λ = (x :: * -> (* -> *) -> (* -> *) -> *) | x -> λ
   type Declaration λ = (x :: * -> (* -> *) -> (* -> *) -> *) | x -> λ
   type Expression λ = (x :: * -> (* -> *) -> (* -> *) -> *) | x -> λ
   type Type λ = (x :: * -> (* -> *) -> (* -> *) -> *) | x -> λ

   type EquationLHS λ = (x :: * -> (* -> *) -> (* -> *) -> *) | x -> λ
   type EquationRHS λ = (x :: * -> (* -> *) -> (* -> *) -> *) | x -> λ
   type GuardedExpression λ = (x :: * -> (* -> *) -> (* -> *) -> *) | x -> λ
   type Pattern λ = (x :: * -> (* -> *) -> (* -> *) -> *) | x -> λ
   type Statement λ = (x :: * -> (* -> *) -> (* -> *) -> *) | x -> λ
   type TypeLHS λ = (x :: * -> (* -> *) -> (* -> *) -> *) | x -> λ

   type Import λ = (x :: * -> (* -> *) -> (* -> *) -> *) | x -> λ
   type ImportSpecification λ = (x :: * -> (* -> *) -> (* -> *) -> *) | x -> λ
   type ImportItem λ = (x :: * -> (* -> *) -> (* -> *) -> *) | x -> λ
   type Export λ = (x :: * -> (* -> *) -> (* -> *) -> *) | x -> λ

   type Context λ = (x :: * -> (* -> *) -> (* -> *) -> *) | x -> λ
   type DataConstructor λ = (x :: * -> (* -> *) -> (* -> *) -> *) | x -> λ
   type DerivingClause λ = (x :: * -> (* -> *) -> (* -> *) -> *) | x -> λ
   type FieldDeclaration λ = (x :: * -> (* -> *) -> (* -> *) -> *) | x -> λ
   type FieldBinding λ = (x :: * -> (* -> *) -> (* -> *) -> *) | x -> λ
   type FieldPattern λ = (x :: * -> (* -> *) -> (* -> *) -> *) | x -> λ
   type CaseAlternative λ = (x :: * -> (* -> *) -> (* -> *) -> *) | x -> λ

   type Constructor λ = (x :: * -> (* -> *) -> (* -> *) -> *) | x -> λ
   type Value λ = (x :: * -> (* -> *) -> (* -> *) -> *) | x -> λ

   type CallingConvention λ = x | x -> λ
   type CallSafety λ = x | x -> λ
   type Fixity λ = x | x -> λ
   type Members λ = x | x -> λ
   type Name λ = x | x -> λ
   type ModuleName λ = x | x -> λ
   type QualifiedName λ = x | x -> λ

   anonymousModule :: [s (Import l l d d)] -> [s (Declaration l l d d)] -> Module λ l d s
   namedModule :: ModuleName λ -> Maybe [s (Export l l d d)] -> [s (Import l l d d)] -> [s (Declaration l l d d)]
               -> Module λ l d s

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
   fixityDeclaration :: Fixity λ -> Maybe Int -> NonEmpty (Name λ) -> Declaration λ l d s
   foreignExport :: CallingConvention λ -> Maybe Text -> Name λ -> s (Type l l d d) -> Declaration λ l d s
   foreignImport :: CallingConvention λ -> Maybe (CallSafety λ) -> Maybe Text -> Name λ -> s (Type l l d d)
                 -> Declaration λ l d s
   instanceDeclaration :: s (Context l l d d) -> s (TypeLHS l l d d) -> [s (Declaration l l d d)] -> Declaration λ l d s
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
   leftSectionExpression :: QualifiedName λ -> s (Expression l l d d) -> Expression λ l d s
   lambdaExpression :: [s (Pattern l l d d)] -> s (Expression l l d d) -> Expression λ l d s
   letExpression :: [s (Declaration l l d d)] -> s (Expression l l d d) -> Expression λ l d s
   listComprehension :: s (Expression l l d d) -> NonEmpty (s (Statement l l d d)) -> Expression λ l d s
   listExpression :: [s (Expression l l d d)] -> Expression λ l d s
   literalExpression :: s (Value l l d d) -> Expression λ l d s
   negate :: Expression λ l d s
   recordExpression :: s (Expression l l d d) -> [s (FieldBinding l l d d)] -> Expression λ l d s
   referenceExpression :: QualifiedName λ -> Expression λ l d d
   rightSectionExpression :: s (Expression l l d d) -> QualifiedName λ -> Expression λ l d s
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

   generalTypeLHS :: QualifiedName λ -> s (Type l l d d) -> TypeLHS λ l d s
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

   infixNonAssociative, infixLeft, infixRight :: Fixity λ

   cCall, cppCall, dotNetCall, jvmCall, stdCall :: CallingConvention λ
   safeCall, unsafeCall :: CallSafety λ
