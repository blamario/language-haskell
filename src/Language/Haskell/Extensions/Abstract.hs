module Language.Haskell.Extensions.Abstract (ExtendedHaskell(..), module Language.Haskell.Abstract) where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Language.Haskell.Abstract 

class Haskell λ => ExtendedHaskell λ where
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
   existentialConstructor :: [Name λ] -> s (Context l l d d) -> s (DataConstructor l l d d) -> DataConstructor λ l d s
