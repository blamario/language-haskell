{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings,
             ScopedTypeVariables, StandaloneDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Language.Haskell.Reorganizer where

import Control.Applicative ((<|>))
import Data.Either (partitionEithers)
import Data.Either.Validation (Validation(..), validationToEither)
import Data.Functor.Compose (Compose(..))
import Data.List.NonEmpty (NonEmpty(..), fromList, toList, nonEmpty)
import Data.Functor.Const (Const)
import qualified Data.Map.Lazy as Map
import Data.Monoid (Sum)
import Data.Semigroup (sconcat)
import Data.Semigroup.Factorial (Factorial)
import Data.Semigroup.Union (UnionWith(..))
import Data.String (IsString)

import qualified Transformation
import qualified Transformation.Deep as Deep
import qualified Transformation.Full as Full
import qualified Transformation.Rank2
import qualified Transformation.AG.Monomorphic as AG.Mono
import Text.Grampa (Ambiguous(..))
import Text.Parser.Input.Position (Position)

import qualified Language.Haskell.Abstract as Abstract
import qualified Language.Haskell.AST as AST
import qualified Language.Haskell.Extensions.AST as ExtAST
import qualified Language.Haskell.Binder as Binder
import qualified Language.Haskell.Reserializer as Reserializer
import Language.Haskell.Reserializer (Lexeme (Token, lexemeText, lexemeType), TokenType (Delimiter))

import Prelude hiding (mod, span)

data Reorganization l pos s = Reorganization

type Reorganized l f = Validation (NonEmpty (Error l f))

prefixMinusPrecedence :: Int
prefixMinusPrecedence = 6

instance Transformation.Transformation (Reorganization l pos s) where
    type Domain (Reorganization l pos s) = Binder.WithEnvironment l (Reserializer.Wrapped pos s)
    type Codomain (Reorganization l pos s) = Compose (Reorganized l (Reserializer.Wrapped pos s)) (Reserializer.Wrapped pos s)

instance {-# overlappable #-} Reorganization l pos s
         `Transformation.At` g (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s) where
  Reorganization $ Compose (_, x) = Compose (Success x)

instance {-# overlaps #-} forall l pos s f.
         (Eq s, Factorial s, IsString s, Eq pos, Position pos, f ~ Reserializer.Wrapped pos s,
          Show pos, Show s, Show (ExtAST.Expression l l f f),
          Abstract.DeeplyFoldable (Transformation.Rank2.Fold (Reserializer.Wrapped pos s) (Sum Int)) l,
          Abstract.DeeplyTraversable (Reserializer.PositionAdjustment pos s) l,
          Abstract.Rank2lyFoldable l (Const (Sum Int)),
          Eq (ExtAST.Expression l l f f),
          Abstract.Expression l ~ ExtAST.Expression l,
          Abstract.ModuleName l ~ ExtAST.ModuleName l,
          Abstract.QualifiedName l ~ ExtAST.QualifiedName l,
          Abstract.Name l ~ ExtAST.Name l) =>
         Reorganization l pos s
         `Transformation.At` ExtAST.Expression l l (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s) where
  _res $ Compose (AG.Mono.Atts{AG.Mono.inh= UnionWith bindings}, expression) =
      let reorganizeExpression :: f (ExtAST.Expression l l f f)
                               -> Validation (NonEmpty (Error l f)) (f (ExtAST.Expression l l f f))
          reorganizeExpression
             (lexemes', ExtAST.InfixExpression (lexemes, ExtAST.InfixExpression left op right) op' right')
             | not (parenthesized lexemes),
               Just (Binder.InfixDeclaration _ associativity precedence) <- resolve op,
               Just (Binder.InfixDeclaration _ associativity' precedence') <- resolve op',
               precedence < precedence' || precedence == precedence' && associativity /= ExtAST.LeftAssociative
             = if precedence == precedence'
                  && (associativity /= associativity' || associativity == ExtAST.NonAssociative)
               then Failure (pure ContradictoryAssociativity)
               else reorganizeExpression (lexemes, ExtAST.InfixExpression right op' right')
                    >>= reorganizeExpression . (,) lexemes' . ExtAST.InfixExpression left op
                        . Reserializer.adjustPositions
          reorganizeExpression
             (lexemes', ExtAST.InfixExpression left' op' (lexemes, ExtAST.InfixExpression left op right))
             | not (parenthesized lexemes),
               Just (Binder.InfixDeclaration _ associativity precedence) <- resolve op,
               Just (Binder.InfixDeclaration _ associativity' precedence') <- resolve op',
               precedence < precedence' || precedence == precedence' && associativity /= ExtAST.RightAssociative
             = if precedence == precedence'
                  && (associativity /= associativity' || associativity == ExtAST.NonAssociative)
               then Failure (pure ContradictoryAssociativity)
               else reorganizeExpression (lexemes, ExtAST.InfixExpression left' op' left)
                    >>= \l-> reorganizeExpression (lexemes',
                                                   ExtAST.InfixExpression (Reserializer.adjustPositions l) op right)
          reorganizeExpression
             (lexemes', ExtAST.ApplyExpression neg@(_, ExtAST.Negate{}) (lexemes, ExtAST.InfixExpression left op right))
             | not (parenthesized lexemes),
               Just (Binder.InfixDeclaration _ associativity precedence) <- resolve op,
               precedence < prefixMinusPrecedence
               || precedence == prefixMinusPrecedence && associativity /= ExtAST.RightAssociative
             = if precedence == prefixMinusPrecedence && associativity /= ExtAST.LeftAssociative
               then Failure (pure ContradictoryAssociativity)
               else reorganizeExpression (lexemes, ExtAST.ApplyExpression neg left)
                    >>= \l-> reorganizeExpression (lexemes',
                                                   ExtAST.InfixExpression (Reserializer.adjustPositions l) op right)
          reorganizeExpression e = Success e
          resolve ((_, lexemes, _), ExtAST.ReferenceExpression name) =
            Map.lookup name bindings <|> defaultInfixDeclaration lexemes
      in Compose (reorganizeExpression expression)

--defaultInfixDeclaration :: ExtAST.QualifiedName l -> Maybe (Binder.Binding l)
defaultInfixDeclaration (Reserializer.Trailing lexemes)
   | any (== Token{lexemeType= Delimiter, lexemeText= "`"}) lexemes =
     Just (Binder.InfixDeclaration False AST.LeftAssociative 9)
   | otherwise = Nothing

verifyInfixApplication :: (Maybe (AST.Associativity λ) -> Int -> e -> a -> a) -> e -> e -> Binder.Binding l -> a -> a
verifyInfixApplication verifyArg left right (Binder.InfixDeclaration _ AST.LeftAssociative precedence) =
   verifyArg (Just AST.LeftAssociative) precedence left . verifyArg Nothing precedence right
verifyInfixApplication verifyArg left right (Binder.InfixDeclaration _ AST.RightAssociative precedence) =
   verifyArg (Just AST.RightAssociative) precedence right . verifyArg Nothing precedence left
verifyInfixApplication verifyArg left right (Binder.InfixDeclaration _ AST.NonAssociative precedence) =
   verifyArg Nothing precedence left . verifyArg Nothing precedence right

modifying, parenthesized :: (Eq s, IsString s) => (pos, Reserializer.ParsedLexemes s, pos) -> Bool
modifying (_, Reserializer.Trailing [Reserializer.Token{Reserializer.lexemeType= Reserializer.Modifier}], _) = True
modifying _ = False
parenthesized (_, Reserializer.Trailing (paren:_), _) = Reserializer.lexemeText paren == "("
parenthesized (_, Reserializer.Trailing [], _) = False

data Error l f = ContradictoryAssociativity
               | ClashingImports
               | ClashingNames
               | TupleSectionWithNoOmission (NonEmpty (f (ExtAST.Expression l l f f)))
               | UnknownOperator (ExtAST.QualifiedName l)

deriving instance (Show (AST.Expression l l f f), Show (ExtAST.Expression l l f f),
                   Show (f (ExtAST.Expression l l f f)), Show (ExtAST.QualifiedName l)) => Show (Error l f)

instance Monad (Validation (NonEmpty (Error l f))) where
   Success s >>= f = f s
   Failure errors >>= _ = Failure errors

-- | Reorganize ambiguities in the given collection of modules, a 'Map' keyed by module name. Note that all class
-- constraints in the function's type signature are satisfied by the Haskell 'AST.Language'.
reorganizeModules :: forall l pos s f. (f ~ Reserializer.Wrapped pos s,
                                Abstract.Haskell l,
                                Abstract.Module l l ~ AST.Module l l,
                                Abstract.ModuleName l ~ AST.ModuleName l,
                                Abstract.Export l l ~ AST.Export l l,
                                Abstract.Import l l ~ AST.Import l l,
                                Abstract.ImportSpecification l l ~ AST.ImportSpecification l l,
                                Abstract.ImportItem l l ~ AST.ImportItem l l,
                                Abstract.Members l ~ AST.Members l,
                                Abstract.Declaration l ~ ExtAST.Declaration l,
                                Abstract.QualifiedName l ~ AST.QualifiedName l,
                                Abstract.Name l ~ AST.Name l,
                                Deep.Traversable (Reorganization l pos s) (Abstract.Declaration l l),
                                Full.Traversable (Reorganization l pos s) (Abstract.Module l l),
                                Full.Traversable (Reorganization l pos s) (Abstract.Declaration l l)) =>
                  Map.Map (Abstract.ModuleName l)
                          (Binder.WithEnvironment l (Reserializer.Wrapped pos s)
                                                  (AST.Module l l
                                                              (Binder.WithEnvironment l (Reserializer.Wrapped pos s))
                                                              (Binder.WithEnvironment l (Reserializer.Wrapped pos s))))
               -> Validation (NonEmpty (Abstract.ModuleName l, NonEmpty (Error l f)))
                             (Map.Map (Abstract.ModuleName l) (f (AST.Module l l f f)))
reorganizeModules modules = Map.traverseWithKey extractErrors reorganizedModules
   where reorganizedModules = Full.traverse Reorganization <$> modules
         extractErrors moduleKey (Failure e)   = Failure ((moduleKey, e) :| [])
         extractErrors _         (Success mod) = Success mod

instance (Deep.Traversable (Reorganization l pos s) g,
          Transformation.At (Reorganization l pos s) (g (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s))) =>
         Full.Traversable (Reorganization l pos s) g where
   traverse = Full.traverseUpDefault


