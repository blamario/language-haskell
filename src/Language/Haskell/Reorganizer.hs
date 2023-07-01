{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings,
             ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeFamilies, TypeOperators,
             UndecidableInstances #-}

-- | An AST traversal for adjusting the infix operator applications

module Language.Haskell.Reorganizer (Reorganization (Reorganization), reorganizeModules, NestedAdjustment) where

import Control.Applicative ((<|>))
import Control.Monad.Trans.State.Strict (State, StateT(..), evalState, runState, state)
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

import qualified Rank2
import qualified Transformation
import qualified Transformation.Deep as Deep
import qualified Transformation.Full as Full
import qualified Transformation.Rank2
import qualified Transformation.AG.Dimorphic as Di
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

type Wrap l pos s = Binder.WithEnvironment l (Reserializer.Wrapped pos s)

type Reorganized l f = Validation (NonEmpty (Error l f))

type NestedAdjustment l pos s = Reserializer.NestedPositionAdjustment (WithAtts l) pos s

type WithAtts l = (,) (Di.Atts (Binder.Environment l) (Binder.LocalEnvironment l))

prefixMinusPrecedence :: Int
prefixMinusPrecedence = 6

instance Transformation.Transformation (Reorganization l pos s) where
    type Domain (Reorganization l pos s) = Wrap l pos s
    type Codomain (Reorganization l pos s) = Compose (Reorganized l (Wrap l pos s)) (Wrap l pos s)

instance {-# overlappable #-} Reorganization l pos s
         `Transformation.At` g (Wrap l pos s) (Wrap l pos s) where
  Reorganization $ x = Compose (Success x)

instance {-# overlaps #-} forall l pos s f.
         (Eq s, Factorial s, IsString s, Eq pos, Position pos, f ~ Wrap l pos s,
          Show pos, Show s, Show (ExtAST.Expression l l f f),
          Full.Traversable (NestedAdjustment l pos s) (ExtAST.Expression l l),
          Abstract.Rank2lyFoldable l (Const (Sum Int)),
          Abstract.Expression l ~ ExtAST.Expression l,
          Abstract.ModuleName l ~ ExtAST.ModuleName l,
          Abstract.QualifiedName l ~ ExtAST.QualifiedName l,
          Abstract.Name l ~ ExtAST.Name l) =>
         Reorganization l pos s
         `Transformation.At` ExtAST.Expression l l (Wrap l pos s) (Wrap l pos s) where
  _res $ Compose (atts@Di.Atts{Di.inh= bindings}, expression) =
      let reorganizeExpression :: Reserializer.Wrapped pos s (ExtAST.Expression l l f f)
                               -> Validation (NonEmpty (Error l f)) (f (ExtAST.Expression l l f f))
          reorganizeExpression
             (root,
              ExtAST.InfixExpression
                 (Compose (env', (leftBranch,
                                  ExtAST.InfixExpression left lOp middle@(Compose (_, ((lrStart, _, _), _))))))
                 rOp
                 right@(Compose (_, ((_, _, rEnd), _))))
             | not (parenthesized leftBranch),
               Just (Binder.InfixDeclaration associativity precedence _) <- resolve lOp,
               Just (Binder.InfixDeclaration associativity' precedence' _) <- resolve rOp,
               precedence < precedence' || precedence == precedence' && associativity /= ExtAST.LeftAssociative
             = if precedence == precedence'
                  && (associativity /= associativity' || associativity == ExtAST.NonAssociative)
               then Failure (pure ContradictoryAssociativity)
               else reorganizeExpression ((lrStart, mempty, rEnd), ExtAST.InfixExpression middle rOp right)
                    >>= reorganizeExpression . (,) root . ExtAST.InfixExpression left lOp
                    >>= pure . adjustPositions
          reorganizeExpression
             (root,
              ExtAST.InfixExpression
                 left@(Compose (_, ((lStart, _, _), _)))
                 lOp
                 (Compose (_,
                           (rightBranch, ExtAST.InfixExpression middle@(Compose (_, ((_, _, rlEnd), _))) rOp right))))
             | not (parenthesized rightBranch),
               Just (Binder.InfixDeclaration associativity precedence _) <- resolve rOp,
               Just (Binder.InfixDeclaration associativity' precedence' _) <- resolve lOp,
               precedence < precedence' || precedence == precedence' && associativity /= ExtAST.RightAssociative
             = if precedence == precedence'
                  && (associativity /= associativity' || associativity == ExtAST.NonAssociative)
               then Failure (pure ContradictoryAssociativity)
               else reorganizeExpression ((lStart, mempty, rlEnd), ExtAST.InfixExpression left lOp middle)
                    >>= \l-> reorganizeExpression (root, ExtAST.InfixExpression l rOp right)
                    >>= pure . adjustPositions
          reorganizeExpression
             (root,
              ExtAST.ApplyExpression
                 neg@(Compose (_, ((negStart, _, _), ExtAST.Negate{})))
                 (Compose (_, (arg, ExtAST.InfixExpression left@(Compose (_, ((_, _, middleEnd), _))) op right))))
             | not (parenthesized arg),
               Just (Binder.InfixDeclaration associativity precedence _) <- resolve op,
               precedence < prefixMinusPrecedence
               || precedence == prefixMinusPrecedence && associativity /= ExtAST.RightAssociative
             = if precedence == prefixMinusPrecedence && associativity /= ExtAST.LeftAssociative
               then Failure (pure ContradictoryAssociativity)
               else reorganizeExpression ((negStart, mempty, middleEnd), ExtAST.ApplyExpression neg left)
                    >>= \l-> reorganizeExpression (root, ExtAST.InfixExpression l op right)
          reorganizeExpression e = Success (Compose (atts, e))
          adjustPositions :: f (ExtAST.Expression l l f f) -> f (ExtAST.Expression l l f f)
          adjustPositions node = evalState (Full.traverse Reserializer.NestedPositionAdjustment node) 0
          resolve (Compose (_, ((_, lexemes, _), ExtAST.ReferenceExpression name))) =
             Binder.lookupValue name bindings <|> defaultInfixDeclaration lexemes
      in Compose (reorganizeExpression expression)


--defaultInfixDeclaration :: ExtAST.QualifiedName l -> Maybe (Binder.Binding l)
defaultInfixDeclaration (Reserializer.Trailing lexemes)
   | any (== Token{lexemeType= Delimiter, lexemeText= "`"}) lexemes =
     Just (Binder.InfixDeclaration AST.LeftAssociative 9 Nothing)
   | otherwise = Nothing

verifyInfixApplication :: (Maybe (AST.Associativity λ) -> Int -> e -> a -> a)
                       -> e -> e -> Binder.ValueBinding l -> a -> a
verifyInfixApplication verifyArg left right (Binder.InfixDeclaration AST.LeftAssociative precedence _) =
   verifyArg (Just AST.LeftAssociative) precedence left . verifyArg Nothing precedence right
verifyInfixApplication verifyArg left right (Binder.InfixDeclaration AST.RightAssociative precedence _) =
   verifyArg (Just AST.RightAssociative) precedence right . verifyArg Nothing precedence left
verifyInfixApplication verifyArg left right (Binder.InfixDeclaration AST.NonAssociative precedence _) =
   verifyArg Nothing precedence left . verifyArg Nothing precedence right

modifying, parenthesized :: (Eq s, IsString s) => (pos, Reserializer.ParsedLexemes s, pos) -> Bool
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
reorganizeModules :: forall l pos s f. (f ~ Wrap l pos s,
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
                  Map.Map (Abstract.ModuleName l) (f (AST.Module l l f f))
               -> Validation (NonEmpty (Abstract.ModuleName l, NonEmpty (Error l f)))
                             (Map.Map (Abstract.ModuleName l) (f (AST.Module l l f f)))
reorganizeModules modules = Map.traverseWithKey extractErrors reorganizedModules
   where reorganizedModules = Full.traverse Reorganization <$> modules
         extractErrors moduleKey (Failure e)   = Failure ((moduleKey, e) :| [])
         extractErrors _         (Success mod) = Success mod

instance (Rank2.Traversable (g (Wrap l pos s)), Deep.Traversable (Reorganization l pos s) g,
          Transformation.At (Reorganization l pos s) (g (Wrap l pos s) (Wrap l pos s))) =>
         Full.Traversable (Reorganization l pos s) g where
   traverse = Full.traverseUpDefault
