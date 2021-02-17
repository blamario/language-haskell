{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

-- | This module exports functions for reserializing the parsed tree from the tokens stored with every node.

module Language.Haskell.Reserializer (ParsedLexemes(..), Lexeme(..), TokenType(..), Wrapped,
                                      adjustPositions, lexemes, reserialize, sourceLength, mapWrappings,
                                      PositionAdjustment, Serialization) where

import Control.Arrow (first)
import Control.Monad.Trans.State.Strict (State, StateT(..), evalState, runState, state)
import Data.Data (Data)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Const (Const(..))
import Data.Monoid (Ap(Ap, getAp), Sum(Sum, getSum))
import Data.Semigroup.Factorial (Factorial)
import qualified Data.Semigroup.Factorial as Factorial

import qualified Rank2
import qualified Transformation
import qualified Transformation.Rank2
import qualified Transformation.Deep as Deep
import qualified Transformation.Full as Full

type Parsed s = Wrapped Int s

type Wrapped pos s = (,) (pos, ParsedLexemes s, pos)

newtype ParsedLexemes s = Trailing [Lexeme s]
                          deriving (Data, Eq, Functor, Show, Semigroup, Monoid)

data Lexeme s = WhiteSpace{lexemeText :: s}
              | Comment{lexemeText :: s}
              | Token{lexemeType :: TokenType,
                      lexemeText :: s}
                deriving (Data, Eq, Functor, Show)

data TokenType = Delimiter | Keyword | Operator | Other
               deriving (Data, Eq, Show)

-- | Re-calculates the position of every node in the parse tree from the tokens stored with it and its children.
adjustPositions :: (Factorial s, Rank2.Foldable (g (Const (Sum Int))),
                    Deep.Foldable (Transformation.Rank2.Fold (Parsed s) (Sum Int)) g,
                    Deep.Traversable (PositionAdjustment s) g)
                => Parsed s (g (Parsed s) (Parsed s)) -> Parsed s (g (Parsed s) (Parsed s))
adjustPositions node@((pos, _, _), _) = evalState (Full.traverse PositionAdjustment node) 0

-- | Serializes the tree back into the text it was parsed from.
reserialize :: (Monoid s, Factorial s, Deep.Foldable (Serialization s) g) => Parsed s (g (Parsed s) (Parsed s)) -> s
reserialize = foldMap lexemeText . lexemes

-- | Serializes the tree into the lexemes it was parsed from.
lexemes :: (Factorial s, Deep.Foldable (Serialization s) g) => Parsed s (g (Parsed s) (Parsed s)) -> [Lexeme s]
lexemes = finalize . (`runState` (0, [])) . getAp . Full.foldMap Serialization
   where finalize (s, (_pos, rest)) = s <> rest

-- | The length of the source code parsed into the argument node
sourceLength :: forall g s. (Factorial s, Rank2.Foldable (g (Const (Sum Int))),
                        Deep.Foldable (Transformation.Rank2.Fold (Parsed s) (Sum Int)) g)
             => Parsed s (g (Parsed s) (Parsed s)) -> Int
sourceLength root@((_, Trailing rootLexemes, _), node) = getSum (nodeLength root
                                                                 <> Transformation.Rank2.foldMap nodeLength node)
   where nodeLength ((_, Trailing ls, _), _) = foldMap (Sum . Factorial.length . lexemeText) ls

-- | Transformation type used by 'reserialize'
data Serialization s = Serialization
-- | Transformation type used by 'adjustPositions'
data PositionAdjustment s = PositionAdjustment

-- | Map the stored positions and lexeme inputs in the entire tree and its wrapping
mapWrappings :: forall g pos pos' s s'. Deep.Functor (Transformation.Rank2.Map (Wrapped pos s) (Wrapped pos' s')) g
             => (pos -> pos') -> (s -> s')
             -> Wrapped pos s (g (Wrapped pos s) (Wrapped pos s))
             -> Wrapped pos' s' (g (Wrapped pos' s') (Wrapped pos' s'))
mapWrappings f g x = mapWrapping ((mapWrapping Transformation.Rank2.<$>) <$> x)
   where mapWrapping :: forall a. Wrapped pos s a -> Wrapped pos' s' a
         mapWrapping ((start, ls, end), a) = ((f start, g <$> ls, f end), a)
{-# INLINE mapWrappings #-}

instance Transformation.Transformation (Serialization s) where
    type Domain (Serialization s) = Parsed s
    type Codomain (Serialization s) = Const (Ap (State (Int, [Lexeme s])) [Lexeme s])

instance Transformation.Transformation (PositionAdjustment s) where
    type Domain (PositionAdjustment s) = Parsed s
    type Codomain (PositionAdjustment s) = Compose (State Int) (Parsed s)

instance forall g s. Factorial s => Serialization s `Transformation.At` g (Parsed s) (Parsed s) where
   Serialization $ ((nodePos, Trailing nodeLexemes, _), _) = Const (Ap $ state f)
      where f :: (Int, [Lexeme s]) -> ([Lexeme s], (Int, [Lexeme s]))
            f (pos, parentLexemes)
               | nodePos > pos, l:ls <- parentLexemes = first (l:) (f (pos + Factorial.length (lexemeText l), ls))
               | otherwise = (mempty, (pos, nodeLexemes <> parentLexemes))

instance forall g s. (Factorial s, Rank2.Foldable (g (Parsed s)), Deep.Foldable (Serialization s) g) =>
         Full.Foldable (Serialization s) g where
   foldMap trans ((nodeStart, Trailing nodeLexemes, _), node) = Ap (state f)
      where f :: (Int, [Lexeme s]) -> ([Lexeme s], (Int, [Lexeme s]))
            f (pos, parentLexemes)
               | nodeStart > pos, l:ls <- parentLexemes = first (l:) (f (pos + Factorial.length (lexemeText l), ls))
               | let (ls, (pos', lexemes')) = runState (getAp $ Deep.foldMap trans node) (pos, nodeLexemes) =
                     (ls <> lexemes',
                      (pos' + getSum (foldMap (Sum . Factorial.length . lexemeText) lexemes'), parentLexemes))

instance (Factorial s, Rank2.Foldable (g (Const (Sum Int))),
          Deep.Foldable (Transformation.Rank2.Fold (Parsed s) (Sum Int)) g) =>
         PositionAdjustment s `Transformation.At` g (Parsed s) (Parsed s) where
   PositionAdjustment $ root@((nodeStart, lexemes, nodeEnd), node) = Compose (state f)
      where f adjustment = (((nodeStart + adjustment, lexemes, nodeEnd' + adjustment), node),
                            adjustment + nodeEnd' - nodeEnd)
               where nodeEnd' = nodeStart + sourceLength root

instance (Factorial s, Rank2.Foldable (g (Const (Sum Int))),
          Deep.Foldable (Transformation.Rank2.Fold (Parsed s) (Sum Int)) g,
          Deep.Traversable (PositionAdjustment s) g) => Full.Traversable (PositionAdjustment s) g where
   traverse PositionAdjustment root@((nodeStart, lexemes, nodeEnd), node) = state f
      where f adjustment = (((nodeStart + adjustment, lexemes, nodeEnd' + adjustment),
                             evalState (Deep.traverse PositionAdjustment node) adjustment),
                            adjustment + nodeEnd' - nodeEnd)
               where nodeEnd' = nodeStart + sourceLength root

instance (Rank2.Foldable (g (Parsed s)),
          Deep.Foldable (Transformation.Rank2.Fold (Parsed s) (Sum Int)) g) =>
         Full.Foldable (Transformation.Rank2.Fold (Parsed s) (Sum Int)) g where
   foldMap = Full.foldMapDownDefault
