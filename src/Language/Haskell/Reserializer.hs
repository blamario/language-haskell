{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

-- | This module exports functions for reserializing the parsed tree from the tokens stored with every node.

module Language.Haskell.Reserializer (ParsedLexemes(..), Lexeme(..), TokenType(..), Wrapped,
                                      adjustPositions, lexemes, reserialize, sourceLength, joinWrapped,
                                      mergeLexemes, mapWrappings,
                                      PositionAdjustment, Serialization) where

import Control.Monad.Trans.State.Strict (State, StateT(..), evalState, state)
import Data.Bifunctor (first)
import Data.Data (Data)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Const (Const(..))
import Data.Monoid (Sum(Sum, getSum))
import Data.Semigroup.Factorial (Factorial)
import qualified Data.Semigroup.Factorial as Factorial
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Text.Parser.Input.Position (Position(distance, move))

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

data TokenType = Delimiter | Keyword | Operator | Modifier | Other
               deriving (Data, Eq, Show)

-- | Re-calculates the position of every node in the parse tree from the tokens stored with it and its children.
adjustPositions :: (Factorial s, Position pos, Rank2.Foldable (g (Const (Sum Int))),
                    Deep.Foldable (Transformation.Rank2.Fold (Wrapped pos s) (Sum Int)) g,
                    Deep.Traversable (PositionAdjustment pos s) g)
                => Wrapped pos s (g (Wrapped pos s) (Wrapped pos s))
                -> Wrapped pos s (g (Wrapped pos s) (Wrapped pos s))
adjustPositions node@((pos, _, _), _) = evalState (Full.traverse PositionAdjustment node) 0

-- | Serializes the tree back into the text it was parsed from.
reserialize :: (Monoid s, Factorial s, Position pos, Deep.Foldable (Serialization pos s) g)
            => Wrapped pos s (g (Wrapped pos s) (Wrapped pos s)) -> s
reserialize = foldMap lexemeText . lexemes

-- | Serializes the tree into the lexemes it was parsed from.
lexemes :: (Factorial s, Position pos, Deep.Foldable (Serialization pos s) g)
        => Wrapped pos s (g (Wrapped pos s) (Wrapped pos s)) -> [Lexeme s]
lexemes root = finalize $ Full.foldMap Serialization root
   where finalize (PositionedLexemes ranges) = foldMap lexemeList ranges
         lexemeList (_, Trailing ls, _) = ls

-- | The length of the source code parsed into the argument node
sourceLength :: forall g s pos. (Factorial s, Rank2.Foldable (g (Const (Sum Int))),
                                 Deep.Foldable (Transformation.Rank2.Fold (Wrapped pos s) (Sum Int)) g)
             => Wrapped pos s (g (Wrapped pos s) (Wrapped pos s)) -> Int
sourceLength root@((_, Trailing rootLexemes, _), node) = getSum (nodeLength root
                                                                 <> Transformation.Rank2.foldMap nodeLength node)
   where nodeLength ((_, Trailing ls, _), _) = foldMap (Sum . Factorial.length . lexemeText) ls

-- | Join the two wrappings of a double-'Wrapped' value into one.
joinWrapped :: (Position pos, Factorial s) => Wrapped pos s (Wrapped pos s a) -> Wrapped pos s a
joinWrapped ((start, Trailing lexemes, end), ((innerStart, Trailing innerLexemes, innerEnd), x)) =
   ((start, Trailing $ mergeLexemes start lexemes innerStart innerLexemes, end), x)

-- | Given two lists of lexemes where the first wraps the second and their starting positions, return a single list
-- | sorted by position.
mergeLexemes :: (Position pos, Factorial s) => pos -> [Lexeme s] -> pos -> [Lexeme s] -> [Lexeme s]
mergeLexemes pos1 outer@(lexeme1:rest1) pos2 inner@(lexeme2:rest2)
   | pos1 < pos2 = lexeme1 : mergeLexemes (move (Factorial.length $ lexemeText lexeme1) pos1) rest1 pos2 inner
mergeLexemes _ outer _ inner = inner <> outer

-- | Transformation type used by 'reserialize'
data Serialization pos s = Serialization
-- | Transformation type used by 'adjustPositions'
data PositionAdjustment pos s = PositionAdjustment

-- | Map the stored positions and lexeme inputs in the entire tree and its wrapping
mapWrappings :: forall g pos pos' s s'. Deep.Functor (Transformation.Rank2.Map (Wrapped pos s) (Wrapped pos' s')) g
             => (pos -> pos') -> (s -> s')
             -> Wrapped pos s (g (Wrapped pos s) (Wrapped pos s))
             -> Wrapped pos' s' (g (Wrapped pos' s') (Wrapped pos' s'))
mapWrappings f g x = mapWrapping ((mapWrapping Transformation.Rank2.<$>) <$> x)
   where mapWrapping :: forall a. Wrapped pos s a -> Wrapped pos' s' a
         mapWrapping ((start, ls, end), a) = ((f start, g <$> ls, f end), a)
{-# INLINE mapWrappings #-}

instance Transformation.Transformation (Serialization pos s) where
    type Domain (Serialization pos s) = Wrapped pos s
    type Codomain (Serialization pos s) = Const (PositionedLexemes pos s)

instance Transformation.Transformation (PositionAdjustment pos s) where
    type Domain (PositionAdjustment pos s) = Wrapped pos s
    type Codomain (PositionAdjustment pos s) = Compose (State Int) (Wrapped pos s)

newtype PositionedLexemes pos s = PositionedLexemes (Seq (pos, ParsedLexemes s, pos))

instance (Factorial s, Position pos) => Semigroup (PositionedLexemes pos s) where
   PositionedLexemes ranges1 <> PositionedLexemes ranges2 = combine (Seq.viewr ranges1) (Seq.viewl ranges2)

combine :: (Factorial s, Position pos)
        => Seq.ViewR (pos, ParsedLexemes s, pos) -> Seq.ViewL (pos, ParsedLexemes s, pos) -> PositionedLexemes pos s
combine (ranges1 Seq.:> range1@(start1, ls1, end1)) (range2@(start2, ls2, end2) Seq.:< ranges2)
   | end1 == start2 = PositionedLexemes $ ranges1 <> pure (start1, ls1 <> ls2, end2) <> ranges2
   | end1 < start2 = PositionedLexemes $ ranges1 <> pure range1 <> pure range2 <> ranges2
   | (ls1p, ls1s) <- splitUntil start1 ls1 start2 =
        PositionedLexemes ranges1
        <> (PositionedLexemes (pure (start1, ls1p <> ls2 <> ls1s, end1)) <> PositionedLexemes ranges2)
combine (ranges Seq.:> range) Seq.EmptyL = PositionedLexemes (ranges Seq.|> range)
combine Seq.EmptyR (range Seq.:< ranges) = PositionedLexemes (range Seq.<| ranges)
combine Seq.EmptyR Seq.EmptyL = PositionedLexemes mempty

splitUntil start lexemes end
   | start < end, Trailing (l:ls) <- lexemes =
        first (Trailing [l] <>) (splitUntil (move (Factorial.length (lexemeText l)) start) (Trailing ls) end)
   | otherwise = (mempty, lexemes)

instance (Factorial s, Position pos) => Monoid (PositionedLexemes pos s) where
  mempty = PositionedLexemes mempty

instance forall pos s a. (Factorial s, Position pos) => Serialization pos s `Transformation.At` a where
   Serialization $ (range, _) = Const (PositionedLexemes $ pure range)

instance forall g s pos. (Factorial s, Position pos,
                          Rank2.Foldable (g (Wrapped pos s)), Deep.Foldable (Serialization pos s) g) =>
         Full.Foldable (Serialization pos s) g where
   foldMap = Full.foldMapDownDefault

instance (Factorial s, Rank2.Foldable (g (Const (Sum Int))), Position pos,
          Deep.Foldable (Transformation.Rank2.Fold (Wrapped pos s) (Sum Int)) g) =>
         PositionAdjustment pos s `Transformation.At` g (Wrapped pos s) (Wrapped pos s) where
   PositionAdjustment $ root@((nodeStart, lexemes, nodeEnd), node) = Compose (state f)
      where f adjustment = (((move adjustment nodeStart, lexemes, move adjustment nodeEnd'), node),
                            adjustment + distance nodeEnd nodeEnd')
               where nodeEnd' = move (sourceLength root) nodeStart

instance (Factorial s, Rank2.Foldable (g (Const (Sum Int))), Position pos,
          Deep.Foldable (Transformation.Rank2.Fold (Wrapped pos s) (Sum Int)) g,
          Deep.Traversable (PositionAdjustment pos s) g) => Full.Traversable (PositionAdjustment pos s) g where
   traverse PositionAdjustment root@((nodeStart, lexemes, nodeEnd), node) = state f
      where f adjustment = (((move adjustment nodeStart, lexemes, move adjustment nodeEnd'),
                             evalState (Deep.traverse PositionAdjustment node) adjustment),
                            adjustment + distance nodeEnd nodeEnd')
               where nodeEnd' = move (sourceLength root) nodeStart

instance (Rank2.Foldable (g (Wrapped pos s)),
          Deep.Foldable (Transformation.Rank2.Fold (Wrapped pos s) (Sum Int)) g) =>
         Full.Foldable (Transformation.Rank2.Fold (Wrapped pos s) (Sum Int)) g where
   foldMap = Full.foldMapDownDefault
