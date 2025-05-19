{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

-- | This module exports functions for reserializing the parsed tree from the tokens stored with every node.

module Language.Haskell.Reserializer (ParsedLexemes(..), Lexeme(..), TokenType(..), Wrapped,
                                      adjustPositions, lexemes, reserialize, reserializeNested,
                                      sourceLength, joinWrapped, mergeLexemes, mapWrapping, mapWrappings,
                                      PositionAdjustment (PositionAdjustment),
                                      NestedPositionAdjustment (NestedPositionAdjustment), Serialization) where

import Control.Monad.Trans.State.Strict (State, StateT(..), evalState, state)
import Data.Bifunctor (first)
import Data.Data (Data)
import Data.Foldable (find)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Const (Const(..))
import Data.Kind (Type)
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
adjustPositions node = evalState (Full.traverse PositionAdjustment node) 0

-- | Serializes the tree back into the text it was parsed from.
reserialize :: (Monoid s, Factorial s, Position pos, Deep.Foldable (Serialization pos s) g)
            => Wrapped pos s (g (Wrapped pos s) (Wrapped pos s)) -> s
reserialize = foldMap lexemeText . lexemes

-- | Serializes the tree just like 'reserialize', but with an additional wrapping on every node.
reserializeNested :: (Monoid s, Factorial s, Position pos, Foldable f,
                      Deep.Foldable (Transformation.Folded f (Serialization pos s)) g)
                  => Compose f (Wrapped pos s) (g (Compose f (Wrapped pos s)) (Compose f (Wrapped pos s))) -> s
reserializeNested = foldMap lexemeText . nestedLexemes

-- | Serializes the tree into the lexemes it was parsed from.
lexemes :: (Factorial s, Position pos, Deep.Foldable (Serialization pos s) g)
        => Wrapped pos s (g (Wrapped pos s) (Wrapped pos s)) -> [Lexeme s]
lexemes root = finalize $ Full.foldMap Serialization root
   where finalize (PositionedLexemes ranges) = foldMap lexemeList ranges
         lexemeList (_, Trailing ls, _) = ls

-- | Serializes the tree into the lexemes it was parsed from.
nestedLexemes :: (Factorial s, Position pos, Foldable f,
                  Deep.Foldable (Transformation.Folded f (Serialization pos s)) g)
              => Compose f (Wrapped pos s) (g (Compose f (Wrapped pos s)) (Compose f (Wrapped pos s))) -> [Lexeme s]
nestedLexemes root = finalize $ Full.foldMap (Transformation.Folded Serialization) root
   where finalize (PositionedLexemes ranges) = foldMap lexemeList ranges
         lexemeList (_, Trailing ls, _) = ls

-- | The length of the source code parsed into the argument node
sourceLength :: forall g s pos. (Factorial s, Rank2.Foldable (g (Const (Sum Int))),
                                 Deep.Foldable (Transformation.Rank2.Fold (Wrapped pos s) (Sum Int)) g)
             => Wrapped pos s (g (Wrapped pos s) (Wrapped pos s)) -> Int
sourceLength root = getSum (nodeLength root <> foldMap (Transformation.Rank2.foldMap nodeLength) root)
   where nodeLength ((_, Trailing ls, _), _) = foldMap (Sum . Factorial.length . lexemeText) ls

-- | The length of the parsed source code with nodes 'Wrapped' under a nested wrapper
nestedSourceLength :: forall f g s pos.
                      (Factorial s, Foldable f, Rank2.Foldable (g (Const (Sum Int))),
                       Deep.Foldable (Transformation.Rank2.Fold (Compose f (Wrapped pos s)) (Sum Int)) g)
                   => Compose f (Wrapped pos s) (g (Compose f (Wrapped pos s)) (Compose f (Wrapped pos s))) -> Int
nestedSourceLength root = getSum (nestedNodeLength root <> foldMap (Transformation.Rank2.foldMap nestedNodeLength) root)
   where nestedNodeLength (Compose node) = foldMap nodeLength node
         nodeLength ((_, Trailing ls, _), _) = foldMap (Sum . Factorial.length . lexemeText) ls

-- | Join the two wrappings of a double-'Wrapped' value into one.
joinWrapped :: (Position pos, Factorial s) => Wrapped pos s (Wrapped pos s a) -> Wrapped pos s a
joinWrapped ((start, Trailing lexemes, end), ((innerStart, Trailing innerLexemes, innerEnd), x)) =
   ((start, Trailing $ mergeLexemes start lexemes innerStart innerLexemes, end), x)

-- | Given two lists of lexemes where the first wraps the second and their starting positions, return a single list
-- sorted by position.
mergeLexemes :: (Position pos, Factorial s) => pos -> [Lexeme s] -> pos -> [Lexeme s] -> [Lexeme s]
mergeLexemes pos1 outer@(lexeme1:rest1) pos2 inner@(lexeme2:rest2)
   | pos1 < pos2 = lexeme1 : mergeLexemes (move (Factorial.length $ lexemeText lexeme1) pos1) rest1 pos2 inner
mergeLexemes _ outer _ inner = inner <> outer

-- | Transformation type used by 'reserialize'
data Serialization pos s = Serialization
-- | Transformation type used by 'adjustPositions'
data PositionAdjustment pos s = PositionAdjustment
data NestedPositionAdjustment (f :: Type -> Type) pos s = NestedPositionAdjustment

-- | Map the stored positions and lexeme inputs in the entire tree and its wrapping
mapWrappings :: Deep.Functor (Transformation.Rank2.Map (Wrapped pos s) (Wrapped pos' s')) g
             => (pos -> pos') -> (s -> s')
             -> Wrapped pos s (g (Wrapped pos s) (Wrapped pos s))
             -> Wrapped pos' s' (g (Wrapped pos' s') (Wrapped pos' s'))
mapWrappings f g x = mapWrapping f g ((mapWrapping f g Transformation.Rank2.<$>) <$> x)
{-# INLINE mapWrappings #-}

mapWrapping :: (pos -> pos') -> (s -> s') -> Wrapped pos s a -> Wrapped pos' s' a
mapWrapping f g ((start, ls, end), a) = ((f start, g <$> ls, f end), a)
{-# INLINE mapWrapping #-}

instance Transformation.Transformation (Serialization pos s) where
    type Domain (Serialization pos s) = Wrapped pos s
    type Codomain (Serialization pos s) = Const (PositionedLexemes pos s)

instance Transformation.Transformation (PositionAdjustment pos s) where
    type Domain (PositionAdjustment pos s) = Wrapped pos s
    type Codomain (PositionAdjustment pos s) = Compose (State Int) (Wrapped pos s)

instance Transformation.Transformation (NestedPositionAdjustment f pos s) where
    type Domain (NestedPositionAdjustment f pos s) = Compose f (Wrapped pos s)
    type Codomain (NestedPositionAdjustment f pos s) = Compose (State Int) (Compose f (Wrapped pos s))

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
      where f :: Int -> (Wrapped pos s (g (Wrapped pos s) (Wrapped pos s)), Int)
            f adjustment = (((move adjustment nodeStart, lexemes, move adjustment nodeEnd'), node),
                            adjustment + distance nodeEnd nodeEnd')
               where nodeEnd' = move (sourceLength root) nodeStart

instance (Factorial s, Rank2.Foldable (g (Const (Sum Int))), Rank2.Traversable (g (Wrapped pos s)), Position pos,
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

instance (Factorial s, Traversable f, Rank2.Foldable (g (Const (Sum Int))), Position pos,
          Deep.Foldable (Transformation.Rank2.Fold (Compose f (Wrapped pos s)) (Sum Int)) g) =>
         NestedPositionAdjustment f pos s
         `Transformation.At` g (Compose f (Wrapped pos s)) (Compose f (Wrapped pos s)) where
   NestedPositionAdjustment $ root = Compose $ Compose <$> traverse (state . advance) (getCompose root)
      where advance :: Wrapped pos s (g (Compose f (Wrapped pos s)) (Compose f (Wrapped pos s))) -> Int -> (Wrapped pos s (g (Compose f (Wrapped pos s)) (Compose f (Wrapped pos s))), Int)
            advance ((nodeStart, lexemes, nodeEnd), node) adjustment =
               (((move adjustment nodeStart, lexemes, move adjustment nodeEnd'), node),
                adjustment + distance nodeEnd nodeEnd')
               where nodeEnd' = move (nestedSourceLength root) nodeStart

instance (Factorial s, Position pos, Traversable f,
          Rank2.Foldable (g (Const (Sum Int))), Rank2.Traversable (g (Compose f (Wrapped pos s))),
          Deep.Foldable (Transformation.Rank2.Fold (Compose f (Wrapped pos s)) (Sum Int)) g,
          Deep.Traversable (NestedPositionAdjustment f pos s) g) => Full.Traversable (NestedPositionAdjustment f pos s) g where
   traverse NestedPositionAdjustment root = Compose <$> traverse (state . advance) (getCompose root)
      where advance ((nodeStart, lexemes, nodeEnd), node) adjustment = (((move adjustment nodeStart, lexemes, move adjustment nodeEnd'),
                             evalState (Deep.traverse NestedPositionAdjustment node) adjustment),
                            adjustment + distance nodeEnd nodeEnd')
               where nodeEnd' = move (nestedSourceLength root) nodeStart

instance (Traversable f, Rank2.Foldable (g (Wrapped pos s)), Rank2.Foldable (g (Compose f (Wrapped pos s))),
          Deep.Foldable (Transformation.Rank2.Fold (Compose f (Wrapped pos s)) (Sum Int)) g) =>
         Full.Foldable (Transformation.Rank2.Fold (Compose f (Wrapped pos s)) (Sum Int)) g where
   foldMap = Full.foldMapDownDefault

instance (Factorial s, Position pos, Foldable f, Rank2.Foldable (g (Compose f (Wrapped pos s))),
          Deep.Foldable (Transformation.Folded f (Serialization pos s)) g) =>
         Full.Foldable (Transformation.Folded f (Serialization pos s)) g where
   foldMap = Full.foldMapDownDefault
