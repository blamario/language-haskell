{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

-- | This module exports functions for reserializing the parsed tree from the tokens stored with every node.

module Language.Haskell.Reserializer (ParsedLexemes(..), Lexeme(..), TokenType(..), Wrapped,
                                      adjustPositions, lexemes, reserialize, sourceLength, joinWrapped,
                                      mergeLexemes, mapWrappings,
                                      PositionAdjustment, Serialization) where

import Control.Arrow (first)
import Control.Monad.Trans.State.Strict (State, StateT(..), evalState, runState, state)
import Data.Data (Data)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Const (Const(..))
import Data.Monoid (Ap(Ap, getAp), Sum(Sum, getSum))
import Data.Semigroup.Factorial (Factorial)
import qualified Data.Semigroup.Factorial as Factorial
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

data TokenType = Delimiter | Keyword | Operator | Other
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
lexemes root@((startPos, _, _), _) = finalize $ (`runState` (startPos, [])) $ getAp $ Full.foldMap Serialization root
   where finalize (s, (_pos, rest)) = s <> rest

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
    type Codomain (Serialization pos s) = Const (Ap (State (pos, [Lexeme s])) [Lexeme s])

instance Transformation.Transformation (PositionAdjustment pos s) where
    type Domain (PositionAdjustment pos s) = Wrapped pos s
    type Codomain (PositionAdjustment pos s) = Compose (State Int) (Wrapped pos s)

instance forall g s pos. (Factorial s, Position pos) =>
         Serialization pos s `Transformation.At` g (Wrapped pos s) (Wrapped pos s) where
   Serialization $ ((nodePos, Trailing nodeLexemes, _), _) = Const (Ap $ state f)
      where f :: (pos, [Lexeme s]) -> ([Lexeme s], (pos, [Lexeme s]))
            f (pos, parentLexemes)
               | nodePos > pos, l:ls <- parentLexemes = first (l:) (f (move (Factorial.length $ lexemeText l) pos, ls))
               | otherwise = (mempty, (pos, nodeLexemes <> parentLexemes))

instance forall g s pos. (Factorial s, Position pos,
                      Rank2.Foldable (g (Wrapped pos s)), Deep.Foldable (Serialization pos s) g) =>
         Full.Foldable (Serialization pos s) g where
   foldMap trans ((nodeStart, Trailing nodeLexemes, _), node) = Ap (state f)
      where f :: (pos, [Lexeme s]) -> ([Lexeme s], (pos, [Lexeme s]))
            f (pos, parentLexemes)
               | nodeStart > pos, l:ls <- parentLexemes =
                    first (l:) (f (move (Factorial.length $ lexemeText l) pos, ls))
               | let (ls, (pos', lexemes')) = runState (getAp $ Deep.foldMap trans node) (pos, nodeLexemes) =
                     (ls <> lexemes',
                      (move (getSum $ foldMap (Sum . Factorial.length . lexemeText) lexemes') pos', parentLexemes))

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
