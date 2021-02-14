{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             TypeFamilies, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

-- | This module exports functions for reserializing the parsed tree from the tokens stored with every node.

module Language.Haskell.Reserializer (ParsedLexemes(..), Lexeme(..), TokenType(..),
                                      adjustPositions, reserialize, sourceLength,
                                      PositionAdjustment, Serialization) where

import Control.Arrow (first)
import Control.Monad.Trans.State.Strict (State, StateT(..), evalState, runState, state)
import Data.Data (Data)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Const (Const(..))
import Data.Monoid (Ap(Ap, getAp), Sum(Sum, getSum))
import Data.Text (Text)
import qualified Data.Text as Text

import qualified Rank2
import qualified Transformation
import qualified Transformation.Rank2
import qualified Transformation.Deep as Deep
import qualified Transformation.Full as Full

newtype ParsedLexemes = Trailing [Lexeme]
                      deriving (Data, Eq, Show, Semigroup, Monoid)

data Lexeme = WhiteSpace{lexemeText :: Text}
            | Comment{lexemeText :: Text}
            | Token{lexemeType :: TokenType,
                    lexemeText :: Text}
            deriving (Data, Eq, Show)

data TokenType = Delimiter | Keyword | Operator | Other
               deriving (Data, Eq, Show)

-- | Re-calculates the position of every node in the parse tree from the tokens stored with it and its children.
adjustPositions :: (Rank2.Foldable (g (Const (Sum Int))),
                    Deep.Foldable (Transformation.Rank2.Fold Parsed (Sum Int)) g,
                    Deep.Traversable PositionAdjustment g) => Parsed (g Parsed Parsed) -> Parsed (g Parsed Parsed)
adjustPositions node@((pos, _, _), _) = evalState (Full.traverse PositionAdjustment node) 0

-- | Serializes the tree back into the text it was parsed from.
reserialize :: Deep.Foldable Serialization g => Parsed (g Parsed Parsed) -> Text
reserialize = foldMap lexemeText . lexemes

-- | Serializes the tree into the lexemes it was parsed from.
lexemes :: Deep.Foldable Serialization g => Parsed (g Parsed Parsed) -> [Lexeme]
lexemes = finalize . (`runState` (0, [])) . getAp . Full.foldMap Serialization
   where finalize (s, (_pos, rest)) = s <> rest

-- | The length of the source code parsed into the argument node
sourceLength :: (Rank2.Foldable (g (Const (Sum Int))),
                 Deep.Foldable (Transformation.Rank2.Fold Parsed (Sum Int)) g) => Parsed (g Parsed Parsed) -> Int
sourceLength root@((_, Trailing rootLexemes, _), node) = getSum (nodeLength root
                                                                 <> Transformation.Rank2.foldMap nodeLength node)
   where nodeLength ((_, Trailing ls, _), _) = foldMap (Sum . Text.length . lexemeText) ls

type Parsed = (,) (Int, ParsedLexemes, Int)

-- | Transformation type used by 'reserialize'
data Serialization = Serialization
-- | Transformation type used by 'adjustPositions'
data PositionAdjustment = PositionAdjustment

instance Transformation.Transformation Serialization where
    type Domain Serialization = Parsed
    type Codomain Serialization = Const (Ap (State (Int, [Lexeme])) [Lexeme])

instance Transformation.Transformation PositionAdjustment where
    type Domain PositionAdjustment = Parsed
    type Codomain PositionAdjustment = Compose (State Int) Parsed

instance Serialization `Transformation.At` g Parsed Parsed where
   Serialization $ ((nodePos, Trailing nodeLexemes, _), _) = Const (Ap $ state f)
      where f :: (Int, [Lexeme]) -> ([Lexeme], (Int, [Lexeme]))
            f (pos, parentLexemes)
               | nodePos > pos, l:ls <- parentLexemes = first (l:) (f (pos + Text.length (lexemeText l), ls))
               | otherwise = (mempty, (pos, nodeLexemes <> parentLexemes))

instance (Rank2.Foldable (g Parsed), Deep.Foldable Serialization g) => Full.Foldable Serialization g where
   foldMap trans ((nodeStart, Trailing nodeLexemes, _), node) = Ap (state f)
      where f :: (Int, [Lexeme]) -> ([Lexeme], (Int, [Lexeme]))
            f (pos, parentLexemes)
               | nodeStart > pos, l:ls <- parentLexemes = first (l:) (f (pos + Text.length (lexemeText l), ls))
               | let (ls, (pos', lexemes')) = runState (getAp $ Deep.foldMap trans node) (pos, nodeLexemes) =
                     (ls <> lexemes',
                      (pos' + getSum (foldMap (Sum . Text.length . lexemeText) lexemes'), parentLexemes))

instance (Rank2.Foldable (g (Const (Sum Int))),
          Deep.Foldable (Transformation.Rank2.Fold Parsed (Sum Int)) g) =>
         PositionAdjustment `Transformation.At` g Parsed Parsed where
   PositionAdjustment $ root@((nodeStart, lexemes, nodeEnd), node) = Compose (state f)
      where f adjustment = (((nodeStart + adjustment, lexemes, nodeEnd' + adjustment), node),
                            adjustment + nodeEnd' - nodeEnd)
               where nodeEnd' = nodeStart + sourceLength root

instance (Rank2.Foldable (g (Const (Sum Int))),
          Deep.Foldable (Transformation.Rank2.Fold Parsed (Sum Int)) g,
          Deep.Traversable PositionAdjustment g) => Full.Traversable PositionAdjustment g where
   traverse PositionAdjustment root@((nodeStart, lexemes, nodeEnd), node) = state f
      where f adjustment = (((nodeStart + adjustment, lexemes, nodeEnd' + adjustment),
                             evalState (Deep.traverse PositionAdjustment node) adjustment),
                            adjustment + nodeEnd' - nodeEnd)
               where nodeEnd' = nodeStart + sourceLength root

instance (Rank2.Foldable (g Parsed),
          Deep.Foldable (Transformation.Rank2.Fold Parsed (Sum Int)) g) =>
         Full.Foldable (Transformation.Rank2.Fold Parsed (Sum Int)) g where
   foldMap = Full.foldMapDownDefault
