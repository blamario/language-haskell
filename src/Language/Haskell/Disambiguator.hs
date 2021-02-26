{-# Language FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
             RankNTypes, ScopedTypeVariables, TypeFamilies, UndecidableInstances #-}

module Language.Haskell.Disambiguator where

import Data.Functor.Compose (Compose(..))
import Data.Functor.Identity (Identity(..))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Text.Parser.Input.Position (Position)
import Text.Grampa (Ambiguous, getAmbiguous)

import qualified Rank2
import Transformation (Transformation)
import qualified Transformation
import qualified Transformation.Deep as Deep
import qualified Transformation.Full as Full
import qualified Transformation.Rank2

import qualified Language.Haskell.Reserializer as Reserializer

type Wrapped pos s = Compose ((,) (pos, pos)) (Compose Ambiguous ((,) (Reserializer.ParsedLexemes s)))

data T pos s t = T t

instance (Transformation t,
          Transformation.Domain t ~ Ambiguous,
          Transformation.Codomain t ~ Identity) => Transformation (T pos s t) where
   type Domain (T pos s t) = Wrapped pos s
   type Codomain (T pos s t) = Reserializer.Wrapped pos s

instance (Transformation.At t (Reserializer.ParsedLexemes s, a),
          Transformation.Domain t ~ Ambiguous,
          Transformation.Codomain t ~ Identity) => Transformation.At (T pos s t) a where
   T t $ Compose ((start, end), Compose xs) = ((start, ls, end), node)
      where Identity (ls, node) = t Transformation.$ xs

instance (Transformation t, Transformation.Domain t ~ Ambiguous, Transformation.Codomain t ~ Identity,
          Transformation.At t (Reserializer.ParsedLexemes s,
                               g (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s)),
          Deep.Functor (T pos s t) g) => Full.Functor (T pos s t) g where
   (<$>) = Full.mapUpDefault

-- | Given a disambiguating transformation that picks an 'Identity' out of an 'Ambiguous' collection, simplify the
-- wrappers of all nodes in the tree.
mapWrappings :: forall g pos s t. (Transformation t, Deep.Functor (T pos s t) g,
                              Transformation.At t (Reserializer.ParsedLexemes s,
                                                   g (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s)),
                              Transformation.Domain t ~ Ambiguous,
                              Transformation.Codomain t ~ Identity)
             => t
             -> Wrapped pos s (g (Wrapped pos s) (Wrapped pos s))
             -> Reserializer.Wrapped pos s (g (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s))
mapWrappings t x = T t Full.<$> x

-- | A trivial disambiguating transformation that always selects the first ambiguous choice
headDisambiguator = Transformation.Rank2.Map (Identity . NonEmpty.head . getAmbiguous)
