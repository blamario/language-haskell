{-# Language FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
             RankNTypes, ScopedTypeVariables, TypeFamilies, UndecidableInstances #-}

module Language.Haskell.Disambiguator where

import Control.Arrow (first)
import Data.Either (partitionEithers)
import Data.Either.Validation (Validation(..), validationToEither)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Identity (Identity(..))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Semigroup (Semigroup(..), sconcat)
import Data.Semigroup.Factorial (Factorial)
import Text.Parser.Input.Position (Position)
import Text.Grampa (Ambiguous(Ambiguous, getAmbiguous))

import qualified Rank2
import Transformation (Transformation)
import qualified Transformation
import qualified Transformation.Deep as Deep
import qualified Transformation.Full as Full
import qualified Transformation.Rank2

import qualified Language.Haskell.Reserializer as Reserializer

type Wrapped pos s = Compose ((,) (pos, pos)) (Compose Ambiguous ((,) (Reserializer.ParsedLexemes s)))

data Local pos s t = Local t
data Effective pos s (m :: Type -> Type) t = Effective t

-- | Join the two wrappings of a double-'Wrapped' value into one.
joinWrapped :: forall pos s a. (Position pos, Factorial s) => Wrapped pos s (Wrapped pos s a) -> Wrapped pos s a
joinWrapped (Compose (range@(start, end),
             Compose (Ambiguous xs))) = Compose (range, Compose $ Ambiguous $ sconcat $ merge <$> xs)
   where merge :: (Reserializer.ParsedLexemes s, Wrapped pos s a) -> NonEmpty (Reserializer.ParsedLexemes s, a)
         merge (Reserializer.Trailing lexemes,
                Compose ((innerStart, innerEnd), Compose (Ambiguous ys))) = mergeInner <$> ys
            where mergeInner (Reserializer.Trailing innerLexemes, y) =
                     (Reserializer.Trailing $ Reserializer.mergeLexemes start lexemes innerStart innerLexemes, y)

-- Local instances

instance (Transformation t,
          Transformation.Domain t ~ Ambiguous,
          Transformation.Codomain t ~ Identity) => Transformation (Local pos s t) where
   type Domain (Local pos s t) = Wrapped pos s
   type Codomain (Local pos s t) = Reserializer.Wrapped pos s

instance (Transformation.At t (Reserializer.ParsedLexemes s, a),
          Transformation.Domain t ~ Ambiguous,
          Transformation.Codomain t ~ Identity) => Transformation.At (Local pos s t) a where
   Local t $ Compose ((start, end), Compose xs) = ((start, ls, end), node)
      where Identity (ls, node) = t Transformation.$ xs

instance (Transformation t, Transformation.Domain t ~ Ambiguous, Transformation.Codomain t ~ Identity,
          Transformation.At t (Reserializer.ParsedLexemes s,
                               g (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s)),
          Deep.Functor (Local pos s t) g) => Full.Functor (Local pos s t) g where
   (<$>) = Full.mapUpDefault

-- Effective instances

instance (Transformation t,
          Transformation.Domain t ~ Ambiguous,
          Transformation.Codomain t ~ m) => Transformation (Effective pos s m t) where
   type Domain (Effective pos s m t) = Wrapped pos s
   type Codomain (Effective pos s m t) = Compose m (Reserializer.Wrapped pos s)

instance (Transformation.At t (Reserializer.ParsedLexemes s, a),
          Transformation.Domain t ~ Ambiguous,
          Transformation.Codomain t ~ m,
          Functor m) => Transformation.At (Effective pos s m t) a where
   Effective t $ Compose ((start, end), Compose xs) = Compose (rewrap <$> mx)
      where mx = t Transformation.$ xs
            rewrap (ls, node) = ((start, ls, end), node)

instance (Monad m, Transformation t, Transformation.Domain t ~ Ambiguous, Transformation.Codomain t ~ m,
          Transformation.At t (Reserializer.ParsedLexemes s,
                               g (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s)),
          Deep.Traversable (Effective pos s m t) g) => Full.Traversable (Effective pos s m t) g where
   traverse = Full.traverseUpDefault

-- | Given a disambiguating transformation that picks an 'Identity' out of an 'Ambiguous' collection, simplify the
-- wrappers of all nodes in the tree.
mapWrappings :: forall g pos s t. (Transformation t, Deep.Functor (Local pos s t) g,
                              Transformation.At t (Reserializer.ParsedLexemes s,
                                                   g (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s)),
                              Transformation.Domain t ~ Ambiguous,
                              Transformation.Codomain t ~ Identity)
             => t
             -> Wrapped pos s (g (Wrapped pos s) (Wrapped pos s))
             -> Reserializer.Wrapped pos s (g (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s))
mapWrappings t x = Local t Full.<$> x

-- | Given a disambiguating transformation that picks an item out of an 'Ambiguous' collection with a monadic effect
-- 'm', simplify the wrappers of all nodes in the tree.
traverseWrappings :: forall g pos s m t. (Monad m, Transformation t, Deep.Traversable (Effective pos s m t) g,
                                      Transformation.At t (Reserializer.ParsedLexemes s,
                                                           g (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s)),
                                      Transformation.Domain t ~ Ambiguous,
                                      Transformation.Codomain t ~ m)
             => t
             -> Wrapped pos s (g (Wrapped pos s) (Wrapped pos s))
             -> m (Reserializer.Wrapped pos s (g (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s)))
traverseWrappings t x = Full.traverse (Effective t) x

-- | A trivial disambiguating transformation that always selects the first ambiguous choice
firstChoice :: Transformation.Rank2.Map Ambiguous Identity
firstChoice = Transformation.Rank2.Map (Identity . NonEmpty.head . getAmbiguous)

-- | Ensures there's exactly one 'Success' in the given 'Wrapped' 'NonEmpty' collection. If there is none, all the
-- errors are passed to the first argument. If there is more than one, all successes are passed to the second argument
-- to report as errors.
unique :: (Eq s, Eq a)
       => (NonEmpty err -> NonEmpty err) -> ([a] -> NonEmpty err) -> Wrapped pos s (Validation (NonEmpty err) a)
       -> Validation (NonEmpty err) (Reserializer.Wrapped pos s a)
unique _ _ (Compose ((start, end), Compose (Ambiguous (x :| [])))) = first (flip ((,,) start) end) <$> (sequenceA x)
unique inv amb (Compose ((start, end), Compose (Ambiguous xs))) =
   report (partitionEithers $ traverse validationToEither <$> NonEmpty.toList xs)
   where report (_, [(ws, x)]) = Success ((start, ws, end), x)
         report (errors, []) = Failure (inv $ sconcat $ NonEmpty.fromList errors)
         report (errors, x1:x2:xs) | x1 == x2 = report (errors, x2:xs)
         report (_, multi) = Failure (amb $ snd <$> multi)
