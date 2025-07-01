{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tuple
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- Functions associated with the tuple data types.
--
-----------------------------------------------------------------------------

module Data.Tuple
  ( Solo (..)
  , getSolo
  , fst
  , snd
  , curry
  , uncurry
  , swap
  ) where

-- import GHC.Base ()      -- Note [Depend on GHC.Tuple]
-- import GHC.Tuple (Solo (..), getSolo)

default ()              -- Double isn't available yet

-- | @Solo@ is the canonical lifted 1-tuple, just like 'Tuple2' is the canonical
-- lifted 2-tuple (pair) and 'Tuple3' is the canonical lifted 3-tuple (triple).
--
-- The most important feature of @Solo@ is that it is possible to force its
-- "outside" (usually by pattern matching) without forcing its "inside",
-- because it is defined as a datatype rather than a newtype. One situation
-- where this can be useful is when writing a function to extract a value from
-- a data structure. Suppose you write an implementation of arrays and offer
-- only this function to index into them:
--
-- @
-- index :: Array a -> Int -> a
-- @
--
-- Now imagine that someone wants to extract a value from an array and store it
-- in a lazy-valued finite map/dictionary:
--
-- @
-- insert "hello" (arr `index` 12) m
-- @
--
-- This can actually lead to a space leak. The value is not actually extracted
-- from the array until that value (now buried in a map) is forced. That means
-- the entire array may be kept live by just that value!  Often, the solution
-- is to use a strict map, or to force the value before storing it, but for
-- some purposes that's undesirable.
--
-- One common solution is to include an indexing function that can produce its
-- result in an arbitrary @Applicative@ context:
--
-- @
-- indexA :: Applicative f => Array a -> Int -> f a
-- @
--
-- When using @indexA@ in a /pure/ context, @Solo@ serves as a handy
-- @Applicative@ functor to hold the result. You could write a non-leaky
-- version of the above example thus:
--
-- @
-- case arr `indexA` 12 of
--   Solo a -> insert "hello" a m
-- @
--
-- While such simple extraction functions are the most common uses for
-- unary tuples, they can also be useful for fine-grained control of
-- strict-spined data structure traversals, and for unifying the
-- implementations of lazy and strict mapping functions.
data Solo a = MkSolo a

-- This will have to wait for https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0065-type-infix.rst
-- Otherwise the deprecation would apply to the data type as well.
{-# DEPRECATED data Solo "The Solo constructor has been renamed to MkSolo to avoid punning." #-}
pattern Solo :: a -> Solo a
pattern Solo x = MkSolo x
{-# COMPLETE Solo #-}

-- | Extract the value from a 'Solo'. Very often, values should be extracted
-- directly using pattern matching, to control just what gets evaluated when.
-- @getSolo@ is for convenience in situations where that is not the case:
--
-- When the result is passed to a /strict/ function, it makes no difference
-- whether the pattern matching is done on the \"outside\" or on the
-- \"inside\":
--
-- @
-- Data.Set.insert (getSolo sol) set === case sol of Solo v -> Data.Set.insert v set
-- @
--
-- A traversal may be performed in 'Solo' in order to control evaluation
-- internally, while using @getSolo@ to extract the final result. A strict
-- mapping function, for example, could be defined
--
-- @
-- map' :: Traversable t => (a -> b) -> t a -> t b
-- map' f = getSolo . traverse ((Solo $!) . f)
-- @
getSolo :: Solo a -> a
-- getSolo is a standalone function, rather than a record field of Solo,
-- because Solo is a wired-in TyCon, and a wired-in TyCon that  has
-- record fields is a bit more inconvenient than if it doesn't.
-- (No other wired-in TyCon has record fields.)  So it seems easier
-- to have getSolo as its own separate function (#20562)
getSolo (MkSolo a) = a

-- ---------------------------------------------------------------------------
-- Standard functions over tuples

-- | Extract the first component of a pair.
fst                     :: (a,b) -> a
fst (x,_)               =  x

-- | Extract the second component of a pair.
snd                     :: (a,b) -> b
snd (_,y)               =  y

-- | Convert an uncurried function to a curried function.
--
-- ==== __Examples__
--
-- >>> curry fst 1 2
-- 1
curry                   :: ((a, b) -> c) -> a -> b -> c
curry f x y             =  f (x, y)

-- | 'uncurry' converts a curried function to a function on pairs.
--
-- ==== __Examples__
--
-- >>> uncurry (+) (1,2)
-- 3
--
-- >>> uncurry ($) (show, 1)
-- "1"
--
-- >>> map (uncurry max) [(1,2), (3,4), (6,8)]
-- [2,4,8]
uncurry                 :: (a -> b -> c) -> ((a, b) -> c)
uncurry f p             =  f (fst p) (snd p)

-- | Swap the components of a pair.
swap                    :: (a,b) -> (b,a)
swap (a,b)              = (b,a)

-- $setup
-- >>> import Prelude hiding (curry, uncurry, fst, snd)
