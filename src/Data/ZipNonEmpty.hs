{-# Language DeriveDataTypeable, StandaloneDeriving #-}

module Data.ZipNonEmpty where

import Data.Data (Data)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import GHC.Stack (HasCallStack)

-- | A 'NonEmpty' list with 'ZipList' instance semantics
newtype ZipNonEmpty a = ZipNonEmpty (NonEmpty a) deriving (Eq, Ord, Show, Read)

instance Functor ZipNonEmpty where
   fmap f (ZipNonEmpty xs) = ZipNonEmpty (fmap f xs)

instance Foldable ZipNonEmpty where
   foldMap f (ZipNonEmpty xs) = foldMap f xs

instance Traversable ZipNonEmpty where
   traverse f (ZipNonEmpty xs) = ZipNonEmpty <$> traverse f xs

instance Applicative ZipNonEmpty where
   pure = ZipNonEmpty . NonEmpty.repeat
   liftA2 f (ZipNonEmpty xs) (ZipNonEmpty ys) = ZipNonEmpty (NonEmpty.zipWith f xs ys)

instance Semigroup (ZipNonEmpty a) where
   ZipNonEmpty xs <> ZipNonEmpty ys = ZipNonEmpty (xs <> ys)

deriving instance Data a => Data (ZipNonEmpty a)

-- | Converts a normal list to a 'ZipNonEmpty' one, raising an error if given an empty list.
fromList :: HasCallStack => [a] -> ZipNonEmpty a
fromList = ZipNonEmpty . NonEmpty.fromList

-- | List all items the function maps to 'Just'.
mapMaybe :: (a -> Maybe b) -> ZipNonEmpty a -> [b]
mapMaybe p xs = Maybe.mapMaybe p $ toList xs

-- | A 'ZipNonEmpty' list with a single item
singleton :: a -> ZipNonEmpty a
singleton = ZipNonEmpty . NonEmpty.singleton

