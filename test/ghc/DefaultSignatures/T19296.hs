{-# OPTIONS_GHC -Wredundant-constraints -dsuppress-uniques #-}
{-# LANGUAGE DefaultSignatures, InstanceSigs #-}
module M ( f ) where

-- Redundant constraint
f :: Eq a => a -> ()
f _ = ()

-- Redundant constraint in expression signature
g _ = (\x -> ()) :: Eq a => a -> ()

-- GHC highlights more than necessary
h :: (Eq a, Ord b) => a -> b -> b
h _ b
    | b <= b = b
    | otherwise = b
