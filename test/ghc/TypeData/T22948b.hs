{-# LANGUAGE Haskell2010, GADTs, TypeData #-}
module T22948b where

type data T a where
  A :: T Int

f :: T a -> ()
f !x = ()
