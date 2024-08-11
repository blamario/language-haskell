{-# LANGUAGE Haskell2010, GADTs, TypeData #-}
module Main where

type data T a where
  A :: T Int
  B :: T a

main = return ()
