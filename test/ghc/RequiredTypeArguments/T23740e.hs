{-# LANGUAGE Haskell2010, KindSignatures, RequiredTypeArguments #-}

module T23740e where

data T (a :: id) = MkT
