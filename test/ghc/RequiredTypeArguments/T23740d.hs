{-# LANGUAGE Haskell2010, ScopedTypeVariables, RequiredTypeArguments #-}

module T22513d where

f (Just (x :: id) :: Maybe id) = x
