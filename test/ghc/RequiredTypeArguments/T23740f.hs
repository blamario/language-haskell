{-# LANGUAGE Haskell2010, ExplicitForAll, KindSignatures, RequiredTypeArguments #-}

module T23740f where

import Data.Proxy

p :: () -> forall (a :: id). Proxy a
p () = Proxy
