{-# LANGUAGE Haskell2010, ExplicitForAll, KindSignatures, ExplicitNamespaces, RequiredTypeArguments #-}

module T24318 where

import Data.Kind

f :: forall (a :: Type) -> Bool
f (type t) x = True
