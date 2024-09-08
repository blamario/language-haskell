{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE RequiredTypeArguments #-}

module T22326_fail_nonlinear where

import Data.Kind (Type)

f :: forall (a :: Type) (b :: Type) -> ()
f (type t) (type t) = ()
