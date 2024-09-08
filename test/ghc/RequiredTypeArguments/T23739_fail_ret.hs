{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RequiredTypeArguments #-}

module T23739_fail_ret where

bad :: forall (a :: k) -> k
bad t = t
