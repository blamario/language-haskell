{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RequiredTypeArguments #-}

module T22326_fail_bang_pat where

f :: forall (a :: k) -> ()
f !x = ()
