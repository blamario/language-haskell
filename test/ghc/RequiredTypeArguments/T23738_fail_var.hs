{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeAbstractions #-}

module T23738_fail_var where

vfun :: forall (a :: k) -> ()
vfun (type _) = ()

f :: Int -> ()
f a = vfun a
