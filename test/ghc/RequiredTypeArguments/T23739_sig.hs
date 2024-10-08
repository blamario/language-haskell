{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module T23739_sig where

import Data.Kind
import Data.Type.Bool
import Data.Proxy

data SBool b where
  SFalse :: SBool False
  STrue  :: SBool True

class KnownBool b where
  boolSing :: SBool b

boolVal :: forall b -> KnownBool b => Bool
boolVal b =
  case boolSing @b of
    SFalse -> False
    STrue  -> True

g :: forall b -> KnownBool b => If b Integer String
g (b :: Bool) =
  case boolSing @b of
    SFalse -> "Hello"
    STrue  -> 42

type FromJust :: Maybe a -> a
type family FromJust m where
  FromJust (Just x) = x

type KindOf :: k -> Type
type KindOf (a :: k) = k

h :: forall m -> Proxy (KindOf (FromJust m))
h (m :: Maybe a) = Proxy @a

hBool :: Proxy Bool
hBool = h (Just True)
