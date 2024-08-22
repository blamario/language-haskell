{-# OPTIONS_GHC -Wno-implicit-rhs-quantification #-}
{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeAbstractions #-}

module T24470b where

import Data.Kind
import Data.Data

type SynOK :: forall k. k -> Type
type SynOK @j = Proxy :: j -> Type
