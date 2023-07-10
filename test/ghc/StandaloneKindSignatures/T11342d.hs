{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module T11342d where

import GHC.TypeLits
import Data.Type.Equality

f1 :: CmpChar 'x' 'x' :~: EQ
f1 = Refl
