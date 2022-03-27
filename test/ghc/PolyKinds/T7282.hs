 {-# LANGUAGE TypeFamilies, DataKinds, PolyKinds #-}

module T7282 where

import Data.Kind (Type)

class Foo (xs :: [k]) where
     type Bar xs :: Type

instance Foo '[] where
     type Bar '[] = Int
