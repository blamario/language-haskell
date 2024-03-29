{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module T11524 where

import Data.Kind (Type)

type AType :: k -> Type
data AType a where
    AMaybe :: AType Maybe
    AInt :: AType Int
    AApp :: forall k1 k2 (a :: k1 -> k2) (b :: k1).
            AType a -> AType b -> AType (a b)

pattern PApp :: () => (fun ~ a b) => AType a -> AType b -> AType fun
--pattern PApp :: forall k (fun :: k) k1 (a :: k1 -> k) (b :: k1).
--            () => (fun ~ a b) => AType a -> AType b -> AType fun
pattern PApp fun arg <- AApp fun arg
