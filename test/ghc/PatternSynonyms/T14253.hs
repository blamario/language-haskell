{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}

module T14253 where

import GHC.Exts
import Data.Kind

type TypeRep :: k -> Type
data TypeRep a where
    Con :: TypeRep (a :: k)
    TrFun   :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
                      (a :: TYPE r1) (b :: TYPE r2).
               TypeRep a
            -> TypeRep b
            -> TypeRep (a -> b)

pattern Fun :: forall k (fun :: k). ()
            => forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
                      (arg :: TYPE r1) (res :: TYPE r2).
               (k ~ Type, fun ~~ (arg -> res))
            => TypeRep arg
            -> TypeRep res
            -> TypeRep fun
pattern Fun arg res <- TrFun arg res

data Dynamic where
    Dynamic :: forall a. TypeRep a -> a -> Dynamic

-- Adding this results in failure
{-# COMPLETE Con #-}

dynApply :: Dynamic -> Dynamic -> Maybe Dynamic
-- Changing TrFun to Fun also results in failure
dynApply (Dynamic (Fun ta tr) f) (Dynamic ta' x) = undefined
dynApply _ _ = Nothing

