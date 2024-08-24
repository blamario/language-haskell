{-# LANGUAGE Haskell2010, RankNTypes, PolyKinds, KindSignatures, StandaloneKindSignatures, TypeAbstractions #-}

module T24604a where

import Data.Kind

type UF :: forall zk. zk -> Constraint
class UF @kk (xb :: k) where
    op :: (xs::kk) -> Bool
