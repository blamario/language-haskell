{-# LANGUAGE StandaloneKindSignatures, PolyKinds, DataKinds, TypeFamilies,
             UnliftedNewtypes #-}

module T17021a where

import Data.Kind
import GHC.Exts

type LevId2 :: r ~ Id LiftedRep => TYPE r -> TYPE r
