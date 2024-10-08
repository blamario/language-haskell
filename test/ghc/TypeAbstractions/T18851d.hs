{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE DataKinds, TypeApplications, GADTs #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeOperators #-}

module T18851d where

import GHC.TypeNats

data VSucc n where
  F :: VSucc (1 + n)

foo :: VSucc n -> VSucc n -> VSucc n
foo (F @n1) F = F @n1
