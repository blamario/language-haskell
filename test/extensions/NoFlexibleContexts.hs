{-# LANGUAGE Haskell2010, KindSignatures #-}
module Ctx where

import Data.Kind ( Type, Constraint )

data Proxy (ctxt :: Type -> Constraint) = Proxy

nonmeth :: ctxt Int => Proxy ctxt -> a
nonmeth prox = nonmeth prox
