{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Haskell2010, DataKinds, TypeData, KindSignatures #-}
module T22315a.Lib where

data TermLevel = Mk
type data TypeLevel = Mk

class C (a :: TypeLevel)
instance C Mk where

foo :: C a => proxy a -> ()
foo _ = ()
