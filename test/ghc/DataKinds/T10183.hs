{-# LANGUAGE GADTs, DataKinds, TypeOperators, UnicodeSyntax #-}

module Foo where

import GHC.TypeLits

data List l t where
     NilOnlyRatherLonger  ∷ List 0 t
     (:-) ∷ t → List l t → List (l+1) t

head' ∷ (1<=l) ⇒ List l t → t
head' (x :- _) = x

data T_only_rather_longer a where
  TT_only_rather_longer :: T_only_rather_longer Bool
  TF_only_rather_longer :: T_only_rather_longer Int

f :: T_only_rather_longer Bool -> Bool
f TT_only_rather_longer = True

g :: (a ~ Bool) => T_only_rather_longer a -> Bool
g TT_only_rather_longer = True
