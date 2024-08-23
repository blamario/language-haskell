{-# LANGUAGE Haskell2010, FunctionalDependencies #-}

class C a b | a -> b where
   f :: a -> b
