{-# LANGUAGE Haskell2010, ViewPatterns #-}

module ViewPatternsFail where

data Foo = Foo { a :: Int }

foo :: Foo -> Int
foo (a -> l) = l
