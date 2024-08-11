{-# language Haskell2010, TypeData, GADTs #-}

type data Foo where
   Foo1 :: Foo

main = putStrLn "Works."
