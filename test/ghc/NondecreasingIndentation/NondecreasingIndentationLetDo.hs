{-# Language Haskell2010, NondecreasingIndentation #-}

do_let_do = do

  let y = do
      x <- pure ()
      return x

  z <- pure x

  pure z
