{-# LANGUAGE NoPatternGuards #-}

-- f x | 0 <- x, True = []
f x = [y | y <- [x]]

main = do
  x <- return 4
  return x
