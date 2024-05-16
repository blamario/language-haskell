{-# LANGUAGE ImplicitParameters #-}

f :: (?x :: Int) => Int
f = ?x

