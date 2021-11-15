{-# LANGUAGE UnicodeSyntax #-}

eq :: Eq a ⇒ a → a → Bool
eq x y | True ← x == y = True
eq _ _ = False
