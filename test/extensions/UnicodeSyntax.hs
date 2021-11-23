{-# LANGUAGE ExplicitForAll, UnicodeSyntax #-}

eq :: ∀ a. Eq a ⇒ a → a → Bool
eq x y | True ← x == y = True
eq _ _ = False
