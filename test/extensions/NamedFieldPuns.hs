{-# LANGUAGE NamedFieldPuns #-}

data Rec = Rec {a :: Int}

f Rec{a} = Rec{a}
