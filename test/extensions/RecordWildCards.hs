{-# LANGUAGE RecordWildCards #-}

data E = MkE {a :: Int, b :: [Int]}

f MkE{..} = MkE{..}
