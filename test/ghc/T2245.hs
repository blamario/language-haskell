default (T)

data T = T deriving (Eq, Ord, Read, Show)
instance Num T
instance Fractional T
main = interact $ show . (< 1e300) . read
