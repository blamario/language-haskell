{-# LANGUAGE TupleSections, UnboxedTuples #-}

f :: (# #) Int -> (# Int, Int #)
f (# 0 #) = (# 0, #) 1
f (# x #) = (# x, x #)
