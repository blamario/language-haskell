{-# LANGUAGE FlexibleInstances #-}

import Data.Kind ( Type, Constraint )

instance Eq [Int] where
   a:as == b:bs = a == b && as == bs
