{-# LANGUAGE FlexibleContexts #-}
module Ctx where

import Data.Kind ( Type, Constraint )

isZero :: Eq Int => Int -> Bool
isZero x = x == 0
