module Language.Haskell.Extensions.Abstract (ExtendedHaskell(..), module Language.Haskell.Abstract) where

import Language.Haskell.Abstract 

class Haskell λ => ExtendedHaskell λ where
   hashLiteral :: Value λ l d s -> Value λ l d s
