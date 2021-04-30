module Language.Haskell.Extensions.Abstract (ExtendedHaskell(..), module Language.Haskell.Abstract) where

import Language.Haskell.Abstract 

class Haskell λ => ExtendedHaskell λ where
   hashLiteral :: Value λ l d s -> Value λ l d s
   mdoExpression :: s (GuardedExpression l l d d) -> Expression λ l d s
   recursiveStatement :: [s (Statement l l d d)] -> Statement λ l d s
