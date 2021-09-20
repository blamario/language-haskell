{-# Language OverloadedStrings #-}

-- | Missing syntax extensions:
-- * @QualifiedDo@ requires TemplateHaskell 2.17
-- * @TransformListComp@ is not supported by TemplateHaskell
-- * @Arrows@ is not supported by TemplateHaskell
-- * @LexicalNegation@ awaits

module Language.Haskell.Extensions (Extension(..), allExtensions, extensionsByName) where

import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Set (Set)
import Data.String (IsString)

data Extension = IdentifierSyntax
               | UnicodeSyntax
               | Haskell2010
               | MagicHash
               | MonadComprehensions
               | MonadFailDesugaring
               | NoImplicitPrelude
               | OverloadedLists
               | ParallelListComprehensions
               | RebindableSyntax
               | RecursiveDo
               | TupleSections
               | EmptyCase
               | EmptyDataDeclarations
               | LambdaCase
               | MultiWayIf
               | BlockArguments
               | AlternativeLayoutRule
               deriving (Enum, Eq, Ord, Read, Show)

allExtensions :: Set Extension
allExtensions = Set.fromList [IdentifierSyntax ..]

extensionsByName :: (IsString t, Ord t) => Map t Extension
extensionsByName = Map.fromList [
                      ("AlternativeLayoutRule", AlternativeLayoutRule),
                      ("BlockArguments", BlockArguments),
                      ("EmptyCase", EmptyCase),
                      ("EmptyDataDecls", EmptyDataDeclarations),
                      ("IdentifierSyntax", IdentifierSyntax),
                      ("LambdaCase", LambdaCase),
                      ("Haskell2010", Haskell2010),
                      ("MagicHash", MagicHash),
                      ("MonadComprehensions", MonadComprehensions),
                      ("MonadFailDesugaring", MonadFailDesugaring),
                      ("NoImplicitPrelude", NoImplicitPrelude),
                      ("OverloadedLists", OverloadedLists),
                      ("MultiWayIf", MultiWayIf),
                      ("ParallelListComp", ParallelListComprehensions),
                      ("RebindableSyntax", RebindableSyntax),
                      ("RecursiveDo", RecursiveDo),
                      ("TupleSections", TupleSections),
                      ("UnicodeSyntax", UnicodeSyntax)]
