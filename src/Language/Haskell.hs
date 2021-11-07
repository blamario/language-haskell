{-# Language FlexibleContexts, GADTs, OverloadedStrings, ScopedTypeVariables, StandaloneDeriving, TypeFamilies #-}

-- | The programming language Haskell

module Language.Haskell (parseModule, resolvePositions, Placed) where

import qualified Language.Haskell.Binder as Binder
import Language.Haskell.Extensions (Extension)
import qualified Language.Haskell.Extensions.AST as AST
import qualified Language.Haskell.Extensions.Grammar as Grammar

import qualified Language.Haskell.Reserializer as Reserializer
import qualified Language.Haskell.Resolver as Resolver

import qualified Transformation.Deep as Deep
import qualified Transformation.Full as Full
import qualified Transformation.Rank2 as Rank2
import qualified Transformation.AG.Monomorphic as AG.Mono

import Data.Either.Validation (validationToEither)
import Data.Map (Map)
import Data.Monoid.Instances.Positioned (LinePositioned, extract)
import Data.Ord (Down)
import Data.Text (Text)
import Text.Grampa (ParseResults)
import Text.Parser.Input.Position (offset)

import Prelude hiding (readFile)

-- | Every node in a parsed and resolved AST is wrapped with this functor
type Placed = (,) (Int, Reserializer.ParsedLexemes Text, Int)

-- | Parse the given text of a single module.
parseModule :: Map Extension Bool -> Text
            -> ParseResults (LinePositioned Text) [Placed (AST.Module AST.Language AST.Language Placed Placed)]
parseModule extensions source =
  (resolvePositions source <$>) <$> Grammar.parseModule extensions (pure source :: LinePositioned Text)

-- | Replace the stored positions in the entire tree with offsets from the start of the given source text
resolvePositions :: (p ~ Grammar.NodeWrap (LinePositioned Text),
                     q ~ Reserializer.Wrapped (Down Int) (LinePositioned Text), r ~ Placed,
                     Deep.Functor (Grammar.DisambiguatorTrans (LinePositioned Text)) g,
                     Deep.Functor (Rank2.Map q r) g,
                     Full.Traversable (AG.Mono.Keep (Binder.Binder AST.Language p)) g,
                     Full.Traversable (Resolver.Resolution AST.Language (Down Int) (LinePositioned Text)) g)
                 => Text -> p (g p p) -> r (g r r)
resolvePositions src = Reserializer.mapWrappings (offset src) extract
                       . either (error . show) id . validationToEither
                       . Full.traverse Resolver.Resolution
                       . Binder.withBindings (Binder.preludeBindings
                                              <> Binder.predefinedModuleBindings
                                              :: Binder.Environment AST.Language)

