{-# Language FlexibleContexts, GADTs, OverloadedStrings, ScopedTypeVariables, StandaloneDeriving,
             TypeFamilies, TypeOperators #-}

-- | The programming language Haskell

module Language.Haskell (parseModule, resolvePositions, Placed) where

import qualified Language.Haskell.Binder as Binder
import Language.Haskell.Extensions (Extension)
import qualified Language.Haskell.Extensions.AST as AST
import qualified Language.Haskell.Extensions.Grammar as Grammar
import qualified Language.Haskell.Extensions.Verifier as Verifier

import qualified Language.Haskell.Reorganizer as Reorganizer
import qualified Language.Haskell.Reserializer as Reserializer

import qualified Transformation.Deep as Deep
import qualified Transformation.Full as Full
import qualified Transformation.Rank2 as Rank2
import qualified Transformation.AG.Monomorphic as AG.Mono

import Data.Either.Validation (validationToEither)
import Data.Map (Map)
import Data.Monoid.Instances.Positioned (LinePositioned, extract)
import Data.Ord (Down)
import Data.Text (Text)
import Text.Grampa (ParseResults, ParseFailure (errorAlternatives))
import Text.Parser.Input.Position (offset)

import Prelude hiding (readFile)

-- | Every node in a parsed and resolved AST is wrapped with this functor
type Placed = (,) (Int, Reserializer.ParsedLexemes Text, Int)

-- | Parse the given text of a single module.
parseModule :: Map Extension Bool -> Bool -> Text
            -> ParseResults (LinePositioned Text) [Placed (AST.Module AST.Language AST.Language Placed Placed)]
parseModule extensions verify source =
  ((resolvePositions source <$>) <$> Grammar.parseModule extensions (pure source :: LinePositioned Text))
  >>= (if verify then traverse (traverse $ checkRestrictions extensions) else pure)

-- | Replace the stored positions in the entire tree with offsets from the start of the given source text
resolvePositions :: (p ~ Grammar.NodeWrap (LinePositioned Text),
                     q ~ Reserializer.Wrapped (Down Int) (LinePositioned Text), r ~ Placed,
                     Full.Traversable (AG.Mono.Keep (Binder.Binder AST.Language p)) g,
                     Full.Traversable (Reorganizer.Reorganization AST.Language (Down Int) (LinePositioned Text)) g,
                     Deep.Functor (Rank2.Map q r) g)
                 => Text -> p (g p p) -> r (g r r)
resolvePositions src = Reserializer.mapWrappings (offset src) extract
                       . either (error . show) id . validationToEither
                       . Full.traverse Reorganizer.Reorganization
                       . Binder.withBindings (Binder.preludeBindings
                                              <> Binder.predefinedModuleBindings
                                              :: Binder.Environment AST.Language)


checkRestrictions :: Map Extension Bool
                  -> AST.Module AST.Language AST.Language Placed Placed
                  -> ParseResults (LinePositioned Text) (AST.Module AST.Language AST.Language Placed Placed)
checkRestrictions extensions m = case Verifier.verifyModule extensions m of
   [] -> pure m
   errors -> Left mempty{errorAlternatives= show <$> errors}
