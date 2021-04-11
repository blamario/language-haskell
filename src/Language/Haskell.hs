{-# Language FlexibleContexts, GADTs, OverloadedStrings, ScopedTypeVariables, StandaloneDeriving, TypeFamilies #-}

-- | The programming language Haskell

module Language.Haskell (parseModule, resolvePositions, Placed) where

import qualified Language.Haskell.Abstract as Abstract
import qualified Language.Haskell.AST as AST
import qualified Language.Haskell.Binder as Binder
import qualified Language.Haskell.Grammar as Grammar
import qualified Language.Haskell.Template as Template

import qualified Language.Haskell.Disambiguator as Disambiguator
import qualified Language.Haskell.Reserializer as Reserializer
import qualified Language.Haskell.Resolver as Resolver

import qualified Transformation.Deep as Deep
import qualified Transformation.Full as Full
import qualified Transformation.Rank2 as Rank2

import Data.Either.Validation (validationToEither)
import Data.Functor.Compose (Compose(Compose, getCompose))
import qualified Data.Map.Lazy as Map
import Data.Monoid.Instances.Positioned (LinePositioned, extract)
import Data.Ord (Down)
import Data.Text (Text)
import Text.Grampa (Grammar, ParseResults, parseComplete)
import Text.Parser.Input.Position (Position, offset)

import Prelude hiding (readFile)

-- | Every node in a parsed and resolved AST is wrapped with this functor
type Placed = (,) (Int, Reserializer.ParsedLexemes Text, Int)

-- | Parse the given text of a single module.
parseModule :: Text -> ParseResults (LinePositioned Text) [Placed (AST.Module AST.Language AST.Language Placed Placed)]
parseModule source = resolve source (parseComplete Grammar.grammar2010 (pure source :: LinePositioned Text))

resolve :: (l ~ AST.Language,
            Deep.Functor (Rank2.Map (Reserializer.Wrapped (Down Int) (LinePositioned Text)) Placed) (AST.Module l l),
            Deep.Functor (Grammar.DisambiguatorTrans (LinePositioned Text)) (Abstract.Module l l))
        => Text
        -> Grammar.HaskellGrammar l (Grammar.NodeWrap (LinePositioned Text))
                                    (Compose (Compose (ParseResults (LinePositioned Text)) [])
                                             ((,) [[Reserializer.Lexeme (LinePositioned Text)]]))
        -> ParseResults (LinePositioned Text) [Placed (Abstract.Module l l Placed Placed)]
resolve source results = getCompose (resolvePositions source . snd <$> getCompose (Grammar.haskellModule results))

-- | Replace the stored positions in the entire tree with offsets from the start of the given source text
resolvePositions :: (p ~ Grammar.NodeWrap (LinePositioned Text),
                     q ~ Reserializer.Wrapped (Down Int) (LinePositioned Text), r ~ Placed,
                     Deep.Functor (Grammar.DisambiguatorTrans (LinePositioned Text)) g,
                     Deep.Functor (Rank2.Map q r) g,
                     Full.Traversable (Binder.Binder AST.Language p) g,
                     Full.Traversable (Resolver.Resolution AST.Language (Down Int) (LinePositioned Text)) g)
                 => Text -> p (g p p) -> r (g r r)
resolvePositions src = Reserializer.mapWrappings (offset src) extract
                       . either (error . show) id . validationToEither
                       . Full.traverse Resolver.Resolution
                       . Binder.withBindings (Binder.preludeBindings
                                              <> Binder.predefinedModuleBindings
                                              :: Binder.Environment AST.Language)

