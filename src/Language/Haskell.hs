{-# Language FlexibleContexts, GADTs, OverloadedStrings, ScopedTypeVariables, StandaloneDeriving, TypeFamilies #-}

-- | The programming language Haskell

module Language.Haskell (parseModule, resolvePositions, Placed) where

import qualified Language.Haskell.Abstract as Abstract
import qualified Language.Haskell.AST as AST
import qualified Language.Haskell.Grammar as Grammar
import qualified Language.Haskell.Template as Template

import qualified Language.Haskell.Disambiguator as Disambiguator
import qualified Language.Haskell.Reserializer as Reserializer

import qualified Transformation.Deep as Deep
import qualified Transformation.Rank2 as Rank2

import Data.Functor.Compose (Compose(Compose, getCompose))
import Data.Monoid.Instances.Positioned (LinePositioned, extract)
import Data.Text (Text)
import Text.Grampa (Grammar, ParseResults, parseComplete)
import Text.Parser.Input.Position (Position, offset)

import Prelude hiding (readFile)

-- | Every node in a parsed and resolved AST is wrapped with this functor
type Placed = (,) (Int, Reserializer.ParsedLexemes Text, Int)

-- | Replace the stored positions in the entire tree with offsets from the start of the given source text
resolvePositions :: (p ~ Grammar.NodeWrap (LinePositioned Text),
                     q ~ Reserializer.Wrapped Position (LinePositioned Text), r ~ Placed,
                     Deep.Functor (Grammar.DisambiguatorTrans (LinePositioned Text)) g,
                     Deep.Functor (Rank2.Map q r) g)
                 => Text -> p (g p p) -> r (g r r)
resolvePositions src = Reserializer.mapWrappings (offset src) extract
                       . Disambiguator.mapWrappings Disambiguator.headDisambiguator

-- | Parse the given text of a single module.
parseModule :: Text -> ParseResults (LinePositioned Text) [Placed (Abstract.Module AST.Language AST.Language Placed Placed)]
parseModule source = resolve source (parseComplete Grammar.grammar2010 (pure source :: LinePositioned Text))

resolve :: (Deep.Functor (Rank2.Map (Reserializer.Wrapped Position (LinePositioned Text)) Placed) (Abstract.Module l l),
            Deep.Functor (Grammar.DisambiguatorTrans (LinePositioned Text)) (Abstract.Module l l))
        => Text
        -> Grammar.HaskellGrammar l (Grammar.NodeWrap (LinePositioned Text))
                                    (Compose (Compose (ParseResults (LinePositioned Text)) [])
                                             ((,) [[Reserializer.Lexeme (LinePositioned Text)]]))
        -> ParseResults (LinePositioned Text) [Placed (Abstract.Module l l Placed Placed)]
resolve source results = getCompose (resolvePositions source . snd <$> getCompose (Grammar.haskellModule results))
