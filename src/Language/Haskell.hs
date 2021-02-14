{-# Language FlexibleContexts, GADTs, OverloadedStrings, ScopedTypeVariables, StandaloneDeriving, TypeFamilies #-}

-- | The programming language Haskell

module Language.Haskell (parseModule, resolvePosition, resolvePositions, Placed) where

import qualified Language.Haskell.Abstract as Abstract
import qualified Language.Haskell.AST as AST
import qualified Language.Haskell.Grammar as Grammar
import qualified Language.Haskell.Template as Template

import qualified Language.Haskell.Reserializer as Reserializer

import qualified Transformation.Deep as Deep
import qualified Transformation.Rank2 as Rank2

import Data.Functor.Compose (Compose(Compose, getCompose))
import Data.Monoid.Instances.Positioned (LinePositioned, extract)
import Data.Text (Text)
import Text.Grampa (Grammar, ParseResults, parseComplete)
import qualified Text.Parser.Input.Position as Position

import Prelude hiding (readFile)

-- | Every node in a parsed and resolved AST is wrapped with this functor
type Placed = (,) (Int, Reserializer.ParsedLexemes Text, Int)

-- | Replace the stored positions in the entire tree with offsets from the start of the given source text
resolvePositions :: (p ~ Grammar.NodeWrap (LinePositioned Text), q ~ Placed, Deep.Functor (Rank2.Map p q) g)
                 => Text -> p (g p p) -> q (g q q)
resolvePositions src t = resolvePosition src ((resolvePosition src Rank2.<$>) <$> t)

-- | Replace the stored positions of the given node with offset from the start of the given source text
resolvePosition :: Text -> Grammar.NodeWrap (LinePositioned Text) a -> Placed a
resolvePosition src = \((start, ls, end), a)-> ((Position.offset src start, extract <$> ls, Position.offset src end), a)

-- | Parse the given text of a single module.
parseModule :: Text -> ParseResults (LinePositioned Text) [Placed (Abstract.Module AST.Language AST.Language Placed Placed)]
parseModule source = resolve source (parseComplete Grammar.grammar2010 (pure source :: LinePositioned Text))

resolve :: Deep.Functor (Rank2.Map (Grammar.NodeWrap (LinePositioned Text)) Placed) (Abstract.Module l l)
        => Text
        -> Grammar.HaskellGrammar l (Grammar.NodeWrap (LinePositioned Text))
                                    (Compose (Compose (ParseResults (LinePositioned Text)) [])
                                             ((,) [[Reserializer.Lexeme (LinePositioned Text)]]))
        -> ParseResults (LinePositioned Text) [Placed (Abstract.Module l l Placed Placed)]
resolve source results = getCompose (resolvePositions source . snd <$> getCompose (Grammar.haskellModule results))
