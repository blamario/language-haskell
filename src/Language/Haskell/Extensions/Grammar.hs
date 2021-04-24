{-# Language FlexibleContexts, FlexibleInstances, OverloadedStrings,
             Rank2Types, RecordWildCards,
             ScopedTypeVariables, TemplateHaskell, TupleSections, TypeSynonymInstances #-}

module Language.Haskell.Extensions.Grammar (grammar, extendedGrammar, module Report) where

import Control.Applicative
import Data.Ord (Down)
import Text.Grampa
import Text.Grampa.ContextFree.LeftRecursive.Transformer (ParserT)
import qualified Transformation.Deep as Deep

import qualified Language.Haskell.Abstract as Abstract
import qualified Language.Haskell.AST as AST (Language)
import qualified Language.Haskell.Grammar as Report
import Language.Haskell.Grammar (HaskellGrammar(..), Parser, OutlineMonoid, DisambiguatorTrans, NodeWrap,
                                 delimiter)
import Language.Haskell.Reserializer (Lexeme, Serialization)

extendedGrammar :: (Abstract.Haskell l, LexicalParsing (Parser (HaskellGrammar l (NodeWrap t)) t),
                    Ord t, Show t, OutlineMonoid t,
                    Deep.Foldable (Serialization (Down Int) t) (Abstract.CaseAlternative l l),
                    Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l),
                    Deep.Foldable (Serialization (Down Int) t) (Abstract.Expression l l),
                    Deep.Foldable (Serialization (Down Int) t) (Abstract.Import l l),
                    Deep.Foldable (Serialization (Down Int) t) (Abstract.Statement l l),
                    Deep.Functor (DisambiguatorTrans t) (Abstract.CaseAlternative l l),
                    Deep.Functor (DisambiguatorTrans t) (Abstract.Declaration l l),
                    Deep.Functor (DisambiguatorTrans t) (Abstract.Expression l l),
                    Deep.Functor (DisambiguatorTrans t) (Abstract.Import l l),
                    Deep.Functor (DisambiguatorTrans t) (Abstract.Statement l l))
                 => Grammar (HaskellGrammar l (NodeWrap t)) (ParserT ((,) [[Lexeme t]])) t
extendedGrammar = fixGrammar grammar

grammar :: forall l g t. (Abstract.Haskell l, LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t,
                      Deep.Foldable (Serialization (Down Int) t) (Abstract.CaseAlternative l l),
                      Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l),
                      Deep.Foldable (Serialization (Down Int) t) (Abstract.Expression l l),
                      Deep.Foldable (Serialization (Down Int) t) (Abstract.Import l l),
                      Deep.Foldable (Serialization (Down Int) t) (Abstract.Statement l l),
                      Deep.Functor (DisambiguatorTrans t) (Abstract.CaseAlternative l l),
                      Deep.Functor (DisambiguatorTrans t) (Abstract.Declaration l l),
                      Deep.Functor (DisambiguatorTrans t) (Abstract.Expression l l),
                      Deep.Functor (DisambiguatorTrans t) (Abstract.Import l l),
                      Deep.Functor (DisambiguatorTrans t) (Abstract.Statement l l))
        => GrammarBuilder (HaskellGrammar l (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
grammar g@HaskellGrammar{..} = Report.grammar g
