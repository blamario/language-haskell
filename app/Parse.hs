{-# LANGUAGE FlexibleContexts, FlexibleInstances, RankNTypes, RecordWildCards, ScopedTypeVariables, TypeFamilies #-}

module Main where

import Language.Haskell (Placed, parseModule, resolvePosition, resolvePositions)
import Language.Haskell.AST (Language, Module(..), Expression)
import qualified Language.Haskell.Abstract as Abstract
import qualified Language.Haskell.AST as AST
import qualified Language.Haskell.Grammar as Grammar
import qualified Language.Haskell.Reserializer as Reserializer
import qualified Language.Haskell.Template as Template

import qualified Rank2 as Rank2 (Product(Pair), snd)
import qualified Transformation.Rank2 as Rank2
import qualified Transformation.Deep as Deep
import qualified Transformation.Full as Full

import Control.Monad
import Data.Data (Data)
import Data.Functor.Identity (Identity(Identity))
import Data.Functor.Compose (Compose, getCompose)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Monoid.Instances.Positioned (LinePositioned, extract)
import Data.Text (Text, unpack)
import Data.Text.IO (getLine, readFile, getContents)
import qualified Data.Text.IO as Text
import Data.Typeable (Typeable)
import Options.Applicative
import Text.Grampa (Ambiguous, Grammar, ParseResults, parseComplete, failureDescription)
import qualified Text.Grampa.ContextFree.LeftRecursive as LeftRecursive
import ReprTree
import System.FilePath (FilePath, addExtension, combine, takeDirectory)

import Prelude hiding (getLine, getContents, readFile)

data GrammarMode = ModuleMode | ExpressionMode
    deriving Show

data Output = Original | Plain | Pretty | Tree
            deriving Show

data Opts = Opts
    { optsMode        :: GrammarMode
    , optsIndex       :: Int
    , optsOutput      :: Output
    , optsInclude     :: Maybe FilePath
    , optsFile        :: Maybe FilePath
    } deriving Show

main :: IO ()
main = execParser opts >>= main'
  where
    opts = info (helper <*> p)
        ( fullDesc
       <> progDesc "Parse a Haskell file, or parse interactively"
       <> header "Haskell parser")

    p :: Parser Opts
    p = Opts
        <$> mode
        <*> (option auto (long "index" <> help "Index of ambiguous parse" <> showDefault <> value 0 <> metavar "INT"))
        <*> (flag' Pretty (long "pretty" <> help "Pretty-print output")
             <|> flag' Tree (long "tree" <> help "Print the output as an abstract syntax tree")
             <|> flag' Original (long "original" <> help "Print the output with the original tokens and whitespace")
             <|> pure Plain)
        <*> optional (strOption (short 'i' <> long "include" <> metavar "DIRECTORY"
                                 <> help "Where to look for imports"))
        <*> optional (strArgument
            ( metavar "FILE"
              <> help "Haskell file to parse"))

    mode :: Parser GrammarMode
    mode = ModuleMode          <$ switch (long "module")
       <|> ExpressionMode      <$ switch (long "expression")

main' :: Opts -> IO ()
main' Opts{..} = case optsFile
                 of Just file -> (if file == "-" then getContents else readFile file) >>= go Grammar.haskellModule file
                    Nothing ->
                        forever $
                        getLine >>=
                        case optsMode of
                            ModuleMode     -> go Grammar.haskellModule "<stdin>"
                            ExpressionMode -> go Grammar.expression "<stdin>"
   where
      go :: (Data a, Show a, Template.PrettyViaTH a, a ~ g l l Placed Placed, l ~ Language,
             Deep.Functor (Rank2.Map Grammar.NodeWrap Placed) (g l l),
             Deep.Foldable Reserializer.Serialization (g l l)) =>
            (forall p. Functor p => Grammar.HaskellGrammar l Grammar.NodeWrap p -> p (Grammar.NodeWrap (g l l Grammar.NodeWrap Grammar.NodeWrap)))
         -> String -> Text -> IO ()
      go production filename contents =
         report contents (getCompose $ resolvePositions contents . snd
                          <$> getCompose (production $ parseComplete Grammar.grammar2010 $ pure contents))
      report :: (Data a, Show a, Template.PrettyViaTH a, a ~ Placed (g l l Placed Placed), l ~ Language,
                 Deep.Foldable Reserializer.Serialization (g l l))
             => Text -> ParseResults (LinePositioned Text) [a] -> IO ()
      report _ (Right [x]) = case optsOutput
                                  of Original -> Text.putStr (Reserializer.reserialize x)
                                     Plain -> print x
                                     Pretty -> putStrLn (Template.pprint x)
                                     Tree -> putStrLn (reprTreeString x)
      report contents (Right l) = putStrLn ("Ambiguous: " ++ show optsIndex ++ "/" ++ show (length l) ++ " parses")
                                  >> report contents (Right [l !! optsIndex])
      report contents (Left err) = Text.putStrLn (failureDescription contents (extract <$> err) 4)

type NodeWrap = ((,) Int)
