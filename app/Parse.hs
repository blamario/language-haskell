{-# LANGUAGE FlexibleContexts, FlexibleInstances, RankNTypes, RecordWildCards, ScopedTypeVariables, TypeFamilies #-}

module Main where

import Language.Haskell (Placed, parseModule, resolvePositions)
import Language.Haskell.Extensions (ExtensionSwitch(Yes), allExtensions)
import Language.Haskell.Extensions.AST (Language, Module(..), Expression)
import qualified Language.Haskell.Abstract as Abstract
import qualified Language.Haskell.Extensions.AST as AST
import qualified Language.Haskell.Binder as Binder
import qualified Language.Haskell.Extensions.Grammar as Grammar
import qualified Language.Haskell.Reserializer as Reserializer
import qualified Language.Haskell.Resolver as Resolver
import qualified Language.Haskell.Template as Template

import qualified Transformation.Rank2 as Rank2
import qualified Transformation.Deep as Deep
import qualified Transformation.Full as Full
import qualified Transformation.AG.Monomorphic

import Control.Monad
import Data.Data (Data)
import Data.Functor.Identity (Identity(Identity))
import Data.Functor.Compose (Compose(..))
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Monoid.Instances.Positioned (LinePositioned, extract)
import Data.Ord (Down)
import qualified Data.Set as Set
import Data.Text (Text, unpack)
import Data.Text.IO (getLine, readFile, getContents)
import qualified Data.Text.IO as Text
import Data.Typeable (Typeable)
import Options.Applicative
import Text.Grampa (Ambiguous, Grammar, ParseResults, Position, parseComplete, failureDescription)
import qualified Text.Grampa.ContextFree.LeftRecursive as LeftRecursive
import ReprTree
import System.FilePath (FilePath, addExtension, combine, takeDirectory)

import Prelude hiding (getLine, getContents, readFile)

data GrammarMode = ModuleMode | ExpressionMode
    deriving Show

data Output = Original | Plain | Pretty | Tree
            deriving Show

data Stage = Parsed | Bound | Resolved
    deriving Show

data Opts = Opts
    { optsMode         :: GrammarMode
    , optsIndex        :: Int
    , optsOutput       :: Output
    , optsStage        :: Stage
    , optsInclude      :: Maybe FilePath
    , optsFile         :: Maybe FilePath
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
        <*> (flag' Parsed (long "parsed" <> help "show raw ambiguous parse tree")
             <|> flag' Bound (long "bound" <> help "show ambiguous parse tree with identifier bindings")
             <|> flag' Resolved (long "resolved" <> help "show parse tree with all ambiguities resolved")
             <|> pure Resolved)
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
                 of Just file -> (if file == "-" then getContents else readFile file)
                                 >>= go (Grammar.parseModule mempty) file
                    Nothing ->
                        forever $
                        getLine >>=
                        case optsMode of
                            ModuleMode     -> go (Grammar.parseModule mempty) "<stdin>"
                            ExpressionMode -> go parseExpression "<stdin>"
   where
      parseExpression t = getCompose
                          $ snd <$> getCompose (Grammar.expression
                                                $ parseComplete (Grammar.extendedGrammar $ Set.map Yes allExtensions) t)
      go :: (Data a, Show a, Template.PrettyViaTH a, Typeable g,
             a ~ g l l Placed Placed, l ~ Language, w ~ Grammar.NodeWrap (LinePositioned Text),
             e ~ Binder.WithEnvironment Language w,
             Abstract.QualifiedName l ~ AST.QualifiedName l,
             Data (Transformation.AG.Monomorphic.Atts (Binder.Environment Language)),
             Show (Transformation.AG.Monomorphic.Atts (Binder.Environment Language)),
             Data (g Language Language e e), Data (g Language Language w w),
             Show (g Language Language e e), Show (g Language Language w w),
             Full.Traversable (Transformation.AG.Monomorphic.Keep (Binder.Binder Language w)) (g Language Language),
             Deep.Functor (Rank2.Map (Reserializer.Wrapped (Down Int) (LinePositioned Text)) Placed) (g l l),
             Deep.Functor (Grammar.DisambiguatorTrans (LinePositioned Text)) (g Language Language),
             Deep.Foldable (Reserializer.Serialization Int Text) (g l l),
             Full.Traversable (Resolver.Resolution AST.Language (Down Int) (LinePositioned Text)) (g l l)) =>
            (LinePositioned Text -> ParseResults (LinePositioned Text) [w (g l l w w)])
         -> String -> Text -> IO ()
      go parser filename contents = report contents (parser $ pure contents)
      report :: forall g l a e w.
                (Data a, Show a, Template.PrettyViaTH a, Typeable g,
                 a ~ Placed (g l l Placed Placed), l ~ Language, w ~ Grammar.NodeWrap (LinePositioned Text),
                 e ~ Binder.WithEnvironment Language w,
                 Abstract.QualifiedName l ~ AST.QualifiedName l,
                 Data (Transformation.AG.Monomorphic.Atts (Binder.Environment Language)),
                 Show (Transformation.AG.Monomorphic.Atts (Binder.Environment Language)),
                 Data (g Language Language e e), Data (g Language Language w w),
                 Show (g Language Language e e), Show (g Language Language w w),
                 Full.Traversable (Transformation.AG.Monomorphic.Keep (Binder.Binder Language w)) (g Language Language),
                 Deep.Functor (Rank2.Map (Reserializer.Wrapped (Down Int) (LinePositioned Text)) Placed) (g l l),
                 Deep.Functor (Grammar.DisambiguatorTrans (LinePositioned Text)) (g l l),
                 Deep.Foldable (Reserializer.Serialization Int Text) (g l l),
                 Full.Traversable (Resolver.Resolution AST.Language (Down Int) (LinePositioned Text)) (g l l))
             => Text -> ParseResults (LinePositioned Text) [w (g l l w w)] -> IO ()
      report contents (Right [parsed]) = case optsOutput of
         Original -> Text.putStr (Reserializer.reserialize resolved)
         Plain -> case optsStage of
            Parsed -> print parsed
            Bound -> print bound
            Resolved -> print resolved
         Pretty -> putStrLn (Template.pprint resolved)
         Tree -> putStrLn $ case optsStage of
            Parsed -> reprTreeString parsed
            Bound -> reprTreeString bound
            Resolved -> reprTreeString resolved
         where resolved = resolvePositions contents parsed
               bound = Binder.withBindings (Binder.predefinedModuleBindings :: Binder.Environment l) parsed
      report contents (Right l) =
         putStrLn ("Ambiguous: " ++ show optsIndex ++ "/" ++ show (length l) ++ " parses")
         >> report contents (Right [l !! optsIndex])
      report contents (Left err) = Text.putStrLn (failureDescription contents (extract <$> err) 4)

type NodeWrap = ((,) Int)
