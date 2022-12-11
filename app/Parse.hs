{-# LANGUAGE FlexibleContexts, FlexibleInstances, RankNTypes, RecordWildCards, ScopedTypeVariables,
             TypeFamilies, TypeOperators #-}

module Main where

import Language.Haskell (Placed, resolvePositions)
import qualified Language.Haskell.Extensions as Extensions
import Language.Haskell.Extensions.AST (Language)
import qualified Language.Haskell.Abstract as Abstract
import qualified Language.Haskell.Extensions.AST as AST
import qualified Language.Haskell.Binder as Binder
import qualified Language.Haskell.Extensions.Grammar as Grammar
import qualified Language.Haskell.Extensions.Verifier as Verifier
import qualified Language.Haskell.Reorganizer as Reorganizer
import qualified Language.Haskell.Reserializer as Reserializer
import qualified Language.Haskell.Template as Template

import qualified Transformation
import qualified Transformation.AG.Dimorphic as Di
import qualified Transformation.Rank2 as Rank2
import qualified Transformation.Deep as Deep
import qualified Transformation.Full as Full

import Control.Monad
import Data.Data (Data)
import Data.Functor.Compose (Compose(..))
import Data.Monoid.Instances.Positioned (LinePositioned, extract)
import Data.Ord (Down)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Text.IO (getLine, readFile, getContents)
import qualified Data.Text.IO as Text
import Data.Typeable (Typeable)
import Options.Applicative
import Text.Grampa (ParseResults, parseComplete, failureDescription)
import ReprTree

import Prelude hiding (getLine, getContents, readFile)

data GrammarMode = ModuleMode | ExpressionMode
    deriving Show

data Output = Original | Plain | Pretty | Tree
            deriving Show

data Stage = Parsed | Bound | Resolved | Verified
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
             <|> flag' Verified (long "verified" <> help "show resolved parse tree with all extensions verified")
             <|> pure Verified)
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
                                 >>= go parseModule file
                    Nothing ->
                        forever $
                        getLine >>=
                        case optsMode of
                            ModuleMode     -> go parseModule "<stdin>"
                            ExpressionMode -> go parseExpression "<stdin>"
   where
      parseModule = Grammar.parseModule (Map.fromSet (const True) Extensions.includedByDefault)
      parseExpression t = getCompose
                          $ snd <$> getCompose (Grammar.expression . Grammar.report
                                                $ parseComplete (Grammar.extendedGrammar Extensions.allExtensions) t)
      go :: (Data a, Show a, Template.PrettyViaTH a, Typeable g,
             a ~ g l l Placed Placed, l ~ Language, w ~ Grammar.NodeWrap (LinePositioned Text),
             e ~ Binder.WithEnvironment Language w,
             Abstract.QualifiedName l ~ AST.QualifiedName l,
             Data (Di.Atts (Binder.Environment Language) (Binder.LocalEnvironment Language)),
             Show (Di.Atts (Binder.Environment Language) (Binder.LocalEnvironment Language)),
             Data (g Language Language e e), Data (g Language Language w w),
             Show (g Language Language e e), Show (g Language Language w w),
             Transformation.At (Verifier.Verification Int Text) (g l l Placed Placed),
             Full.Traversable (Di.Keep (Binder.Binder l w)) (g l l),
             Full.Traversable (Reorganizer.Reorganization l (Down Int) (LinePositioned Text)) (g l l),
             Deep.Functor (Rank2.Map (Reserializer.Wrapped (Down Int) (LinePositioned Text)) Placed) (g l l),
             Deep.Functor (Rank2.Map (Reserializer.Wrapped (Down Int) (LinePositioned Text))
                                     (Reserializer.Wrapped (Down Int) Text)) (g l l),
             Deep.Foldable (Reserializer.Serialization Int Text) (g l l),
             Deep.Foldable (Reserializer.Serialization (Down Int) (LinePositioned Text)) (g l l)) =>
            (LinePositioned Text -> ParseResults (LinePositioned Text) [w (g l l w w)])
         -> String -> Text -> IO ()
      go parser _filename contents = report contents (parser $ pure contents)
      report :: forall g l a e w.
                (Data a, Show a, Template.PrettyViaTH a, Typeable g,
                 a ~ Placed (g l l Placed Placed), l ~ Language, w ~ Grammar.NodeWrap (LinePositioned Text),
                 e ~ Binder.WithEnvironment Language w,
                 Abstract.QualifiedName l ~ AST.QualifiedName l,
                 Data (Di.Atts (Binder.Environment Language) (Binder.LocalEnvironment Language)),
                 Show (Di.Atts (Binder.Environment Language) (Binder.LocalEnvironment Language)),
                 Data (g Language Language e e), Data (g Language Language w w),
                 Show (g Language Language e e), Show (g Language Language w w),
                 Transformation.At (Verifier.Verification Int Text) (g l l Placed Placed),
                 Full.Traversable (Di.Keep (Binder.Binder l w)) (g l l),
                 Full.Traversable (Reorganizer.Reorganization l (Down Int) (LinePositioned Text)) (g l l),
                 Deep.Functor (Rank2.Map (Reserializer.Wrapped (Down Int) (LinePositioned Text)) Placed) (g l l),
                 Deep.Functor (Rank2.Map (Reserializer.Wrapped (Down Int) (LinePositioned Text))
                                         (Reserializer.Wrapped (Down Int) Text)) (g l l),
                 Deep.Foldable (Reserializer.Serialization Int Text) (g l l),
                 Deep.Foldable (Reserializer.Serialization (Down Int) (LinePositioned Text)) (g l l))
             => Text -> ParseResults (LinePositioned Text) [w (g l l w w)] -> IO ()
      report contents (Right [parsed]) = case optsOutput of
         Original -> case optsStage of
            Parsed -> Text.putStr (extract $ Reserializer.reserialize parsed)
            Resolved -> Text.putStr $ Reserializer.reserialize resolved
            Verified -> verifyBefore (Text.putStr . Reserializer.reserialize)
         Plain -> case optsStage of
            Parsed -> print parsed
            Bound -> print bound
            Resolved -> print resolved
            Verified -> verifyBefore print
         Pretty -> case optsStage of
           Resolved -> putStrLn $ Template.pprint resolved
           Verified -> verifyBefore (putStrLn . Template.pprint)
         Tree -> case optsStage of
            Parsed -> putStrLn $ reprTreeString parsed
            Bound -> putStrLn $ reprTreeString bound
            Resolved -> putStrLn $ reprTreeString resolved
            Verified -> verifyBefore (putStrLn . reprTreeString)
         where verifyBefore :: (a -> IO ()) -> IO ()
               verifyBefore action = case getConst (t Transformation.$ resolved) mempty of
                  [] -> action resolved
                  errors -> mapM_ (putStrLn . show) errors
               t :: Verifier.Verification Int Text
               t = Verifier.Verification
               resolved = resolvePositions contents parsed
               bound = Binder.withBindings
                          Binder.predefinedModuleBindings
                          (Binder.preludeBindings :: Binder.Environment l)
                          parsed
      report contents (Right l) =
         putStrLn ("Ambiguous: " ++ show optsIndex ++ "/" ++ show (length l) ++ " parses")
         >> report contents (Right [l !! optsIndex])
      report contents (Left err) = Text.putStrLn (failureDescription contents (extract <$> err) 4)

type NodeWrap = ((,) Int)
