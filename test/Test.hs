module Main where

import Control.Monad (unless)
import Data.Either.Validation (Validation(..))
import Data.Functor.Identity (Identity(Identity))
import Data.List (isSuffixOf)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Monoid.Instances.Positioned (extract)
import qualified Data.Map as Map
import Data.Text (Text, pack, unpack)
import Data.Text.IO (readFile)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath.Posix (combine)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, assertEqual, testCase)
import Text.Grampa (failureDescription)

import qualified Transformation.Rank2 as Rank2

import qualified Language.Haskell.Reserializer as Reserializer

import Language.Haskell (parseModule, Placed)
import Language.Haskell.AST (Language, Module)
import qualified Language.Haskell.Extensions as Extensions
import qualified Language.Haskell.Template as Template

import Prelude hiding (readFile)

main = exampleTree "" "test/" >>= defaultMain . testGroup "positive"

width = 80
contextLines = 3

exampleTree :: FilePath -> FilePath -> IO [TestTree]
exampleTree ancestry path =
   do let fullPath = combine ancestry path
      isDir <- doesDirectoryExist fullPath
      if isDir
         then (:[]) . testGroup path . concat <$> (listDirectory fullPath >>= mapM (exampleTree fullPath))
         else return . (:[]) . testCase path $
              do moduleSource <- readFile fullPath
                 (originalModule, prettyModule) <- prettyFile fullPath moduleSource
                 (originalModule', prettyModule') <- prettyFile fullPath originalModule
                 (originalModule'', prettyModule'') <- prettyFile fullPath prettyModule
                 assertEqual "original=original" originalModule originalModule'
                 assertEqual "pretty=pretty" prettyModule prettyModule'
                 assertEqual "pretty=pretty'" prettyModule prettyModule''
                 assertEqual "original=pretty" originalModule'' prettyModule''

prettyFile :: FilePath -> Text -> IO (Text, Text)
prettyFile path src = case parseModule (Map.fromSet (const True) Extensions.includedByDefault) False src of
   Right [tree] -> return (Reserializer.reserialize tree, pack $ Template.pprint tree)
   Right trees -> error (show (length trees) ++ " ambiguous parses.")
   Left err -> error (unpack $ failureDescription src (extract <$> err) 4)
