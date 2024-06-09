module Main where

import Data.List (sort)
import Data.Monoid.Instances.PrefixMemory (content)
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

import qualified Language.Haskell as Haskell
import Language.Haskell.Extensions.AST (Language)
import qualified Language.Haskell.Binder as Binder
import qualified Language.Haskell.Extensions as Extensions
import qualified Language.Haskell.Template as Template

import Prelude hiding (readFile)

main = do
   predefinedModuleBindings <- Haskell.predefinedModuleBindings
   preludeBindings <- Haskell.preludeBindings
   exampleTree predefinedModuleBindings preludeBindings "" "test/" >>= defaultMain . testGroup "positive"

width = 80
contextLines = 3

exampleTree :: Binder.ModuleEnvironment Language
            -> Binder.Environment Language
            -> FilePath -> FilePath -> IO [TestTree]
exampleTree moduleBindings preludeBindings ancestry path =
   do let fullPath = combine ancestry path
          treeLeaf = prettyFile moduleBindings preludeBindings fullPath
      isDir <- doesDirectoryExist fullPath
      if isDir
         then (:[]) . testGroup path . concat
              <$> (listDirectory fullPath >>= mapM (exampleTree moduleBindings preludeBindings fullPath) . sort)
         else return . (:[]) . testCase path $
              do moduleSource <- readFile fullPath
                 (originalModule, prettyModule) <- treeLeaf moduleSource
                 (originalModule', prettyModule') <- treeLeaf originalModule
                 (originalModule'', prettyModule'') <- treeLeaf prettyModule
                 assertEqual "original=original" originalModule originalModule'
                 assertEqual "pretty=pretty" prettyModule prettyModule'
                 assertEqual "pretty=pretty'" prettyModule prettyModule''
                 assertEqual "original=pretty" originalModule'' prettyModule''

prettyFile :: Binder.ModuleEnvironment Language
           -> Binder.Environment Language
           -> FilePath -> Text -> IO (Text, Text)
prettyFile moduleBindings preludeBindings path src = do
   let extensions = Map.fromSet (const True) Extensions.includedByDefault
   case Haskell.parseModule extensions moduleBindings preludeBindings False src of
      Right [tree] -> return (Reserializer.reserializeNested tree, pack $ Template.pprint tree)
      Right trees -> error (show (length trees) ++ " ambiguous parses.")
      Left err -> error (unpack $ failureDescription src (content <$> err) 4)
