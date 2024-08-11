{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid.Instances.PrefixMemory (content)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.IO (readFile)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath.Posix (combine)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)
import Text.Grampa (failureDescription)

import qualified Transformation.Rank2 as Rank2

import qualified Language.Haskell.Reserializer as Reserializer

import qualified Language.Haskell as Haskell
import Language.Haskell.Extensions.AST (Language, Module)
import qualified Language.Haskell.Binder as Binder
import qualified Language.Haskell.Extensions as Extensions
import qualified Language.Haskell.Template as Template

import Prelude hiding (readFile)

main = do
   predefinedModules <- Haskell.predefinedModuleBindings
   preludeBindings <- Haskell.preludeBindings
   testDir predefinedModules preludeBindings "test" "extensions" >>= defaultMain . testGroup "extensions"

testDir :: Binder.ModuleEnvironment Language -> Binder.Environment Language -> FilePath -> FilePath -> IO [TestTree]
testDir predefinedModules preludeBindings ancestry path = do
   let fullPath = combine ancestry path
   isDir <- doesDirectoryExist fullPath
   if isDir
      then (:[]) . testGroup path . concat
           <$> (listDirectory fullPath >>= mapM (testDir predefinedModules preludeBindings fullPath) . List.sort)
      else return . (:[]) . testCase path $ testModule predefinedModules preludeBindings fullPath

testModule :: Binder.ModuleEnvironment Language -> Binder.Environment Language -> FilePath -> Assertion
testModule predefinedModules preludeBindings path = do
   moduleSource <- readFile path
   let Just (extensions, rest) = (Text.unlines <$>) <$> List.uncons (Text.lines moduleSource)
       inverse w
          | w `elem` ["{-#", "LANGUAGE", "Haskell2010,", "#-}"] = w
          | Just rest <- Text.stripPrefix "No" w = rest
          | otherwise = "No" <> w
   assertCompiles predefinedModules preludeBindings moduleSource
   assertFails predefinedModules preludeBindings moduleSource (Text.unwords (inverse <$> Text.words extensions) <> rest)

assertCompiles :: Binder.ModuleEnvironment Language -> Binder.Environment Language -> Text -> Assertion
assertCompiles predefinedModules preludeBindings src =
   case Haskell.parseModule extensions predefinedModules preludeBindings True src of
      Right [tree] -> pure ()
      Right trees -> assertFailure (show (length trees) ++ " ambiguous parses.")
      Left err -> assertFailure (Text.unpack $ failureDescription src (content <$> err) 4)

assertFails :: Binder.ModuleEnvironment Language -> Binder.Environment Language -> Text -> Text -> Assertion
assertFails predefinedModules preludeBindings ext noExt =
   case Haskell.parseModule extensions predefinedModules preludeBindings True noExt of
      Right [tree]
        | let Right [extTree] = Haskell.parseModule extensions predefinedModules preludeBindings True ext,
          Template.pprint tree == Template.pprint extTree -> assertFailure "False positive, the module verifies."
        | otherwise -> pure ()
      Right trees -> assertFailure (show (length trees) ++ " ambiguous parses.")
      Left err -> pure ()

extensions = Map.fromSet (const True) Extensions.includedByDefault
