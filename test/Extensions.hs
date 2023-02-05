module Main where

import Data.Monoid.Instances.Positioned (extract)
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

import Language.Haskell (parseModule, Placed)
import Language.Haskell.AST (Language, Module)
import qualified Language.Haskell.Extensions as Extensions
import qualified Language.Haskell.Template as Template

import Prelude hiding (readFile)

main = testDir "test" "extensions" >>= defaultMain . testGroup "extensions"

testDir :: FilePath -> FilePath -> IO [TestTree]
testDir ancestry path =
   do let fullPath = combine ancestry path
      isDir <- doesDirectoryExist fullPath
      if isDir
         then (:[]) . testGroup path . concat <$> (listDirectory fullPath >>= mapM (testDir fullPath))
         else return . (:[]) . testCase path $ testModule fullPath

testModule :: FilePath -> Assertion
testModule path = do moduleSource <- readFile path
                     let (extensions, rest) = Text.unlines <$> splitAt 1 (Text.lines moduleSource)
                     assertCompiles moduleSource
                     assertFails rest

assertCompiles :: Text -> Assertion
assertCompiles src = case parseModule (Map.fromSet (const True) Extensions.includedByDefault) True src of
   Right [tree] -> pure ()
   Right trees -> assertFailure (show (length trees) ++ " ambiguous parses.")
   Left err -> assertFailure (Text.unpack $ failureDescription src (extract <$> err) 4)

assertFails :: Text -> Assertion
assertFails src = case parseModule (Map.fromSet (const True) Extensions.includedByDefault) True src of
   Right [tree] -> assertFailure "False positive, the module verifies."
   Right trees -> assertFailure (show (length trees) ++ " ambiguous parses.")
   Left err -> pure ()
