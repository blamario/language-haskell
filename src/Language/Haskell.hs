{-# Language DataKinds, FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses, OverloadedStrings,
             ScopedTypeVariables, StandaloneDeriving,
             TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}

-- | The programming language Haskell

module Language.Haskell (parseModule, predefinedModuleBindings, preludeBindings, resolvePositions, Bound, Placed) where

import qualified Language.Haskell.Abstract as Abstract
import qualified Language.Haskell.Binder as Binder
import Language.Haskell.Extensions (Extension)
import qualified Language.Haskell.Extensions.AST as AST
import qualified Language.Haskell.Extensions.Grammar as Grammar
import qualified Language.Haskell.Extensions.Verifier as Verifier
import qualified Language.Haskell.Reorganizer as Reorganizer
import qualified Language.Haskell.Reserializer as Reserializer

import qualified Rank2
import qualified Transformation
import qualified Transformation.Deep as Deep
import qualified Transformation.Full as Full
import qualified Transformation.Rank2 as Rank2
import qualified Transformation.AG.Dimorphic as Di

import Control.Monad ((>=>))
import Data.Either.Validation (validationToEither)
import Data.Functor.Compose (Compose(..))
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid.Instances.Positioned (LinePositioned, extract)
import Data.Ord (Down)
import Data.Semigroup.Union (UnionWith(..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import System.Directory (listDirectory)
import System.FilePath.Posix (combine)
import System.IO.Unsafe (unsafeInterleaveIO)
import Text.Grampa (ParseResults, ParseFailure (errorAlternatives))
import Text.Parser.Input.Position (offset)

import Paths_language_haskell (getDataDir)

import Prelude hiding (readFile)

-- | Every node in a parsed AST is wrapped with this functor
type Placed = Reserializer.Wrapped Int Text

-- | Every node in a parsed and resolved AST is wrapped with this functor
type Bound = Binder.WithEnvironment AST.Language Placed

-- | Parse the given text of a single module.
parseModule :: Map Extension Bool
            -> Binder.ModuleEnvironment AST.Language
            -> Binder.Environment AST.Language
            -> Bool
            -> Text
            -> ParseResults (LinePositioned Text) [Bound (AST.Module AST.Language AST.Language Bound Bound)]
parseModule extensions modEnv env verify source =
   ((resolvePositions extensions modEnv env source <$>)
    <$> Grammar.parseModule extensions (pure source :: LinePositioned Text))
   >>= (if verify then traverse (checkAllBound >=> checkRestrictions extensions) else pure)

-- | Replace the stored positions in the entire tree with offsets from the start of the given source text
resolvePositions :: (p ~ Grammar.NodeWrap (LinePositioned Text),
                     q ~ Reserializer.Wrapped (Down Int) (LinePositioned Text), r ~ Bound,
                     Full.Traversable (Di.Keep (Binder.Binder AST.Language p)) g,
                     Full.Traversable (Reorganizer.Reorganization AST.Language (Down Int) (LinePositioned Text)) g,
                     Deep.Functor
                        (Transformation.Mapped
                            ((,) (Di.Atts (Binder.Environment AST.Language) (Binder.LocalEnvironment AST.Language)))
                            (Rank2.Map q Placed))
                        g)
                 => Map Extension Bool
                 -> Binder.ModuleEnvironment AST.Language
                 -> Binder.Environment AST.Language
                 -> Text
                 -> p (g p p)
                 -> r (g r r)
resolvePositions extensions modEnv env src =
   (Transformation.Mapped (Rank2.Map rewrap) Full.<$>)
   . either (error . show) id . validationToEither
   . Full.traverse Reorganizer.Reorganization
   . Binder.withBindings extensions modEnv env
   where rewrap :: forall a. Reserializer.Wrapped (Down Int) (LinePositioned Text) a -> Reserializer.Wrapped Int Text a
         rewrap = Reserializer.mapWrapping (offset src) extract

checkAllBound :: Bound (AST.Module AST.Language AST.Language Bound Bound)
              -> ParseResults (LinePositioned Text) (Bound (AST.Module AST.Language AST.Language Bound Bound))
checkAllBound m = if unbounds == mempty then pure m
                  else Left mempty{errorAlternatives= [show unbounds]}
   where unbounds = Binder.unboundNames m

-- | Check if the given module conforms to and depends on the given extensions.
checkRestrictions :: Map Extension Bool
                  -> Bound (AST.Module AST.Language AST.Language Bound Bound)
                  -> ParseResults (LinePositioned Text) (Bound (AST.Module AST.Language AST.Language Bound Bound))
checkRestrictions extensions m = case Verifier.verifyModule extensions m of
   [] -> pure m
   errors -> Left mempty{errorAlternatives= show <$> errors}

instance (Rank2.Functor (g (Compose ((,) (Binder.Attributes AST.Language)) q)),
          Deep.Functor (Transformation.Mapped ((,) (Binder.Attributes AST.Language)) (Rank2.Map q Placed)) g) =>
         Full.Functor (Transformation.Mapped ((,) (Binder.Attributes AST.Language)) (Rank2.Map q Placed)) g where
   (<$>) = Full.mapDownDefault

predefinedModuleBindings :: IO (Binder.ModuleEnvironment AST.Language)
predefinedModuleBindings = UnionWith . Map.fromList . pure . (,) Binder.preludeName <$> unqualifiedPreludeBindings

preludeBindings :: IO (Binder.Environment AST.Language)
preludeBindings = Binder.onMap (Map.mapKeysMonotonic $ Abstract.qualifiedName Nothing) <$> unqualifiedPreludeBindings

unqualifiedPreludeBindings :: IO (Binder.LocalEnvironment AST.Language)
unqualifiedPreludeBindings = do
   preludeModuleDir <- flip combine "report" <$> getDataDir
   moduleFileNames <- filter (List.isSuffixOf ".hs") <$> listDirectory preludeModuleDir
   moduleTexts <- mapM (unsafeInterleaveIO . Text.IO.readFile . combine preludeModuleDir) moduleFileNames
   let Just moduleNames = traverse (Text.stripSuffix ".hs" . Text.pack) moduleFileNames
       parsedModules = assertSuccess . parseModule mempty moduleEnv mempty False <$> moduleTexts
       assertSuccess ~(Right ~[parsed]) = parsed
       moduleEnv = UnionWith $ Map.fromList $ zip (Abstract.moduleName @AST.Language . pure . Abstract.name <$> moduleNames) (Di.syn . fst . getCompose <$> parsedModules)
       Just prelude = Map.lookup Binder.preludeName (getUnionWith moduleEnv)
   pure prelude
