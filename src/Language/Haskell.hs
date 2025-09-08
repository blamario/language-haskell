{-# Language DataKinds, FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses, OverloadedStrings,
             ScopedTypeVariables, StandaloneDeriving,
             TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}

-- | The programming language Haskell

module Language.Haskell (parseModule, predefinedModuleBindings, preludeBindings, resolvePositions, Input,
                         -- * Node wrappers
                         -- | An abstract syntax tree produced by this library contains nodes of different types
                         -- (declarations, types, expressions, patterns, etc), but every node is contained by the
                         -- same type of /wrapper/ node.
                         Parsed, Placed, Bound) where

import qualified Language.Haskell.Abstract as Abstract
import qualified Language.Haskell.Binder as Binder
import Language.Haskell.Extensions as Extensions (Extension, includedByDefault)
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
import qualified Transformation.AG as AG
import qualified Transformation.AG.Dimorphic as Di

import Control.Category ((>>>))
import Control.Monad ((>=>))
import Data.Either.Validation (validationToEither)
import Data.Functor.Compose (Compose(..))
import Data.Foldable (foldrM)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Monoid.Instances.PrefixMemory (Shadowed, content)
import Data.Monoid.Textual (fromText)
import Data.Ord (Down)
import Data.Semigroup.Union (UnionWith(..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath.Posix (combine)
import System.IO.Unsafe (unsafeInterleaveIO)
import Text.Grampa (ParseResults, ParseFailure (errorAlternatives))
import Text.Parser.Input.Position (offset)

import Paths_language_haskell (getDataDir)

import Prelude hiding (readFile)

-- | Every node in a parsed AST is originally wrapped with this functor
type Parsed = Grammar.NodeWrap Input

-- | Every node in a parsed AST with calculated positions is wrapped with this functor
type Placed = Reserializer.Wrapped Int Text

-- | Every node in a parsed and resolved AST is wrapped with this functor
type Bound = Binder.WithEnvironment AST.Language Placed

-- | The input monoid type
type Input = Shadowed Text

-- | Parse the given text of a single module according to the specified language extensions and resolve all
-- identifiers inside the module.
parseModule :: Map Extension Bool                      -- ^ language extension switches
            -> Binder.ModuleEnvironment AST.Language   -- ^ modules available for import
            -> Binder.Environment AST.Language         -- ^ names available without import
            -> Bool                                    -- ^ verify if the identifiers are bound and extensions used?
            -> Text                                    -- ^ the module's source code
            -> ParseResults Input [Bound (AST.Module AST.Language AST.Language Bound Bound)]
parseModule extensions modEnv env verify source =
   ((resolvePositions extensions modEnv env source <$>)
    <$> Grammar.parseModule extensions (fromText source :: Input))
   >>= (if verify then traverse (checkAllBound >=> checkRestrictions extensions) else pure)

-- | Resolve identifiers in the given parsed AST, and replace the stored positions in the entire tree with
-- offsets from the start of the given source text.
resolvePositions :: (Full.Traversable (Reorganizer.Reorganization AST.Language (Down Int) Input) node,
                     AG.Attribution (Binder.BinderWorker AST.Language Parsed) node,
                     AG.Atts (AG.Synthesized (Binder.BinderWorker AST.Language Parsed)) node
                     ~ Binder.LocalEnvironment AST.Language,
                     Rank2.Apply (node (AG.Semantics (AG.Keep (Binder.BinderWorker AST.Language Parsed)))),
                     Rank2.Traversable (node (AG.Semantics (AG.Keep (Binder.BinderWorker AST.Language Parsed)))),
                     Deep.Functor (AG.Keep (Binder.BinderWorker AST.Language Parsed)) node,
                     Deep.Functor
                        (Rank2.Map
                           (AG.Kept (Binder.BinderWorker AST.Language Parsed))
                           (Binder.WithEnvironment AST.Language Parsed))
                        node,
                     Deep.Functor
                        (Transformation.Mapped
                            ((,) (Binder.Attributes AST.Language))
                            (Rank2.Map Parsed Placed))
                        node)
                 => Map Extension Bool                      -- ^ language extension switches
                 -> Binder.ModuleEnvironment AST.Language   -- ^ modules available for import
                 -> Binder.Environment AST.Language         -- ^ names available without import
                 -> Text                                    -- ^ the module's source code for adjusting node positions
                 -> Parsed (node Parsed Parsed)             -- ^ parsed AST
                 -> Bound (node Bound Bound)
resolvePositions extensions modEnv env src =
   (Transformation.Mapped (Rank2.Map rewrap) Full.<$>)
   . either (error . show) id . validationToEither
   . Full.traverse Reorganizer.Reorganization
   . Binder.withBindings extensions modEnv env
   where rewrap :: forall a. Reserializer.Wrapped (Down Int) Input a -> Reserializer.Wrapped Int Text a
         rewrap = Reserializer.mapWrapping (offset src) content

-- | Check if all the identifiers in the given resolved module are properly bound.
checkAllBound :: Bound (AST.Module AST.Language AST.Language Bound Bound)
              -> ParseResults Input (Bound (AST.Module AST.Language AST.Language Bound Bound))
checkAllBound m = if unbounds == mempty then pure m
                  else Left mempty{errorAlternatives= [show unbounds]}
   where unbounds = Binder.unboundNames m

-- | Check if the given resolved module conforms to and depends on the given extensions.
checkRestrictions :: Map Extension Bool
                  -> Bound (AST.Module AST.Language AST.Language Bound Bound)
                  -> ParseResults Input (Bound (AST.Module AST.Language AST.Language Bound Bound))
checkRestrictions extensions m = case Verifier.verify extensions m of
   [] -> pure m
   errors -> Left mempty{errorAlternatives= show <$> errors}

instance (Rank2.Functor (g (Compose ((,) (Binder.Attributes AST.Language)) q)),
          Deep.Functor (Transformation.Mapped ((,) (Binder.Attributes AST.Language)) (Rank2.Map q Placed)) g) =>
         Full.Functor (Transformation.Mapped ((,) (Binder.Attributes AST.Language)) (Rank2.Map q Placed)) g where
   (<$>) = Full.mapDownDefault

-- | All the qualified bindings available without any import statement, such as @Prelude.id@
predefinedModuleBindings :: IO (Binder.ModuleEnvironment AST.Language)
predefinedModuleBindings =
   liftA2 (<>) baseModuleBindings
   $ UnionWith . Map.fromList . pure . (,) Binder.preludeName <$> unqualifiedPreludeBindings

-- | All the @Prelude@ bindings available without any import statement
preludeBindings :: IO (Binder.Environment AST.Language)
preludeBindings = Binder.onMap (Map.mapKeysMonotonic $ Abstract.qualifiedName Nothing) <$> unqualifiedPreludeBindings

unqualifiedPreludeBindings :: IO (Binder.LocalEnvironment AST.Language)
unqualifiedPreludeBindings = do
   preludeModuleDir <- flip combine "report" <$> getDataDir
   moduleFileNames <- filter (List.isSuffixOf ".hs") <$> listDirectory preludeModuleDir
   moduleTexts <- mapM (unsafeInterleaveIO . Text.IO.readFile . combine preludeModuleDir) moduleFileNames
   baseModuleEnv <- unsafeInterleaveIO baseModuleBindings
   let Just moduleNames = traverse (Text.stripSuffix ".hs" . Text.pack) moduleFileNames
       parsedModules = assertSuccess . parseModule defaultExtensions moduleEnv mempty False <$> moduleTexts
       assertSuccess ~(Right ~[parsed]) = parsed
       moduleEnv = baseModuleEnv <> preludeModuleEnv
       preludeModuleEnv = UnionWith $ Map.fromList $ zip (Abstract.moduleName @AST.Language . pure . Abstract.name <$> moduleNames) (Di.syn . fst . getCompose <$> parsedModules)
       Just prelude = Map.lookup Binder.preludeName (getUnionWith preludeModuleEnv)
       defaultExtensions = Map.fromSet (const True) Extensions.includedByDefault
   pure (prelude <> Binder.builtinPreludeBindings)

baseModuleBindings :: IO (Binder.ModuleEnvironment AST.Language)
baseModuleBindings = do
   baseModuleDir <- flip combine "base" <$> getDataDir
   moduleFilePaths <- filter (List.isSuffixOf ".hs") <$> listDirectoryRecursively baseModuleDir
   prelude <- preludeBindings
   modules <- unsafeInterleaveIO predefinedModuleBindings
   let assertSuccess ~(Right ~[parsed]) = parsed
       defaultExtensions = Map.fromSet (const True) Extensions.includedByDefault
       bindModule :: FilePath -> IO (AST.ModuleName AST.Language, Binder.LocalEnvironment AST.Language)
       bindModule path = do
          moduleText <- Text.IO.readFile path
          let parsedModule :: ParseResults Input [Bound (AST.Module AST.Language AST.Language Bound Bound)]
              parsedModule = parseModule defaultExtensions modules prelude False moduleText
              env = Di.syn . fst . getCompose $ assertSuccess parsedModule
              modulesFromPath :: Text -> Maybe (NonEmpty (AST.Name AST.Language))
              modulesFromPath = Text.stripSuffix ".hs"
                                >=> Text.stripPrefix (Text.pack baseModuleDir <> "/")
                                >=> (Text.split (== '/') >>> (Abstract.name <$>) >>> nonEmpty)
          pure (maybe (error $ "Module path " <> path <> " is malformed.") Abstract.moduleName (modulesFromPath $ Text.pack path), env)
   UnionWith . Map.fromList <$> mapM (unsafeInterleaveIO . bindModule) moduleFilePaths

listDirectoryRecursively :: FilePath -> IO [FilePath]
listDirectoryRecursively path = listDirectory path >>= foldMapM (recurseDirectory path)

recurseDirectory :: FilePath -> FilePath -> IO [FilePath]
recurseDirectory ancestry name = do
   let path = combine ancestry name
   isDir <- doesDirectoryExist path
   if isDir
      then listDirectoryRecursively path
      else pure [path]

foldMapM :: (Monad m, Foldable f, Monoid b) => (a -> m b) -> f a -> m b
foldMapM f = foldrM (\x y-> (<> y) <$> f x) mempty
