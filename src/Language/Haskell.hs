{-# Language DataKinds, FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses, OverloadedStrings,
             ScopedTypeVariables, StandaloneDeriving,
             TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}

-- | The programming language Haskell

module Language.Haskell (parseModule, resolvePositions, Bound, Placed) where

import qualified Language.Haskell.Abstract as Abstract
import qualified Language.Haskell.Binder as Binder
import Language.Haskell.Extensions (Extension)
import qualified Language.Haskell.Extensions.AST as AST
import qualified Language.Haskell.Extensions.Grammar as Grammar
import qualified Language.Haskell.Extensions.Verifier as Verifier

import qualified Language.Haskell.Reorganizer as Reorganizer
import qualified Language.Haskell.Reserializer as Reserializer

import qualified Transformation
import qualified Transformation.Deep as Deep
import qualified Transformation.Full as Full
import qualified Transformation.Rank2 as Rank2
import qualified Transformation.AG.Dimorphic as Di

import Control.Monad ((>=>))
import Data.Either.Validation (validationToEither)
import Data.Map (Map)
import Data.Monoid.Instances.Positioned (LinePositioned, extract)
import Data.Ord (Down)
import Data.Text (Text)
import Text.Grampa (ParseResults, ParseFailure (errorAlternatives))
import Text.Parser.Input.Position (offset)

import Prelude hiding (readFile)

-- | Every node in a parsed AST is wrapped with this functor
type Placed = Reserializer.Wrapped Int Text

-- | Every node in a parsed and resolved AST is wrapped with this functor
type Bound = Binder.WithEnvironment AST.Language Placed

-- | Parse the given text of a single module.
parseModule :: Map Extension Bool -> Bool -> Text
            -> ParseResults (LinePositioned Text) [Bound (AST.Module AST.Language AST.Language Bound Bound)]
parseModule extensions verify source =
  ((resolvePositions source <$>) <$> Grammar.parseModule extensions (pure source :: LinePositioned Text))
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
                 => Text -> p (g p p) -> r (g r r)
resolvePositions src = (Transformation.Mapped (Rank2.Map rewrap) Full.<$>)
                       . either (error . show) id . validationToEither
                       . Full.traverse Reorganizer.Reorganization
                       . Binder.withBindings
                            Binder.predefinedModuleBindings
                            (Binder.preludeBindings :: Binder.Environment AST.Language)
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

instance Deep.Functor
            (Transformation.Mapped
                ((,) (Di.Atts (Binder.Environment AST.Language) (Binder.LocalEnvironment AST.Language)))
                (Rank2.Map q Placed))
            g =>
         Full.Functor
            (Transformation.Mapped
                ((,) (Di.Atts (Binder.Environment AST.Language) (Binder.LocalEnvironment AST.Language)))
                (Rank2.Map q Placed))
            g where
   (<$>) = Full.mapDownDefault
