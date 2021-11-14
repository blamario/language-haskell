{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings,
             ScopedTypeVariables, StandaloneDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Language.Haskell.Extensions.Verifier where

import Data.Foldable (toList)
import Data.Functor.Const (Const(Const, getConst))
import Data.Functor.Compose (Compose(..))
import Data.Map.Lazy (Map)
import Data.Maybe (isJust)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Data.String (IsString)

import qualified Transformation
import qualified Transformation.Deep as Deep
import qualified Transformation.Full as Full
import qualified Transformation.AG.Monomorphic as AG.Mono
import Text.Grampa (Ambiguous(..))

import qualified Language.Haskell.Abstract as Abstract
import qualified Language.Haskell.AST as AST
import Language.Haskell.Extensions (Extension, ExtensionSwitch, partitionContradictory, withImplications)
import qualified Language.Haskell.Extensions as Extensions
import qualified Language.Haskell.Extensions.AST as ExtAST
import qualified Language.Haskell.Reserializer as Reserializer
import Language.Haskell.Reserializer (Lexeme(..), ParsedLexemes(..), TokenType(..))

data Accounting l pos s = Accounting
data Verification l pos s = Verification

type Accounted l pos = Const (Map Extension [(pos, pos)])
type Verified l pos = Const (Map Extension Bool -> [Error pos])

data Error pos = ContradictoryExtensionSwitches (Set ExtensionSwitch)
               | UndeclaredExtensionUse Extension [(pos, pos)]
               | UnusedExtension Extension
                 deriving (Show)

instance Transformation.Transformation (Accounting l pos s) where
    type Domain (Accounting l pos s) = Reserializer.Wrapped pos s
    type Codomain (Accounting l pos s) = Accounted l pos

instance Transformation.Transformation (Verification l pos s) where
    type Domain (Verification l pos s) = Reserializer.Wrapped pos s
    type Codomain (Verification l pos s) = Verified l pos

verifyModule :: forall l pos s. (Abstract.DeeplyFoldable (Accounting l pos s) l,
                               Abstract.Haskell l,
                               Abstract.Module l l ~ AST.Module l l) =>
                  Map Extension Bool
               -> AST.Module l l (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s)
               -> [Error pos]
verifyModule extensions (AST.ExtendedModule localExtensionSwitches m) =
   (if null contradictions then mempty else [ContradictoryExtensionSwitches contradictions])
   <> (UnusedExtension
       <$> toList (Map.keysSet (localExtensions Map.\\ usedExtensions) Set.\\ Extensions.languageVersions))
   <> (uncurry UndeclaredExtensionUse <$> Map.toList (usedExtensions Map.\\ declaredExtensions))
   where usedExtensions :: Map Extension [(pos, pos)]
         usedExtensions = Full.foldMap (Accounting :: Accounting l pos s) m
         declaredExtensions = Map.filter id (withImplications (localExtensions <> extensions)
                                             <> Map.fromSet (const True) Extensions.includedByDefault)
         (contradictions, localExtensions) = partitionContradictory (Set.fromList localExtensionSwitches)
verifyModule extensions m =
   uncurry UndeclaredExtensionUse
   <$> Map.toList (Deep.foldMap (Accounting :: Accounting l pos s) m Map.\\ declaredExtensions)
   where declaredExtensions = Map.filter id (withImplications extensions
                                             <> Map.fromSet (const True) Extensions.includedByDefault)

instance {-# overlappable #-} Accounting l pos s
         `Transformation.At` g (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s) where
   Accounting $ _ = mempty

instance {-# overlappable #-} Deep.Foldable (Accounting l pos s) g =>
         Verification l pos s
         `Transformation.At` g (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s) where
   Verification $ (_, m) = Const $ \extensions->
      uncurry UndeclaredExtensionUse
      <$> Map.toList (Deep.foldMap (Accounting :: Accounting l pos s) m Map.\\ withImplications extensions)

instance (Abstract.DeeplyFoldable (Accounting l pos s) l, Abstract.Haskell l, Abstract.Module l l ~ AST.Module l l) =>
         Verification l pos s
         `Transformation.At` AST.Module l l (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s) where
   Verification $ (_, m) = Const $ flip verifyModule m

instance (Eq s, IsString s) =>
         Accounting l pos s
         `Transformation.At` ExtAST.Import l l (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s) where
   Accounting $ ((start, Trailing lexemes, end), ExtAST.Import safe _qualified package name alias spec) = Const $
      (if safe then Map.singleton Extensions.SafeImports [(start, end)] else mempty)
      <>
      (if isJust package then Map.singleton Extensions.PackageImports [(start, end)] else mempty)
      <>
      (if null qualifiedAndAfter || any (not . isKeyword) beforeQualified then mempty
       else Map.singleton Extensions.ImportQualifiedPost [(start, end)])
      where (beforeQualified, qualifiedAndAfter) = break isQualified (filter isToken lexemes)
            isToken Token{} = True
            isToken _ = False
            isQualified Token{lexemeType= Keyword, lexemeText= "qualified"} = True
            isQualified _ = False
            isKeyword Token{lexemeType= Keyword} = True
            isKeyword _ = False

instance (Abstract.Context l ~ AST.Context l) =>
         Accounting l pos s
         `Transformation.At` AST.Declaration l l (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s) where
   Accounting $ ((start, _, end), AST.DataDeclaration context _lhs constructors _derivings) = Const $
      (if null constructors then Map.singleton Extensions.EmptyDataDeclarations [(start, end)] else mempty)
      <>
      (case snd context
       of AST.NoContext -> mempty
          _ -> Map.singleton Extensions.DatatypeContexts [(start, end)])
   Accounting $ _ = mempty

instance (Deep.Foldable (Accounting l pos s) g,
          Transformation.At (Accounting l pos s) (g (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s))) =>
         Full.Foldable (Accounting l pos s) g where
   foldMap = Full.foldMapDownDefault
