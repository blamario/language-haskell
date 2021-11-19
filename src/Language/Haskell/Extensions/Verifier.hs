{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings,
             ScopedTypeVariables, StandaloneDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Language.Haskell.Extensions.Verifier where

import Control.Applicative (liftA2)
import qualified Data.Char as Char
import Data.Foldable (toList)
import Data.Functor.Const (Const(Const, getConst))
import Data.Functor.Compose (Compose(..))
import Data.Map.Lazy (Map)
import Data.Maybe (isJust)
import Data.Map (Map)
import Data.Monoid.Textual (TextualMonoid, characterPrefix)
import Data.Semigroup (Any(Any, getAny))
import Data.Semigroup.Cancellative (LeftReductive(isPrefixOf, stripPrefix))
import Data.Semigroup.Factorial (Factorial)
import qualified Data.Semigroup.Factorial as Factorial
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

data Accounting pos s = Accounting
data LabelAccounting pos s = LabelAccounting
data UnicodeSyntaxAccounting pos s = UnicodeSyntaxAccounting
data Verification pos s = Verification

type Accounted pos = Const (Map Extension [(pos, pos)])
type Verified pos = Const (Map Extension Bool -> [Error pos])

data Error pos = ContradictoryExtensionSwitches (Set ExtensionSwitch)
               | UndeclaredExtensionUse Extension [(pos, pos)]
               | UnusedExtension Extension
                 deriving (Show)

instance Transformation.Transformation (Accounting pos s) where
    type Domain (Accounting pos s) = Reserializer.Wrapped pos s
    type Codomain (Accounting pos s) = Accounted pos

instance Transformation.Transformation (LabelAccounting pos s) where
    type Domain (LabelAccounting pos s) = Reserializer.Wrapped pos s
    type Codomain (LabelAccounting pos s) = Accounted pos

instance Transformation.Transformation (UnicodeSyntaxAccounting pos s) where
    type Domain (UnicodeSyntaxAccounting pos s) = Reserializer.Wrapped pos s
    type Codomain (UnicodeSyntaxAccounting pos s) = Accounted pos

instance Transformation.Transformation (Verification pos s) where
    type Domain (Verification pos s) = Reserializer.Wrapped pos s
    type Codomain (Verification pos s) = Verified pos

verifyModule :: forall l pos s. (TextualMonoid s, Abstract.DeeplyFoldable (Accounting pos s) l,
                               Abstract.DeeplyFoldable (LabelAccounting pos s) l,
                               Abstract.Haskell l, Abstract.Module l l ~ AST.Module l l) =>
                  Map Extension Bool
               -> AST.Module l l (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s)
               -> [Error pos]
verifyModule extensions (AST.ExtendedModule localExtensionSwitches m) =
   (if null contradictions then mempty else [ContradictoryExtensionSwitches contradictions])
   <> (UnusedExtension
       <$> toList (Map.keysSet localExtensions Set.\\ usedExtensionsWithPremises Set.\\ Extensions.languageVersions))
   <> (uncurry UndeclaredExtensionUse <$> Map.toList (usedExtensions Map.\\ declaredExtensions))
   where usedExtensions :: Map Extension [(pos, pos)]
         usedExtensions = Full.foldMap (Accounting :: Accounting pos s) m
         declaredExtensions = Map.filter id (withImplications (localExtensions <> extensions)
                                             <> Map.fromSet (const True) Extensions.includedByDefault)
         (contradictions, localExtensions) = partitionContradictory (Set.fromList localExtensionSwitches)
         usedExtensionsWithPremises = Map.foldMapWithKey extensionAndPremises usedExtensions
         extensionAndPremises x _ = Set.singleton x <> Map.findWithDefault mempty x Extensions.inverseImplications
verifyModule extensions m =
   uncurry UndeclaredExtensionUse
   <$> Map.toList (Deep.foldMap (Accounting :: Accounting pos s) m Map.\\ declaredExtensions)
   where declaredExtensions = Map.filter id (withImplications extensions
                                             <> Map.fromSet (const True) Extensions.includedByDefault)

instance {-# overlappable #-} Accounting pos s
         `Transformation.At` g (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s) where
   Accounting $ _ = mempty

instance {-# overlappable #-} Deep.Foldable (Accounting pos s) g =>
         Verification pos s
         `Transformation.At` g (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s) where
   Verification $ (_, m) = Const $ \extensions->
      uncurry UndeclaredExtensionUse
      <$> Map.toList (Deep.foldMap (Accounting :: Accounting pos s) m Map.\\ withImplications extensions)

instance (TextualMonoid s, Abstract.DeeplyFoldable (Accounting pos s) l,
          Abstract.DeeplyFoldable (LabelAccounting pos s) l,
          Abstract.Haskell l, Abstract.Module l l ~ AST.Module l l) =>
         Verification pos s
         `Transformation.At` AST.Module l l (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s) where
   Verification $ (_, m) = Const $ flip verifyModule m

instance (TextualMonoid s, Abstract.DeeplyFoldable (LabelAccounting pos s) l) =>
         Accounting pos s
         `Transformation.At` AST.Module l l (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s) where
   Accounting $ (_, m) = Const (Deep.foldMap LabelAccounting m)

instance (Eq s, IsString s) =>
         Accounting pos s
         `Transformation.At` ExtAST.Import l l (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s) where
   Accounting $ ((start, Trailing lexemes, end), ExtAST.Import safe _qualified package name alias spec) = Const $
      (if safe then Map.singleton Extensions.SafeImports [(start, end)] else mempty)
      <>
      (if isJust package then Map.singleton Extensions.PackageImports [(start, end)] else mempty)
      <>
      (if null qualifiedAndAfter || any (not . isAnyKeyword) beforeQualified then mempty
       else Map.singleton Extensions.ImportQualifiedPost [(start, end)])
      where (beforeQualified, qualifiedAndAfter) = break (isKeyword "qualified") (filter isAnyToken lexemes)

instance (Eq s, IsString s) =>
         Accounting pos s
         `Transformation.At` AST.ImportItem l l (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s) where
   Accounting $ ((start, Trailing lexemes, end), AST.ImportClassOrType{}) = Const $
      if any (isKeyword "type") lexemes then Map.singleton Extensions.ExplicitNamespaces [(start, end)] else mempty

instance (Abstract.Context l ~ AST.Context l, Eq s, IsString s,
          Abstract.DeeplyFoldable (UnicodeSyntaxAccounting pos s) l) =>
         Accounting pos s
         `Transformation.At` AST.Declaration l l (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s) where
   Accounting $ d@((start, _, end), AST.DataDeclaration context _lhs constructors _derivings) = Const $
      (if null constructors then Map.singleton Extensions.EmptyDataDeclarations [(start, end)] else mempty)
      <>
      (case snd context
       of AST.NoContext -> mempty
          _ -> Map.singleton Extensions.DatatypeContexts [(start, end)])
      <>
      (Full.foldMap UnicodeSyntaxAccounting d)
   Accounting $ d = Const (Full.foldMap UnicodeSyntaxAccounting d)

instance Accounting pos s
         `Transformation.At` ExtAST.Expression l l (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s) where
   Accounting $ ((start, _, end), e) = Const . ($ [(start, end)]) $
      (case e
       of ExtAST.CaseExpression _ [] -> Map.singleton Extensions.EmptyCase
          ExtAST.LambdaCaseExpression{} -> Map.singleton Extensions.LambdaCase
          ExtAST.MDoExpression{} -> Map.singleton Extensions.RecursiveDo
          ExtAST.ParallelListComprehension{} -> Map.singleton Extensions.ParallelListComprehensions
          ExtAST.TupleSectionExpression{} -> Map.singleton Extensions.TupleSections
          _ -> mempty)

instance Accounting pos s
         `Transformation.At` ExtAST.Statement l l (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s) where
   Accounting $ ((start, _, end), e) = Const $
      (case e
       of ExtAST.RecursiveStatement{} -> Map.singleton Extensions.RecursiveDo [(start, end)]
          _ -> mempty)

instance (Eq s, IsString s, LeftReductive s, Factorial s) =>
         Accounting pos s
         `Transformation.At` ExtAST.Value l l (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s) where
   Accounting $ ((start, Trailing lexemes, end), literal) = Const $
      (if any ((isPrefixOf "0b" ||| isPrefixOf "0B") . lexemeText) lexemes
      then Map.singleton Extensions.BinaryLiterals [(start, end)]
       else mempty)
      <>
      (case hashless literal
       of ExtAST.FloatingLiteral{} | any ((isPrefixOf "0x" ||| isPrefixOf "0X") . lexemeText) lexemes
             -> Map.singleton Extensions.HexFloatLiterals [(start, end)]
          _ -> mempty)
      <>
      ((case hashless literal
        of ExtAST.FloatingLiteral{} -> id
           ExtAST.IntegerLiteral{} -> id
           _ -> const mempty)
       $
       (if any (getAny . Factorial.foldMap (Any . ("_" ==)) . lexemeText) lexemes
        then Map.singleton Extensions.NumericUnderscores [(start, end)]
        else mempty)
       <>
       (if any (("-" `isPrefixOf`) . lexemeText) lexemes
        then Map.singleton Extensions.NegativeLiterals [(start, end)]
        else mempty)
      )
      <>
      (case literal
       of ExtAST.HashLiteral{} -> Map.singleton Extensions.MagicHash [(start, end)]
          _ -> mempty)
      where hashless (ExtAST.HashLiteral l) = hashless l
            hashless l = l

(|||) :: Applicative f => f Bool -> f Bool -> f Bool
(|||) = liftA2 (||)

instance TextualMonoid s =>
         LabelAccounting pos s `Transformation.At` g (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s) where
   LabelAccounting $ ((start, Trailing lexemes, end), _)
      | any isLabel lexemes = Const (Map.singleton Extensions.OverloadedLabels [(start, end)])
      | otherwise = mempty
      where isLabel Token{lexemeType= Other, lexemeText= t}
               | Just t' <- stripPrefix "#" t, Just c <- characterPrefix t' = Char.isLower c || c == '_'
            isLabel _ = False

instance (Eq s, IsString s) =>
         UnicodeSyntaxAccounting pos s
         `Transformation.At` g (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s) where
   UnicodeSyntaxAccounting $ ((start, Trailing lexemes, end), _)
      | any (`elem` unicodeDelimiters) lexemes = Const (Map.singleton Extensions.UnicodeSyntax [(start, end)])
      | otherwise = mempty
      where unicodeDelimiters :: [Lexeme s]
            unicodeDelimiters = Token Delimiter <$> ["∷", "⇒", "→", "←"]

instance (Deep.Foldable (Accounting pos s) g,
          Transformation.At (Accounting pos s) (g (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s))) =>
         Full.Foldable (Accounting pos s) g where
   foldMap = Full.foldMapDownDefault

instance (Deep.Foldable (LabelAccounting pos s) g,
          Transformation.At (LabelAccounting pos s) (g (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s))) =>
         Full.Foldable (LabelAccounting pos s) g where
   foldMap = Full.foldMapDownDefault

instance (Deep.Foldable (UnicodeSyntaxAccounting pos s) g,
          Transformation.At (UnicodeSyntaxAccounting pos s)
                            (g (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s))) =>
         Full.Foldable (UnicodeSyntaxAccounting pos s) g where
   foldMap = Full.foldMapDownDefault

isAnyToken :: Lexeme s -> Bool
isAnyToken Token{} = True
isAnyToken _ = False

isAnyKeyword :: Lexeme s -> Bool
isAnyKeyword Token{lexemeType= Keyword} = True
isAnyKeyword _ = False

isKeyword :: (Eq s, IsString s) => s -> Lexeme s -> Bool
isKeyword s Token{lexemeType= Keyword, lexemeText= t} = s == t
isKeyword _ _ = False
