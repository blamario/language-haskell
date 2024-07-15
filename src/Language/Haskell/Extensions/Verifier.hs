{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings,
             ScopedTypeVariables, StandaloneDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Language.Haskell.Extensions.Verifier (Accounting(Accounting), Verification(Verification), verifyModule) where

import Control.Applicative (liftA2)
import qualified Data.Char as Char
import Data.Foldable (toList)
import Data.Functor.Const (Const(Const, getConst))
import Data.Functor.Compose (Compose(..))
import Data.Maybe (isJust)
import Data.Monoid.Textual (TextualMonoid, characterPrefix)
import qualified Data.Monoid.Textual as Textual
import Data.Semigroup (Any(Any, getAny))
import Data.Semigroup.Cancellative (LeftReductive(isPrefixOf, stripPrefix))
import Data.Semigroup.Factorial (Factorial)
import Data.Semigroup.Union (UnionWith(UnionWith, getUnionWith))
import qualified Data.Semigroup.Factorial as Factorial
import Data.Map.Lazy (Map)
import Data.Set (Set)
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Data.String (IsString)

import qualified Rank2
import qualified Transformation
import qualified Transformation.Deep as Deep
import qualified Transformation.Full as Full
import qualified Transformation.AG.Dimorphic as AG.Di
import Text.Grampa (Ambiguous(..))

import qualified Language.Haskell.AST as AST
import qualified Language.Haskell.Binder as Binder
import Language.Haskell.Grammar (isSymbol)
import Language.Haskell.Extensions (Extension, ExtensionSwitch, partitionContradictory, withImplications)
import qualified Language.Haskell.Extensions as Extensions
import qualified Language.Haskell.Extensions.Abstract as Abstract
import qualified Language.Haskell.Extensions.AST as ExtAST
import qualified Language.Haskell.Reserializer as Reserializer
import Language.Haskell.Reserializer (Lexeme(..), ParsedLexemes(..), TokenType(..))

data Accounting l pos s = Accounting
data UnicodeSyntaxAccounting l pos s = UnicodeSyntaxAccounting
data Verification l pos s = Verification

type Accounted pos = Const (UnionWith (Map Extension) [(pos, pos)])
type Verified pos = Const (Map Extension Bool -> [Error pos])

data Error pos = ContradictoryExtensionSwitches (Set ExtensionSwitch)
               | UndeclaredExtensionUse Extension [(pos, pos)]
               | UnusedExtension Extension
                 deriving (Show)

type Wrap l pos s = Binder.WithEnvironment l (Reserializer.Wrapped pos s)

instance Transformation.Transformation (Accounting l pos s) where
    type Domain (Accounting l pos s) = Wrap l pos s
    type Codomain (Accounting l pos s) = Accounted pos

instance Transformation.Transformation (UnicodeSyntaxAccounting l pos s) where
    type Domain (UnicodeSyntaxAccounting l pos s) = Wrap l pos s
    type Codomain (UnicodeSyntaxAccounting l pos s) = Accounted pos

instance Transformation.Transformation (Verification l pos s) where
    type Domain (Verification l pos s) = Wrap l pos s
    type Codomain (Verification l pos s) = Verified pos

verifyModule :: forall l pos s. (TextualMonoid s, Num pos, Ord pos, Show pos, Abstract.DeeplyFoldable (Accounting l pos s) l,
                                 Abstract.Haskell l, Abstract.Module l l ~ AST.Module l l,
                                 Ord (Abstract.ModuleName l), Ord (Abstract.Name l)) =>
                Map Extension Bool
             -> Wrap l pos s (AST.Module l l (Wrap l pos s) (Wrap l pos s))
             -> [Error pos]
verifyModule extensions (Compose (_, (_, (AST.ExtendedModule localExtensionSwitches m)))) =
   (if null contradictions then mempty else [ContradictoryExtensionSwitches contradictions])
   <> (UnusedExtension
       <$> toList (Map.keysSet (Map.filter id localExtensions)
                   Set.\\ usedExtensionsWithPremises Set.\\ Extensions.languageVersions))
   <> (uncurry UndeclaredExtensionUse <$> Map.toList (usedExtensions Map.\\ declaredExtensions))
   where usedExtensions :: Map Extension [(pos, pos)]
         usedExtensions = filterExtensions $ getUnionWith $ Full.foldMap (Accounting :: Accounting l pos s) m
         declaredExtensions = Map.filter id (withImplications (localExtensions <> extensions)
                                             <> Map.fromSet (const True) Extensions.includedByDefault)
         (contradictions, localExtensions) = partitionContradictory (Set.fromList localExtensionSwitches)
         usedExtensionsWithPremises = Map.foldMapWithKey extensionAndPremises usedExtensions
         extensionAndPremises x _ = Set.singleton x <> Map.findWithDefault mempty x Extensions.inverseImplications
verifyModule extensions m =
   uncurry UndeclaredExtensionUse
   <$> Map.toList (filterExtensions $
                   getUnionWith (Full.foldMap (Accounting :: Accounting l pos s) m) Map.\\ declaredExtensions)
   where declaredExtensions = Map.filter id (withImplications extensions
                                             <> Map.fromSet (const True) Extensions.includedByDefault)

filterExtensions :: (Num pos, Ord pos, Show pos) => Map Extension [(pos, pos)] -> Map Extension [(pos, pos)]
filterExtensions = Map.mapMaybeWithKey filterPatternGuards
   where filterPatternGuards Extensions.PatternGuards ranges =
            case filterActive ranges
            of [] -> Nothing
               rest -> Just rest
         filterPatternGuards _ ranges = Just ranges
         filterActive (outer@(start1, end1) : inner@(start2, end2) : rest)
            | start1 < 0, end1 > 0, start2 >= -start1, end2 > 0, end2 <= end1  = inner : filterActive (outer : rest)
            | start1 > 0, end1 < 0, start2 >= start1, end2 <= -end1 = filterActive (outer : rest)
         filterActive ((start, end) : rest) = filterActive rest
         filterActive [] = []

instance {-# overlappable #-} Accounting l pos s
         `Transformation.At` g (Wrap l pos s) (Wrap l pos s) where
   Accounting $ _ = mempty

instance {-# overlappable #-} Deep.Foldable (Accounting l pos s) g =>
         Verification l pos s
         `Transformation.At` g (Wrap l pos s) (Wrap l pos s) where
   Verification $ Compose (_, (_, m)) = Const $ \extensions->
      uncurry UndeclaredExtensionUse
      <$> Map.toList (getUnionWith (Deep.foldMap (Accounting :: Accounting l pos s) m)
                      Map.\\ withImplications extensions)

instance (TextualMonoid s, Num pos, Ord pos, Show pos, Abstract.DeeplyFoldable (Accounting l pos s) l,
          Abstract.Haskell l, Abstract.Module l l ~ AST.Module l l,
          Ord (Abstract.ModuleName l), Ord (Abstract.Name l)) =>
         Verification l pos s
         `Transformation.At` AST.Module l l (Wrap l pos s) (Wrap l pos s) where
   Verification $ m = Const $ flip verifyModule m

instance (TextualMonoid s, Abstract.Module l l ~ AST.Module l l, Ord (Abstract.ModuleName l), Ord (Abstract.Name l)) =>
         Accounting l pos s
         `Transformation.At` AST.Module l l (Wrap l pos s) (Wrap l pos s) where
   Accounting $ d@(Compose (_, (_, AST.AnonymousModule _ declarations)))
      | Just clashingFieldLocations <- checkDuplicateRecordFields declarations
      = Const $ UnionWith $ Map.singleton Extensions.DuplicateRecordFields clashingFieldLocations
   Accounting $ d@(Compose (_, (_, AST.NamedModule _ _ _ declarations)))
      | Just clashingFieldLocations <- checkDuplicateRecordFields declarations
      = Const $ UnionWith $ Map.singleton Extensions.DuplicateRecordFields clashingFieldLocations
   Accounting $ Compose (_, (_, m)) = mempty

instance (Eq s, IsString s, Show s) =>
         Accounting l pos s
         `Transformation.At` ExtAST.Import l l (Wrap l pos s) (Wrap l pos s) where
   Accounting $ Compose (_, ((start, Trailing lexemes, end), ExtAST.Import safe _qualified package name alias spec)) =
      Const $ UnionWith $
         (if safe then Map.singleton Extensions.SafeImports [(start, end)] else mempty)
         <>
         (if isJust package then Map.singleton Extensions.PackageImports [(start, end)] else mempty)
         <>
         (if null qualifiedAndAfter || all isAnyKeyword beforeQualified then mempty
          else Map.singleton Extensions.ImportQualifiedPost [(start, end)])
      where x@(beforeQualified, qualifiedAndAfter) = break (isKeyword "qualified") (filter isAnyToken lexemes)

instance (Eq s, IsString s) =>
         Accounting l pos s
         `Transformation.At` AST.ImportItem l l (Wrap l pos s) (Wrap l pos s) where
   Accounting $ Compose (_, ((start, Trailing lexemes, end), AST.ImportClassOrType{})) = Const $ UnionWith $
      if any (isKeyword "type") lexemes then Map.singleton Extensions.ExplicitNamespaces [(start, end)] else mempty
   Accounting $ _ = mempty

instance (Eq s, IsString s) =>
         Accounting l pos s
         `Transformation.At` ExtAST.ImportItem l l (Wrap l pos s) (Wrap l pos s) where
   Accounting $ Compose (_, ((start, Trailing lexemes, end), ExtAST.ImportClassOrType{})) = Const $ UnionWith $
      if any (isKeyword "type") lexemes then Map.singleton Extensions.ExplicitNamespaces [(start, end)] else mempty
   Accounting $ _ = mempty

instance (Abstract.Context l ~ ExtAST.Context l, Eq s, IsString s,
          Abstract.DeeplyFoldable (UnicodeSyntaxAccounting l pos s) l) =>
         Accounting l pos s
         `Transformation.At` ExtAST.Declaration l l (Wrap l pos s) (Wrap l pos s) where
   Accounting $ d@(Compose (_, ((start, _, end), dec))) = Const $
      case dec
      of ExtAST.DataDeclaration context _lhs _kind constructors _derivings ->
            (if null constructors
             then UnionWith $ Map.singleton Extensions.EmptyDataDeclarations [(start, end)]
             else mempty)
            <>
            (case snd . snd . getCompose $ context
             of ExtAST.NoContext -> mempty
                _ -> UnionWith $ Map.singleton Extensions.DatatypeContexts [(start, end)])
            <>
            (Full.foldMap UnicodeSyntaxAccounting d)
         ExtAST.GADTDeclaration context _lhs constructors _derivings ->
            UnionWith $ Map.singleton Extensions.GADTSyntax [(start, end)]
         ExtAST.ImplicitParameterDeclaration{} ->
            UnionWith $ Map.singleton Extensions.ImplicitParameters [(start, end)]
         ExtAST.StandaloneDerivingDeclaration{} ->
            UnionWith $ Map.singleton Extensions.StandaloneDeriving [(start, end)]
         ExtAST.ForeignExport{} ->
            UnionWith $ Map.singleton Extensions.ForeignFunctionInterface [(start, end)]
         ExtAST.ForeignImport convention safety id name ty ->
            UnionWith (Map.singleton Extensions.ForeignFunctionInterface [(start, end)])
            <> getConst (foldMap ((Accounting Transformation.$) . (<$ d)) safety)
         _ -> mempty
      <> Full.foldMap UnicodeSyntaxAccounting d

instance Accounting l pos s
         `Transformation.At` ExtAST.DataConstructor l l (Wrap l pos s) (Wrap l pos s) where
   Accounting $ Compose (_, ((start, _, end), ExtAST.ExistentialConstructor{})) =
      Const (UnionWith $ Map.singleton Extensions.ExistentialQuantification [(start, end)])
   Accounting $ Compose (_, ((start, _, end), ExtAST.RecordConstructor{})) =
      Const (UnionWith $ Map.singleton Extensions.TraditionalRecordSyntax [(start, end)])
   Accounting $ _ = mempty

instance (Abstract.Context l ~ ExtAST.Context l, Abstract.Type l ~ ExtAST.Type l, Eq s, IsString s) =>
         Accounting l pos s `Transformation.At` ExtAST.Context l l (Wrap l pos s) (Wrap l pos s) where
   Accounting $ d@(Compose (_, ((start, _, end), con))) =
      case con
      of ExtAST.NoContext -> mempty
         ExtAST.Constraints{} -> mempty
         ExtAST.ClassConstraint className t -> Const (UnionWith $ foldMap checkFlexibleContextHead t)
         ExtAST.TypeConstraint t ->
            Const $ UnionWith (foldMap checkFlexibleContext t) <> UnionWith (foldMap checkMPTC t)
         ExtAST.TypeEquality{} -> Const $ UnionWith $ Map.singleton Extensions.EqualityConstraints [(start, end)]
         ExtAST.ImplicitParameterConstraint{} ->
            Const $ UnionWith $ Map.singleton Extensions.ImplicitParameters [(start, end)]
      where checkFlexibleContextHead ExtAST.TypeVariable{} = mempty
            checkFlexibleContextHead (ExtAST.TypeApplication left right) = foldMap checkFlexibleContextHead left
            checkFlexibleContextHead _ = Map.singleton Extensions.FlexibleContexts [(start, end)]
            checkFlexibleContext (ExtAST.TypeApplication left right)
               | any isConstructor left = foldMap checkFlexibleContextHead right
               | otherwise = Map.singleton Extensions.TypeVariableConstraints [(start, end)]
                             <> foldMap checkFlexibleContext left
            checkFlexibleContext _ = mempty
            checkMPTC (ExtAST.TypeApplication left _) = foldMap checkMPTC1 left
            checkMPTC c@ExtAST.InfixTypeApplication{} = checkMPTC1 c
            checkMPTC _ = mempty
            checkMPTC1 ExtAST.TypeApplication{} = Map.singleton Extensions.MultiParameterConstraints [(start, end)]
            checkMPTC1 ExtAST.InfixTypeApplication{} = Map.singleton Extensions.MultiParameterConstraints [(start, end)]
            checkMPTC1 _ = mempty
            isConstructor ExtAST.ConstructorType{} = True
            isConstructor _ = False

instance (Abstract.Expression l ~ ExtAST.Expression l, Abstract.QualifiedName l ~ AST.QualifiedName l,
          Ord (Abstract.QualifiedName l), Eq s, IsString s, Num pos) =>
         Accounting l pos s
         `Transformation.At` ExtAST.Expression l l (Wrap l pos s) (Wrap l pos s) where
   Accounting $ Compose (bindings, ((start, _, end), e)) = Const . UnionWith . ($ [(start, end)]) $
      (case e
       of ExtAST.ApplyExpression _ (Compose (_, ((_, Trailing (lexeme1 : _), _), r)))
             | isBlock r && not (isToken "(" lexeme1) -> Map.singleton Extensions.BlockArguments
          ExtAST.CaseExpression _ [] -> Map.singleton Extensions.EmptyCase
          ExtAST.LambdaCaseExpression{} -> Map.singleton Extensions.LambdaCase
          ExtAST.MultiWayIfExpression{} -> Map.singleton Extensions.MultiWayIf
          -- negative end of the range means not really PatternGuards follow
          ExtAST.DoExpression{} -> Map.singleton Extensions.PatternGuards . ((negate <$>) <$>)
          ExtAST.MDoExpression{} ->
             \ranges-> Map.fromList [(Extensions.RecursiveDo, ranges),
                                     (Extensions.PatternGuards, (negate <$>) <$> ranges)]
          ExtAST.ImplicitParameterExpression{} -> Map.singleton Extensions.ImplicitParameters
          ExtAST.ListComprehension{} -> Map.singleton Extensions.PatternGuards . ((negate <$>) <$>)
          ExtAST.ParallelListComprehension{} ->
             \ranges-> Map.fromList [(Extensions.ParallelListComprehensions, ranges),
                                     (Extensions.PatternGuards, (negate <$>) <$> ranges)]
          ExtAST.TupleSectionExpression{} -> Map.singleton Extensions.TupleSections
          ExtAST.OverloadedLabel{} -> Map.singleton Extensions.OverloadedLabels
          ExtAST.ReferenceExpression q
             | Just (Binder.ValueBinding Binder.RecordField) <- Map.lookup q (getUnionWith $ AG.Di.inh bindings)
               -> Map.singleton Extensions.FieldSelectors
          ExtAST.UnboxedSumExpression{} -> Map.singleton Extensions.UnboxedSums
          ExtAST.UnboxedTupleExpression{} -> Map.singleton Extensions.UnboxedTuples
          ExtAST.UnboxedTupleSectionExpression{} ->
             \ranges-> Map.fromList [(Extensions.TupleSections, ranges), (Extensions.UnboxedTuples, ranges)]
          ExtAST.WildcardRecordExpression{} -> Map.singleton Extensions.RecordWildCards
          _ -> mempty)
      where isBlock ExtAST.CaseExpression{} = True
            isBlock ExtAST.ConditionalExpression{} = True
            isBlock ExtAST.DoExpression{} = True
            isBlock ExtAST.LambdaExpression{} = True
            isBlock ExtAST.LetExpression{} = True
            isBlock _ = False

instance (Abstract.Expression l ~ ExtAST.Expression l, Abstract.QualifiedName l ~ AST.QualifiedName l,
          Ord (Abstract.QualifiedName l), Eq s, IsString s, Num pos) =>
         Accounting l pos s
         `Transformation.At` ExtAST.EquationRHS l l (Wrap l pos s) (Wrap l pos s) where
   -- negative start of the range means real PatternGuards follow
   Accounting $ Compose (_, ((start, _, end), e)) =
      Const $ UnionWith $ Map.singleton Extensions.PatternGuards [(-start, end)]

instance Accounting l pos s
         `Transformation.At` ExtAST.Statement l l (Wrap l pos s) (Wrap l pos s) where
   Accounting $ Compose (_, ((start, _, end), e)) = Const $ UnionWith $
      (case e
       of ExtAST.BindStatement{} -> Map.singleton Extensions.PatternGuards [(start, end)]
          ExtAST.RecursiveStatement{} -> Map.singleton Extensions.RecursiveDo [(start, end)]
          _ -> mempty)

instance (Eq s, IsString s, LeftReductive s, Factorial s) =>
         Accounting l pos s
         `Transformation.At` ExtAST.Value l l (Wrap l pos s) (Wrap l pos s) where
   Accounting $ Compose (_, ((start, Trailing lexemes, end), literal)) = Const $ UnionWith $
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
      where hashless (ExtAST.HashLiteral _ l) = hashless l
            hashless l = l

instance (Eq s, IsString s, LeftReductive s, Factorial s) =>
         Accounting l pos s
         `Transformation.At` ExtAST.ClassInstanceLHS l l (Wrap l pos s) (Wrap l pos s) where
   Accounting $ Compose (_, ((start, Trailing lexemes, end), t)) = Const $ UnionWith $
      case t
      of ExtAST.InfixTypeClassInstanceLHS{} -> typeOperators
         ExtAST.TypeClassInstanceLHS{} | any isAnyDelimiter lexemes -> typeOperators
         ExtAST.ClassReferenceInstanceLHS{} | any isAnyDelimiter lexemes -> typeOperators
         _ -> mempty
         where typeOperators = Map.singleton Extensions.TypeOperators [(start, end)]

instance (Eq s, IsString s, LeftReductive s, TextualMonoid s) =>
         Accounting l pos s
         `Transformation.At` ExtAST.TypeLHS l l (Wrap l pos s) (Wrap l pos s) where
   Accounting $ Compose (_, ((start, Trailing lexemes, end), t)) = Const $ UnionWith $
      case t
      of ExtAST.SimpleTypeLHS op vars
            -> (if any (Textual.any isSymbol . lexemeText) lexemes
                then Map.singleton Extensions.TypeOperators [(start, end)]
                else mempty)
               <> foldMap (checkKindedTypevar (start, end)) vars
         ExtAST.SimpleTypeLHSApplication _ var -> checkKindedTypevar (start, end) var

instance (Eq s, IsString s, LeftReductive s, Factorial s) =>
         Accounting l pos s
         `Transformation.At` ExtAST.FieldBinding l l (Wrap l pos s) (Wrap l pos s) where
   Accounting $ Compose (_, ((start, _, end), t)) = Const $ UnionWith $
      Map.singleton Extensions.TraditionalRecordSyntax [(start, end)]
      <> case t of ExtAST.PunnedFieldBinding {} -> Map.singleton Extensions.NamedFieldPuns [(start, end)]
                   _ -> mempty

instance (Eq s, IsString s, LeftReductive s, Factorial s) =>
         Accounting l pos s
         `Transformation.At` ExtAST.FieldPattern l l (Wrap l pos s) (Wrap l pos s) where
   Accounting $ Compose (_, ((start, _, end), t)) = Const $ UnionWith $
      Map.singleton Extensions.TraditionalRecordSyntax [(start, end)]
      <> case t of ExtAST.PunnedFieldPattern {} -> Map.singleton Extensions.NamedFieldPuns [(start, end)]
                   _ -> mempty

instance (Eq s, IsString s, LeftReductive s, Factorial s) =>
         Accounting l pos s
         `Transformation.At` ExtAST.Pattern l l (Wrap l pos s) (Wrap l pos s) where
   Accounting $ Compose (_, ((start, _, end), t)) = Const $ UnionWith $
      case t of ExtAST.BangPattern {} -> Map.singleton Extensions.BangPatterns [(start, end)]
                ExtAST.NPlusKPattern {} -> Map.singleton Extensions.NPlusKPatterns [(start, end)]
                ExtAST.ViewPattern {} -> Map.singleton Extensions.ViewPatterns [(start, end)]
                ExtAST.UnboxedSumPattern{} -> Map.singleton Extensions.UnboxedSums [(start, end)]
                ExtAST.UnboxedTuplePattern{} -> Map.singleton Extensions.UnboxedTuples [(start, end)]
                ExtAST.WildcardRecordPattern {} -> Map.singleton Extensions.RecordWildCards [(start, end)]
                _ -> mempty

instance (Eq s, IsString s, LeftReductive s, Factorial s) =>
         Accounting l pos s
         `Transformation.At` ExtAST.Type l l (Wrap l pos s) (Wrap l pos s) where
   Accounting $ Compose (_, ((start, _, end), t)) = Const $ UnionWith $
      case t
      of ExtAST.InfixTypeApplication{} -> Map.singleton Extensions.TypeOperators [(start, end)]
         ExtAST.PromotedInfixTypeApplication{} -> Map.fromList [(Extensions.DataKinds, [(start, end)]),
                                                                (Extensions.TypeOperators, [(start, end)])]
         ExtAST.PromotedConstructorType{} -> Map.singleton Extensions.DataKinds [(start, end)]
         ExtAST.PromotedTupleType{} -> Map.singleton Extensions.DataKinds [(start, end)]
         ExtAST.PromotedListType{} -> Map.singleton Extensions.DataKinds [(start, end)]
         ExtAST.PromotedIntegerLiteral{} -> Map.singleton Extensions.DataKinds [(start, end)]
         ExtAST.PromotedCharLiteral{} -> Map.singleton Extensions.DataKinds [(start, end)]
         ExtAST.PromotedStringLiteral{} -> Map.singleton Extensions.DataKinds [(start, end)]
         ExtAST.RecordFunctionType{} -> Map.singleton Extensions.TraditionalRecordSyntax [(start, end)]
         ExtAST.ForallType vars _ ->
            Map.singleton Extensions.ExplicitForAll [(start, end)] <> foldMap (checkKindedTypevar (start, end)) vars
         ExtAST.KindedType{} -> Map.singleton Extensions.KindSignatures [(start, end)]
         ExtAST.GroundTypeKind{} -> Map.singleton Extensions.StarIsType [(start, end)]
         ExtAST.TypeWildcard{} -> Map.singleton Extensions.PartialTypeSignatures [(start, end)]
         ExtAST.UnboxedSumType{} -> Map.singleton Extensions.UnboxedSums [(start, end)]
         ExtAST.UnboxedTupleType{} -> Map.singleton Extensions.UnboxedTuples [(start, end)]
         ExtAST.VisibleDependentType{} -> Map.fromList [(Extensions.ExplicitForAll, [(start, end)]),
                                                        (Extensions.PolyKinds, [(start, end)])]
         _ -> mempty

instance (Eq s, IsString s, LeftReductive s, Factorial s) =>
         Accounting l pos s
         `Transformation.At` ExtAST.Constructor l l (Wrap l pos s) (Wrap l pos s) where
   Accounting $ Compose (_, ((start, _, end), t)) = Const $ UnionWith $
      case t
      of ExtAST.UnboxedTupleConstructor{} -> Map.singleton Extensions.UnboxedTuples [(start, end)]
         ExtAST.UnboxedSumConstructor{} -> Map.singleton Extensions.UnboxedSums [(start, end)]
         _ -> mempty

instance Accounting l pos s `Transformation.At` ExtAST.CallSafety l where
   Accounting $ Compose (_, ((start, _, end), e)) = Const $ UnionWith $
      (case e
       of ExtAST.InterruptibleCall{} -> Map.singleton Extensions.InterruptibleFFI [(start, end)]
          _ -> mempty)

checkKindedTypevar :: (pos, pos) -> ExtAST.TypeVarBinding λ l d s -> Map Extension [(pos, pos)]
checkKindedTypevar span ExtAST.ExplicitlyKindedTypeVariable{} = Map.singleton Extensions.KindSignatures [span]
checkKindedTypevar _ ExtAST.ImplicitlyKindedTypeVariable{} = mempty

(|||) :: Applicative f => f Bool -> f Bool -> f Bool
(|||) = liftA2 (||)

instance (Eq s, IsString s) =>
         UnicodeSyntaxAccounting l pos s
         `Transformation.At` g (Wrap l pos s) (Wrap l pos s) where
   UnicodeSyntaxAccounting $ Compose (_, ((start, Trailing lexemes, end), _))
      | any (`elem` unicodeDelimiters) lexemes =
        Const (UnionWith $ Map.singleton Extensions.UnicodeSyntax [(start, end)])
      | otherwise = mempty
      where unicodeDelimiters :: [Lexeme s]
            unicodeDelimiters = Token Delimiter <$> ["∷", "⇒", "→", "←"]

instance (Rank2.Foldable (g (Wrap l pos s)), Deep.Foldable (Accounting l pos s) g,
          Transformation.At (Accounting l pos s) (g (Wrap l pos s) (Wrap l pos s))) =>
         Full.Foldable (Accounting l pos s) g where
   foldMap = Full.foldMapDownDefault

instance (Rank2.Foldable (g (Wrap l pos s)), Deep.Foldable (UnicodeSyntaxAccounting l pos s) g,
          Transformation.At (UnicodeSyntaxAccounting l pos s)
                            (g (Wrap l pos s) (Wrap l pos s))) =>
         Full.Foldable (UnicodeSyntaxAccounting l pos s) g where
   foldMap = Full.foldMapDownDefault

checkDuplicateRecordFields :: forall l pos s node. (Ord (Abstract.ModuleName l), Ord (Abstract.Name l))
                           => [Wrap l pos s node] -> Maybe [(pos, pos)]
checkDuplicateRecordFields declarations
  | Map.null duplicateBindings = Nothing
  | otherwise = Just (map location $ filter isDuplicate declarations)
  where duplicateBindings = Map.filter duplicateRecordField allDeclarationBindings
        UnionWith allDeclarationBindings = foldMap declarationBindings declarations
        declarationBindings (Compose (bindings, _)) = AG.Di.syn bindings
        duplicateRecordField b = b == Binder.ErroneousBinding Binder.DuplicateRecordField
        isDuplicate :: Wrap l pos s node -> Bool
        isDuplicate = not . Map.disjoint duplicateBindings . getUnionWith . declarationBindings
        location (Compose (_, ((start, _, end), _))) = (start, end)

isAnyToken :: Lexeme s -> Bool
isAnyToken Token{} = True
isAnyToken _ = False

isAnyDelimiter :: Lexeme s -> Bool
isAnyDelimiter Token{lexemeType= Delimiter} = True
isAnyDelimiter _ = False

isAnyKeyword :: Lexeme s -> Bool
isAnyKeyword Token{lexemeType= Keyword} = True
isAnyKeyword _ = False

isKeyword :: (Eq s, IsString s) => s -> Lexeme s -> Bool
isKeyword s Token{lexemeType= Keyword, lexemeText= t} = s == t
isKeyword _ _ = False

isToken :: (Eq s, IsString s) => s -> Lexeme s -> Bool
isToken s Token{lexemeText= t} = s == t
isToken _ _ = False
