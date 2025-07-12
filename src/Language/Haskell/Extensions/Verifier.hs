{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings,
             ScopedTypeVariables, StandaloneDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}

-- | Tracking which extensions are used by the AST
module Language.Haskell.Extensions.Verifier (Accounting(Accounting), Verification(Verification),
                                             Error(..), verify) where

import Control.Applicative (ZipList(ZipList), liftA2)
import Control.Monad.Trans.Accum (Accum, accum, evalAccum)
import qualified Data.Char as Char
import Data.Foldable (toList)
import Data.Functor.Const (Const(Const, getConst))
import Data.Functor.Compose (Compose(..))
import Data.Maybe (isJust)
import Data.Monoid (Any(Any, getAny), Sum(Sum), Ap(Ap, getAp))
import Data.Monoid.Textual (TextualMonoid, characterPrefix)
import qualified Data.Monoid.Textual as Textual
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
import qualified Transformation.Shallow as Shallow
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

-- | Transformation for tracking the uses of language extensions.
data Accounting l pos s = Accounting

data MPTCAccounting l pos s = MPTCAccounting
data FlexibleInstanceHeadAccounting l pos s = FlexibleInstanceHeadAccounting
data FlexibleInstanceTypeAccounting l pos s = FlexibleInstanceTypeAccounting
data FlexibleInstanceTypeArgAccounting l pos s = FlexibleInstanceTypeArgAccounting
data UnicodeSyntaxAccounting l pos s = UnicodeSyntaxAccounting

-- | Transformation for reporting errors about language extension use
data Verification l pos s = Verification

type Accounted pos = Const (UnionWith (Map Extension) [(pos, pos)])
type Verified pos = Const (Ap (Accum (Map Extension Bool)) [Error pos])

-- | Extension misuse or disuse errors
data Error pos = ContradictoryExtensionSwitches (Set ExtensionSwitch)
               | UndeclaredExtensionUse Extension [(pos, pos)]
               | UnusedExtension Extension
               | RecordTypeDataFields [(pos, pos)]
               | StrictTypeDataFields [(pos, pos)]
                 deriving (Show)

type Wrap l pos s = Binder.WithEnvironment l (Reserializer.Wrapped pos s)

instance Transformation.Transformation (Accounting l pos s) where
    type Domain (Accounting l pos s) = Wrap l pos s
    type Codomain (Accounting l pos s) = Accounted pos

instance Transformation.Transformation (MPTCAccounting l pos s) where
    type Domain (MPTCAccounting l pos s) = Wrap l pos s
    type Codomain (MPTCAccounting l pos s) = Const (Sum Int)

instance Transformation.Transformation (FlexibleInstanceHeadAccounting l pos s) where
    type Domain (FlexibleInstanceHeadAccounting l pos s) = Wrap l pos s
    type Codomain (FlexibleInstanceHeadAccounting l pos s) = Accounted pos

instance Transformation.Transformation (FlexibleInstanceTypeAccounting l pos s) where
    type Domain (FlexibleInstanceTypeAccounting l pos s) = Wrap l pos s
    type Codomain (FlexibleInstanceTypeAccounting l pos s) = Accounted pos

instance Transformation.Transformation (FlexibleInstanceTypeArgAccounting l pos s) where
    type Domain (FlexibleInstanceTypeArgAccounting l pos s) = Wrap l pos s
    type Codomain (FlexibleInstanceTypeArgAccounting l pos s) = Accounted pos

instance Transformation.Transformation (UnicodeSyntaxAccounting l pos s) where
    type Domain (UnicodeSyntaxAccounting l pos s) = Wrap l pos s
    type Codomain (UnicodeSyntaxAccounting l pos s) = Accounted pos

instance Transformation.Transformation (Verification l pos s) where
    type Domain (Verification l pos s) = Wrap l pos s
    type Codomain (Verification l pos s) = Verified pos

-- | Given the list of declared extensions, report errors on the use of undeclared extensions and non-use
-- of declared ones.
verify :: forall w l pos s g. (w ~ Wrap l pos s, Full.Foldable (Verification l pos s) g) =>
                Map Extension Bool
             -> w (g w w)
             -> [Error pos]
verify extensions node = evalAccum (getAp $ Full.foldMap (Verification :: Verification l pos s) node) extensions

verifyModuleExtensions :: forall l pos s. (TextualMonoid s, Num pos, Ord pos,
                                           Abstract.DeeplyFoldable (Accounting l pos s) l,
                                           Abstract.Haskell l, Abstract.Module l l ~ AST.Module l l,
                                           Ord (Abstract.ModuleName l), Ord (Abstract.Name l)) =>
                          Map Extension Bool
                       -> Wrap l pos s (AST.Module l l (Wrap l pos s) (Wrap l pos s))
                       -> ([Error pos], Map Extension Bool)
verifyModuleExtensions extensions (Compose (_, (_, (AST.ExtendedModule localExtensionSwitches m)))) =
   ((if null contradictions then mempty else [ContradictoryExtensionSwitches contradictions])
    <> (UnusedExtension
        <$> toList (Map.keysSet (Map.filter id localExtensions)
                    Set.\\ usedExtensionsWithPremises Set.\\ Extensions.languageVersions))
    <> (uncurry UndeclaredExtensionUse <$> Map.toList (usedExtensions Map.\\ declaredExtensions)),
    declaredExtensions)
   where usedExtensions :: Map Extension [(pos, pos)]
         usedExtensions = filterExtensions $ getUnionWith $ Full.foldMap (Accounting :: Accounting l pos s) m
         declaredExtensions = Map.filter id (withImplications (localExtensions <> extensions)
                                             <> Map.fromSet (const True) Extensions.includedByDefault)
         (contradictions, localExtensions) = partitionContradictory (Set.fromList localExtensionSwitches)
         usedExtensionsWithPremises = Map.foldMapWithKey extensionAndPremises usedExtensions
         extensionAndPremises x _ = Set.singleton x <> Map.findWithDefault mempty x Extensions.inverseImplications
verifyModuleExtensions extensions m =
   (uncurry UndeclaredExtensionUse
    <$> Map.toList (filterExtensions $
                    getUnionWith (Full.foldMap (Accounting :: Accounting l pos s) m) Map.\\ declaredExtensions),
    declaredExtensions)
   where declaredExtensions = Map.filter id (withImplications extensions
                                             <> Map.fromSet (const True) Extensions.includedByDefault)

filterExtensions :: (Num pos, Ord pos) => Map Extension [(pos, pos)] -> Map Extension [(pos, pos)]
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

instance {-# overlappable #-}
         Verification l pos s
         `Transformation.At` g (Wrap l pos s) (Wrap l pos s) where
   Verification $ _ = mempty

instance (TextualMonoid s, Num pos, Ord pos, Abstract.DeeplyFoldable (Accounting l pos s) l,
          Abstract.Haskell l, Abstract.Module l l ~ AST.Module l l,
          Abstract.DeeplyFoldable (Accounting l pos s) l,
          Ord (Abstract.ModuleName l), Ord (Abstract.Name l)) =>
         Verification l pos s
         `Transformation.At` AST.Module l l (Wrap l pos s) (Wrap l pos s) where
   Verification $ m = Const $ Ap $ accum $ flip verifyModuleExtensions m

instance (Full.Foldable (Accounting l pos s) (Abstract.DataConstructor l l),
          Full.Foldable (Accounting l pos s) (Abstract.GADTConstructor l l)) =>
         Verification l pos s
         `Transformation.At` ExtAST.Declaration l l (Wrap l pos s) (Wrap l pos s) where
   Verification $ Compose (_, (_, d))
     | ExtAST.TypeDataDeclaration _sup _lhs _kind constructors <- d = verifyTypeData constructors
     | ExtAST.TypeGADTDeclaration _sup1 _sup2 _lhs _kind constructors <- d = verifyTypeData constructors
     | otherwise = mempty
     where verifyTypeData :: (w ~ Wrap l pos s, Foldable f, Full.Foldable (Accounting l pos s) g)
                          => f (w (g w w)) -> Verified pos x
           verifyTypeData =
              Const
              . Ap
              . accum
              . (,)
              . Map.foldMapWithKey (\k v-> case k of Extensions.BangDataFields -> [StrictTypeDataFields v]
                                                     Extensions.TraditionalRecordSyntax -> [RecordTypeDataFields v]
                                                     _ -> [])
              . getUnionWith
              . foldMap (Full.foldMap Accounting)

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
          Abstract.DeeplyFoldable (UnicodeSyntaxAccounting l pos s) l,
          Deep.Foldable (UnicodeSyntaxAccounting l pos s) (ExtAST.Declaration l l),
          FlexibleInstanceHeadAccounting l pos s
          `Transformation.At` Abstract.ClassInstanceLHS l l (Wrap l pos s) (Wrap l pos s),
          Full.Foldable (MPTCAccounting l pos s) (Abstract.TypeLHS l l)) =>
         Accounting l pos s
         `Transformation.At` ExtAST.Declaration l l (Wrap l pos s) (Wrap l pos s) where
   Accounting $ d@(Compose (_, ((start, _, end), dec))) = Const $ UnionWith $
      case dec
      of ExtAST.DataDeclaration context _lhs _kind constructors _derivings ->
            (if null constructors then emptyDataDeclarationsUse else mempty)
            <>
            (case snd . snd . getCompose $ context
             of ExtAST.NoContext -> mempty
                _ -> Map.singleton Extensions.DatatypeContexts [(start, end)])
         ExtAST.GADTDeclaration context _lhs constructors _derivings -> gadtSyntaxUse
         ExtAST.NamedDefaultDeclaration{} ->
            Map.singleton Extensions.NamedDefaults [(start, end)]
         ExtAST.TypeDataDeclaration _sup _lhs _kind constructors ->
            (if null constructors then emptyDataDeclarationsUse else mempty) <> typeDataUse
         ExtAST.TypeGADTDeclaration{} -> gadtSyntaxUse <> typeDataUse
         ExtAST.ImplicitParameterDeclaration{} -> Map.singleton Extensions.ImplicitParameters [(start, end)]
         ExtAST.StandaloneDerivingDeclaration{} -> Map.singleton Extensions.StandaloneDeriving [(start, end)]
         ExtAST.ForeignExport convention id name ty ->
            ffiUse <> getUnionWith (getConst $ Accounting Transformation.$ (convention <$ d))
         ExtAST.ForeignImport convention safety id name ty ->
            ffiUse <> getUnionWith (getConst (Accounting Transformation.$ (convention <$ d))
                                    <> getConst (foldMap ((Accounting Transformation.$) . (<$ d)) safety))
         ExtAST.ClassDeclaration _ lhs _ -> case Full.foldMap MPTCAccounting lhs of
            Sum 1 -> mempty
            _ -> Map.singleton Extensions.MultiParamTypeClasses [(start, end)]
         ExtAST.FunDepClassDeclaration{} -> Map.singleton Extensions.FunctionalDependencies [(start, end)]
         ExtAST.InstanceDeclaration _vars _context lhs _methods ->
            getUnionWith (getConst $ FlexibleInstanceHeadAccounting Transformation.$ lhs)
         ExtAST.ExplicitTypeFixityDeclaration{} -> explicitNamespacesUse
         ExtAST.ExplicitDataFixityDeclaration{} -> explicitNamespacesUse
         ExtAST.DataFamilyDeclaration{} -> typeFamiliesUse
         ExtAST.ClosedTypeFamilyDeclaration{} -> typeFamiliesUse
         ExtAST.OpenTypeFamilyDeclaration{} -> typeFamiliesUse
         ExtAST.InjectiveClosedTypeFamilyDeclaration{} -> typeFamilyDependenciesUse
         ExtAST.InjectiveOpenTypeFamilyDeclaration{} -> typeFamilyDependenciesUse
         ExtAST.DataFamilyInstance{} -> typeFamiliesUse
         ExtAST.GADTDataFamilyInstance{} -> typeFamiliesUse
         ExtAST.GADTNewtypeFamilyInstance{} -> typeFamiliesUse
         ExtAST.NewtypeFamilyInstance{} -> typeFamiliesUse
         ExtAST.TypeFamilyInstance{} -> typeFamiliesUse
         _ -> mempty
      <> getUnionWith (Full.foldMap UnicodeSyntaxAccounting d)
      where
      typeFamiliesUse = Map.singleton Extensions.TypeFamilies [(start, end)]
      typeFamilyDependenciesUse = Map.singleton Extensions.TypeFamilyDependencies [(start, end)]
      typeDataUse = Map.singleton Extensions.TypeData [(start, end)]
      gadtSyntaxUse = Map.singleton Extensions.GADTSyntax [(start, end)]
      explicitNamespacesUse = Map.singleton Extensions.ExplicitNamespaces [(start, end)]
      emptyDataDeclarationsUse = Map.singleton Extensions.EmptyDataDeclarations [(start, end)]
      ffiUse = Map.singleton Extensions.ForeignFunctionInterface [(start, end)]

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
          ExtAST.CaseExpression _ branches | null branches -> Map.singleton Extensions.EmptyCase
          ExtAST.LambdaCaseExpression{} -> Map.singleton Extensions.LambdaCase
          ExtAST.MultiWayIfExpression{} -> Map.singleton Extensions.MultiWayIf
          -- negative end of the range means not really PatternGuards follow
          ExtAST.DoExpression{} -> Map.singleton Extensions.PatternGuards . ((negate <$>) <$>)
          ExtAST.MDoExpression{} ->
             \ranges-> Map.fromList [(Extensions.RecursiveDo, ranges),
                                     (Extensions.PatternGuards, (negate <$>) <$> ranges)]
          ExtAST.QualifiedDoExpression{} ->
             getAp $ foldMap Ap [Map.singleton Extensions.QualifiedDo,
                                 Map.singleton Extensions.PatternGuards . fmap (negate <$>)]
          ExtAST.MDoQualifiedExpression{} ->
             getAp $ foldMap Ap [Map.singleton Extensions.QualifiedDo,
                                 Map.singleton Extensions.RecursiveDo,
                                 Map.singleton Extensions.PatternGuards . fmap (negate <$>)]
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
          ExtAST.ExplicitTypeExpression{} -> Map.singleton Extensions.ExplicitNamespaces
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
      (if any ((isPrefixOf "\"\"\"") . lexemeText) lexemes
       then Map.singleton Extensions.MultilineStrings [(start, end)]
       else mempty)
      <>
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
          ExtAST.ExtendedLiteral{} -> Map.singleton Extensions.ExtendedLiterals [(start, end)]
          _ -> mempty)
      where hashless (ExtAST.HashLiteral _ l) = hashless l
            hashless l = l

instance (Eq s, IsString s, LeftReductive s, Factorial s, Abstract.Haskell l,
          Abstract.Name l ~ AST.Name l, Abstract.QualifiedName l ~ AST.QualifiedName l) =>
         Accounting l pos s
         `Transformation.At` ExtAST.DerivingClause l l (Wrap l pos s) (Wrap l pos s) where
   Accounting $ Compose (_, ((start, _, end), t)) = Const $ UnionWith $
      case t
      of ExtAST.SimpleDerive c -> derivingExtensions start end c
         _ -> mempty

instance (Eq s, IsString s, LeftReductive s, Factorial s, Abstract.Haskell l,
          Abstract.Name l ~ AST.Name l, Abstract.QualifiedName l ~ AST.QualifiedName l) =>
         Accounting l pos s
         `Transformation.At` ExtAST.ClassInstanceLHS l l (Wrap l pos s) (Wrap l pos s) where
   Accounting $ Compose (_, ((start, Trailing lexemes, end), t)) = Const $ UnionWith $
      case t
      of ExtAST.InfixTypeClassInstanceLHS _ c _ -> derivingExtensions start end c <> typeOperators
         ExtAST.TypeClassInstanceLHS c _ -> check c
         ExtAST.ClassReferenceInstanceLHS c -> check c
         _ -> mempty
         where typeOperators = Map.singleton Extensions.TypeOperators [(start, end)]
               check c = derivingExtensions start end c <> if any isAnyDelimiter lexemes then typeOperators else mempty

derivingExtensions :: (Abstract.Haskell l,
                       Abstract.Name l ~ AST.Name l, Abstract.QualifiedName l ~ AST.QualifiedName l)
                   => pos -> pos -> AST.QualifiedName l -> Map Extension [(pos, pos)]
derivingExtensions start end (AST.QualifiedName _ c)
  | c == Abstract.name "Functor" = Map.singleton Extensions.DeriveFunctor [(start, end)]
  | c == Abstract.name "Foldable" = Map.singleton Extensions.DeriveFoldable [(start, end)]
  | c == Abstract.name "Traversable" = Map.singleton Extensions.DeriveTraversable [(start, end)]
  | c == Abstract.name "Data" = Map.singleton Extensions.DeriveDataTypeable [(start, end)]
  | c == Abstract.name "Typeable" = Map.singleton Extensions.DeriveDataTypeable [(start, end)]
  | otherwise = mempty

instance (Eq s, IsString s, LeftReductive s, TextualMonoid s) =>
         Accounting l pos s
         `Transformation.At` ExtAST.TypeLHS l l (Wrap l pos s) (Wrap l pos s) where
   Accounting $ Compose (_, ((start, Trailing lexemes, end), t)) = Const $ UnionWith $
      case t
      of ExtAST.SimpleTypeLHS{}
            -> if any (Textual.any isSymbol . lexemeText) lexemes
               then Map.singleton Extensions.TypeOperators [(start, end)]
               else mempty
         ExtAST.SimpleKindedTypeLHS{}
            -> if any (Textual.any isSymbol . lexemeText) lexemes
               then Map.singleton Extensions.TypeOperators [(start, end)]
               else mempty
         ExtAST.InfixTypeLHSApplication{} -> Map.singleton Extensions.TypeOperators [(start, end)]
         ExtAST.TypeLHSApplication{} -> mempty
         ExtAST.TypeLHSTypeApplication{} -> Map.singleton Extensions.TypeOperators [(start, end)]

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
                ExtAST.OrPattern {} -> Map.singleton Extensions.OrPatterns [(start, end)]
                ExtAST.ViewPattern {} -> Map.singleton Extensions.ViewPatterns [(start, end)]
                ExtAST.UnboxedSumPattern{} -> Map.singleton Extensions.UnboxedSums [(start, end)]
                ExtAST.UnboxedTuplePattern{} -> Map.singleton Extensions.UnboxedTuples [(start, end)]
                ExtAST.InvisibleTypePattern {} -> Map.singleton Extensions.TypeAbstractions [(start, end)]
                ExtAST.ExplicitTypePattern{} -> Map.singleton Extensions.ExplicitNamespaces [(start, end)]
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
         ExtAST.ForallType{} -> Map.singleton Extensions.ExplicitForAll [(start, end)]
         ExtAST.KindedType{} -> Map.singleton Extensions.KindSignatures [(start, end)]
         ExtAST.GroundTypeKind{} -> Map.singleton Extensions.StarIsType [(start, end)]
         ExtAST.StrictType{} -> Map.singleton Extensions.BangDataFields [(start, end)]
         ExtAST.TypeWildcard{} -> Map.singleton Extensions.PartialTypeSignatures [(start, end)]
         ExtAST.UnboxedSumType{} -> Map.singleton Extensions.UnboxedSums [(start, end)]
         ExtAST.UnboxedTupleType{} -> Map.singleton Extensions.UnboxedTuples [(start, end)]
         ExtAST.VisibleDependentType{} -> Map.fromList [(Extensions.ExplicitForAll, [(start, end)]),
                                                        (Extensions.PolyKinds, [(start, end)])]
         _ -> mempty

instance (Eq s, IsString s, LeftReductive s, Factorial s) =>
         Accounting l pos s
         `Transformation.At` ExtAST.TypeVarBinding l l (Wrap l pos s) (Wrap l pos s) where
   Accounting $ Compose (_, ((start, _, end), t)) | let span = (start, end) = Const $ UnionWith $ case t of
      ExtAST.ExplicitlyKindedTypeVariable{} -> Map.singleton Extensions.KindSignatures [span]
      ExtAST.ImplicitlyKindedTypeVariable{} -> mempty
      ExtAST.WildcardTypeBinding -> Map.singleton Extensions.TypeAbstractionsOrApplicationsInConstructorPatterns [span]
      ExtAST.ExplicitlyKindedWildcardTypeBinding{} ->
         Map.singleton Extensions.TypeAbstractionsOrApplicationsInConstructorPatterns [span]
         <>
         Map.singleton Extensions.KindSignatures [span]

instance (Eq s, IsString s, LeftReductive s, Factorial s) =>
         Accounting l pos s
         `Transformation.At` ExtAST.Constructor l l (Wrap l pos s) (Wrap l pos s) where
   Accounting $ Compose (_, ((start, _, end), t)) = Const $ UnionWith $
      case t
      of ExtAST.UnboxedTupleConstructor{} -> Map.singleton Extensions.UnboxedTuples [(start, end)]
         ExtAST.UnboxedSumConstructor{} -> Map.singleton Extensions.UnboxedSums [(start, end)]
         _ -> mempty

instance Accounting l pos s `Transformation.At` ExtAST.CallingConvention l where
   Accounting $ Compose (_, ((start, _, end), e)) = Const $ UnionWith $
      (case e
       of ExtAST.CApiCall{} -> Map.singleton Extensions.CApiFFI [(start, end)]
          _ -> mempty)

instance Accounting l pos s `Transformation.At` ExtAST.CallSafety l where
   Accounting $ Compose (_, ((start, _, end), e)) = Const $ UnionWith $
      (case e
       of ExtAST.InterruptibleCall{} -> Map.singleton Extensions.InterruptibleFFI [(start, end)]
          _ -> mempty)

(|||) :: Applicative f => f Bool -> f Bool -> f Bool
(|||) = liftA2 (||)

instance {-# OVERLAPS #-} MPTCAccounting l pos s
                          `Transformation.At` ExtAST.TypeLHS l l (Wrap l pos s) (Wrap l pos s) where
   MPTCAccounting $ Compose (_, (_, node)) = Const $ Sum $ case node of
      ExtAST.SimpleTypeLHS _ args -> length args
      ExtAST.SimpleKindedTypeLHS _ args -> length args
      ExtAST.InfixTypeLHSApplication{} -> 2
      ExtAST.TypeLHSApplication{} -> 1
      ExtAST.TypeLHSTypeApplication{} -> 0

instance MPTCAccounting l pos s `Transformation.At` g l l (Wrap l pos s) (Wrap l pos s) where
  MPTCAccounting $ _ = mempty

instance {-# OVERLAPS #-}
   (Abstract.UniversallyApplicable (FlexibleInstanceHeadAccounting l pos s) l (Wrap l pos s),
    Abstract.UniversallyApplicable (FlexibleInstanceTypeAccounting l pos s) l (Wrap l pos s))
   => FlexibleInstanceHeadAccounting l pos s
      `Transformation.At` ExtAST.ClassInstanceLHS l l (Wrap l pos s) (Wrap l pos s) where
   FlexibleInstanceHeadAccounting $ Compose (_, (_, node)) = case node of
      ExtAST.ClassInstanceLHSKindApplication inst _kind ->
         Const $ getConst $ FlexibleInstanceHeadAccounting Transformation.$ inst
      inst -> Const (Shallow.foldMap FlexibleInstanceHeadAccounting inst
                     <> Shallow.foldMap FlexibleInstanceTypeAccounting inst)

instance FlexibleInstanceHeadAccounting l pos s `Transformation.At` g l l (Wrap l pos s) (Wrap l pos s) where
  FlexibleInstanceHeadAccounting $ _ = mempty

instance {-# OVERLAPS #-}
   (Abstract.Type l ~ ExtAST.Type l,
    Abstract.UniversallyApplicable (FlexibleInstanceTypeArgAccounting l pos s) l (Wrap l pos s))
   => FlexibleInstanceTypeAccounting l pos s `Transformation.At` ExtAST.Type l l (Wrap l pos s) (Wrap l pos s) where
   FlexibleInstanceTypeAccounting $ Compose (_, (_, node)) = case node of
      ExtAST.TypeApplication left right ->
         (FlexibleInstanceTypeAccounting Transformation.$ left)
         <> Const (foldMap (Shallow.foldMap FlexibleInstanceTypeArgAccounting) right)
      ExtAST.InfixTypeApplication left op right ->
        Const (foldMap (Shallow.foldMap FlexibleInstanceTypeArgAccounting) left
               <> foldMap (Shallow.foldMap FlexibleInstanceTypeArgAccounting) right)
      ExtAST.KindedType t k -> FlexibleInstanceTypeAccounting Transformation.$ t
      t -> Const $ Shallow.foldMap FlexibleInstanceTypeArgAccounting t

instance FlexibleInstanceTypeAccounting l pos s `Transformation.At` g l l (Wrap l pos s) (Wrap l pos s) where
  FlexibleInstanceTypeAccounting $ _ = mempty

instance {-# OVERLAPS #-}
   FlexibleInstanceTypeArgAccounting l pos s `Transformation.At` Abstract.Type l l (Wrap l pos s) (Wrap l pos s)
   => FlexibleInstanceTypeArgAccounting l pos s `Transformation.At` ExtAST.Type l l (Wrap l pos s) (Wrap l pos s) where
   FlexibleInstanceTypeArgAccounting $ Compose (_, ((start, _, end), node)) = case node of
      ExtAST.TypeVariable{} -> mempty
      ExtAST.TypeWildcard -> mempty
      ExtAST.KindedType t k -> Const $ getConst $ FlexibleInstanceTypeArgAccounting Transformation.$ t
      _ -> Const $ UnionWith $ Map.singleton Extensions.FlexibleInstances [(start, end)]

instance FlexibleInstanceTypeArgAccounting l pos s `Transformation.At` g l l (Wrap l pos s) (Wrap l pos s) where
  FlexibleInstanceTypeArgAccounting $ _ = mempty

instance (Eq s, IsString s) =>
         UnicodeSyntaxAccounting l pos s
         `Transformation.At` g (Wrap l pos s) (Wrap l pos s) where
   UnicodeSyntaxAccounting $ Compose (_, ((start, Trailing lexemes, end), _))
      | any (`elem` unicodeDelimiters) lexemes =
        Const (UnionWith $ Map.singleton Extensions.UnicodeSyntax [(start, end)])
      | otherwise = mempty
      where unicodeDelimiters :: [Lexeme s]
            unicodeDelimiters = Token Delimiter <$> ["∷", "⇒", "→", "←", "★", "⊸"]

instance (Rank2.Foldable (g (Wrap l pos s)), Deep.Foldable (Verification l pos s) g,
          Transformation.At (Verification l pos s) (g (Wrap l pos s) (Wrap l pos s))) =>
         Full.Foldable (Verification l pos s) g where
   foldMap = Full.foldMapDownDefault

instance (Rank2.Foldable (g (Wrap l pos s)), Deep.Foldable (Accounting l pos s) g,
          Transformation.At (Accounting l pos s) (g (Wrap l pos s) (Wrap l pos s))) =>
         Full.Foldable (Accounting l pos s) g where
   foldMap = Full.foldMapDownDefault

instance (Rank2.Foldable (g (Wrap l pos s)), Deep.Foldable (MPTCAccounting l pos s) g,
          Transformation.At (MPTCAccounting l pos s)
                            (g (Wrap l pos s) (Wrap l pos s))) =>
         Full.Foldable (MPTCAccounting l pos s) g where
   foldMap = Full.foldMapDownDefault

instance (Rank2.Foldable (g (Wrap l pos s)), Deep.Foldable (FlexibleInstanceHeadAccounting l pos s) g,
          Transformation.At (FlexibleInstanceHeadAccounting l pos s)
                            (g (Wrap l pos s) (Wrap l pos s))) =>
         Full.Foldable (FlexibleInstanceHeadAccounting l pos s) g where
   foldMap = Full.foldMapDownDefault

instance (Rank2.Foldable (g (Wrap l pos s)), Deep.Foldable (FlexibleInstanceTypeAccounting l pos s) g,
          Transformation.At (FlexibleInstanceTypeAccounting l pos s)
                            (g (Wrap l pos s) (Wrap l pos s))) =>
         Full.Foldable (FlexibleInstanceTypeAccounting l pos s) g where
   foldMap = Full.foldMapDownDefault

instance (Rank2.Foldable (g (Wrap l pos s)), Deep.Foldable (FlexibleInstanceTypeArgAccounting l pos s) g,
          Transformation.At (FlexibleInstanceTypeArgAccounting l pos s)
                            (g (Wrap l pos s) (Wrap l pos s))) =>
         Full.Foldable (FlexibleInstanceTypeArgAccounting l pos s) g where
   foldMap = Full.foldMapDownDefault

instance (Rank2.Foldable (g (Wrap l pos s)), Deep.Foldable (UnicodeSyntaxAccounting l pos s) g,
          Transformation.At (UnicodeSyntaxAccounting l pos s)
                            (g (Wrap l pos s) (Wrap l pos s))) =>
         Full.Foldable (UnicodeSyntaxAccounting l pos s) g where
   foldMap = Full.foldMapDownDefault

checkDuplicateRecordFields :: forall l pos s node. (Ord (Abstract.ModuleName l), Ord (Abstract.Name l))
                           => ZipList (Wrap l pos s node) -> Maybe [(pos, pos)]
checkDuplicateRecordFields (ZipList declarations)
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
