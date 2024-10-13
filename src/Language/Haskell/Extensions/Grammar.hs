{-# Language DataKinds, FlexibleContexts, FlexibleInstances, NamedFieldPuns, OverloadedStrings,
             Rank2Types, RecordWildCards, ScopedTypeVariables,
             TemplateHaskell, TupleSections, TypeApplications, TypeFamilies, TypeOperators, TypeSynonymInstances #-}

-- | Missing syntax extensions:
-- * @TransformListComp@ is not supported by TemplateHaskell
-- * @OverloadedRecordUpdate@ is not supported by TemplateHaskell
-- * @Arrows@ is not supported by TemplateHaskell

module Language.Haskell.Extensions.Grammar (extendedGrammar, parseModule, report, module Report) where

import Control.Applicative
import Control.Monad (void)
import qualified Data.Char as Char
import Data.Foldable (fold, toList)
import Data.Function ((&))
import Data.Functor.Compose (Compose(getCompose))
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Ord (Down)
import Data.Maybe (isJust, isNothing)
import Data.Function.Memoize (memoize)
import Data.Monoid (Endo(..))
import Data.Monoid.Cancellative (RightReductive, isPrefixOf, isSuffixOf)
import Data.Monoid.Instances.Positioned (LinePositioned, column)
import Data.Monoid.Instances.PrefixMemory (Shadowed (content, prefix))
import Data.Monoid.Textual (TextualMonoid, toString)
import qualified Data.Monoid.Factorial as Factorial
import qualified Data.Monoid.Textual as Textual
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Numeric
import qualified Rank2
import qualified Rank2.TH
import qualified Text.Parser.Char
import Text.Parser.Combinators (eof, endBy, sepBy, sepBy1, sepByNonEmpty, sepEndBy)
import Text.Parser.Token (braces, brackets, comma, parens)
import Text.Grampa
import Text.Grampa.Combinators (moptional, someNonEmpty, takeSomeNonEmpty)
import Text.Grampa.ContextFree.SortedMemoizing.Transformer.LeftRecursive (autochain, ParserT, lift)
import qualified Transformation.Deep as Deep
import Witherable (filter, mapMaybe)

import Language.Haskell.Extensions (Extension(..), ExtensionSwitch(..),
                                    on, partitionContradictory, switchesByName, withImplications)
import qualified Language.Haskell.Extensions.Abstract as Abstract
import qualified Language.Haskell.Grammar as Report
import Language.Haskell.Grammar (HaskellGrammar(..), ModuleLevelGrammar(..), DeclarationGrammar(..),
                                 Parser, OutlineMonoid, NodeWrap,
                                 blockOf, delimiter, terminator, inputColumn, isSymbol,
                                 moduleId, nameQualifier,
                                 oneExtendedLine, rewrap, startSepEndBy, storeToken, wrap, unwrap, whiteSpace)
import Language.Haskell.Reserializer (Lexeme(..), Serialization, TokenType(..), lexemes)

import Prelude hiding (exponent, filter)

class TextualMonoid t => SpaceMonoid t where
   precededByString :: t -> t -> Bool
   precededByOpenSpace :: t -> Bool

instance (Eq t, Factorial.StableFactorial t, RightReductive t, TextualMonoid t) => SpaceMonoid (Shadowed t) where
   precededByString s t = content s `isSuffixOf` prefix t
   precededByOpenSpace t = Textual.any isOpenOrSpace (Factorial.primeSuffix $ prefix t) || "-}" `isSuffixOf` prefix t
      where isOpenOrSpace c = Char.isSpace c || c `elem` ("([{,;" :: [Char])

followedByCloseSpace :: TextualMonoid t => t -> Bool
followedByCloseSpace t =
   any isCloseOrSpace (Textual.characterPrefix t) || "{-" `isPrefixOf` t || "--" `isPrefixOf` t
   where isCloseOrSpace c = Char.isSpace c || c `elem` (")]},;" :: [Char])

data ExtendedGrammar l t f p = ExtendedGrammar {
   report :: HaskellGrammar l t f p,
   singleDerivingClause :: p [f (Abstract.DerivingClause l l f f)],
   keywordForall :: p (),
   kindSignature :: p (Abstract.Kind l l f f),
   groundTypeKind :: p (),
   cType, arrowType :: p (Abstract.Type l l f f),
   promotedLiteral, promotedStructure :: p (Abstract.Type l l f f),
   equalityConstraint, implicitParameterConstraint :: p (Abstract.Context  l l f f),
   infixPattern :: p (Abstract.Pattern l l f f),
   gadtNewConstructor, gadtConstructors :: p (Abstract.GADTConstructor l l f f),
   constructorIDs :: p (NonEmpty (Abstract.Name l)),
   derivingStrategy :: p (Abstract.DerivingStrategy l l f f),
   inClassOrInstanceTypeFamilyDeclaration :: p (Abstract.Declaration l l f f),
   instanceDesignatorApplications, instanceDesignatorBase :: p (Abstract.ClassInstanceLHS l l f f),
   optionalForall :: p [Abstract.TypeVarBinding l l f f],
   typeVarBinder :: p (Abstract.TypeVarBinding l l f f),
   optionallyParenthesizedTypeVar :: p (Abstract.Name l),   
   optionallyKindedTypeVar, optionallyKindedAndParenthesizedTypeVar :: p (Abstract.Type l l f f),
   conArgPattern :: p (Abstract.Pattern l l f f),
   gadtNewBody, gadtBody, prefix_gadt_body, record_gadt_body :: p (Abstract.Type l l f f),
   return_type, arg_type :: p (Abstract.Type l l f f),
   binary :: p t}

$(Rank2.TH.deriveAll ''ExtendedGrammar)

type ExtensionOverlay l g t = (Abstract.Haskell l, LexicalParsing (Parser g t), Ord t, Show t, TextualMonoid t,
                               g ~ ExtendedGrammar l t (NodeWrap t)) => GrammarOverlay g (Parser g t)

extensionMixins :: forall l g t. (Abstract.ExtendedHaskell l,
                                  LexicalParsing (Parser (ExtendedGrammar l t (NodeWrap t)) t),
                                  Ord t, Show t, OutlineMonoid t, SpaceMonoid t,
                                  Abstract.DeeplyFoldable (Serialization (Down Int) t) l,
                                  g ~ ExtendedGrammar l t (NodeWrap t))
                => Map (Set Extension) [(Int, GrammarOverlay g (Parser g t))]
extensionMixins =
  Map.fromList [
     (Set.fromList [IdentifierSyntax],               [(0, identifierSyntaxMixin)]),
     (Set.fromList [PackageImports],                 [(0, packageImportsMixin)]),
     (Set.fromList [SafeImports],                    [(0, safeImportsMixin)]),
     (Set.fromList [ImportQualifiedPost],            [(0, importQualifiedPostMixin)]),
     (Set.fromList [UnicodeSyntax],                  [(1, unicodeSyntaxMixin)]),
     (Set.fromList [BinaryLiterals],                 [(1, binaryLiteralsMixin)]),
     (Set.fromList [HexFloatLiterals],               [(1, hexFloatLiteralsMixin)]),
     (Set.fromList [NumericUnderscores],             [(1, numericUnderscoresMixin)]),
     (Set.fromList [MultiParameterConstraints],      [(1, multiParameterConstraintsMixin)]),
     (Set.fromList [BinaryLiterals,
                    NumericUnderscores],             [(9, binaryUnderscoresMixin)]),
     (Set.fromList [PackageImports,
                    SafeImports],                    [(9, safePackageImportsMixin)]),
     (Set.fromList [PackageImports,
                    ImportQualifiedPost],            [(9, packageImportsQualifiedPostMixin)]),
     (Set.fromList [SafeImports,
                    ImportQualifiedPost],            [(9, safeImportsQualifiedPostMixin)]),
     (Set.fromList [PackageImports,
                    SafeImports,
                    ImportQualifiedPost],            [(9, safePackageImportsQualifiedPostMixin)]),
     (Set.fromList [NegativeLiterals],               [(2, negativeLiteralsMixin)]),
     (Set.fromList [LexicalNegation],                [(3, lexicalNegationMixin)]),
     (Set.fromList [MagicHash],                      [(3, magicHashMixin)]),
     (Set.fromList [ParallelListComprehensions],     [(3, parallelListComprehensionsMixin)]),
     (Set.fromList [ExtendedLiterals],               [(4, extendedLiteralsMixin)]),
     (Set.fromList [OverloadedLabels],               [(4, overloadedLabelsMixin)]),
     (Set.fromList [RecursiveDo],                    [(4, recursiveDoMixin)]),
     (Set.fromList [QualifiedDo],                    [(4, qualifiedDoMixin)]),
     (Set.fromList [QualifiedDo, RecursiveDo],       [(4, qualifiedRecursiveDoMixin)]),
     (Set.fromList [TupleSections],                  [(5, tupleSectionsMixin)]),
     (Set.fromList [EmptyCase],                      [(6, emptyCaseMixin)]),
     (Set.fromList [LambdaCase],                     [(7, lambdaCaseMixin)]),
     (Set.fromList [GratuitouslyParenthesizedTypes], [(7, gratuitouslyParenthesizedTypesMixin)]),
     (Set.fromList [EqualityConstraints],            [(7, equalityConstraintsMixin)]),
     (Set.fromList [MultiWayIf],                     [(8, multiWayIfMixin)]),
     (Set.fromList [KindSignatures],                 [(7, kindSignaturesBaseMixin), (8, kindSignaturesMixin)]),
     (Set.fromList [ParenthesizedTypeOperators],     [(8, parenthesizedTypeOperatorsMixin)]),
     (Set.fromList [TypeOperators],                  [(8, typeOperatorsMixin)]),
     (Set.fromList [DataKinds],                      [(8, dataKindsMixin)]),
     (Set.fromList [DataKinds,
                    ListTuplePuns],                  [(9, dataKindsListTuplePunsMixin)]),
     (Set.fromList [ListTuplePuns],                  [(9, listTuplePunsMixin)]),
     (Set.fromList [ListTuplePuns,
                    UnboxedTuples],                  [(9, unboxedListTuplePunsMixin)]),
     (Set.fromList [ListTuplePuns,
                    UnboxedSums],                    [(9, unboxedSumPunsMixin)]),
     (Set.fromList [ExplicitNamespaces],             [(9, explicitNamespacesMixin)]),
     (Set.fromList [BlockArguments],                 [(9, blockArgumentsMixin)]),
     (Set.fromList [ExistentialQuantification],      [(9, existentialQuantificationMixin)]),
     (Set.fromList [ExplicitForAll],                 [(9, explicitForAllMixin)]),
     (Set.fromList [ScopedTypeVariables],            [(9, scopedTypeVariablesMixin)]),
     (Set.fromList [GADTSyntax],                     [(9, gadtSyntaxMixin)]),
     (Set.fromList [TypeFamilies],                   [(9, typeFamiliesMixin)]),
     (Set.fromList [TypeFamilyDependencies],         [(9, typeFamilyDependenciesMixin)]),
     (Set.fromList [TypeData],                       [(9, typeDataMixin)]),
     (Set.fromList [GADTs, TypeData],                [(9, typeDataGADTMixin)]),
     (Set.fromList [StandaloneKindSignatures],       [(7, kindSignaturesBaseMixin),
                                                      (9, standaloneKindSignaturesMixin)]),
     (Set.fromList [StarIsType],                     [(9, starIsTypeMixin)]),
     (Set.fromList [TypeApplications],               [(9, typeApplicationsMixin)]),
     (Set.fromList [TypeAbstractionsOrApplicationsInConstructorPatterns],
                                                     [(9, typeAbstractionsOrApplicationsMixin)]),
     (Set.fromList [TypeAbstractions],               [(9, typeAbstractionsMixin)]),
     (Set.fromList [InferredTypeVariables],          [(9, inferredTypeVariablesMixin)]),
     (Set.fromList [LinearTypes],                    [(9, linearTypesMixin)]),
     (Set.fromList [RoleAnnotations],                [(9, roleAnnotationsMixin)]),
     (Set.fromList [UnboxedTuples],                  [(9, unboxedTuplesMixin)]),
     (Set.fromList [TupleSections, UnboxedTuples],   [(9, unboxedTupleSectionsMixin)]),
     (Set.fromList [UnboxedSums],                    [(9, unboxedSumsMixin)]),
     (Set.fromList [InterruptibleFFI],               [(9, interruptibleFFIMixin)]),
     (Set.fromList [CApiFFI],                        [(9, cApiFFIMixin)]),
     (Set.fromList [NamedFieldPuns],                 [(9, namedFieldPunsMixin)]),
     (Set.fromList [RecordWildCards],                [(9, recordWildCardsMixin)]),
     (Set.fromList [OverloadedRecordDot],            [(9, overloadedRecordDotMixin)]),
     (Set.fromList [ImplicitParameters],             [(9, implicitParametersMixin)]),
     (Set.fromList [BangPatterns],                   [(9, bangPatternsMixin)]),
     (Set.fromList [ViewPatterns],                   [(9, viewPatternsMixin)]),
     (Set.fromList [NPlusKPatterns],                 [(9, nPlusKPatternsMixin)]),
     (Set.fromList [PatternSynonyms],                [(9, patternSynonymsMixin)]),
     (Set.fromList [StandaloneDeriving],             [(9, standaloneDerivingMixin)]),
     (Set.fromList [DerivingStrategies],             [(9, derivingStrategiesMixin)]),
     (Set.fromList [DerivingVia],                    [(9, derivingViaMixin)]),
     (Set.fromList [StandaloneDeriving,
                    DerivingStrategies],             [(9, standaloneDerivingStrategiesMixin)]),
     (Set.fromList [StandaloneDeriving,
                    DerivingVia],                    [(9, standaloneDerivingViaMixin)]),
     (Set.fromList [FunctionalDependencies],         [(9, functionalDependenciesMixin)]),
     (Set.fromList [InstanceSigs],                   [(9, instanceSignaturesMixin)]),
     (Set.fromList [DefaultSignatures],              [(9, defaultSignaturesMixin)]),
     (Set.fromList [NondecreasingIndentation],       [(9, nondecreasingIndentationMixin)]),
     (Set.fromList [LinearTypes, GADTSyntax],        [(9, gadtLinearTypesMixin)]),
     (Set.fromList [LinearTypes, UnicodeSyntax],     [(9, unicodeLinearTypesMixin)]),
     (Set.fromList [GADTSyntax, LinearTypes,
                    UnicodeSyntax],                  [(9, gadtUnicodeLinearTypesMixin)]),
     (Set.fromList [StarIsType,
                    ParenthesizedTypeOperators],     [(9, starIsTypeOperatorsMixin)]),
     (Set.fromList [StarIsType, UnicodeSyntax],      [(9, unicodeStarIsTypeMixin)]),
     (Set.fromList [GADTSyntax, TypeOperators],      [(9, gadtSyntaxTypeOperatorsMixin)]),
     (Set.fromList [DataKinds, TypeOperators],       [(9, dataKindsTypeOperatorsMixin)]),
     (Set.fromList [DataKinds, TypeOperators,
                    GADTSyntax],                     [(9, dataKindsGadtSyntaxTypeOperatorsMixin)]),
     (Set.fromList [VisibleDependedentQuantification],
                                                     [(9, visibleDependentQuantificationMixin)]),
     (Set.fromList [RequiredTypeArguments],          [(9, requiredTypeArgumentsMixin)]),
     (Set.fromList [ConstraintsAreTypes],            [(9, constraintsAreTypesMixin)]),
     (Set.fromList [SpaceSensitiveOperators],        [(9, spaceSensitiveOperatorsMixin)])]

languagePragmas :: (Rank2.Apply g, Ord t, Show t, TextualMonoid t, LexicalParsing (Parser g t)) =>
                   Parser g t [ExtensionSwitch]
languagePragmas = spaceChars
                 *> admit (string "{-#" *> whiteSpace *> filter isLanguagePragma (takeCharsWhile Char.isAlphaNum)
                           *> commit (whiteSpace
                                      *> liftA2 (<>)
                                            (extension `sepBy` (string "," *> whiteSpace) <* string "#-}")
                                            languagePragmas)
                           <<|> comment *> commit languagePragmas
                           <<|> commit (pure mempty))
   where spaceChars = takeCharsWhile Char.isSpace
         isLanguagePragma pragmaName = Text.toUpper (Textual.toText mempty pragmaName) == "LANGUAGE"
         extension = do extensionName <- takeCharsWhile Char.isAlphaNum
                        void whiteSpace
                        case Map.lookup extensionName switchesByName of
                           Just ext -> pure ext
                           Nothing -> fail ("Unknown language extension " <> toString mempty extensionName)
         comment = string "--" <* takeCharsWhile Report.isLineChar <|> blockComment
         blockComment = string "{-"
                        *> skipMany (blockComment
                                     <|> notFollowedBy (string "-}") *> anyToken <> takeCharsWhile  (/= '-'))
                        *> string "-}"

parseModule :: forall l t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser (ExtendedGrammar l t (NodeWrap t)) t),
                            Ord t, Show t, OutlineMonoid t, SpaceMonoid t,
                            Abstract.DeeplyFoldable (Serialization (Down Int) t) l)
            => Map Extension Bool -> t
            -> ParseResults t [NodeWrap t (Abstract.Module l l (NodeWrap t) (NodeWrap t))]
parseModule extensions source = case moduleExtensions of
   Left err -> Left err
   Right [extensions']
      | let (contradictions, extensionMap) = partitionContradictory (Set.fromList extensions') ->
        if Set.null contradictions then
           (if List.null extensions' then id else fmap $ fmap $ rewrap $ Abstract.withLanguagePragma extensions')
           $ parseResults $ Report.haskellModule $ report
           $ parseComplete (extendedGrammar $ positiveKeys $ withImplications $ extensionMap <> extensions) source
        else Left mempty{errorAlternatives= ["Contradictory extension switches " <> show (toList contradictions)]}
   Right extensionses -> error ("Ambiguous extensions: " <> show extensionses)
   where moduleExtensions = parseResults $ fmap snd $ getCompose $ simply parsePrefix languagePragmas source
         parseResults = getCompose . fmap snd . getCompose
         positiveKeys = Map.keysSet . Map.filter id
         getSwitch (ExtensionSwitch s) = s

extendedGrammar :: forall l t.
                   (Abstract.ExtendedHaskell l, LexicalParsing (Parser (ExtendedGrammar l t (NodeWrap t)) t),
                    Ord t, Show t, OutlineMonoid t, SpaceMonoid t,
                    Abstract.DeeplyFoldable (Serialization (Down Int) t) l)
                 => Set Extension -> Grammar (ExtendedGrammar l t (NodeWrap t)) (ParserT ((,) [[Lexeme t]])) t
extendedGrammar extensions = memoize extendWith mixinKeys
   where mixinKeys :: [Set Extension]
         mixinKeys =  filter (all (`Set.member` extensions)) $ toList $ Map.keysSet $ extensionMixins @l @_ @t
         extendWith :: [Set Extension] -> Grammar (ExtendedGrammar l t (NodeWrap t)) (ParserT ((,) [[Lexeme t]])) t
         extendWith = overlay extendedReport . reverse
                      . (initialOverlay :) . map snd . List.sortOn fst . fold . map (extensionMixins Map.!)
         extendedReport g@ExtendedGrammar{report = r} = g{report = Report.grammar r}

-- | Reorganize the grammar to make it more extensible, without adding any extensions
initialOverlay :: forall l g t. (Abstract.ExtendedHaskell l,
                                 Deep.Foldable (Serialization (Down Int) t) (Abstract.CaseAlternative l l),
                                 Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l),
                                 Deep.Foldable (Serialization (Down Int) t) (Abstract.Expression l l),
                                 Deep.Foldable (Serialization (Down Int) t) (Abstract.Import l l),
                                 Deep.Foldable (Serialization (Down Int) t) (Abstract.Statement l l))
               => ExtensionOverlay l g t
initialOverlay self@ExtendedGrammar{report = selfReport@Report.HaskellGrammar{declarationLevel = selfDeclarations}}
               super@ExtendedGrammar{report = superReport} = super{
   report= superReport{
      declarationLevel= (declarationLevel superReport){
         classLHS = selfDeclarations & simpleType,
         simpleType =
            Abstract.simpleTypeLHS <$> (self & report & typeConstructor) <*> pure []
            <|> Abstract.infixTypeLHSApplication
                <$> (self & typeVarBinder)
                <* terminator "`"
                <*> (selfDeclarations & typeClass)
                <* terminator "`"
                <*> (self & typeVarBinder)
            <|> Abstract.typeLHSApplication
                <$> wrap (selfDeclarations & simpleType)
                <*> (self & typeVarBinder),
         optionalTypeSignatureContext = pure Abstract.noContext,
         context =
            (selfDeclarations & constraint)
            <|> (self & equalityConstraint)
            <|> Abstract.constraints <$> parens (wrap ((selfDeclarations & constraint)
                                                       <|> (self & equalityConstraint)
                                                       <|> (self & implicitParameterConstraint)) `sepBy` comma),
         instanceDesignator = self & instanceDesignatorApplications,
         instanceTypeDesignator = selfReport & aType},
      pattern = self & infixPattern,
      lPattern = (selfReport & aPattern)
                 <|> Abstract.literalPattern
                     <$ delimiter "-"
                     <*> wrap ((Abstract.integerLiteral . negate) <$> (selfReport & integer)
                               <|> (Abstract.floatingLiteral . negate) <$> (selfReport & float))
                 <|> Abstract.constructorPattern
                     <$> wrap (selfReport & generalConstructor)
                     <*> some (wrap $ conArgPattern self),
      aPattern = self & conArgPattern,
      -- NoListTuplePuns
      aType = generalTypeConstructor selfReport
              <|> Abstract.typeVariable <$> typeVar selfReport
              <|> parens (typeTerm selfReport)
              <|> Abstract.typeWildcard <$ keyword "_"
              <|> Abstract.groundType <$ (self & groundTypeKind),
      generalTypeConstructor =
         Abstract.constructorType <$> wrap (Abstract.constructorReference <$> qualifiedConstructor selfReport)
         <|> Abstract.functionConstructorType <$ parens (rightArrow selfReport),
      typeTerm = self & arrowType},
   keywordForall = empty,
   kindSignature = empty,
   groundTypeKind = empty,
   derivingStrategy = empty,
   arrowType = (self & cType)
      <|> Abstract.functionType <$> wrap (self & cType)
                                <* (selfReport & rightArrow)
                                <*> wrap (self & arrowType)
      <|> Abstract.constrainedType <$> wrap (selfDeclarations & context)
                                   <* (selfReport & rightDoubleArrow)
                                   <*> wrap (self & arrowType),
   cType = selfReport & bType,
   equalityConstraint = empty,
   implicitParameterConstraint = empty,
   infixPattern = pattern superReport,
   promotedLiteral = empty,
   promotedStructure = empty,
   inClassOrInstanceTypeFamilyDeclaration = empty,
   instanceDesignatorBase =
      Abstract.classReferenceInstanceLHS <$> (selfDeclarations & qualifiedTypeClass)
      <|> parens (selfReport & declarationLevel & instanceDesignator),
   instanceDesignatorApplications =
      (self & instanceDesignatorBase)
      <|> Abstract.classInstanceLHSApplication
          <$> wrap (self & instanceDesignatorApplications)
          <*> wrap (selfReport & declarationLevel & instanceTypeDesignator),
   optionalForall = pure [],
   optionallyParenthesizedTypeVar = selfReport & typeVar,
   optionallyKindedAndParenthesizedTypeVar = Abstract.typeVariable <$> (self & optionallyParenthesizedTypeVar),
   optionallyKindedTypeVar = empty,
   typeVarBinder = Abstract.implicitlyKindedTypeVariable <$> (selfReport & typeVar),
   gadtConstructors =
      Abstract.gadtConstructors <$> (self & constructorIDs)
                                <* (selfReport & doubleColon)
                                <*> (self & optionalForall)
                                <*> wrap (selfDeclarations & optionalContext)
                                <*> wrap (self & gadtBody),
   gadtNewConstructor =
      Abstract.gadtConstructors <$> ((:|[]) <$> (self & report & constructor)) <* (selfReport & doubleColon)
                                <*> (self & optionalForall)
                                <*> wrap (pure Abstract.noContext
                                          <|> (selfDeclarations & context)
                                              *> (self & report & rightDoubleArrow)
                                              *> fail "No context allowed on GADT newtype")
                                <*> wrap (self & gadtNewBody),
   constructorIDs = (self & report & constructor) `sepByNonEmpty` comma,
   gadtNewBody =
      parens (self & gadtNewBody)
      <|> Abstract.functionType
          <$> wrap ((self & report & bType)
                    <|> Abstract.strictType <$ delimiter "!" <*> wrap (self & report & bType))
          <* (selfReport & rightArrow)
          <*> wrap (self & return_type)
      <|> Abstract.recordFunctionType
          <$> braces ((:[]) <$> wrap (selfDeclarations & fieldDeclaration))
          <* (selfReport & rightArrow)
          <*> wrap (self & return_type),
   gadtBody = prefix_gadt_body self <|> record_gadt_body self,
   prefix_gadt_body =
      parens (self & prefix_gadt_body)
      <|> (self & return_type)
      <|> Abstract.functionType
          <$> wrap ((self & report & bType)
                    <|> Abstract.strictType <$ delimiter "!" <*> wrap (self & report & bType))
          <* (selfReport & rightArrow)
          <*> wrap (self & prefix_gadt_body),
   record_gadt_body =
      parens (self & record_gadt_body)
      <|> Abstract.recordFunctionType
          <$> braces (wrap (selfDeclarations & fieldDeclaration) `sepBy` comma)
          <* (selfReport & rightArrow)
          <*> wrap (self & return_type),
   return_type = Abstract.typeApplication
                    <$> wrap (return_type self <|> parens (self & return_type))
                    <*> wrap (self & arg_type)
                 <|> (selfReport & generalTypeConstructor),
   conArgPattern = aPattern superReport,
   arg_type = selfReport & aType,
   binary = empty}

identifierSyntaxMixin :: ExtensionOverlay l g t
identifierSyntaxMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} = super{
   report= superReport{
      variableIdentifier = token (Abstract.name . Text.pack . toString mempty <$> variableLexeme),
      constructorIdentifier = token (Abstract.name . Text.pack . toString mempty <$> constructorLexeme),
      variableSymbol = token (Abstract.name . Text.pack . toString mempty <$> Report.variableSymbolLexeme),
      constructorSymbol = token (Abstract.name . Text.pack . toString mempty <$> Report.constructorSymbolLexeme)}}

overloadedLabelsMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
overloadedLabelsMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} = super{
   report= superReport{
      bareExpression = (superReport & bareExpression)
                       <|> Abstract.overloadedLabel . Text.pack . toString mempty
                           <$> token (string "#" *> variableLexeme),
      variableSymbol = notFollowedBy (string "#" *> variableLexeme) *> (superReport & variableSymbol)}}

unicodeSyntaxMixin :: ExtensionOverlay l g t
unicodeSyntaxMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} = super{
   keywordForall = keywordForall super <|> delimiter "∀",
   report= superReport{
      doubleColon = (superReport & doubleColon) <|> delimiter "∷",
      rightDoubleArrow = (superReport & rightDoubleArrow) <|> delimiter "⇒",
      rightArrow = (superReport & rightArrow) <|> delimiter "→",
      leftArrow = (superReport & leftArrow) <|> delimiter "←",
      variableSymbol = notSatisfyChar (`elem` ("∀←→⇒∷★" :: [Char])) *> (superReport & variableSymbol)}}

listTuplePunsMixin :: forall l g t. ExtensionOverlay l g t
listTuplePunsMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} = super{
   report= superReport{
      aType = (superReport & aType)
              <|> Abstract.tupleType <$> parens ((:|) <$> wrap (selfReport & typeTerm)
                                                      <*> some (comma *> wrap (selfReport & typeTerm)))
              <|> Abstract.listType <$> brackets (wrap (selfReport & typeTerm)),
      generalTypeConstructor = (superReport & generalTypeConstructor)
                               <|> Abstract.constructorType
                                   <$> wrap (Abstract.unitConstructor <$ terminator "(" <* terminator ")"
                                             <|> Abstract.emptyListConstructor <$ terminator "[" <* terminator "]"
                                             <|> Abstract.tupleConstructor . succ . length <$> parens (some comma))}}

unboxedTuplesMixin :: forall l g t. Abstract.ExtendedWith '[ 'UnboxedTuples ] l => ExtensionOverlay l g t
unboxedTuplesMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} = super{
   conArgPattern = (super & conArgPattern)
                   <|> Abstract.unboxedTuplePattern Abstract.build
                       <$> hashParens (wrap (selfReport & pPattern) `sepByNonEmpty` comma),
   report= superReport{
      generalConstructor = (superReport & generalConstructor)
                           <|> Abstract.unboxedTupleConstructor Abstract.build . succ . length
                               <$> hashParens (many comma),
      bareExpression = (superReport & bareExpression)
                       <|> Abstract.unboxedTupleExpression Abstract.build
                           <$> hashParens ((selfReport & expression) `sepByNonEmpty` comma)}}
   where hashParens p = delimiter "(#" *> p <* terminator "#)"

unboxedListTuplePunsMixin :: forall l g t. Abstract.ExtendedWith '[ 'UnboxedTuples ] l => ExtensionOverlay l g t
unboxedListTuplePunsMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} = super{
   report= superReport{
      aType = (superReport & aType)
              <|> Abstract.unboxedTupleType Abstract.build
                  <$> hashParens (wrap (selfReport & typeTerm) `sepByNonEmpty` comma),
      generalTypeConstructor = (superReport & generalTypeConstructor)
                               <|> Abstract.constructorType
                                   <$> wrap (Abstract.unboxedTupleConstructor Abstract.build . succ . length
                                             <$> hashParens (many comma))}}
   where hashParens p = delimiter "(#" *> p <* terminator "#)"

unboxedTupleSectionsMixin :: forall l g t. Abstract.ExtendedWith '[ 'UnboxedTuples ] l => ExtensionOverlay l g t
unboxedTupleSectionsMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} = super{
   report= superReport{
      bareExpression = (superReport & bareExpression)
                       <|> Abstract.unboxedTupleSectionExpression Abstract.build
                           <$> hashParens (filter (\l-> any isJust l && any isNothing l)
                                           $ optional (selfReport & expression) `sepByNonEmpty` comma)}}
   where hashParens p = delimiter "(#" *> p <* terminator "#)"

unboxedSumsMixin :: forall l g t. Abstract.ExtendedWith '[ 'UnboxedSums ] l => ExtensionOverlay l g t
unboxedSumsMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} = super{
   conArgPattern = (super & conArgPattern)
                   <|> hashParens (Abstract.unboxedSumPattern Abstract.build
                                      <$> (length <$> some (delimiter "|"))
                                      <*> wrap (selfReport & pPattern)
                                      <*> (length <$> many (delimiter "|"))
                                   <|> Abstract.unboxedSumPattern Abstract.build 0
                                      <$> wrap (selfReport & pPattern)
                                      <*> (length <$> some (delimiter "|"))),
   report= superReport{
      generalConstructor = (superReport & generalConstructor)
                           <|> Abstract.unboxedSumConstructor Abstract.build . succ . length
                               <$> hashParens (some $ delimiter "|"),
      bareExpression = (superReport & bareExpression)
                       <|> hashParens (Abstract.unboxedSumExpression Abstract.build
                                          <$> (length <$> some (delimiter "|")) 
                                          <*> (selfReport & expression)
                                          <*> (length <$> many (delimiter "|"))
                                       <|> Abstract.unboxedSumExpression Abstract.build 0
                                          <$> (selfReport & expression)
                                          <*> (length <$> some (delimiter "|")))}}
   where hashParens p = delimiter "(#" *> p <* terminator "#)"

unboxedSumPunsMixin :: forall l g t. Abstract.ExtendedWith '[ 'UnboxedSums ] l => ExtensionOverlay l g t
unboxedSumPunsMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} = super{
   report= superReport{
      aType = (superReport & aType)
              <|> Abstract.unboxedSumType Abstract.build
                  <$> hashParens ((:|) <$> wrap (selfReport & typeTerm)
                                       <*> some (delimiter "|" *> wrap (selfReport & typeTerm))),
      generalTypeConstructor = (superReport & generalTypeConstructor)
                               <|> Abstract.constructorType
                                   <$> wrap (Abstract.unboxedSumConstructor Abstract.build . succ . length
                                             <$> hashParens (some $ delimiter "|"))}}
   where hashParens p = delimiter "(#" *> p <* terminator "#)"

interruptibleFFIMixin :: forall l g t. Abstract.ExtendedWith '[ 'InterruptibleFFI ] l => ExtensionOverlay l g t
interruptibleFFIMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} = super{
   report= superReport{
      declarationLevel= (superReport & declarationLevel){
         safety = (superReport & declarationLevel & safety)
                  <|> Abstract.interruptibleCall Abstract.build <$ keyword "interruptible"}}}

cApiFFIMixin :: forall l g t. Abstract.ExtendedWith '[ 'CApiFFI ] l => ExtensionOverlay l g t
cApiFFIMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} = super{
   report= superReport{
      declarationLevel= (superReport & declarationLevel){
         callingConvention = (superReport & declarationLevel & callingConvention)
                             <|> Abstract.cApiCall Abstract.build <$ keyword "capi"}}}

extendedLiteralsMixin :: (SpaceMonoid t, Abstract.ExtendedWith '[ 'ExtendedLiterals ] l) => ExtensionOverlay l g t
extendedLiteralsMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} = super{
   report = superReport{
      literalLexeme = (superReport & literalLexeme)
                      <* notFollowedBy ((filter (precededByString "#") getInput <|> string "#")
                                        *> void (Text.Parser.Char.satisfy Char.isUpper))
                      <|> Abstract.extendedLiteral Abstract.build
                          <$> storeToken (selfReport & integerLexeme)
                          <*> hashType}}
   where hashType = storeToken (string "#") *> typeConstructor selfReport

magicHashMixin :: forall l g t. (SpaceMonoid t, Abstract.ExtendedHaskell l) => ExtensionOverlay l g t
magicHashMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} =
  let prefixMinusFollow = takeCharsWhile1 Char.isDigit *> takeCharsWhile isNumChar *> string "#"
      isNumChar c = Char.isHexDigit c || c `elem` ("eE.bBoOxX_" :: String)
  in super{report= superReport{
        variableIdentifier =
           token (Abstract.name . Text.pack . toString mempty <$> (variableLexeme <> concatAll (string "#"))),
        constructorIdentifier =
           token (Abstract.name . Text.pack . toString mempty <$> (constructorLexeme <> concatAll (string "#"))),
        literalLexeme = (superReport & literalLexeme)
                        <**> (Abstract.hashLiteral . Abstract.hashLiteral <$ string "##"
                              <<|> Abstract.hashLiteral <$ string "#"
                              <<|> pure id),
        integerLexeme = (superReport & integerLexeme)
                        <<|> negate <$ string "-" <*> (superReport & integerLexeme) <* lookAhead (string "#"),
        floatLexeme = (superReport & floatLexeme)
                      <<|> negate <$ string "-" <*> (superReport & floatLexeme) <* lookAhead (string "#")}}
     & negationConstraintMixin prefixMinusFollow self

recursiveDoMixin :: (OutlineMonoid t, Abstract.ExtendedWith '[ 'RecursiveDo ] l,
                     Abstract.DeeplyFoldable (Serialization (Down Int) t) l)
                 => ExtensionOverlay l g t
recursiveDoMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} = super{
   report= superReport{
      closedBlockExpression = (superReport & closedBlockExpression)
         <|> Abstract.mdoExpression' Abstract.build <$ keyword "mdo" <*> wrap (selfReport & statements),
      statement = (superReport & statement)
                  <|> wrap (Deep.InL
                            <$> Abstract.recursiveStatement' Abstract.build
                                . map Report.expressionToStatement
                                <$ keyword "rec"
                                <*> blockOf (selfReport & statement)),
      variableIdentifier = notFollowedBy (keyword "mdo" <|> keyword "rec") *> (superReport & variableIdentifier)}}

qualifiedDoMixin :: forall g t l. (OutlineMonoid t, Abstract.Haskell l, Abstract.ExtendedWith '[ 'QualifiedDo ] l,
                                   Abstract.DeeplyFoldable (Serialization (Down Int) t) l)
                 => ExtensionOverlay l g t
qualifiedDoMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} = super{
   report= superReport{
      closedBlockExpression = (superReport & closedBlockExpression)
         <|> Abstract.qualifiedDoExpression Abstract.build
            <$> Report.storeToken (Abstract.moduleName <$> Report.moduleLexeme <* string ".")
            <* keyword "do"
            <*> wrap (selfReport & statements),
       qualifiedVariableSymbol =
          notFollowedBy (string "." *> optional (Report.moduleLexeme @g @l *> string ".") *> keyword "do")
          *> (superReport & qualifiedVariableSymbol)}}

qualifiedRecursiveDoMixin :: forall g t l. (OutlineMonoid t, Abstract.Haskell l,
                                            Abstract.ExtendedWith '[ 'QualifiedDo, 'RecursiveDo ] l,
                                            Abstract.DeeplyFoldable (Serialization (Down Int) t) l)
                          => ExtensionOverlay l g t
qualifiedRecursiveDoMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} = super{
   report= superReport{
      closedBlockExpression = (superReport & closedBlockExpression)
         <|> Abstract.mdoQualifiedExpression Abstract.build
            <$> Report.storeToken (Abstract.moduleName <$> Report.moduleLexeme <* string ".")
            <* keyword "mdo"
            <*> wrap (selfReport & statements),
       qualifiedVariableSymbol =
          notFollowedBy (string "." *> optional (Report.moduleLexeme @g @l *> string ".") *> keyword "mdo")
          *> (superReport & qualifiedVariableSymbol)}}

parallelListComprehensionsMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
parallelListComprehensionsMixin self@ExtendedGrammar{report= HaskellGrammar{qualifiers, expression}}
                                super@ExtendedGrammar{report = superReport} = super{
   report= superReport{
      bareExpression = (superReport & bareExpression)
                       <|> brackets (Abstract.parallelListComprehension
                                     <$> expression <*> qualifiers <*> qualifiers <*> many qualifiers)}}

tupleSectionsMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
tupleSectionsMixin self@ExtendedGrammar{report= HaskellGrammar{expression}}
                   super@ExtendedGrammar{report = superReport} = super{
   report= superReport{
      bareExpression = (superReport & bareExpression)
         <|> Abstract.tupleSectionExpression
             <$> parens (filter (\l-> any isJust l && any isNothing l)
                         $ (:|) <$> optional expression <*> some (comma *> optional expression))}}

lambdaCaseMixin :: forall l g t. (Abstract.ExtendedWith '[ 'LambdaCase ] l, OutlineMonoid t,
                                  Deep.Foldable (Serialization (Down Int) t) (Abstract.LambdaCasesAlternative l l))
                => ExtensionOverlay l g t
lambdaCaseMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} = super{
   report= superReport{
      openBlockExpression = notFollowedBy (delimiter "\\" *> keyword "cases") *> (superReport & openBlockExpression),
      closedBlockExpression = (superReport & closedBlockExpression)
         <|> Abstract.lambdaCaseExpression Abstract.build <$ (delimiter "\\" *> keyword "case")
             <*> (selfReport & alternatives)
         <|> Abstract.lambdaCasesExpression Abstract.build <$ (delimiter "\\" *> keyword "cases")
             <*> blockOf (wrap $ Abstract.lambdaCasesAlternative @l Abstract.build
                                     <$> many (wrap (selfReport & aPattern))
                                     <*> wrap (Abstract.normalRHS <$ delimiter "->" <*> expression selfReport
                                               <|> Abstract.guardedRHS
                                                   <$> takeSomeNonEmpty (wrap $ Abstract.guardedExpression . toList
                                                                                <$> (selfReport & guards)
                                                                                <* delimiter "->"
                                                                                <*> (selfReport & expression))))}}

emptyCaseMixin :: (OutlineMonoid t, Abstract.ExtendedHaskell l,
                   Deep.Foldable (Serialization (Down Int) t) (Abstract.CaseAlternative l l))
               => ExtensionOverlay l g t
emptyCaseMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} = super{
   report= superReport{
      alternatives = blockOf (wrap (alternative $ report super))}}

multiWayIfMixin :: (OutlineMonoid t, Abstract.ExtendedHaskell l,
                    Deep.Foldable (Serialization (Down Int) t) (Abstract.GuardedExpression l l))
                => ExtensionOverlay l g t
multiWayIfMixin self@ExtendedGrammar{report= HaskellGrammar{expression, guards, rightArrow}}
                super@ExtendedGrammar{report = superReport} = super{
   report= superReport{
      closedBlockExpression = (superReport & closedBlockExpression)
         <|> Abstract.multiWayIfExpression <$ keyword "if"
             <*> blockOf' (wrap (Abstract.guardedExpression . toList
                                 <$> guards <* rightArrow <*> expression))}}

packageImportsMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
packageImportsMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} = super{
   report= superReport{
      moduleLevel= (superReport & moduleLevel){
         importDeclaration = (superReport & moduleLevel & importDeclaration)
                             <|> Abstract.packageQualifiedImportDeclaration <$ keyword "import"
                                 <*> (True <$ keyword "qualified" <|> pure False)
                                 <*> (selfReport & stringLiteral)
                                 <*> moduleId
                                 <*> optional (keyword "as" *> moduleId)
                                 <*> optional (wrap $ selfReport & moduleLevel & importSpecification)}}}

safeImportsMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
safeImportsMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} = super{
   report= superReport{
      moduleLevel= (superReport & moduleLevel){
         importDeclaration = (superReport & moduleLevel & importDeclaration)
                             <|> Abstract.safeImportDeclaration <$ keyword "import" <* keyword "safe"
                                 <*> (True <$ keyword "qualified" <|> pure False)
                                 <*> moduleId
                                 <*> optional (keyword "as" *> moduleId)
                                 <*> optional (wrap $ selfReport & moduleLevel & importSpecification)}}}

importQualifiedPostMixin :: ExtensionOverlay l g t
importQualifiedPostMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} = super{
   report= superReport{
      moduleLevel= (superReport & moduleLevel){
         importDeclaration = (superReport & moduleLevel & importDeclaration)
                             <|> flip Abstract.importDeclaration <$ keyword "import"
                                 <*> moduleId
                                 <*> (True <$ keyword "qualified")
                                 <*> optional (keyword "as" *> moduleId)
                                 <*> optional (wrap $ selfReport & moduleLevel & importSpecification)}}}

safePackageImportsMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
safePackageImportsMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} = super{
   report= superReport{
      moduleLevel= (superReport & moduleLevel){
         importDeclaration = (superReport & moduleLevel & importDeclaration)
                             <|> Abstract.safePackageQualifiedImportDeclaration <$ keyword "import" <* keyword "safe"
                                 <*> (True <$ keyword "qualified" <|> pure False)
                                 <*> (selfReport & stringLiteral)
                                 <*> moduleId
                                 <*> optional (keyword "as" *> moduleId)
                                 <*> optional (wrap $ selfReport & moduleLevel & importSpecification)}}}

packageImportsQualifiedPostMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
packageImportsQualifiedPostMixin self@ExtendedGrammar{report = selfReport}
                                 super@ExtendedGrammar{report = superReport} = super{
   report= superReport{
      moduleLevel= (superReport & moduleLevel){
         importDeclaration = (superReport & moduleLevel & importDeclaration)
                             <|> Abstract.packageQualifiedImportDeclaration <$ keyword "import"
                                 <**> pure flip
                                 <*> (selfReport & stringLiteral)
                                 <**> pure flip
                                 <*> moduleId
                                 <*> (True <$ keyword "qualified" <|> pure False)
                                 <*> optional (keyword "as" *> moduleId)
                                 <*> optional (wrap $ selfReport & moduleLevel & importSpecification)}}}

safeImportsQualifiedPostMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
safeImportsQualifiedPostMixin self@ExtendedGrammar{report = selfReport}
                              super@ExtendedGrammar{report = superReport} = super{
   report= superReport{
      moduleLevel= (superReport & moduleLevel){
         importDeclaration = (superReport & moduleLevel & importDeclaration)
                             <|> flip Abstract.safeImportDeclaration <$ keyword "import" <* keyword "safe"
                                 <*> moduleId
                                 <*> (True <$ keyword "qualified" <|> pure False)
                                 <*> optional (keyword "as" *> moduleId)
                                 <*> optional (wrap $ selfReport & moduleLevel & importSpecification)}}}

safePackageImportsQualifiedPostMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
safePackageImportsQualifiedPostMixin self@ExtendedGrammar{report = selfReport}
                                     super@ExtendedGrammar{report = superReport} = super{
   report= superReport{
      moduleLevel= (superReport & moduleLevel){
         importDeclaration = (superReport & moduleLevel & importDeclaration)
                             <|> Abstract.safePackageQualifiedImportDeclaration <$ keyword "import" <* keyword "safe"
                                 <**> pure flip
                                 <*> (selfReport & stringLiteral)
                                 <**> pure flip
                                 <*> moduleId
                                 <*> (True <$ keyword "qualified" <|> pure False)
                                 <*> optional (keyword "as" *> moduleId)
                                 <*> optional (wrap $ selfReport & moduleLevel & importSpecification)}}}

explicitNamespacesMixin :: Abstract.ExtendedWith '[ 'ExplicitNamespaces ] l => ExtensionOverlay l g t
explicitNamespacesMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} = super{
   report= superReport{
      moduleLevel= (superReport & moduleLevel){
         export = (superReport & moduleLevel & export)
            <|> Abstract.exportClassOrType <$ keyword "type"
                <*> parens (selfReport & qualifiedVariableSymbol)
                <*> optional (selfReport & moduleLevel & members),
         importItem = (superReport & moduleLevel & importItem)
            <|> Abstract.importClassOrType <$ keyword "type"
                <*> parens ((selfReport & variableSymbol) <|> (selfReport & constructorSymbol))
                <*> optional (selfReport & moduleLevel & members),
         members = parens (Abstract.allMembers <$ delimiter ".."
                           <|> Abstract.explicitlyNamespacedMemberList Abstract.build
                            <$> (Abstract.defaultMember Abstract.build <$> (selfReport & moduleLevel & cname)
                                 <|> Abstract.typeMember Abstract.build <$ keyword "type"
                                     <*> (selfReport & moduleLevel & cname))
                               `sepEndBy` comma)},
      declarationLevel = (superReport & declarationLevel) {
         generalDeclaration = (superReport & declarationLevel & generalDeclaration)
            <|> Abstract.explicitTypeFixityDeclaration Abstract.build
                   <$> (selfReport & declarationLevel & fixity)
                   <*> optional (fromIntegral <$> integer selfReport)
                   <* keyword "type"
                   <*> (operator selfReport `sepByNonEmpty` comma)
            <|> Abstract.explicitDataFixityDeclaration Abstract.build
                   <$> (selfReport & declarationLevel & fixity)
                   <*> optional (fromIntegral <$> integer selfReport)
                   <* keyword "data"
                   <*> (operator selfReport `sepByNonEmpty` comma)},
      expression = (superReport & expression)
         <|> wrap (Abstract.explicitTypeExpression Abstract.build <$ keyword "type" <*> wrap (typeTerm selfReport)),
      pPattern = (superReport & pPattern)
         <|> Abstract.explicitTypePattern Abstract.build <$ keyword "type" <*> wrap (typeTerm selfReport)}}

blockArgumentsMixin :: ExtensionOverlay l g t
blockArgumentsMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} = super{
   report= superReport{
      lExpression = (superReport & lExpression)
         <|> wrap (Abstract.applyExpression <$> (selfReport & fExpression)
                                            <*> wrap (selfReport & openBlockExpression)),
      dExpression = (selfReport & fExpression),
      bareExpression = (superReport & bareExpression) <|> (selfReport & closedBlockExpression)}}

spaceSensitiveOperatorsMixin :: SpaceMonoid t => ExtensionOverlay l g t
spaceSensitiveOperatorsMixin self@ExtendedGrammar{report = selfReport}
                             super@ExtendedGrammar{report = superReport} = super{
   conArgPattern = Abstract.variablePattern <$> (superReport & variable) <* lookAhead unreservedSymbolLexeme
                   <<|> notFollowedBy unreservedSymbolLexeme *> (super & conArgPattern),
   report= superReport{
      variableSymbol = (superReport & variableSymbol) <|> Report.nameToken unreservedSymbolLexeme}}

unreservedSymbolLexeme :: (Rank2.Apply g, Ord t, SpaceMonoid t) => Parser g t t
unreservedSymbolLexeme =
   filter precededByOpenSpace getInput
      *> (string "@" <|> string "~") <* filter followedByCloseSpace getInput
   <|> filter (not . precededByOpenSpace) getInput *> string "~"

lexicalNegationMixin :: SpaceMonoid t => ExtensionOverlay l g t
lexicalNegationMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} = super{
   report= superReport{
      qualifiedVariableSymbol = notFollowedBy (filter precededByOpenSpace getInput
                                               *> string "-"
                                               *> satisfyCharInput (\c-> Char.isAlphaNum c || c == '(' || c == '['))
                                *> token (nameQualifier <*> (selfReport & variableSymbol)),
      prefixNegation = empty,
      bareExpression = (superReport & bareExpression)
         <|> Abstract.applyExpression <$> wrap (Abstract.negate <$ prefixMinus) <*> (selfReport & aExpression)
         <|> parens (Abstract.rightSectionExpression
                     <$> (notFollowedBy prefixMinus *> (selfReport & qualifiedOperator))
                     <*> (selfReport & infixExpression))}}
   where prefixMinus = void (filter precededByOpenSpace getInput
                             *> string "-"
                             <* lookAhead (satisfyCharInput $ \c-> Char.isAlphaNum c || c == '(' || c == '[')
                             <* lift ([[Token Modifier "-"]], ()))
                       <?> "prefix -"

negativeLiteralsMixin :: ExtensionOverlay l g t
negativeLiteralsMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} =
   super{
      report= superReport{
         integerLexeme = (negate <$ string "-" <|> pure id) <*> (superReport & integerLexeme),
         floatLexeme = (negate <$ string "-" <|> pure id) <*> (superReport & floatLexeme)}}
   & negationConstraintMixin (satisfyCharInput Char.isDigit) self

binaryLiteralsMixin :: ExtensionOverlay l g t
binaryLiteralsMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} = super{
   binary = (string "0b" <|> string "0B") *> (takeCharsWhile1 (\c-> c == '0' || c == '1') <?> "binary number"),
   report = superReport{
      integerLexeme = List.foldl' addBinary 0 . toString mempty <$> (self & binary)
                      <<|> (superReport & integerLexeme)}}
   where addBinary n '0' = 2*n
         addBinary n '1' = 2*n + 1
         addBinary _ _ = error "non-binary"

hexFloatLiteralsMixin :: ExtensionOverlay l g t
hexFloatLiteralsMixin self@ExtendedGrammar{report= HaskellGrammar{decimal, hexadecimal}}
                      super@ExtendedGrammar{report = superReport} = super{
   report= superReport{
      integerLexeme = notFollowedBy ((string "0x" <|> string "0X")
                                    *> hexadecimal *> satisfyCharInput (`elem` ['.', 'p', 'P']))
                      *> (superReport & integerLexeme),
      floatLexeme = (string "0x" <|> string "0X")
                    *> (readHexFloat <$> hexadecimal <* string "." <*> hexadecimal <*> (hexExponent <<|> pure 0)
                       <|> readHexFloat <$> hexadecimal <*> pure mempty <*> hexExponent)
                    <|> (superReport & floatLexeme)}}
   where hexExponent =
           (string "p" <|> string "P")
           *> (id <$ string "+" <|> negate <$ string "-" <|> pure id)
           <*> (fst . head . Numeric.readDec . toString mempty <$> decimal)
         readHexFloat whole fraction magnitude =
           fst (head $ Numeric.readHex $ toString mempty $ whole <> fraction)
           * 2 ^^ (magnitude - Factorial.length fraction)

numericUnderscoresMixin :: ExtensionOverlay l g t
numericUnderscoresMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} = super{
   report= superReport{
      decimal = takeCharsWhile1 Char.isDigit <> concatAll (char '_' *> takeCharsWhile1 Char.isDigit)
                <?> "decimal number",
      octal = takeCharsWhile1 Char.isOctDigit <> concatAll (char '_' *> takeCharsWhile1 Char.isOctDigit)
              <?> "octal number",
      hexadecimal = takeCharsWhile1 Char.isHexDigit <> concatAll (char '_' *> takeCharsWhile1 Char.isHexDigit)
                    <?> "hexadecimal number"}}

binaryUnderscoresMixin :: ExtensionOverlay l g t
binaryUnderscoresMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} = super{
   binary = (string "0b" <|> string "0B") *> (binaryDigits <> concatAll (char '_' *> binaryDigits) <?> "binary number")}
   where binaryDigits = takeCharsWhile1 (\c-> c == '0' || c == '1')

parenthesizedTypeOperatorsMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
parenthesizedTypeOperatorsMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} =
   super{
      report= superReport{
         moduleLevel= (superReport & moduleLevel){
            export = Abstract.exportVar <$> (selfReport & qualifiedVariable)
               <|> Abstract.exportClassOrType
                   <$> ((selfReport & qualifiedConstructorIdentifier)
                        <|> parens (selfReport & qualifiedConstructorSymbol))
                   <*> pure Nothing
               <|> Abstract.exportClassOrType
                   <$> (selfReport & qualifiedTypeConstructor)
                   <*> (Just <$> (selfReport & moduleLevel & members))
               <|> Abstract.reExportModule <$ keyword "module" <*> Report.moduleId},
         qualifiedTypeConstructor = (selfReport & qualifiedConstructorIdentifier) <|> parens anyQualifiedOperator,
         generalTypeConstructor = (superReport & generalTypeConstructor)
           <|> Abstract.constructorType
               <$> wrap (Abstract.constructorReference <$> parens (selfReport & qualifiedVariableSymbol)),
         declarationLevel= (superReport & declarationLevel){
            qualifiedTypeClass =
               (superReport & declarationLevel & qualifiedTypeClass) <|> parens anyQualifiedOperator}}}
   where anyQualifiedOperator =
            (selfReport & qualifiedConstructorOperator) <|> (selfReport & qualifiedVariableOperator)

typeOperatorsMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
typeOperatorsMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} =
   super{
      report= superReport{
         moduleLevel= (superReport & moduleLevel){
            importItem = Abstract.importVar <$> (selfReport & variable)
               <|> Abstract.importClassOrType
                   <$> ((selfReport & constructorIdentifier) <|> parens (selfReport & constructorSymbol))
                   <*> pure Nothing
               <|> Abstract.importClassOrType
                   <$> (selfReport & typeConstructor)
                   <*> (Just <$> (selfReport & moduleLevel & members))},
         declarationLevel= (superReport & declarationLevel){
            typeClass = (superReport & declarationLevel & typeClass) <|> parens anyOperator,
            simpleType = (superReport & declarationLevel & simpleType)
               <|> Abstract.infixTypeLHSApplication
                            <$> typeVarBinder self
                            <*> (notFollowedBy (string "`") *> anyOperator)
                            <*> typeVarBinder self,
            instanceDesignator =
               (superReport & declarationLevel & instanceDesignator)
               <|> Abstract.infixTypeClassInstanceLHS
                   <$> wrap (selfReport & bType)
                   <*> (selfReport & qualifiedOperator)
                   <*> wrap (selfReport & bType)},
         typeConstructor = (selfReport & constructorIdentifier) <|> parens anyOperator},
      equalityConstraint = empty,
      cType = (super & cType)
         <|> Abstract.infixTypeApplication
                <$> wrap (self & cType)
                <*> (selfReport & qualifiedOperator)
                <*> wrap (selfReport & bType)}
   where anyOperator = (selfReport & constructorOperator) <|> (selfReport & variableOperator)

equalityConstraintsMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
equalityConstraintsMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} = super{
   equalityConstraint =
      Abstract.typeEquality
      <$> wrap (selfReport & bType)
      <* delimiter "~"
      <*> wrap ((selfReport & bType))}

multiParameterConstraintsMixin :: forall l g t. Abstract.ExtendedHaskell l => ExtensionOverlay l g t
multiParameterConstraintsMixin self@ExtendedGrammar{report = selfReport}
                               super@ExtendedGrammar{report = superReport} = super{
   report= superReport{
      declarationLevel= (superReport & declarationLevel){
         derivingClause =
            keyword "deriving"
            *> (pure <$> wrap (parens (Abstract.strategicDerive Abstract.build
                                       <$> wrap (pure $ Abstract.defaultStrategy @l Abstract.build)
                                       <*> wrap (selfReport & typeTerm) `sepBy` comma)
                               <<|> Abstract.simpleDerive <$> (selfReport & declarationLevel & qualifiedTypeClass)))}}}

gratuitouslyParenthesizedTypesMixin :: (OutlineMonoid t, Abstract.ExtendedHaskell l,
                                        Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l),
                                        Deep.Foldable (Serialization (Down Int) t) (Abstract.GADTConstructor l l))
                                    => ExtensionOverlay l g t
gratuitouslyParenthesizedTypesMixin self@ExtendedGrammar{report = selfReport}
                                    super@ExtendedGrammar{report = superReport} = super{
   report= superReport{
      declarationLevel = (superReport & declarationLevel){
         qualifiedTypeClass = (superReport & declarationLevel & qualifiedTypeClass) <|> parens qtc,
         typeVarApplications = (selfReport & generalTypeConstructor)
            <|> Abstract.typeApplication
                <$> wrap ((selfReport & declarationLevel & typeVarApplications)
                          <|> parens (selfReport & declarationLevel & typeVarApplications))
                <*> wrap (optionallyKindedAndParenthesizedTypeVar self),
         simpleType = (superReport & declarationLevel & simpleType)
            <|> parens (selfReport & declarationLevel & simpleType)}},
   gadtConstructors = (super & gadtConstructors)
      <|> Abstract.gadtConstructors <$> (self & constructorIDs)
                                    <* (selfReport & doubleColon)
                                    <**> pure uncurry3
                                    <*> (parens forallAndContextAndBody <|> forallAndParenContextBody),
   gadtNewConstructor = (super & gadtNewConstructor)
      <|> Abstract.gadtConstructors <$> ((:|[]) <$> (selfReport & constructor))
                                    <* (selfReport & doubleColon)
                                    <**> pure uncurry3
                                    <*> parens forallAndNewBody,
   gadtNewBody = (super & gadtNewBody)
      <|> Abstract.functionType
          <$> wrap ((selfReport & bType)
                    <|> Abstract.strictType <$ delimiter "!" <*> wrap (selfReport & bType))
          <* (selfReport & Report.rightArrow)
          <*> wrap paren_return_type
      <|> Abstract.recordFunctionType
          <$> braces ((:[]) <$> wrap (selfReport & declarationLevel & fieldDeclaration))
          <* (selfReport & Report.rightArrow)
          <*> wrap paren_return_type,
   record_gadt_body = (super & record_gadt_body)
      <|> Abstract.recordFunctionType
          <$> braces (wrap (selfReport & declarationLevel & fieldDeclaration) `sepBy` comma)
          <* (selfReport & Report.rightArrow)
          <*> wrap paren_return_type,
   optionallyParenthesizedTypeVar = (selfReport & typeVar)
                                    <|> parens (optionallyParenthesizedTypeVar self),
   typeVarBinder = Abstract.implicitlyKindedTypeVariable <$> optionallyParenthesizedTypeVar self}
   where qtc = (selfReport & declarationLevel & qualifiedTypeClass)
         paren_return_type = parens ((self & return_type) <|> parens paren_return_type)
         optionalContextAndGadtBody =
            contextAndGadtBody <|> (,) <$> wrap (pure Abstract.noContext) <*> wrap (self & gadtBody)
         contextAndGadtBody =
            (,) <$> wrap (selfReport & declarationLevel & context)
                <*  (selfReport & rightDoubleArrow)
                <*> wrap (self & gadtBody)
            <|> parens contextAndGadtBody
         forallAndContextAndBody =
            (,,) <$ keywordForall self
                 <*> many (typeVarBinder self)
                 <* delimiter "."
                 <**> pure uncurry
                 <*> optionalContextAndGadtBody
            <|> uncurry ((,,) []) <$> contextAndGadtBody
            <|> parens forallAndContextAndBody
         forallAndParenContextBody =
            (,,) <$ keywordForall self
                 <*> many (typeVarBinder self)
                 <* delimiter "."
                 <**> pure uncurry
                 <*> parens contextAndGadtBody
         forallAndNewBody =
            (,,) <$ keywordForall self
                 <*> many (typeVarBinder self)
                 <* delimiter "."
                 <*> wrap (pure Abstract.noContext
                           <|> (selfReport & declarationLevel & context)
                               *> (selfReport & rightDoubleArrow)
                               *> fail "No context allowed on GADT newtype")
                 <*> wrap (self & gadtNewBody)
            <|> parens forallAndNewBody
         uncurry3 f (a, b, c) = f a b c

typeFamiliesMixin :: forall l g t. (OutlineMonoid t, Abstract.ExtendedHaskell l,
                                    Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l),
                                    Deep.Foldable (Serialization (Down Int) t) (Abstract.GADTConstructor l l))
                  => ExtensionOverlay l g t
typeFamiliesMixin self@ExtendedGrammar
                  {report= selfReport@HaskellGrammar{
                     declarationLevel= DeclarationGrammar{optionalContext, simpleType, derivingClause,
                                                          declaredConstructors, newConstructor}}}
                  super@ExtendedGrammar{report = superReport} =
  super{
    report= superReport{
      declarationLevel= (superReport & declarationLevel){
         topLevelDeclaration = (superReport & declarationLevel & topLevelDeclaration)
            <|> Abstract.dataFamilyDeclaration <$ keyword "data" <* keyword "family"
                <*> wrap simpleType <*> optional (wrap $ kindSignature self)
            <|> Abstract.openTypeFamilyDeclaration <$ keyword "type" <* keyword "family"
                <*> wrap simpleType <*> optional (wrap $ kindSignature self)
            <|> Abstract.closedTypeFamilyDeclaration <$ keyword "type" <* keyword "family"
                <*> wrap simpleType <*> optional (wrap $ kindSignature self) <* keyword "where"
                <*> blockOf (wrap
                             $ Abstract.typeFamilyInstance
                             <$> optionalForall self
                             <*> wrap (selfReport & declarationLevel & instanceDesignator) <* delimiter "="
                             <*> wrap (selfReport & typeTerm))
            <|> Abstract.dataFamilyInstance <$ (keyword "data" *> keyword "instance")
                <*> optionalForall self
                <*> wrap optionalContext
                <*> wrap (selfReport & declarationLevel & instanceDesignator)
                <*> optional (wrap $ kindSignature self)
                <*> moptional (delimiter "=" *> declaredConstructors)
                <*> moptional derivingClause
            <|> Abstract.newtypeFamilyInstance <$ (keyword "newtype" *> keyword "instance")
                <*> optionalForall self
                <*> wrap optionalContext
                <*> wrap (selfReport & declarationLevel & instanceDesignator)
                <*> optional (wrap $ kindSignature self)
                <* delimiter "="
                <*> wrap newConstructor
                <*> moptional derivingClause
            <|> Abstract.gadtDataFamilyInstance <$ (keyword "data" *> keyword "instance")
                <*> optionalForall self
                <*> wrap (selfReport & declarationLevel & instanceDesignator)
                <*> optional (wrap $ kindSignature self)
                <* keyword "where"
                <*> blockOf (wrap $ gadtConstructors self)
                <*> moptional derivingClause
            <|> Abstract.gadtNewtypeFamilyInstance <$ (keyword "newtype" *> keyword "instance")
                <*> optionalForall self
                <*> wrap (selfReport & declarationLevel & instanceDesignator)
                <*> optional (wrap $ kindSignature self)
                <* keyword "where"
                <*> wrap (gadtNewConstructor self)
                <*> moptional derivingClause
            <|> Abstract.typeFamilyInstance <$ (keyword "type" *> keyword "instance")
                <*> optionalForall self
                <*> wrap (selfReport & declarationLevel & instanceDesignator)
                <* delimiter "="
                <*> wrap (selfReport & typeTerm),
         inClassDeclaration = (superReport & declarationLevel & inClassDeclaration)
            <|> Abstract.dataFamilyDeclaration <$ keyword "data" <* optional (keyword "family")
                <*> wrap simpleType <*> optional (wrap $ kindSignature self)
            <|> Abstract.openTypeFamilyDeclaration <$ keyword "type" <* optional (keyword "family")
                <*> wrap simpleType <*> optional (wrap $ kindSignature self)
            <|> inClassOrInstanceTypeFamilyDeclaration self,
         inInstanceDeclaration = (superReport & declarationLevel & inInstanceDeclaration)
            <|> Abstract.dataFamilyInstance <$ keyword "data" <* optional (keyword "instance")
                <*> optionalForall self
                <*> wrap optionalContext
                <*> wrap (selfReport & declarationLevel & instanceDesignator)
                <*> optional (wrap $ kindSignature self)
                <*> moptional (delimiter "=" *> declaredConstructors)
                <*> moptional derivingClause
            <|> Abstract.newtypeFamilyInstance <$ keyword "newtype" <* optional (keyword "instance")
                <*> optionalForall self
                <*> wrap optionalContext
                <*> wrap (selfReport & declarationLevel & instanceDesignator)
                <*> optional (wrap $ kindSignature self)
                <* delimiter "="
                <*> wrap newConstructor
                <*> moptional derivingClause
            <|> Abstract.gadtDataFamilyInstance <$ (keyword "data" *> optional (keyword "instance"))
                <*> optionalForall self
                <*> wrap (selfReport & declarationLevel & instanceDesignator)
                <*> optional (wrap $ kindSignature self)
                <* keyword "where"
                <*> blockOf (wrap $ gadtConstructors self)
                <*> moptional derivingClause
            <|> Abstract.gadtNewtypeFamilyInstance <$ (keyword "newtype" *> optional (keyword "instance"))
                <*> optionalForall self
                <*> wrap (selfReport & declarationLevel & instanceDesignator)
                <*> optional (wrap $ kindSignature self)
                <* keyword "where"
                <*> wrap (gadtNewConstructor self)
                <*> moptional derivingClause
            <|> inClassOrInstanceTypeFamilyDeclaration self}},
    inClassOrInstanceTypeFamilyDeclaration =
       Abstract.typeFamilyInstance <$ keyword "type" <* optional (keyword "instance")
           <*> optionalForall self
           <*> wrap (selfReport & declarationLevel & instanceDesignator)
           <* delimiter "="
           <*> wrap (selfReport & typeTerm)}

typeFamilyDependenciesMixin :: (OutlineMonoid t, Abstract.ExtendedHaskell l,
                                Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l),
                                Deep.Foldable (Serialization (Down Int) t) (Abstract.GADTConstructor l l))
                            => ExtensionOverlay l g t
typeFamilyDependenciesMixin
  self@ExtendedGrammar{report= selfReport@HaskellGrammar{declarationLevel= DeclarationGrammar{simpleType}}}
  super@ExtendedGrammar{report = superReport} =
  super{
    report= superReport{
      declarationLevel= (superReport & declarationLevel){
         topLevelDeclaration = (superReport & declarationLevel & topLevelDeclaration)
            <|> Abstract.injectiveOpenTypeFamilyDeclaration <$ keyword "type" <* keyword "family"
                <*> wrap simpleType <* delimiter "="
                <*> typeVarBinder self
                <*> optional dependencies
            <|> Abstract.injectiveClosedTypeFamilyDeclaration <$ keyword "type" <* keyword "family"
                <*> wrap simpleType <* delimiter "="
                <*> typeVarBinder self
                <*> optional dependencies
                <* keyword "where"
                <*> blockOf (wrap $ Abstract.typeFamilyInstance
                             <$> optionalForall self
                             <*> wrap (selfReport & declarationLevel & instanceDesignator) <* delimiter "="
                             <*> wrap (selfReport & typeTerm)),
         inClassDeclaration = (superReport & declarationLevel & inClassDeclaration)
            <|> Abstract.injectiveOpenTypeFamilyDeclaration <$ keyword "type" <* optional (keyword "family")
                <*> wrap simpleType <* delimiter "="
                <*> typeVarBinder self
                <*> (Just <$> dependencies)}}}
   where dependencies = (,) <$> (delimiter "|" *> (selfReport & typeVar)) <* (selfReport & rightArrow)
                            <*> someNonEmpty (selfReport & typeVar)

dataKindsMixin :: forall l g t. (Abstract.ExtendedHaskell l, TextualMonoid t) => ExtensionOverlay l g t
dataKindsMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} = super{
   report= superReport{
      aType = (superReport & aType)
         <|> promotedLiteral self
         <|> promotedStructure self,
      generalTypeConstructor = (superReport & generalTypeConstructor)
         <|> Abstract.promotedConstructorType
             <$ terminator "'"
             <*> wrap (Abstract.constructorReference <$> qualifiedConstructor selfReport)},
   promotedLiteral =
      Abstract.promotedIntegerLiteral <$> (selfReport & integer)
      <|> Abstract.promotedCharLiteral <$> (selfReport & charLiteral)
      <|> Abstract.promotedStringLiteral <$> (selfReport & stringLiteral),
   promotedStructure =
      Abstract.promotedTupleType <$> parens (pure []
                                             <|> (:) <$> wrap (selfReport & typeTerm)
                                                     <*> some (comma *> wrap (selfReport & typeTerm)))
      <|> Abstract.promotedListType <$> brackets (wrap (selfReport & typeTerm) `sepBy` comma)}

dataKindsListTuplePunsMixin :: forall l g t. (Abstract.ExtendedHaskell l, TextualMonoid t) => ExtensionOverlay l g t
dataKindsListTuplePunsMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} = super{
   promotedStructure =
      Abstract.promotedTupleType <$ terminator "'" <*> parens (wrap (selfReport & typeTerm) `sepBy` comma)
      <|> Abstract.promotedListType <$ terminator "'" <*> brackets (wrap (selfReport & typeTerm) `sepBy` comma)
      <|> Abstract.promotedListType <$> brackets ((:) <$> wrap (selfReport & typeTerm)
                                                      <*> some (comma *> wrap (selfReport & typeTerm)))}

dataKindsTypeOperatorsMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
dataKindsTypeOperatorsMixin self@ExtendedGrammar{report = selfReport}
                            super@ExtendedGrammar{report = superReport} = super{
   cType = (super & cType)
      <|> Abstract.promotedInfixTypeApplication
          <$> wrap (cType self)
          <* terminator "'"
          <*> (selfReport & qualifiedOperator)
          <*> wrap (selfReport & bType)}

typeDataMixin :: Abstract.ExtendedWith '[ 'TypeData ] l => ExtensionOverlay l g t
typeDataMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} = super{
   report = (superReport){
      declarationLevel= (superReport & declarationLevel){
         topLevelDeclaration = (superReport & declarationLevel & topLevelDeclaration)
            <|> Abstract.typeDataDeclaration Abstract.build <$ keyword "type" <* keyword "data"
                <*> wrap (selfReport & declarationLevel & simpleType)
                <*> optional (wrap $ kindSignature self)
                <*> (delimiter "=" *> (selfReport & declarationLevel & declaredConstructors) <|> pure [])}}}

typeDataGADTMixin :: (OutlineMonoid t,
                      Abstract.ExtendedWith '[ 'GADTs, 'TypeData ] l,
                      Abstract.DeeplyFoldable (Serialization (Down Int) t) l) => ExtensionOverlay l g t
typeDataGADTMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} = super{
   report = (superReport){
      declarationLevel= (superReport & declarationLevel){
         topLevelDeclaration = (superReport & declarationLevel & topLevelDeclaration)
            <|> Abstract.typeGADTDeclaration Abstract.build <$ keyword "type" <* keyword "data"
                <*> wrap (selfReport & declarationLevel & simpleType)
                <*> optional (wrap $ kindSignature self) <* keyword "where"
                <*> blockOf (wrap $ gadtConstructors self)}}}

visibleDependentQuantificationMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
visibleDependentQuantificationMixin self@ExtendedGrammar{report = selfReport}
                                    super@ExtendedGrammar{report = superReport} = super{
   arrowType = arrowType super
      <|> Abstract.visibleDependentType
          <$ keywordForall self
          <*> many (typeVarBinder self)
          <* (selfReport & rightArrow)
          <*> wrap (arrowType self)}

requiredTypeArgumentsMixin :: Abstract.ExtendedWith '[ 'ExplicitNamespaces ] l => ExtensionOverlay l g t
requiredTypeArgumentsMixin self@ExtendedGrammar{report = selfReport}
                           super@ExtendedGrammar{report = superReport} = super{
   report = superReport {
      aExpression = (superReport & aExpression)
                    <<|> wrap (Abstract.explicitTypeExpression Abstract.build <$> wrap (aType selfReport))}}

kindSignaturesBaseMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
kindSignaturesBaseMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} = super{
   kindSignature = Abstract.typeKind <$ (selfReport & doubleColon) <*> wrap (selfReport & typeTerm)}

starIsTypeMixin :: ExtensionOverlay l g t
starIsTypeMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} = super{
   groundTypeKind = groundTypeKind super <|> delimiter "*"}

unicodeStarIsTypeMixin :: ExtensionOverlay l g t
unicodeStarIsTypeMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} = super{
   groundTypeKind = groundTypeKind super <|> delimiter "★"}

starIsTypeOperatorsMixin :: ExtensionOverlay l g t
starIsTypeOperatorsMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} = super{
   report = superReport {
      aType = parens (Abstract.constructorType
                      <$> wrap (Abstract.constructorReference . Abstract.qualifiedName Nothing
                                <$> token (Report.nameToken $ string "*")))
              <<|> (superReport & aType)}}

roleAnnotationsMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
roleAnnotationsMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} = super{
   report = superReport {
      declarationLevel= (superReport & declarationLevel) {
         topLevelDeclaration = (superReport & declarationLevel & topLevelDeclaration)
            <|> Abstract.typeRoleDeclaration <$ keyword "type" <* keyword "role"
                <*> (selfReport & qualifiedTypeConstructor)
                <*> some (Abstract.nominalRole <$ keyword "nominal"
                          <|> Abstract.representationalRole <$ keyword "representational"
                          <|> Abstract.phantomRole <$ keyword "phantom"
                          <|> Abstract.inferredRole <$ keyword "_")
         }
      }
   }

inferredTypeVariablesMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
inferredTypeVariablesMixin self@ExtendedGrammar{report = selfReport}
                           super@ExtendedGrammar{report = superReport} = super{
   typeVarBinder = typeVarBinder super
      <|> braces (Abstract.inferredTypeVariable <$> (selfReport & typeVar)
                  <|> Abstract.inferredExplicitlyKindedTypeVariable
                      <$> (selfReport & typeVar)
                      <*> wrap (self & kindSignature))}

typeApplicationsMixin :: (Abstract.ExtendedHaskell l, Abstract.DeeplyFoldable (Serialization (Down Int) t) l,
                          SpaceMonoid t) => ExtensionOverlay l g t
typeApplicationsMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} = super{
   report = superReport{
      bType = (superReport & bType)
         <|> Abstract.visibleKindApplication
             <$> filter whiteSpaceTrailing (wrap $ selfReport & bType)
             <* typeApplicationDelimiter
             <*> wrap (Abstract.typeKind <$> wrap (selfReport & aType)),
      bareExpression = (superReport & bareExpression)
         <|> Abstract.visibleTypeApplication
             <$> filter whiteSpaceTrailing (selfReport & aExpression)
             <* typeApplicationDelimiter
             <*> wrap (selfReport & aType)},
   return_type = (super & return_type)
      <|> Abstract.visibleKindApplication
          <$> filter whiteSpaceTrailing (wrap $ self & return_type)
          <* typeApplicationDelimiter
          <*> wrap (Abstract.typeKind <$> wrap (selfReport & aType))}
   where typeApplicationDelimiter = notFollowedBy unreservedSymbolLexeme *> delimiter "@"


typeAbstractionsOrApplicationsMixin :: (Abstract.ExtendedHaskell l, SpaceMonoid t,
                                        Abstract.DeeplyFoldable (Serialization (Down Int) t) l)
                                    => ExtensionOverlay l g t
typeAbstractionsOrApplicationsMixin self@ExtendedGrammar{report = selfReport}
                                    super@ExtendedGrammar{report = superReport} = super{
   report = superReport{
      lPattern = (superReport & lPattern)
         <|> Abstract.constructorPatternWithTypeApplications
             <$> filter whiteSpaceTrailing (wrap $ selfReport & generalConstructor)
             <*> some (typeApplicationDelimiter *> wrap (selfReport & aType))
             <*> many (wrap $ self & conArgPattern)},
   instanceDesignatorApplications = (super & instanceDesignatorApplications)
      <|> Abstract.classInstanceLHSKindApplication
          <$> filter whiteSpaceTrailing (wrap $ self & instanceDesignatorApplications)
          <* typeApplicationDelimiter
          <*> wrap (Abstract.typeKind <$> wrap (selfReport & aType))}
   where typeApplicationDelimiter = notFollowedBy unreservedSymbolLexeme *> delimiter "@"

typeAbstractionsMixin :: (Abstract.ExtendedWith '[ 'TypeAbstractions ] l,
                          SpaceMonoid t) => ExtensionOverlay l g t
typeAbstractionsMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} = super{
   report = superReport{
      declarationLevel = (superReport & declarationLevel){
         simpleType = (superReport & declarationLevel & simpleType)
            <|> Abstract.typeLHSTypeApplication Abstract.build
                   <$> wrap (selfReport & declarationLevel & simpleType)
                   <* delimiter "@"
                   <*> (self & typeVarBinder)},
      aPattern =
         Abstract.invisibleTypePattern Abstract.build
            <$ (filter precededByOpenSpace getInput *> delimiter "@")
            <*> wrap (selfReport & aType)
         <|> notFollowedBy ((selfReport & variable) *> filter precededByOpenSpace getInput *> delimiter "@")
             *> (superReport & aPattern)}}

linearTypesMixin :: (SpaceMonoid t, Abstract.ExtendedHaskell l) => ExtensionOverlay l g t
linearTypesMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} = super{
   report= superReport{
      variableSymbol = notFollowedBy prefixPercent *> (superReport & variableSymbol)},
   arrowType = (super & arrowType)
      <|> Abstract.linearFunctionType
          <$> wrap (self & cType)
          <* token prefixPercent
          <* keyword "1"
          <* (selfReport & rightArrow)
          <*> wrap (self & arrowType)
      <|> Abstract.multiplicityFunctionType
          <$> wrap (self & cType)
          <* token prefixPercent
          <* notFollowedBy (keyword "1")
          <*> wrap (selfReport & aType)
          <* (selfReport & rightArrow)
          <*> wrap (self & arrowType)}
   where prefixPercent =
            filter precededByOpenSpace getInput *> string "%" <* filter (not . followedByCloseSpace) getInput

gadtLinearTypesMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
gadtLinearTypesMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} = super{
  prefix_gadt_body = (super & prefix_gadt_body)
    <|> Abstract.linearFunctionType
        <$> wrap ((selfReport & bType) <|> Abstract.strictType <$ delimiter "!" <*> wrap (selfReport & bType))
        <* delimiter "%"
        <* keyword "1"
        <* (selfReport & rightArrow)
        <*> wrap (self & prefix_gadt_body)
    <|> Abstract.multiplicityFunctionType
        <$> wrap ((selfReport & bType) <|> Abstract.strictType <$ delimiter "!" <*> wrap (selfReport & bType))
        <* delimiter "%"
        <* notFollowedBy (keyword "1")
        <*> wrap (selfReport & aType)
        <* (selfReport & rightArrow)
        <*> wrap (self & prefix_gadt_body),
  gadtNewBody = (super & gadtNewBody)
    <|> Abstract.linearFunctionType
        <$> wrap ((selfReport & bType) <|> Abstract.strictType <$ delimiter "!" <*> wrap (selfReport & bType))
        <* delimiter "%"
        <* keyword "1"
        <* (selfReport & rightArrow)
        <*> wrap (self & return_type)
    <|> Abstract.multiplicityFunctionType
        <$> wrap ((selfReport & bType) <|> Abstract.strictType <$ delimiter "!" <*> wrap (selfReport & bType))
        <* delimiter "%"
        <* notFollowedBy (keyword "1")
        <*> wrap (selfReport & aType)
        <* (selfReport & rightArrow)
        <*> wrap (self & return_type)}

unicodeLinearTypesMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
unicodeLinearTypesMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} = super{
  arrowType = (super & arrowType)
    <|> Abstract.linearFunctionType
        <$> wrap (self & cType)
        <* delimiter "⊸"
        <*> wrap (self & arrowType)}

gadtUnicodeLinearTypesMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
gadtUnicodeLinearTypesMixin self@ExtendedGrammar{report = selfReport}
                            super@ExtendedGrammar{report = superReport} = super{
  prefix_gadt_body = (super & prefix_gadt_body)
    <|> Abstract.linearFunctionType
        <$> wrap ((selfReport & bType) <|> Abstract.strictType <$ delimiter "!" <*> wrap (selfReport & bType))
        <* delimiter "⊸"
        <*> wrap (self & prefix_gadt_body),
  gadtNewBody = (super & gadtNewBody)
    <|> Abstract.linearFunctionType
        <$> wrap ((selfReport & bType) <|> Abstract.strictType <$ delimiter "!" <*> wrap (selfReport & bType))
        <* delimiter "⊸"
        <*> wrap (self & return_type)}

standaloneKindSignaturesMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
standaloneKindSignaturesMixin self@ExtendedGrammar{report = selfReport}
                              super@ExtendedGrammar{report = superReport} = super{
   report= superReport{
      declarationLevel= (superReport & declarationLevel){
         topLevelDeclaration = (superReport & declarationLevel & topLevelDeclaration)
            <|> Abstract.kindSignature <$ keyword "type"
                  <*> (selfReport & typeConstructor)
                  <*> wrap (self & kindSignature)}}}

kindSignaturesMixin :: (OutlineMonoid t, Abstract.ExtendedHaskell l,
                        Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l))
                    => ExtensionOverlay l g t
kindSignaturesMixin
   self@ExtendedGrammar{
     report= selfReport@HaskellGrammar{
        declarationLevel= DeclarationGrammar{optionalContext, declaredConstructors, newConstructor,
                                             simpleType, typeClass, derivingClause, inClassDeclaration}}}
   super@ExtendedGrammar{report = superReport} =
   super{
      report= superReport{
         declarationLevel= (superReport & declarationLevel){
            topLevelDeclaration = (superReport & declarationLevel & topLevelDeclaration)
               <|> Abstract.kindedDataDeclaration <$ keyword "data"
                      <*> wrap optionalContext
                      <*> wrap simpleType
                      <*> wrap (kindSignature self)
                      <*> (delimiter "=" *> declaredConstructors <|> pure [])
                      <*> moptional derivingClause
               <|> Abstract.kindedNewtypeDeclaration <$ keyword "newtype"
                      <*> wrap optionalContext
                      <*> wrap simpleType
                      <*> wrap (kindSignature self)
                      <* delimiter "="
                      <*> wrap newConstructor
                      <*> moptional derivingClause},
         typeTerm = (superReport & typeTerm) <|>
            Abstract.kindedType <$> wrap (selfReport & typeTerm) <*> wrap (kindSignature self)},
      optionallyKindedAndParenthesizedTypeVar =
         Abstract.typeVariable <$> optionallyParenthesizedTypeVar self
         <|> parens (Abstract.kindedType
                     <$> wrap (Abstract.typeVariable <$> optionallyParenthesizedTypeVar self)
                     <*> wrap (kindSignature self)),
      optionallyKindedTypeVar =
         Abstract.typeVariable <$> (selfReport & typeVar)
         <|> Abstract.kindedType
             <$> wrap (Abstract.typeVariable <$> (selfReport & typeVar))
             <*> wrap (kindSignature self),
      typeVarBinder = typeVarBinder super
                      <|> parens (Abstract.explicitlyKindedTypeVariable
                                  <$> (selfReport & typeVar)
                                  <*> wrap (kindSignature self))}

existentialQuantificationMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
existentialQuantificationMixin self@ExtendedGrammar{report = selfReport}
                               super@ExtendedGrammar{report = superReport} = super{
   report= superReport{
      declarationLevel= (superReport & declarationLevel){
         declaredConstructor = (superReport & declarationLevel & declaredConstructor)
            <|> Abstract.existentialConstructor
                <$ keywordForall self
                <*> many (typeVarBinder self) <* delimiter "."
                <*> wrap (selfReport & declarationLevel & optionalContext)
                <*> wrap (superReport & declarationLevel & declaredConstructor)
            <|> Abstract.existentialConstructor []
                <$> wrap (selfReport & declarationLevel & context)
                <* (selfReport & rightDoubleArrow)
                <*> wrap (superReport & declarationLevel & declaredConstructor)}}}

scopedTypeVariablesMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
scopedTypeVariablesMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} = super{
   report= superReport{
      pattern = (superReport & pattern)
         <|> Abstract.typedPattern
                <$> wrap (self & infixPattern)
                <* (selfReport & doubleColon)
                <*> wrap (selfReport & typeTerm)}}

explicitForAllMixin :: (OutlineMonoid t, Abstract.ExtendedHaskell l,
                        Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l))
                    => ExtensionOverlay l g t
explicitForAllMixin
   self@ExtendedGrammar{
      report= selfReport@HaskellGrammar{
         declarationLevel= DeclarationGrammar{context, optionalContext, instanceDesignator}}}
   super@ExtendedGrammar{report = superReport} = super{
      report= superReport{
         declarationLevel= (superReport & declarationLevel){
            optionalTypeSignatureContext = pure Abstract.noContext,
            topLevelDeclaration = (superReport & declarationLevel & topLevelDeclaration)
               <|> Abstract.explicitlyScopedInstanceDeclaration <$ keyword "instance"
                   <* keywordForall self
                   <*> many (typeVarBinder self)
                   <* delimiter "."
                   <*> wrap optionalContext
                   <*> wrap instanceDesignator
                   <*> (keyword "where"
                        *> blockOf (wrap (selfReport & declarationLevel & inInstanceDeclaration))
                        <|> pure [])},
         typeVar = notFollowedBy (keywordForall self) *> (superReport & typeVar)},
      arrowType = arrowType super
         <|> Abstract.forallType <$ keywordForall self
             <*> many (typeVarBinder self) <* delimiter "."
             <*> wrap (arrowType self),
      keywordForall = keywordForall super <|> keyword "forall",
      optionalForall = keywordForall self *> many (typeVarBinder self) <* delimiter "." <<|> pure []}

gadtSyntaxMixin :: (OutlineMonoid t, Abstract.ExtendedHaskell l,
                    Deep.Foldable (Serialization (Down Int) t) (Abstract.GADTConstructor l l))
                => ExtensionOverlay l g t
gadtSyntaxMixin
   self@ExtendedGrammar{report= HaskellGrammar{declarationLevel= DeclarationGrammar{simpleType, derivingClause}}}
   super@ExtendedGrammar{report = superReport} = super{
      report= superReport{
         declarationLevel= (superReport & declarationLevel){
            topLevelDeclaration = (superReport & declarationLevel & topLevelDeclaration)
               <|> Abstract.gadtDeclaration <$ keyword "data"
                   <*> wrap simpleType
                   <*> optional (wrap $ kindSignature self) <* keyword "where"
                   <*> blockOf (wrap $ gadtConstructors self)
                   <*> moptional derivingClause
               <|> Abstract.gadtNewtypeDeclaration <$ keyword "newtype"
                   <*> wrap simpleType
                   <*> optional (wrap $ kindSignature self) <* keyword "where"
                   <*> wrap (gadtNewConstructor self)
                   <*> moptional derivingClause}},
      optionalForall = keywordForall self *> many (typeVarBinder self) <* delimiter "." <<|> pure []}

gadtSyntaxTypeOperatorsMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
gadtSyntaxTypeOperatorsMixin self@ExtendedGrammar{report = selfReport}
                             super@ExtendedGrammar{report = superReport} = super{
   prefix_gadt_body = (super & prefix_gadt_body)
      <|> Abstract.functionType
             <$> wrap (Abstract.infixTypeApplication
                          <$> wrap (self & cType)
                          <*> (selfReport & qualifiedOperator)
                          <*> wrap (selfReport & bType))
             <* (self & (Report.rightArrow . report))
             <*> wrap (self & prefix_gadt_body),
   return_type = return_type super <|>
       Abstract.infixTypeApplication <$> wrap (arg_type self)
                                     <*> (selfReport & qualifiedOperator)
                                     <*> wrap (arg_type self)}

dataKindsGadtSyntaxTypeOperatorsMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
dataKindsGadtSyntaxTypeOperatorsMixin self@ExtendedGrammar{report = selfReport}
                                      super@ExtendedGrammar{report = superReport} =
   super{
      return_type = return_type super <|>
         Abstract.promotedInfixTypeApplication
         <$> wrap (arg_type self)
         <* terminator "'"
         <*> (selfReport & qualifiedOperator)
         <*> wrap (arg_type self)}

namedFieldPunsMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
namedFieldPunsMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} =
   super{
      report = superReport{
         fieldBinding = (superReport & fieldBinding) <|>
            Abstract.punnedFieldBinding <$> (selfReport & qualifiedVariable),
         fieldPattern = (superReport & fieldPattern) <|>
            Abstract.punnedFieldPattern <$> (selfReport & qualifiedVariable)}}

recordWildCardsMixin :: Abstract.ExtendedWith '[ 'RecordWildCards ] l => ExtensionOverlay l g t
recordWildCardsMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} =
   super{
     conArgPattern = (super & conArgPattern)
        <|> Abstract.wildcardRecordPattern' Abstract.build <$> (selfReport & qualifiedConstructor)
            <*> braces (wrap (selfReport & fieldPattern) `endBy` comma <* delimiter ".."),
      report = superReport{
         bareExpression = (superReport & bareExpression)
            <|> Abstract.wildcardRecordExpression' Abstract.build <$> (selfReport & qualifiedConstructor)
                <*> braces (wrap (selfReport & fieldBinding) `endBy` comma <* delimiter "..")}}

overloadedRecordDotMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
overloadedRecordDotMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} =
   super{
      report = superReport{
         qualifiedVariableSymbol =
            notFollowedBy (string "." *> satisfyCharInput varStart) *> (superReport & qualifiedVariableSymbol),
         bareExpression = (superReport & bareExpression) <|>
            Abstract.getField <$> (selfReport & aExpression) <* prefixDot <*> (selfReport & variableIdentifier)
            <|>
            Abstract.fieldProjection <$> parens (someNonEmpty $ prefixDot *> (selfReport & variableIdentifier))}}
   where prefixDot = void (string "."
                           <* lookAhead (satisfyCharInput varStart)
                           <* lift ([[Token Modifier "."]], ()))
                     <?> "prefix ."

implicitParametersMixin :: Abstract.ExtendedWith '[ 'ImplicitParameters ] l => ExtensionOverlay l g t
implicitParametersMixin self@ExtendedGrammar{report = selfReport}
                        super@ExtendedGrammar{report= superReport@HaskellGrammar{declarationLevel, bareExpression}} =
   super{
      implicitParameterConstraint =
         Abstract.implicitParameterConstraint Abstract.build
         <$ delimiter "?"
         <*> (selfReport & variableIdentifier)
         <* (selfReport & doubleColon)
         <*> wrap (selfReport & typeTerm),
      report = superReport{
          declarationLevel = declarationLevel{
              declaration = declaration declarationLevel
                 <|> Abstract.implicitParameterDeclaration Abstract.build
                     <$ delimiter "?"
                     <*> (selfReport & variableIdentifier)
                     <* delimiter "="
                     <*> (selfReport & expression)},
          bareExpression = bareExpression
             <|> Abstract.implicitParameterExpression Abstract.build
                 <$ delimiter "?" <*> (selfReport & variableIdentifier),
          qualifiedVariableSymbol = notFollowedBy (delimiter "?") *> (superReport & qualifiedVariableSymbol)}}

bangPatternsMixin :: (SpaceMonoid t, Abstract.ExtendedWith '[ 'BangPatterns ] l) => ExtensionOverlay l g t
bangPatternsMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} =
   super{
      conArgPattern = (super & conArgPattern)
         <|> Abstract.bangPattern Abstract.build <$ bang <*> wrap (self & conArgPattern),
      report = superReport{
         variableOperator = notFollowedBy bang *> (superReport & variableOperator)}}
   where bang = filter precededByOpenSpace getInput
                *> string "!"
                <* notSatisfyChar (\c-> Char.isSpace c || isSymbol c)
                <* notFollowedBy Report.comment
                <* lift ([[Token Delimiter "!"]], ())

viewPatternsMixin :: Abstract.ExtendedWith '[ 'ViewPatterns ] l => ExtensionOverlay l g t
viewPatternsMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} =
   super{
      report = superReport{
         pPattern = (superReport & pPattern)
            <|> Abstract.viewPattern Abstract.build
                <$> (selfReport & expression)
                <* (selfReport & rightArrow)
                <*> wrap (selfReport & pPattern)}}

nPlusKPatternsMixin :: Abstract.ExtendedWith '[ 'NPlusKPatterns ] l => ExtensionOverlay l g t
nPlusKPatternsMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} =
   super{
      report = superReport{
         pPattern = (superReport & pPattern)
            <|> Abstract.nPlusKPattern Abstract.build
                <$> (selfReport & variable)
                <* delimiter "+"
                <*> (selfReport & integer)}}

patternSynonymsMixin :: forall l g t. (OutlineMonoid t, Abstract.ExtendedWith '[ 'PatternSynonyms ] l,
                                       Deep.Foldable (Serialization (Down Int) t) (Abstract.PatternEquationClause l l),
                                       Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l))
                     => ExtensionOverlay l g t
patternSynonymsMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} =
   super{
      report= superReport{
         moduleLevel= (superReport & moduleLevel){
            export = (superReport & moduleLevel & export)
               <|> Abstract.exportPattern Abstract.build <$ keyword "pattern"
                   <*> (selfReport & qualifiedConstructor),
            importItem = (superReport & moduleLevel & importItem)
               <|> Abstract.importPattern Abstract.build <$ keyword "pattern"
                   <*> (selfReport & constructor),
            members = (superReport & moduleLevel & members)
               <|> parens (Abstract.allMembersPlus Abstract.build
                           <$> filter (not . null)
                                  (moptional ((selfReport & moduleLevel & cname) `sepBy` comma <* comma)
                                   <> ([] <$ delimiter "..")
                                   <> moptional (comma *> (selfReport & moduleLevel & cname) `sepEndBy` comma)))},
         declarationLevel= (superReport & declarationLevel){
            topLevelDeclaration = (superReport & declarationLevel & topLevelDeclaration)
               <|> keyword "pattern" *>
               (Abstract.implicitPatternSynonym Abstract.build
                   <$> wrap lhsPattern <* delimiter "=" <*> wrap (selfReport & pattern)
                <|> Abstract.unidirectionalPatternSynonym Abstract.build
                       <$> wrap lhsPattern <* (selfReport & leftArrow) <*> wrap (selfReport & pattern)
                <|> Abstract.explicitPatternSynonym Abstract.build
                       <$> wrap lhsPattern
                       <* (selfReport & leftArrow)
                       <*> wrap (selfReport & pattern)
                       <*> patternClauses
                <|> Abstract.patternSynonymSignature Abstract.build
                       <$> (selfReport & constructor) `sepByNonEmpty` comma
                       <* (selfReport & doubleColon)
                       <*> optionalForall self
                       <*> wrap ((selfReport & declarationLevel & context) <* (selfReport & rightDoubleArrow)
                                 <<|> pure Abstract.noContext)
                       <*> optionalForall self
                       <*> wrap (selfReport & declarationLevel & optionalContext)
                       <*> many (wrap (self & cType) <* (selfReport & rightArrow))
                       <*> wrap (self & cType))},
         variableIdentifier = notFollowedBy (keyword "pattern") *> (superReport & variableIdentifier)}}
   where lhsPattern =
            Abstract.prefixPatternLHS Abstract.build
               <$> (selfReport & constructor)
               <*> many (selfReport & variableIdentifier)
            <|> Abstract.infixPatternLHS Abstract.build
                   <$> (selfReport & variableIdentifier)
                   <*> (selfReport & constructorOperator)
                   <*> (selfReport & variableIdentifier)
            <|> Abstract.recordPatternLHS Abstract.build
                   <$> (selfReport & constructor)
                   <*> braces ((selfReport & variable) `sepBy` comma)
         patternClauses = keyword "where" *> blockOf (wrap patternClause)
         patternClause = Abstract.patternEquationClause @l Abstract.build
                         <$> wrap patternClauseLHS
                         <*> wrap (selfReport & rhs)
                         <*> (selfReport & declarationLevel & whereClauses)
         patternClauseLHS =
            Abstract.prefixPatternEquationLHS Abstract.build
               <$> (selfReport & constructor)
               <*> many (wrap (selfReport & pattern))
            <|> Abstract.infixPatternEquationLHS Abstract.build
                   <$> wrap (selfReport & pattern)
                   <*> (selfReport & constructorOperator)
                   <*> wrap (selfReport & pattern)

standaloneDerivingMixin :: Abstract.ExtendedWith '[ 'StandaloneDeriving ] l => ExtensionOverlay l g t
standaloneDerivingMixin self@ExtendedGrammar{
                           report= HaskellGrammar{
                              declarationLevel= DeclarationGrammar{optionalContext, instanceDesignator}}}
                        super@ExtendedGrammar{report= report@HaskellGrammar{declarationLevel}} =
   super{
      report = report{
         declarationLevel= declarationLevel{
            topLevelDeclaration = (declarationLevel & topLevelDeclaration)
               <|> Abstract.standaloneDerivingDeclaration Abstract.build <$ keyword "deriving" <* keyword "instance"
                   <*> optionalForall self
                   <*> wrap optionalContext
                   <*> wrap instanceDesignator}}}

derivingStrategiesMixin :: forall l g t. Abstract.ExtendedWith '[ 'DerivingStrategies ] l => ExtensionOverlay l g t
derivingStrategiesMixin self@ExtendedGrammar{
                           report= selfReport@HaskellGrammar{generalConstructor, typeTerm}}
                        super@ExtendedGrammar{report= superReport@HaskellGrammar{declarationLevel}} =
   super{
      report = superReport{
         declarationLevel= declarationLevel{
            derivingClause = concatSome (self & singleDerivingClause)}},
      singleDerivingClause = (declarationLevel & derivingClause)
         <|> takeSome (wrap $
                       Abstract.strategicDerive Abstract.build <$ keyword "deriving"
                       <*> wrap (self & derivingStrategy)
                       <*> (pure <$> wrap (Abstract.constructorType <$> wrap generalConstructor)
                            <<|> parens (wrap typeTerm `sepBy` comma))),
      derivingStrategy = Abstract.stockStrategy @l Abstract.build <$ keyword "stock"
                         <|> Abstract.anyClassStrategy @l Abstract.build <$ keyword "anyclass"
                         <|> Abstract.newtypeStrategy @l Abstract.build <$ keyword "newtype"}

standaloneDerivingStrategiesMixin :: (Abstract.ExtendedWith '[ 'StandaloneDeriving ] l,
                                      Abstract.ExtendedWith '[ 'DerivingStrategies ] l)
                                  => ExtensionOverlay l g t
standaloneDerivingStrategiesMixin self@ExtendedGrammar{
                                     report= HaskellGrammar{
                                        declarationLevel= DeclarationGrammar{optionalContext, instanceDesignator}}}
                                  super@ExtendedGrammar{report= report@HaskellGrammar{declarationLevel}} =
   super{
      report = report{
         declarationLevel= declarationLevel{
            topLevelDeclaration = (declarationLevel & topLevelDeclaration)
               <|> Abstract.standaloneStrategicDerivingDeclaration Abstract.build
                      <$ keyword "deriving"
                      <*> wrap (self & derivingStrategy)
                      <* keyword "instance"
                      <*> optionalForall self
                      <*> wrap optionalContext
                      <*> wrap instanceDesignator}}}

derivingViaMixin :: forall l g t. Abstract.ExtendedWith '[ 'DerivingVia ] l => ExtensionOverlay l g t
derivingViaMixin self@ExtendedGrammar{
                    report= HaskellGrammar{generalConstructor, typeTerm}}
                 super@ExtendedGrammar{report= HaskellGrammar{declarationLevel}} =
   super{
      singleDerivingClause =
         singleDerivingClause super
         <|> takeSome (wrap $
                       Abstract.deriveVia Abstract.build <$ keyword "deriving"
                       <*> (parens (wrap typeTerm `sepBy` comma)
                            <<|> pure <$> wrap (Abstract.constructorType <$> wrap generalConstructor))
                       <* keyword "via"
                       <*> wrap typeTerm)}

standaloneDerivingViaMixin :: forall l g t. (Abstract.ExtendedWith '[ 'StandaloneDeriving ] l,
                                             Abstract.ExtendedWith '[ 'DerivingStrategies ] l,
                                             Abstract.ExtendedWith '[ 'DerivingVia ] l)
                           => ExtensionOverlay l g t
standaloneDerivingViaMixin self@ExtendedGrammar{
                              report= selfReport@HaskellGrammar{
                                 declarationLevel= DeclarationGrammar{optionalContext, instanceDesignator}}}
                           super@ExtendedGrammar{report= superReport@HaskellGrammar{declarationLevel}} =
   super{
      report = superReport{
         declarationLevel= declarationLevel{
            topLevelDeclaration = (declarationLevel & topLevelDeclaration)
               <|> Abstract.standaloneStrategicDerivingDeclaration Abstract.build
                      <$ keyword "deriving"
                      <*> wrap derivingVia
                      <* keyword "instance"
                      <*> optionalForall self
                      <*> wrap optionalContext
                      <*> wrap instanceDesignator}}}
   where derivingVia = Abstract.derivingViaStrategy @l Abstract.build <$ keyword "via"
                       <*> wrap (selfReport & typeTerm)

functionalDependenciesMixin :: forall l g t. (OutlineMonoid t, Abstract.ExtendedWith '[ 'FunctionalDependencies ] l,
                                              Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l))
                            => ExtensionOverlay l g t
functionalDependenciesMixin
   self@ExtendedGrammar{
     typeVarBinder,
     report= HaskellGrammar{
        rightArrow, typeVar,
        declarationLevel= DeclarationGrammar{optionalContext, classLHS, inClassDeclaration}}}
   super@ExtendedGrammar{report = superReport} =
   super{
      report= superReport{
         declarationLevel= (superReport & declarationLevel){
            topLevelDeclaration = (superReport & declarationLevel & topLevelDeclaration)
               <|> Abstract.fundepClassDeclaration Abstract.build
                      <$ keyword "class"
                      <*> wrap optionalContext
                      <*> wrap classLHS
                      <* delimiter "|"
                      <*> wrap (Abstract.functionalDependency Abstract.build
                                   <$> many typeVar <* rightArrow <*> many typeVar)
                         `sepBy` comma
                      <*> moptional (keyword "where" *> blockOf (wrap inClassDeclaration))}}}

constraintsAreTypesMixin :: forall l g t. Abstract.ExtendedHaskell l => ExtensionOverlay l g t
constraintsAreTypesMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} =
   super{
      cType = cType super <|> Abstract.constraintType <$> wrap (equalityConstraint self),
      report= superReport{
         typeTerm = (superReport & typeTerm) <|> Abstract.constraintType <$> wrap (implicitParameterConstraint self),
         declarationLevel= (superReport & declarationLevel){
            context = (selfReport & declarationLevel & constraint),
            constraint = Abstract.typeConstraint <$> wrap (self & cType)}}}

instanceSignaturesMixin :: ExtensionOverlay l g t
instanceSignaturesMixin
   self@ExtendedGrammar{
      report= HaskellGrammar{doubleColon, typeTerm,
                             declarationLevel= DeclarationGrammar{optionalTypeSignatureContext, variables}}}
   super@ExtendedGrammar{report = superReport} =
   super{
      report= superReport{
         declarationLevel= (superReport & declarationLevel){
            inInstanceDeclaration = (superReport & declarationLevel & inInstanceDeclaration)
               <|> Abstract.typeSignature <$> variables <* doubleColon <*> wrap optionalTypeSignatureContext
                                          <*> wrap typeTerm}}}

defaultSignaturesMixin :: Abstract.ExtendedWith '[ 'DefaultSignatures ] l => ExtensionOverlay l g t
defaultSignaturesMixin
   self@ExtendedGrammar{
      report= HaskellGrammar{doubleColon, typeTerm, variable,
                             declarationLevel= DeclarationGrammar{optionalTypeSignatureContext}}}
   super@ExtendedGrammar{report = superReport} =
   super{
      report= superReport{
         declarationLevel= (superReport & declarationLevel){
            inClassDeclaration = (superReport & declarationLevel & inClassDeclaration)
               <|> Abstract.defaultMethodSignature Abstract.build <$ keyword "default"
                      <*> variable <* doubleColon <*> wrap optionalTypeSignatureContext <*> wrap typeTerm}}}

-- | Not an extension by itself, common to magicHashMixin and negativeLiteralsMixin.
negationConstraintMixin :: Parser g t t -> ExtensionOverlay l g t
negationConstraintMixin prefixMinusFollow self super@ExtendedGrammar{report = superReport} = super{
   report= superReport{
      variableSymbol = negationGuard *> (superReport & variableSymbol),
      qualifiedVariableSymbol = negationGuard *> (superReport & qualifiedVariableSymbol),
      prefixNegation = negationGuard *> (superReport & prefixNegation)}}
   where negationGuard = notFollowedBy (string "-" *> prefixMinusFollow)

nondecreasingIndentationMixin :: (Deep.Foldable (Serialization (Down Int) t) (Abstract.Expression l l),
                                  Deep.Foldable (Serialization (Down Int) t) (Abstract.Statement l l),
                                  OutlineMonoid t)
                              => ExtensionOverlay l g t
nondecreasingIndentationMixin self@ExtendedGrammar{report = selfReport} super@ExtendedGrammar{report = superReport} =
   super{
      report = superReport{
         statements = Report.blockWith nonDecreasingIndentLine Report.blockTerminatorKeyword (selfReport & statement)
                      >>= Report.verifyStatements}}

variableLexeme, constructorLexeme, identifierTail :: (Rank2.Apply g, Ord t, Show t, TextualMonoid t) => Parser g t t
variableLexeme = filter (`Set.notMember` Report.reservedWords) (satisfyCharInput varStart <> identifierTail)
                 <?> "variable"
constructorLexeme = satisfyCharInput Char.isUpper <> identifierTail <?> "constructor"
identifierTail = takeCharsWhile isNameTailChar

varStart :: Char -> Bool
varStart c = (Char.isLetter c && not (Char.isUpper c)) ||  c == '_'

isNameTailChar :: Char -> Bool
isNameTailChar c = Report.isNameTailChar c || Char.isMark c

whiteSpaceTrailing :: (Show t, Factorial.Factorial t, Deep.Foldable (Serialization (Down Int) t) node)
                   => NodeWrap t (node (NodeWrap t) (NodeWrap t)) -> Bool
whiteSpaceTrailing node = case lexemes node of
  ws@(_:_) -> case last ws of
    WhiteSpace{} -> True
    Comment{} -> True
    _ -> False
  _ -> False

rewrap2 :: (NodeWrap t a -> NodeWrap t b -> c) -> NodeWrap t a -> NodeWrap t b -> NodeWrap t c
rewrap2 f node1@((start, _, _), _) node2@((_, _, end), _) = ((start, mempty, end), f node1 node2)

blockOf' :: (Rank2.Apply g, Ord t, Show t, OutlineMonoid t, LexicalParsing (Parser g t),
             Deep.Foldable (Serialization (Down Int) t) node)
         => Parser g t (NodeWrap t (node (NodeWrap t) (NodeWrap t)))
         -> Parser g t [NodeWrap t (node (NodeWrap t) (NodeWrap t))]
blockOf' p = braces (many (many semi *> p) <* many semi) <|> (inputColumn >>= alignedBlock pure)
   where alignedBlock cont indent =
            do rest <- getInput
               item <- filter (oneExtendedLine indent rest) p
               -- don't stop at a higher indent unless there's a terminator
               void (filter (indent >=) inputColumn)
                  <<|> lookAhead (void (Text.Parser.Char.satisfy (`elem` terminators))
                                  <|> string "|" *> notSatisfyChar isSymbol
                                  <|> (string "else" <|> string "in"
                                       <|> string "of" <|> string "where") *> notSatisfyChar isNameTailChar
                                  <|> eof)
               indent' <- inputColumn
               let cont' = cont . (item :)
               if indent == indent'
                  then many semi *> alignedBlock cont' indent
                  else if indent < indent'
                  then many semi *> alignedBlock cont' indent <<|> cont' []
                  else some semi *> alignedBlock cont' indent <<|> cont' []
            <<|> cont []
         terminators :: [Char]
         terminators = ",;)]}"

nonDecreasingIndentLine :: (Ord t, Show t, OutlineMonoid t, Deep.Foldable (Serialization (Down Int) t) node)
                        => Int -> t -> NodeWrap t (node (NodeWrap t) (NodeWrap t)) -> Bool
nonDecreasingIndentLine indent _input node = allIndented False (lexemes node) where
   allIndented nested (WhiteSpace _ : Token Delimiter _tok : rest) = allIndented nested rest
   allIndented nested (WhiteSpace ws : rest@(Token _ tok : _))
      | Textual.all Report.isLineChar ws = allIndented nested rest
      | tokenIndent < indent = False
      | tokenIndent == indent && not nested && tok `Set.notMember` Report.reservedWords
        && all (`notElem` terminators) (Textual.characterPrefix tok) = False
      where tokenIndent = Report.currentColumn (Factorial.dropWhile (const True) ws)
   allIndented False (Token Keyword k : rest)
      | k `elem` ["do", "of", "let", "where"] = allIndented True rest
   allIndented nested (x : rest) = allIndented nested rest
   allIndented _ [] = True
   terminators :: [Char]
   terminators = ",;)]}"
