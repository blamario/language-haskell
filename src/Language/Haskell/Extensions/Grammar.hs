{-# Language DataKinds, FlexibleContexts, FlexibleInstances,
             NamedFieldPuns, NoFieldSelectors, OverloadedRecordDot, OverloadedStrings,
             Rank2Types, RecordWildCards, ScopedTypeVariables,
             TemplateHaskell, TupleSections, TypeApplications, TypeFamilies, TypeOperators, TypeSynonymInstances #-}

-- | The grammar(s) of Haskell with syntactic extensions.
--
-- The following extensions are not implemented, mainly due to not being supported by TemplateHaskell:
--
-- * 'TransformListComp'
-- * 'OverloadedRecordUpdate'
-- * 'Arrows'
-- * 'TemplateHaskell' and 'TemplateHaskellQuotes'

module Language.Haskell.Extensions.Grammar (ExtendedGrammar(report), extendedGrammar, parseModule, NodeWrap) where

import Control.Applicative
import Control.Monad (void)
import qualified Data.Char as Char
import Data.Bifunctor (first)
import Data.Foldable (fold, toList)
import Data.Function ((&))
import Data.Functor.Compose (Compose(getCompose))
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Ord (Down)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Function.Memoize (memoize)
import Data.Monoid (Endo(..))
import Data.Monoid.Cancellative (RightReductive, commonPrefix, isPrefixOf, isSuffixOf, stripPrefix, stripSuffix)
import Data.Monoid.Instances.Positioned (LinePositioned, column)
import Data.Monoid.Instances.PrefixMemory (Shadowed (content, prefix))
import Data.Monoid.Textual (TextualMonoid, toString)
import qualified Data.Monoid.Factorial as Factorial
import qualified Data.Monoid.Textual as Textual
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Set (Set)
import Data.String (fromString)
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

-- | Contains the refactored Haskell2010 'HaskellGrammar' with additional fields for language extensions
data ExtendedGrammar l t f p = ExtendedGrammar {
   report :: HaskellGrammar l t f p,
   extensions :: GrammarExtensions l t f p}

data GrammarExtensions l t f p = GrammarExtensions {
   singleDerivingClause :: p [f (Abstract.DerivingClause l l f f)],
   keywordForall :: p (),
   kindSignature :: p (Abstract.Kind l l f f),
   groundTypeKind :: p (),
   cType, arrowType :: p (Abstract.Type l l f f),
   promotedLiteral, promotedStructure :: p (Abstract.Type l l f f),
   equalityConstraint, implicitParameterConstraint :: p (Abstract.Context  l l f f),
   typedPattern, infixPattern :: p (Abstract.Pattern l l f f),
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
$(Rank2.TH.deriveAll ''GrammarExtensions)

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
     (Set.fromList [NamedDefaults],                  [(9, namedDefaultsMixin)]),
     (Set.fromList [NegativeLiterals],               [(2, negativeLiteralsMixin)]),
     (Set.fromList [LexicalNegation],                [(3, lexicalNegationMixin)]),
     (Set.fromList [MagicHash],                      [(3, magicHashMixin)]),
     (Set.fromList [ParallelListComprehensions],     [(3, parallelListComprehensionsMixin)]),
     (Set.fromList [ExtendedLiterals],               [(4, extendedLiteralsMixin)]),
     (Set.fromList [MultilineStrings],               [(4, multilineStringsMixin)]),
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
     (Set.fromList [StrictData],                     [(9, strictDataMixin)]),
     (Set.fromList [Strict],                         [(9, strictMixin)]),
     (Set.fromList [BangPatterns],                   [(9, bangPatternsMixin)]),
     (Set.fromList [OrPatterns],                     [(9, orPatternsMixin)]),
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

-- | Parse a Haskell module using the grammar corresponding to the given set of language extensions and the
-- extensions specified by the module's @LANGUAGE@ pragmas.
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
           $ parseResults $ (.report.haskellModule)
           $ parseComplete (extendedGrammar $ positiveKeys $ withImplications $ extensionMap <> extensions) source
        else Left mempty{errorAlternatives= ["Contradictory extension switches " <> show (toList contradictions)]}
   Right extensionses -> error ("Ambiguous extensions: " <> show extensionses)
   where moduleExtensions = parseResults $ fmap snd $ getCompose $ simply parsePrefix languagePragmas source
         parseResults = getCompose . fmap snd . getCompose
         positiveKeys = Map.keysSet . Map.filter id
         getSwitch (ExtensionSwitch s) = s

-- | Construct the Haskell grammar corresponding to the given set of language extensions
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
         extendedReport g = g{report = Report.grammar g.report}

-- | Reorganize the grammar to make it more extensible, without adding any extensions
initialOverlay :: forall l g t. (Abstract.ExtendedHaskell l,
                                 Deep.Foldable (Serialization (Down Int) t) (Abstract.CaseAlternative l l),
                                 Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l),
                                 Deep.Foldable (Serialization (Down Int) t) (Abstract.Expression l l),
                                 Deep.Foldable (Serialization (Down Int) t) (Abstract.Import l l),
                                 Deep.Foldable (Serialization (Down Int) t) (Abstract.Statement l l))
               => ExtensionOverlay l g t
initialOverlay self super = super{
   report= super.report{
      declarationLevel= super.report.declarationLevel{
         classLHS = self.report.declarationLevel.simpleType,
         simpleType =
            Abstract.simpleTypeLHS <$> self.report.typeConstructor <*> pure []
            <|> Abstract.infixTypeLHSApplication
                <$> self.extensions.typeVarBinder
                <* terminator "`"
                <*> self.report.declarationLevel.typeClass
                <* terminator "`"
                <*> self.extensions.typeVarBinder
            <|> Abstract.typeLHSApplication
                <$> wrap self.report.declarationLevel.simpleType
                <*> self.extensions.typeVarBinder,
         optionalTypeSignatureContext = pure Abstract.noContext,
         context =
            self.report.declarationLevel.constraint
            <|> self.extensions.equalityConstraint
            <|> Abstract.constraints <$> parens (wrap (self.report.declarationLevel.constraint
                                                       <|> self.extensions.equalityConstraint
                                                       <|> self.extensions.implicitParameterConstraint) `sepBy` comma),
         instanceDesignator = self.extensions.instanceDesignatorApplications,
         instanceTypeDesignator = self.report.aType},
      pattern = self.extensions.typedPattern,
      lPattern = self.report.aPattern
                 <|> Abstract.literalPattern
                     <$ delimiter "-"
                     <*> wrap ((Abstract.integerLiteral . negate) <$> self.report.integer
                               <|> (Abstract.floatingLiteral . negate) <$> self.report.float)
                 <|> Abstract.constructorPattern
                     <$> wrap self.report.generalConstructor
                     <*> some (wrap self.extensions.conArgPattern),
      aPattern = self.extensions.conArgPattern,
      -- NoListTuplePuns
      aType = self.report.generalTypeConstructor
              <|> Abstract.typeVariable <$> self.report.typeVar
              <|> parens self.report.typeTerm
              <|> Abstract.typeWildcard <$ keyword "_"
              <|> Abstract.groundType <$ self.extensions.groundTypeKind,
      generalTypeConstructor =
         Abstract.constructorType <$> wrap (Abstract.constructorReference <$> self.report.qualifiedConstructor)
         <|> Abstract.functionConstructorType <$ parens self.report.rightArrow,
      typeTerm = self.extensions.arrowType},
   extensions = super.extensions{
      keywordForall = empty,
      kindSignature = empty,
      groundTypeKind = empty,
      derivingStrategy = empty,
      arrowType = self.extensions.cType
         <|> Abstract.functionType <$> wrap self.extensions.cType
                                   <* self.report.rightArrow
                                   <*> wrap self.extensions.arrowType
         <|> Abstract.constrainedType <$> wrap self.report.declarationLevel.context
                                      <* self.report.rightDoubleArrow
                                      <*> wrap self.extensions.arrowType,
      cType = self.report.bType,
      equalityConstraint = empty,
      implicitParameterConstraint = empty,
      typedPattern = self.extensions.infixPattern,
      infixPattern =
         Abstract.infixPattern <$> wrap self.report.lPattern
                               <*> self.report.qualifiedConstructorOperator
                               <*> wrap self.extensions.infixPattern
         <|> self.report.lPattern,
      promotedLiteral = empty,
      promotedStructure = empty,
      inClassOrInstanceTypeFamilyDeclaration = empty,
      instanceDesignatorBase =
         Abstract.classReferenceInstanceLHS <$> self.report.declarationLevel.qualifiedTypeClass
         <|> parens self.report.declarationLevel.instanceDesignator,
      instanceDesignatorApplications =
         self.extensions.instanceDesignatorBase
         <|> Abstract.classInstanceLHSApplication
             <$> wrap self.extensions.instanceDesignatorApplications
             <*> wrap self.report.declarationLevel.instanceTypeDesignator,
      optionalForall = pure [],
      optionallyParenthesizedTypeVar = self.report.typeVar,
      optionallyKindedAndParenthesizedTypeVar =
         Abstract.typeVariable <$> self.extensions.optionallyParenthesizedTypeVar,
      optionallyKindedTypeVar = empty,
      typeVarBinder = Abstract.implicitlyKindedTypeVariable <$> self.report.typeVar,
      gadtConstructors =
         Abstract.gadtConstructors <$> self.extensions.constructorIDs
                                   <* self.report.doubleColon
                                   <*> self.extensions.optionalForall
                                   <*> wrap self.report.declarationLevel.optionalContext
                                   <*> wrap self.extensions.gadtBody,
      gadtNewConstructor =
         Abstract.gadtConstructors <$> ((:|[]) <$> self.report.constructor) <* self.report.doubleColon
                                   <*> self.extensions.optionalForall
                                   <*> wrap (pure Abstract.noContext
                                             <|> self.report.declarationLevel.context
                                                 *> self.report.rightDoubleArrow
                                                 *> fail "No context allowed on GADT newtype")
                                   <*> wrap self.extensions.gadtNewBody,
      constructorIDs = self.report.constructor `sepByNonEmpty` comma,
      gadtNewBody =
         parens self.extensions.gadtNewBody
         <|> Abstract.functionType
             <$> wrap self.report.bType
             <* self.report.rightArrow
             <*> wrap (self.extensions.return_type)
         <|> Abstract.recordFunctionType
             <$> braces ((:[]) <$> wrap self.report.declarationLevel.fieldDeclaration)
             <* self.report.rightArrow
             <*> wrap (self.extensions.return_type),
      gadtBody = (self.extensions.prefix_gadt_body) <|> (self.extensions.record_gadt_body),
      prefix_gadt_body =
         parens (self.extensions.prefix_gadt_body)
         <|> (self.extensions.return_type)
         <|> Abstract.functionType
             <$> wrap (self.report.bType <|> self.report.declarationLevel.strictType)
             <* self.report.rightArrow
             <*> wrap (self.extensions.prefix_gadt_body),
      record_gadt_body =
         parens (self.extensions.record_gadt_body)
         <|> Abstract.recordFunctionType
             <$> braces (wrap self.report.declarationLevel.fieldDeclaration `sepBy` comma)
             <* self.report.rightArrow
             <*> wrap (self.extensions.return_type),
      return_type = Abstract.typeApplication
                       <$> wrap ((self.extensions.return_type) <|> parens (self.extensions.return_type))
                       <*> wrap (self.extensions.arg_type)
                    <|> self.report.generalTypeConstructor,
      conArgPattern = super.report.aPattern,
      arg_type = self.report.aType,
      binary = empty}}

identifierSyntaxMixin :: ExtensionOverlay l g t
identifierSyntaxMixin self super = super{
   report= super.report{
      variableIdentifier = token (Abstract.name . Text.pack . toString mempty <$> variableLexeme),
      constructorIdentifier = token (Abstract.name . Text.pack . toString mempty <$> constructorLexeme),
      variableSymbol = token (Abstract.name . Text.pack . toString mempty <$> Report.variableSymbolLexeme),
      constructorSymbol = token (Abstract.name . Text.pack . toString mempty <$> Report.constructorSymbolLexeme)}}

overloadedLabelsMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
overloadedLabelsMixin self super = super{
   report= super.report{
      bareExpression = super.report.bareExpression
                       <|> Abstract.overloadedLabel . Text.pack . toString mempty
                           <$> token (string "#" *> variableLexeme),
      variableSymbol = notFollowedBy (string "#" *> variableLexeme) *> super.report.variableSymbol}}

unicodeSyntaxMixin :: ExtensionOverlay l g t
unicodeSyntaxMixin self super = super{
   extensions = super.extensions{
      keywordForall = super.extensions.keywordForall <|> delimiter "∀"},
   report= super.report{
      doubleColon = super.report.doubleColon <|> delimiter "∷",
      rightDoubleArrow = super.report.rightDoubleArrow <|> delimiter "⇒",
      rightArrow = super.report.rightArrow <|> delimiter "→",
      leftArrow = super.report.leftArrow <|> delimiter "←",
      variableSymbol = notSatisfyChar (`elem` ("∀←→⇒∷★" :: [Char])) *> super.report.variableSymbol}}

listTuplePunsMixin :: forall l g t. ExtensionOverlay l g t
listTuplePunsMixin self super = super{
   report= super.report{
      aType = super.report.aType
              <|> Abstract.tupleType <$> parens ((:|) <$> wrap self.report.typeTerm
                                                      <*> some (comma *> wrap self.report.typeTerm))
              <|> Abstract.listType <$> brackets (wrap self.report.typeTerm),
      generalTypeConstructor = super.report.generalTypeConstructor
                               <|> Abstract.constructorType
                                   <$> wrap (Abstract.unitConstructor <$ terminator "(" <* terminator ")"
                                             <|> Abstract.emptyListConstructor <$ terminator "[" <* terminator "]"
                                             <|> Abstract.tupleConstructor . succ . length <$> parens (some comma))}}

unboxedTuplesMixin :: forall l g t. Abstract.ExtendedWith '[ 'UnboxedTuples ] l => ExtensionOverlay l g t
unboxedTuplesMixin self super = super{
   extensions = super.extensions{
      conArgPattern = super.extensions.conArgPattern
                      <|> Abstract.unboxedTuplePattern Abstract.build
                          <$> hashParens (wrap self.report.pPattern `sepByNonEmpty` comma)},
   report= super.report{
      generalConstructor = super.report.generalConstructor
                           <|> Abstract.unboxedTupleConstructor Abstract.build . succ . length
                               <$> hashParens (many comma),
      bareExpression = super.report.bareExpression
                       <|> Abstract.unboxedTupleExpression Abstract.build
                           <$> hashParens (self.report.expression `sepByNonEmpty` comma)}}
   where hashParens p = delimiter "(#" *> p <* terminator "#)"

unboxedListTuplePunsMixin :: forall l g t. Abstract.ExtendedWith '[ 'UnboxedTuples ] l => ExtensionOverlay l g t
unboxedListTuplePunsMixin self super = super{
   report= super.report{
      aType = super.report.aType
              <|> Abstract.unboxedTupleType Abstract.build
                  <$> hashParens (wrap self.report.typeTerm `sepByNonEmpty` comma),
      generalTypeConstructor = super.report.generalTypeConstructor
                               <|> Abstract.constructorType
                                   <$> wrap (Abstract.unboxedTupleConstructor Abstract.build . succ . length
                                             <$> hashParens (many comma))}}
   where hashParens p = delimiter "(#" *> p <* terminator "#)"

unboxedTupleSectionsMixin :: forall l g t. Abstract.ExtendedWith '[ 'UnboxedTuples ] l => ExtensionOverlay l g t
unboxedTupleSectionsMixin self super = super{
   report= super.report{
      bareExpression = super.report.bareExpression
                       <|> Abstract.unboxedTupleSectionExpression Abstract.build
                           <$> hashParens (filter (\l-> any isJust l && any isNothing l)
                                           $ optional self.report.expression `sepByNonEmpty` comma)}}
   where hashParens p = delimiter "(#" *> p <* terminator "#)"

unboxedSumsMixin :: forall l g t. Abstract.ExtendedWith '[ 'UnboxedSums ] l => ExtensionOverlay l g t
unboxedSumsMixin self super = super{
   extensions = super.extensions{
      conArgPattern = super.extensions.conArgPattern
                      <|> hashParens (Abstract.unboxedSumPattern Abstract.build
                                         <$> (length <$> some (delimiter "|"))
                                         <*> wrap self.report.pPattern
                                         <*> (length <$> many (delimiter "|"))
                                      <|> Abstract.unboxedSumPattern Abstract.build 0
                                         <$> wrap self.report.pPattern
                                         <*> (length <$> some (delimiter "|")))},
   report= super.report{
      generalConstructor = super.report.generalConstructor
                           <|> Abstract.unboxedSumConstructor Abstract.build . succ . length
                               <$> hashParens (some $ delimiter "|"),
      bareExpression = super.report.bareExpression
                       <|> hashParens (Abstract.unboxedSumExpression Abstract.build
                                          <$> (length <$> some (delimiter "|")) 
                                          <*> self.report.expression
                                          <*> (length <$> many (delimiter "|"))
                                       <|> Abstract.unboxedSumExpression Abstract.build 0
                                          <$> self.report.expression
                                          <*> (length <$> some (delimiter "|")))}}
   where hashParens p = delimiter "(#" *> p <* terminator "#)"

unboxedSumPunsMixin :: forall l g t. Abstract.ExtendedWith '[ 'UnboxedSums ] l => ExtensionOverlay l g t
unboxedSumPunsMixin self super = super{
   report= super.report{
      aType = super.report.aType
              <|> Abstract.unboxedSumType Abstract.build
                  <$> hashParens ((:|) <$> wrap self.report.typeTerm
                                       <*> some (delimiter "|" *> wrap self.report.typeTerm)),
      generalTypeConstructor = super.report.generalTypeConstructor
                               <|> Abstract.constructorType
                                   <$> wrap (Abstract.unboxedSumConstructor Abstract.build . succ . length
                                             <$> hashParens (some $ delimiter "|"))}}
   where hashParens p = delimiter "(#" *> p <* terminator "#)"

interruptibleFFIMixin :: forall l g t. Abstract.ExtendedWith '[ 'InterruptibleFFI ] l => ExtensionOverlay l g t
interruptibleFFIMixin self super = super{
   report= super.report{
      declarationLevel= super.report.declarationLevel{
         safety = super.report.declarationLevel.safety
                  <|> Abstract.interruptibleCall Abstract.build <$ keyword "interruptible"}}}

cApiFFIMixin :: forall l g t. Abstract.ExtendedWith '[ 'CApiFFI ] l => ExtensionOverlay l g t
cApiFFIMixin self super = super{
   report= super.report{
      declarationLevel= super.report.declarationLevel{
         callingConvention = super.report.declarationLevel.callingConvention
                             <|> Abstract.cApiCall Abstract.build <$ keyword "capi"}}}

extendedLiteralsMixin :: (SpaceMonoid t, Abstract.ExtendedWith '[ 'ExtendedLiterals ] l) => ExtensionOverlay l g t
extendedLiteralsMixin self super = super{
   report = super.report{
      literalLexeme = super.report.literalLexeme
                      <* notFollowedBy ((filter (precededByString "#") getInput <|> string "#")
                                        *> void (Text.Parser.Char.satisfy Char.isUpper))
                      <|> Abstract.extendedLiteral Abstract.build
                          <$> storeToken self.report.integerLexeme
                          <*> hashType}}
   where hashType = storeToken (string "#") *> self.report.typeConstructor

multilineStringsMixin :: ExtensionOverlay l g t
multilineStringsMixin self super = super{
   report = super.report{
      stringLexeme = Text.pack . toString mempty <$> (edge *> (normalize . Textual.toText mempty <$> content) <* edge)
                     <<|> super.report.stringLexeme}}
   where content = concatMany (takeCharsWhile1 (\c-> c /= '"' && c /= '\\')
                               <|> notFollowedBy edge *> string "\""
                               <|> Textual.singleton <$> self.report.escape
                               <|> char '\\' *> (char '&' <|> takeCharsWhile1 Char.isSpace *> char '\\') *> pure "")
         edge = string "\"\"\""
         normalize =
             try (stripSuffix "\n") . try (stripPrefix "\n")
             . Text.unlines
             . map removeWhiteSpace . stripCommonPrefixFromAll . map untabify
             . Text.lines
         stripCommonPrefixFromAll [] = []
         stripCommonPrefixFromAll lines = fold . stripPrefix common <$> lines
            where common = foldr1 commonPrefix lines
         removeWhiteSpace line
            | Text.all Char.isSpace line = mempty
            | otherwise = line
         untabify =
            uncurry (<>)
            . first (flip Text.replicate " " . (* 8) . Factorial.length)
            . Text.span (== '\t')
         try f a = fromMaybe a (f a)

magicHashMixin :: forall l g t. (SpaceMonoid t, Abstract.ExtendedHaskell l) => ExtensionOverlay l g t
magicHashMixin self super =
  let prefixMinusFollow = takeCharsWhile1 Char.isDigit *> takeCharsWhile isNumChar *> string "#"
      isNumChar c = Char.isHexDigit c || c `elem` ("eE.bBoOxX_" :: String)
  in super{report= super.report{
        variableIdentifier =
           token (Abstract.name . Text.pack . toString mempty <$> (variableLexeme <> concatAll (string "#"))),
        constructorIdentifier =
           token (Abstract.name . Text.pack . toString mempty <$> (constructorLexeme <> concatAll (string "#"))),
        literalLexeme = super.report.literalLexeme
                        <**> (Abstract.hashLiteral . Abstract.hashLiteral <$ string "##"
                              <<|> Abstract.hashLiteral <$ string "#"
                              <<|> pure id),
        integerLexeme = super.report.integerLexeme
                        <<|> negate <$ string "-" <*> super.report.integerLexeme <* lookAhead (string "#"),
        floatLexeme = super.report.floatLexeme
                      <<|> negate <$ string "-" <*> super.report.floatLexeme <* lookAhead (string "#")}}
    & negationConstraintMixin prefixMinusFollow self

recursiveDoMixin :: (OutlineMonoid t, Abstract.ExtendedWith '[ 'RecursiveDo ] l,
                     Abstract.DeeplyFoldable (Serialization (Down Int) t) l)
                 => ExtensionOverlay l g t
recursiveDoMixin self super = super{
   report= super.report{
      closedBlockExpression = super.report.closedBlockExpression
         <|> Abstract.mdoExpression Abstract.build <$ keyword "mdo" <*> wrap self.report.statements,
      statement = super.report.statement
                  <|> wrap (Deep.InL
                            <$> Abstract.recursiveStatement Abstract.build
                                . map Report.expressionToStatement
                                <$ keyword "rec"
                                <*> blockOf self.report.statement),
      variableIdentifier = notFollowedBy (keyword "mdo" <|> keyword "rec") *> super.report.variableIdentifier}}

qualifiedDoMixin :: forall g t l. (OutlineMonoid t, Abstract.Haskell l, Abstract.ExtendedWith '[ 'QualifiedDo ] l,
                                   Abstract.DeeplyFoldable (Serialization (Down Int) t) l)
                 => ExtensionOverlay l g t
qualifiedDoMixin self super = super{
   report= super.report{
      closedBlockExpression = super.report.closedBlockExpression
         <|> Abstract.qualifiedDoExpression Abstract.build
            <$> Report.storeToken (Abstract.moduleName <$> Report.moduleLexeme <* string ".")
            <* keyword "do"
            <*> wrap self.report.statements,
       qualifiedVariableSymbol =
          notFollowedBy (string "." *> optional (Report.moduleLexeme @g @l *> string ".") *> keyword "do")
          *> super.report.qualifiedVariableSymbol}}

qualifiedRecursiveDoMixin :: forall g t l. (OutlineMonoid t, Abstract.Haskell l,
                                            Abstract.ExtendedWith '[ 'QualifiedDo, 'RecursiveDo ] l,
                                            Abstract.DeeplyFoldable (Serialization (Down Int) t) l)
                          => ExtensionOverlay l g t
qualifiedRecursiveDoMixin self super = super{
   report= super.report{
      closedBlockExpression = super.report.closedBlockExpression
         <|> Abstract.mdoQualifiedExpression Abstract.build
            <$> Report.storeToken (Abstract.moduleName <$> Report.moduleLexeme <* string ".")
            <* keyword "mdo"
            <*> wrap self.report.statements,
       qualifiedVariableSymbol =
          notFollowedBy (string "." *> optional (Report.moduleLexeme @g @l *> string ".") *> keyword "mdo")
          *> super.report.qualifiedVariableSymbol}}

parallelListComprehensionsMixin :: Abstract.ExtendedWith '[ ParallelListComprehensions ] l => ExtensionOverlay l g t
parallelListComprehensionsMixin self@ExtendedGrammar{report= HaskellGrammar{qualifiers, expression}} super = super{
   report= super.report{
      bareExpression = super.report.bareExpression
                       <|> brackets (Abstract.parallelListComprehension Abstract.build
                                     <$> expression <*> qualifiers <*> qualifiers <*> many qualifiers)}}

tupleSectionsMixin :: Abstract.ExtendedWith '[ TupleSections ] l => ExtensionOverlay l g t
tupleSectionsMixin self super = super{
   report= super.report{
      bareExpression = super.report.bareExpression
         <|> Abstract.tupleSectionExpression Abstract.build
             <$> parens (filter (\l-> any isJust l && any isNothing l)
                         $ (:|) <$> optional self.report.expression
                                <*> some (comma *> optional self.report.expression))}}

lambdaCaseMixin :: forall l g t. (Abstract.ExtendedWith '[ 'LambdaCase ] l, OutlineMonoid t,
                                  Deep.Foldable (Serialization (Down Int) t) (Abstract.LambdaCasesAlternative l l))
                => ExtensionOverlay l g t
lambdaCaseMixin self super = super{
   report= super.report{
      openBlockExpression = notFollowedBy (delimiter "\\" *> keyword "cases") *> super.report.openBlockExpression,
      closedBlockExpression = super.report.closedBlockExpression
         <|> Abstract.lambdaCaseExpression Abstract.build <$ (delimiter "\\" *> keyword "case")
             <*> self.report.alternatives
         <|> Abstract.lambdaCasesExpression Abstract.build <$ (delimiter "\\" *> keyword "cases")
             <*> blockOf (wrap $ Abstract.lambdaCasesAlternative @l Abstract.build
                                     <$> many (wrap self.report.aPattern)
                                     <*> wrap (Abstract.normalRHS <$ delimiter "->" <*> self.report.expression
                                               <|> Abstract.guardedRHS
                                                   <$> takeSomeNonEmpty (wrap $ Abstract.guardedExpression . toList
                                                                                <$> self.report.guards
                                                                                <* delimiter "->"
                                                                                <*> self.report.expression)))}}

emptyCaseMixin :: (OutlineMonoid t, Abstract.ExtendedHaskell l,
                   Deep.Foldable (Serialization (Down Int) t) (Abstract.CaseAlternative l l))
               => ExtensionOverlay l g t
emptyCaseMixin self super = super{
   report= super.report{
      alternatives = blockOf (wrap super.report.alternative)}}

multiWayIfMixin :: (OutlineMonoid t, Abstract.ExtendedHaskell l,
                    Deep.Foldable (Serialization (Down Int) t) (Abstract.GuardedExpression l l))
                => ExtensionOverlay l g t
multiWayIfMixin self super = super{
   report= super.report{
      closedBlockExpression = super.report.closedBlockExpression
         <|> Abstract.multiWayIfExpression <$ keyword "if"
             <*> blockOf' (wrap (Abstract.guardedExpression . toList
                                    <$> self.report.guards
                                    <*  self.report.rightArrow
                                    <*> self.report.expression))}}

packageImportsMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
packageImportsMixin self super = super{
   report= super.report{
      moduleLevel= super.report.moduleLevel{
         importDeclaration = super.report.moduleLevel.importDeclaration
                             <|> Abstract.packageQualifiedImportDeclaration <$ keyword "import"
                                 <*> (True <$ keyword "qualified" <|> pure False)
                                 <*> self.report.stringLiteral
                                 <*> moduleId
                                 <*> optional (keyword "as" *> moduleId)
                                 <*> optional (wrap self.report.moduleLevel.importSpecification)}}}

safeImportsMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
safeImportsMixin self super = super{
   report= super.report{
      moduleLevel= super.report.moduleLevel{
         importDeclaration = super.report.moduleLevel.importDeclaration
                             <|> Abstract.safeImportDeclaration <$ keyword "import" <* keyword "safe"
                                 <*> (True <$ keyword "qualified" <|> pure False)
                                 <*> moduleId
                                 <*> optional (keyword "as" *> moduleId)
                                 <*> optional (wrap self.report.moduleLevel.importSpecification)}}}

importQualifiedPostMixin :: ExtensionOverlay l g t
importQualifiedPostMixin self super = super{
   report= super.report{
      moduleLevel= super.report.moduleLevel{
         importDeclaration = super.report.moduleLevel.importDeclaration
                             <|> flip Abstract.importDeclaration <$ keyword "import"
                                 <*> moduleId
                                 <*> (True <$ keyword "qualified")
                                 <*> optional (keyword "as" *> moduleId)
                                 <*> optional (wrap self.report.moduleLevel.importSpecification)}}}

safePackageImportsMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
safePackageImportsMixin self super = super{
   report= super.report{
      moduleLevel= super.report.moduleLevel{
         importDeclaration = super.report.moduleLevel.importDeclaration
                             <|> Abstract.safePackageQualifiedImportDeclaration <$ keyword "import" <* keyword "safe"
                                 <*> (True <$ keyword "qualified" <|> pure False)
                                 <*> self.report.stringLiteral
                                 <*> moduleId
                                 <*> optional (keyword "as" *> moduleId)
                                 <*> optional (wrap self.report.moduleLevel.importSpecification)}}}

packageImportsQualifiedPostMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
packageImportsQualifiedPostMixin self super = super{
   report= super.report{
      moduleLevel= super.report.moduleLevel{
         importDeclaration = super.report.moduleLevel.importDeclaration
                             <|> Abstract.packageQualifiedImportDeclaration <$ keyword "import"
                                 <**> pure flip
                                 <*> self.report.stringLiteral
                                 <**> pure flip
                                 <*> moduleId
                                 <*> (True <$ keyword "qualified" <|> pure False)
                                 <*> optional (keyword "as" *> moduleId)
                                 <*> optional (wrap self.report.moduleLevel.importSpecification)}}}

safeImportsQualifiedPostMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
safeImportsQualifiedPostMixin self super = super{
   report= super.report{
      moduleLevel= super.report.moduleLevel{
         importDeclaration = super.report.moduleLevel.importDeclaration
                             <|> flip Abstract.safeImportDeclaration <$ keyword "import" <* keyword "safe"
                                 <*> moduleId
                                 <*> (True <$ keyword "qualified" <|> pure False)
                                 <*> optional (keyword "as" *> moduleId)
                                 <*> optional (wrap self.report.moduleLevel.importSpecification)}}}

safePackageImportsQualifiedPostMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
safePackageImportsQualifiedPostMixin self super = super{
   report= super.report{
      moduleLevel= super.report.moduleLevel{
         importDeclaration = super.report.moduleLevel.importDeclaration
                             <|> Abstract.safePackageQualifiedImportDeclaration <$ keyword "import" <* keyword "safe"
                                 <**> pure flip
                                 <*> self.report.stringLiteral
                                 <**> pure flip
                                 <*> moduleId
                                 <*> (True <$ keyword "qualified" <|> pure False)
                                 <*> optional (keyword "as" *> moduleId)
                                 <*> optional (wrap self.report.moduleLevel.importSpecification)}}}

namedDefaultsMixin :: Abstract.ExtendedWith '[ 'NamedDefaults ] l => ExtensionOverlay l g t
namedDefaultsMixin self super = super{
   report= super.report{
      declarationLevel= super.report.declarationLevel{
         topLevelDeclaration = super.report.declarationLevel.topLevelDeclaration
            <|> Abstract.namedDefaultDeclaration Abstract.build <$ keyword "default"
                               <*> self.report.qualifiedConstructor
                               <*> parens (wrap self.report.typeTerm `sepBy` comma)}}}

explicitNamespacesMixin :: Abstract.ExtendedWith '[ 'ExplicitNamespaces ] l => ExtensionOverlay l g t
explicitNamespacesMixin self super = super{
   report= super.report{
      moduleLevel= super.report.moduleLevel{
         export = super.report.moduleLevel.export
            <|> Abstract.exportClassOrType <$ keyword "type"
                <*> parens self.report.qualifiedVariableSymbol
                <*> optional self.report.moduleLevel.members,
         importItem = super.report.moduleLevel.importItem
            <|> Abstract.importClassOrType <$ keyword "type"
                <*> parens (self.report.variableSymbol <|> self.report.constructorSymbol)
                <*> optional self.report.moduleLevel.members,
         members = parens (Abstract.allMembers <$ delimiter ".."
                           <|> Abstract.explicitlyNamespacedMemberList Abstract.build
                            <$> (Abstract.defaultMember Abstract.build <$> self.report.moduleLevel.cname
                                 <|> Abstract.typeMember Abstract.build <$ keyword "type"
                                     <*> self.report.moduleLevel.cname)
                               `sepEndBy` comma)},
      declarationLevel = super.report.declarationLevel {
         generalDeclaration = super.report.declarationLevel.generalDeclaration
            <|> Abstract.explicitTypeFixityDeclaration Abstract.build
                   <$> self.report.declarationLevel.fixity
                   <*> optional (fromIntegral <$> self.report.integer)
                   <* keyword "type"
                   <*> (self.report.operator `sepByNonEmpty` comma)
            <|> Abstract.explicitDataFixityDeclaration Abstract.build
                   <$> self.report.declarationLevel.fixity
                   <*> optional (fromIntegral <$> self.report.integer)
                   <* keyword "data"
                   <*> (self.report.operator `sepByNonEmpty` comma)},
      expression = super.report.expression
         <|> wrap (Abstract.explicitTypeExpression Abstract.build <$ keyword "type" <*> wrap self.report.typeTerm),
      pPattern = super.report.pPattern
         <|> Abstract.explicitTypePattern Abstract.build <$ keyword "type" <*> wrap self.report.typeTerm}}

blockArgumentsMixin :: ExtensionOverlay l g t
blockArgumentsMixin self super = super{
   report= super.report{
      lExpression = super.report.lExpression
         <|> wrap (Abstract.applyExpression <$> self.report.fExpression
                                            <*> wrap self.report.openBlockExpression),
      dExpression = self.report.fExpression,
      bareExpression = super.report.bareExpression <|> self.report.closedBlockExpression}}

spaceSensitiveOperatorsMixin :: SpaceMonoid t => ExtensionOverlay l g t
spaceSensitiveOperatorsMixin self super = super{
   extensions = super.extensions{
      conArgPattern = Abstract.variablePattern <$> super.report.variable <* lookAhead unreservedSymbolLexeme
                      <<|> notFollowedBy unreservedSymbolLexeme *> super.extensions.conArgPattern},
   report= super.report{
      variableSymbol = super.report.variableSymbol <|> Report.nameToken unreservedSymbolLexeme}}

unreservedSymbolLexeme :: (Rank2.Apply g, Ord t, SpaceMonoid t) => Parser g t t
unreservedSymbolLexeme =
   filter precededByOpenSpace getInput
      *> (string "@" <|> string "~") <* filter followedByCloseSpace getInput
   <|> filter (not . precededByOpenSpace) getInput *> string "~"

lexicalNegationMixin :: SpaceMonoid t => ExtensionOverlay l g t
lexicalNegationMixin self super = super{
   report= super.report{
      qualifiedVariableSymbol = notFollowedBy (filter precededByOpenSpace getInput
                                               *> string "-"
                                               *> satisfyCharInput (\c-> Char.isAlphaNum c || c == '(' || c == '['))
                                *> token (nameQualifier <*> self.report.variableSymbol),
      prefixNegation = empty,
      bareExpression = super.report.bareExpression
         <|> Abstract.applyExpression <$> wrap (Abstract.negate <$ prefixMinus) <*> self.report.aExpression
         <|> parens (Abstract.rightSectionExpression
                     <$> (notFollowedBy prefixMinus *> self.report.qualifiedOperator)
                     <*> self.report.infixExpression)}}
   where prefixMinus = void (filter precededByOpenSpace getInput
                             *> string "-"
                             <* lookAhead (satisfyCharInput $ \c-> Char.isAlphaNum c || c == '(' || c == '[')
                             <* lift ([[Token Modifier "-"]], ()))
                       <?> "prefix -"

negativeLiteralsMixin :: ExtensionOverlay l g t
negativeLiteralsMixin self super =
   super{
      report= super.report{
         integerLexeme = (negate <$ string "-" <|> pure id) <*> super.report.integerLexeme,
         floatLexeme = (negate <$ string "-" <|> pure id) <*> super.report.floatLexeme}}
   & negationConstraintMixin (satisfyCharInput Char.isDigit) self

binaryLiteralsMixin :: ExtensionOverlay l g t
binaryLiteralsMixin self super = super{
   extensions = super.extensions{
      binary = (string "0b" <|> string "0B") *> (takeCharsWhile1 (\c-> c == '0' || c == '1') <?> "binary number")},
   report = super.report{
      integerLexeme = List.foldl' addBinary 0 . toString mempty <$> self.extensions.binary
                      <<|> super.report.integerLexeme}}
   where addBinary n '0' = 2*n
         addBinary n '1' = 2*n + 1
         addBinary _ _ = error "non-binary"

hexFloatLiteralsMixin :: ExtensionOverlay l g t
hexFloatLiteralsMixin self@ExtendedGrammar{report= HaskellGrammar{decimal, hexadecimal}} super = super{
   report= super.report{
      integerLexeme = notFollowedBy ((string "0x" <|> string "0X")
                                    *> hexadecimal *> satisfyCharInput (`elem` ['.', 'p', 'P']))
                      *> super.report.integerLexeme,
      floatLexeme = (string "0x" <|> string "0X")
                    *> (readHexFloat <$> hexadecimal <* string "." <*> hexadecimal <*> (hexExponent <<|> pure 0)
                       <|> readHexFloat <$> hexadecimal <*> pure mempty <*> hexExponent)
                    <|> super.report.floatLexeme}}
   where hexExponent =
           (string "p" <|> string "P")
           *> (id <$ string "+" <|> negate <$ string "-" <|> pure id)
           <*> (fst . head . Numeric.readDec . toString mempty <$> decimal)
         readHexFloat whole fraction magnitude =
           fst (head $ Numeric.readHex $ toString mempty $ whole <> fraction)
           * 2 ^^ (magnitude - Factorial.length fraction)

numericUnderscoresMixin :: ExtensionOverlay l g t
numericUnderscoresMixin self super = super{
   report= super.report{
      decimal = takeCharsWhile1 Char.isDigit <> concatAll (char '_' *> takeCharsWhile1 Char.isDigit)
                <?> "decimal number",
      octal = takeCharsWhile1 Char.isOctDigit <> concatAll (char '_' *> takeCharsWhile1 Char.isOctDigit)
              <?> "octal number",
      hexadecimal = takeCharsWhile1 Char.isHexDigit <> concatAll (char '_' *> takeCharsWhile1 Char.isHexDigit)
                    <?> "hexadecimal number"}}

binaryUnderscoresMixin :: ExtensionOverlay l g t
binaryUnderscoresMixin self super = super{
   extensions = super.extensions{
      binary = (string "0b" <|> string "0B") *> (binaryDigits <> concatAll (char '_' *> binaryDigits) <?> "binary number")}}
   where binaryDigits = takeCharsWhile1 (\c-> c == '0' || c == '1')

parenthesizedTypeOperatorsMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
parenthesizedTypeOperatorsMixin self super = super{
   report= super.report{
      moduleLevel= super.report.moduleLevel{
         export = Abstract.exportVar <$> self.report.qualifiedVariable
            <|> Abstract.exportClassOrType
                <$> (self.report.qualifiedConstructorIdentifier
                     <|> parens self.report.qualifiedConstructorSymbol)
                <*> pure Nothing
            <|> Abstract.exportClassOrType
                <$> self.report.qualifiedTypeConstructor
                <*> (Just <$> self.report.moduleLevel.members)
            <|> Abstract.reExportModule <$ keyword "module" <*> Report.moduleId},
      qualifiedTypeConstructor = self.report.qualifiedConstructorIdentifier <|> parens anyQualifiedOperator,
      generalTypeConstructor = super.report.generalTypeConstructor
        <|> Abstract.constructorType
            <$> wrap (Abstract.constructorReference <$> parens self.report.qualifiedVariableSymbol),
      declarationLevel= super.report.declarationLevel{
         qualifiedTypeClass =
            super.report.declarationLevel.qualifiedTypeClass <|> parens anyQualifiedOperator}}}
   where anyQualifiedOperator =
            self.report.qualifiedConstructorOperator <|> self.report.qualifiedVariableOperator

typeOperatorsMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
typeOperatorsMixin self super = super{
   report= super.report{
      moduleLevel= super.report.moduleLevel{
         importItem = Abstract.importVar <$> self.report.variable
            <|> Abstract.importClassOrType
                <$> (self.report.constructorIdentifier <|> parens self.report.constructorSymbol)
                <*> pure Nothing
            <|> Abstract.importClassOrType
                <$> self.report.typeConstructor
                <*> (Just <$> self.report.moduleLevel.members)},
      declarationLevel= super.report.declarationLevel{
         typeClass = super.report.declarationLevel.typeClass <|> parens anyOperator,
         simpleType = super.report.declarationLevel.simpleType
            <|> Abstract.infixTypeLHSApplication
                         <$> self.extensions.typeVarBinder
                         <*> (notFollowedBy (string "`") *> anyOperator)
                         <*> self.extensions.typeVarBinder,
         instanceDesignator =
            super.report.declarationLevel.instanceDesignator
            <|> Abstract.infixTypeClassInstanceLHS
                <$> wrap self.report.bType
                <*> self.report.qualifiedOperator
                <*> wrap self.report.bType},
      typeConstructor = self.report.constructorIdentifier <|> parens anyOperator},
   extensions = super.extensions{
      equalityConstraint = empty,
      cType = super.extensions.cType
         <|> Abstract.infixTypeApplication
                <$> wrap self.extensions.cType
                <*> self.report.qualifiedOperator
                <*> wrap self.report.bType}}
   where anyOperator = self.report.constructorOperator <|> self.report.variableOperator

equalityConstraintsMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
equalityConstraintsMixin self super = super{
   extensions = super.extensions{
      equalityConstraint =
         Abstract.typeEquality
         <$> wrap self.report.bType
         <* delimiter "~"
         <*> wrap (self.report.bType)}}

multiParameterConstraintsMixin :: forall l g t. Abstract.ExtendedHaskell l => ExtensionOverlay l g t
multiParameterConstraintsMixin self super = super{
   report= super.report{
      declarationLevel= super.report.declarationLevel{
         derivingClause =
            keyword "deriving"
            *> (pure <$> wrap (parens (Abstract.strategicDerive Abstract.build
                                       <$> wrap (pure $ Abstract.defaultStrategy @l Abstract.build)
                                       <*> wrap self.report.typeTerm `sepBy` comma)
                               <<|> Abstract.simpleDerive <$> self.report.declarationLevel.qualifiedTypeClass))}}}

gratuitouslyParenthesizedTypesMixin :: (OutlineMonoid t, Abstract.ExtendedHaskell l,
                                        Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l),
                                        Deep.Foldable (Serialization (Down Int) t) (Abstract.GADTConstructor l l))
                                    => ExtensionOverlay l g t
gratuitouslyParenthesizedTypesMixin self super = super{
   report= super.report{
      declarationLevel = super.report.declarationLevel{
         qualifiedTypeClass = super.report.declarationLevel.qualifiedTypeClass <|> parens qtc,
         typeVarApplications = self.report.generalTypeConstructor
            <|> Abstract.typeApplication
                <$> wrap (self.report.declarationLevel.typeVarApplications
                          <|> parens self.report.declarationLevel.typeVarApplications)
                <*> wrap self.extensions.optionallyKindedAndParenthesizedTypeVar,
         simpleType = super.report.declarationLevel.simpleType
            <|> parens self.report.declarationLevel.simpleType}},
   extensions = super.extensions{
      gadtConstructors = super.extensions.gadtConstructors
         <|> Abstract.gadtConstructors <$> self.extensions.constructorIDs
                                       <* self.report.doubleColon
                                       <**> pure uncurry3
                                       <*> (parens forallAndContextAndBody <|> forallAndParenContextBody),
      gadtNewConstructor = super.extensions.gadtNewConstructor
         <|> Abstract.gadtConstructors <$> ((:|[]) <$> self.report.constructor)
                                       <* self.report.doubleColon
                                       <**> pure uncurry3
                                       <*> parens forallAndNewBody,
      gadtNewBody = super.extensions.gadtNewBody
         <|> Abstract.functionType
             <$> wrap self.report.bType
             <* self.report.rightArrow
             <*> wrap paren_return_type
         <|> Abstract.recordFunctionType
             <$> braces ((:[]) <$> wrap self.report.declarationLevel.fieldDeclaration)
             <* self.report.rightArrow
             <*> wrap paren_return_type,
      record_gadt_body = (super.extensions.record_gadt_body)
         <|> Abstract.recordFunctionType
             <$> braces (wrap self.report.declarationLevel.fieldDeclaration `sepBy` comma)
             <* self.report.rightArrow
             <*> wrap paren_return_type,
      optionallyParenthesizedTypeVar = self.report.typeVar
                                       <|> parens self.extensions.optionallyParenthesizedTypeVar,
      typeVarBinder = Abstract.implicitlyKindedTypeVariable <$> self.extensions.optionallyParenthesizedTypeVar}}
   where qtc = self.report.declarationLevel.qualifiedTypeClass
         paren_return_type = parens ((self.extensions.return_type) <|> parens paren_return_type)
         optionalContextAndGadtBody =
            contextAndGadtBody <|> (,) <$> wrap (pure Abstract.noContext) <*> wrap self.extensions.gadtBody
         contextAndGadtBody =
            (,) <$> wrap self.report.declarationLevel.context
                <*  self.report.rightDoubleArrow
                <*> wrap self.extensions.gadtBody
            <|> parens contextAndGadtBody
         forallAndContextAndBody =
            (,,) <$ self.extensions.keywordForall
                 <*> many self.extensions.typeVarBinder
                 <* delimiter "."
                 <**> pure uncurry
                 <*> optionalContextAndGadtBody
            <|> uncurry ((,,) []) <$> contextAndGadtBody
            <|> parens forallAndContextAndBody
         forallAndParenContextBody =
            (,,) <$ self.extensions.keywordForall
                 <*> many self.extensions.typeVarBinder
                 <* delimiter "."
                 <**> pure uncurry
                 <*> parens contextAndGadtBody
         forallAndNewBody =
            (,,) <$ self.extensions.keywordForall
                 <*> many self.extensions.typeVarBinder
                 <* delimiter "."
                 <*> wrap (pure Abstract.noContext
                           <|> self.report.declarationLevel.context
                               *> self.report.rightDoubleArrow
                               *> fail "No context allowed on GADT newtype")
                 <*> wrap self.extensions.gadtNewBody
            <|> parens forallAndNewBody
         uncurry3 f (a, b, c) = f a b c

typeFamiliesMixin :: forall l g t. (OutlineMonoid t, Abstract.ExtendedHaskell l,
                                    Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l),
                                    Deep.Foldable (Serialization (Down Int) t) (Abstract.GADTConstructor l l))
                  => ExtensionOverlay l g t
typeFamiliesMixin self@ExtendedGrammar{
                     report= HaskellGrammar{
                        declarationLevel= DeclarationGrammar{optionalContext, simpleType, derivingClause}}}
                  super =
  super{
    report= super.report{
      declarationLevel= super.report.declarationLevel{
         topLevelDeclaration = super.report.declarationLevel.topLevelDeclaration
            <|> Abstract.dataFamilyDeclaration <$ keyword "data" <* keyword "family"
                <*> wrap simpleType <*> optional (wrap self.extensions.kindSignature)
            <|> Abstract.openTypeFamilyDeclaration <$ keyword "type" <* keyword "family"
                <*> wrap simpleType <*> optional (wrap self.extensions.kindSignature)
            <|> Abstract.closedTypeFamilyDeclaration <$ keyword "type" <* keyword "family"
                <*> wrap simpleType <*> optional (wrap self.extensions.kindSignature) <* keyword "where"
                <*> blockOf (wrap
                             $ Abstract.typeFamilyInstance
                               <$> self.extensions.optionalForall
                               <*> wrap self.report.declarationLevel.instanceDesignator <* delimiter "="
                               <*> wrap self.report.typeTerm)
            <|> Abstract.dataFamilyInstance <$ (keyword "data" *> keyword "instance")
                <*> self.extensions.optionalForall
                <*> wrap optionalContext
                <*> wrap self.report.declarationLevel.instanceDesignator
                <*> optional (wrap self.extensions.kindSignature)
                <*> moptional (delimiter "=" *> self.report.declarationLevel.declaredConstructors)
                <*> moptional derivingClause
            <|> Abstract.newtypeFamilyInstance <$ (keyword "newtype" *> keyword "instance")
                <*> self.extensions.optionalForall
                <*> wrap optionalContext
                <*> wrap self.report.declarationLevel.instanceDesignator
                <*> optional (wrap self.extensions.kindSignature)
                <* delimiter "="
                <*> wrap self.report.declarationLevel.newConstructor
                <*> moptional derivingClause
            <|> Abstract.gadtDataFamilyInstance <$ (keyword "data" *> keyword "instance")
                <*> self.extensions.optionalForall
                <*> wrap self.report.declarationLevel.instanceDesignator
                <*> optional (wrap self.extensions.kindSignature)
                <* keyword "where"
                <*> blockOf (wrap self.extensions.gadtConstructors)
                <*> moptional derivingClause
            <|> Abstract.gadtNewtypeFamilyInstance <$ (keyword "newtype" *> keyword "instance")
                <*> self.extensions.optionalForall
                <*> wrap self.report.declarationLevel.instanceDesignator
                <*> optional (wrap self.extensions.kindSignature)
                <* keyword "where"
                <*> wrap self.extensions.gadtNewConstructor
                <*> moptional derivingClause
            <|> Abstract.typeFamilyInstance <$ (keyword "type" *> keyword "instance")
                <*> self.extensions.optionalForall
                <*> wrap self.report.declarationLevel.instanceDesignator
                <* delimiter "="
                <*> wrap self.report.typeTerm,
         inClassDeclaration = super.report.declarationLevel.inClassDeclaration
            <|> Abstract.dataFamilyDeclaration <$ keyword "data" <* optional (keyword "family")
                <*> wrap simpleType <*> optional (wrap self.extensions.kindSignature)
            <|> Abstract.openTypeFamilyDeclaration <$ keyword "type" <* optional (keyword "family")
                <*> wrap simpleType <*> optional (wrap self.extensions.kindSignature)
            <|> self.extensions.inClassOrInstanceTypeFamilyDeclaration,
         inInstanceDeclaration = super.report.declarationLevel.inInstanceDeclaration
            <|> Abstract.dataFamilyInstance <$ keyword "data" <* optional (keyword "instance")
                <*> self.extensions.optionalForall
                <*> wrap optionalContext
                <*> wrap self.report.declarationLevel.instanceDesignator
                <*> optional (wrap self.extensions.kindSignature)
                <*> moptional (delimiter "=" *> self.report.declarationLevel.declaredConstructors)
                <*> moptional derivingClause
            <|> Abstract.newtypeFamilyInstance <$ keyword "newtype" <* optional (keyword "instance")
                <*> self.extensions.optionalForall
                <*> wrap optionalContext
                <*> wrap self.report.declarationLevel.instanceDesignator
                <*> optional (wrap self.extensions.kindSignature)
                <* delimiter "="
                <*> wrap self.report.declarationLevel.newConstructor
                <*> moptional derivingClause
            <|> Abstract.gadtDataFamilyInstance <$ (keyword "data" *> optional (keyword "instance"))
                <*> self.extensions.optionalForall
                <*> wrap self.report.declarationLevel.instanceDesignator
                <*> optional (wrap self.extensions.kindSignature)
                <* keyword "where"
                <*> blockOf (wrap self.extensions.gadtConstructors)
                <*> moptional derivingClause
            <|> Abstract.gadtNewtypeFamilyInstance <$ (keyword "newtype" *> optional (keyword "instance"))
                <*> self.extensions.optionalForall
                <*> wrap self.report.declarationLevel.instanceDesignator
                <*> optional (wrap self.extensions.kindSignature)
                <* keyword "where"
                <*> wrap self.extensions.gadtNewConstructor
                <*> moptional derivingClause
            <|> self.extensions.inClassOrInstanceTypeFamilyDeclaration}},
  extensions = super.extensions{
    inClassOrInstanceTypeFamilyDeclaration =
       Abstract.typeFamilyInstance <$ keyword "type" <* optional (keyword "instance")
           <*> self.extensions.optionalForall
           <*> wrap self.report.declarationLevel.instanceDesignator
           <* delimiter "="
           <*> wrap self.report.typeTerm}}

typeFamilyDependenciesMixin :: (OutlineMonoid t, Abstract.ExtendedHaskell l,
                                Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l),
                                Deep.Foldable (Serialization (Down Int) t) (Abstract.GADTConstructor l l))
                            => ExtensionOverlay l g t
typeFamilyDependenciesMixin self super = super{
   report= super.report{
      declarationLevel= super.report.declarationLevel{
         topLevelDeclaration = super.report.declarationLevel.topLevelDeclaration
            <|> Abstract.injectiveOpenTypeFamilyDeclaration <$ keyword "type" <* keyword "family"
                <*> wrap self.report.declarationLevel.simpleType <* delimiter "="
                <*> self.extensions.typeVarBinder
                <*> optional dependencies
            <|> Abstract.injectiveClosedTypeFamilyDeclaration <$ keyword "type" <* keyword "family"
                <*> wrap self.report.declarationLevel.simpleType <* delimiter "="
                <*> self.extensions.typeVarBinder
                <*> optional dependencies
                <* keyword "where"
                <*> blockOf (wrap $ Abstract.typeFamilyInstance
                                    <$> self.extensions.optionalForall
                                    <*> wrap self.report.declarationLevel.instanceDesignator <* delimiter "="
                                    <*> wrap self.report.typeTerm),
         inClassDeclaration = super.report.declarationLevel.inClassDeclaration
            <|> Abstract.injectiveOpenTypeFamilyDeclaration <$ keyword "type" <* optional (keyword "family")
                <*> wrap self.report.declarationLevel.simpleType <* delimiter "="
                <*> self.extensions.typeVarBinder
                <*> (Just <$> dependencies)}}}
   where dependencies = (,) <$> (delimiter "|" *> self.report.typeVar) <* self.report.rightArrow
                            <*> someNonEmpty self.report.typeVar

dataKindsMixin :: forall l g t. Abstract.ExtendedWith '[ 'DataKinds ] l => ExtensionOverlay l g t
dataKindsMixin self super = super{
   report= super.report{
      aType = super.report.aType
         <|> self.extensions.promotedLiteral
         <|> self.extensions.promotedStructure,
      generalTypeConstructor = super.report.generalTypeConstructor
         <|> Abstract.promotedConstructorType Abstract.build
             <$ terminator "'"
             <*> wrap (Abstract.constructorReference <$> self.report.qualifiedConstructor)},
   extensions = super.extensions{
      promotedLiteral =
         Abstract.promotedIntegerLiteral Abstract.build <$> self.report.integer
         <|> Abstract.promotedCharLiteral Abstract.build <$> self.report.charLiteral
         <|> Abstract.promotedStringLiteral Abstract.build <$> self.report.stringLiteral,
      promotedStructure =
         Abstract.promotedTupleType Abstract.build
            <$> parens (pure [] <|> (:) <$> wrap self.report.typeTerm <*> some (comma *> wrap self.report.typeTerm))
         <|> Abstract.promotedListType Abstract.build <$> brackets (wrap self.report.typeTerm `sepBy` comma)}}

dataKindsListTuplePunsMixin :: Abstract.ExtendedWith '[ 'DataKinds ] l => ExtensionOverlay l g t
dataKindsListTuplePunsMixin self super = super{
   extensions = super.extensions{
      promotedStructure =
         Abstract.promotedTupleType Abstract.build <$ terminator "'"
                                                   <*> parens (wrap self.report.typeTerm `sepBy` comma)
         <|> Abstract.promotedListType Abstract.build <$ terminator "'"
                                                      <*> brackets (wrap self.report.typeTerm `sepBy` comma)
         <|> Abstract.promotedListType Abstract.build <$> brackets ((:) <$> wrap self.report.typeTerm
                                                                        <*> some (comma *> wrap self.report.typeTerm))}}

dataKindsTypeOperatorsMixin :: Abstract.ExtendedWith '[ 'DataKinds ] l => ExtensionOverlay l g t
dataKindsTypeOperatorsMixin self super = super{
   extensions = super.extensions{
      cType = super.extensions.cType
         <|> Abstract.promotedInfixTypeApplication Abstract.build
             <$> wrap self.extensions.cType
             <* terminator "'"
             <*> self.report.qualifiedOperator
             <*> wrap self.report.bType}}

typeDataMixin :: Abstract.ExtendedWith '[ 'TypeData ] l => ExtensionOverlay l g t
typeDataMixin self super = super{
   report = super.report{
      declarationLevel= super.report.declarationLevel{
         topLevelDeclaration = super.report.declarationLevel.topLevelDeclaration
            <|> Abstract.typeDataDeclaration Abstract.build <$ keyword "type" <* keyword "data"
                <*> wrap self.report.declarationLevel.simpleType
                <*> optional (wrap self.extensions.kindSignature)
                <*> (delimiter "=" *> self.report.declarationLevel.declaredConstructors <|> pure [])}}}

typeDataGADTMixin :: (OutlineMonoid t,
                      Abstract.ExtendedWith '[ 'GADTs, 'TypeData ] l,
                      Abstract.DeeplyFoldable (Serialization (Down Int) t) l) => ExtensionOverlay l g t
typeDataGADTMixin self super = super{
   report = super.report{
      declarationLevel= super.report.declarationLevel{
         topLevelDeclaration = super.report.declarationLevel.topLevelDeclaration
            <|> Abstract.typeGADTDeclaration Abstract.build <$ keyword "type" <* keyword "data"
                <*> wrap self.report.declarationLevel.simpleType
                <*> optional (wrap self.extensions.kindSignature) <* keyword "where"
                <*> blockOf (wrap self.extensions.gadtConstructors)}}}

visibleDependentQuantificationMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
visibleDependentQuantificationMixin self super = super{
   extensions = super.extensions{
      arrowType = super.extensions.arrowType
         <|> Abstract.visibleDependentType
             <$ self.extensions.keywordForall
             <*> many self.extensions.typeVarBinder
             <* self.report.rightArrow
             <*> wrap self.extensions.arrowType}}

requiredTypeArgumentsMixin :: Abstract.ExtendedWith '[ 'ExplicitNamespaces ] l => ExtensionOverlay l g t
requiredTypeArgumentsMixin self super = super{
   report = super.report{
      aExpression = super.report.aExpression
                    <<|> wrap (Abstract.explicitTypeExpression Abstract.build <$> wrap self.report.aType)}}

kindSignaturesBaseMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
kindSignaturesBaseMixin self super = super{
   extensions = super.extensions{
      kindSignature = Abstract.typeKind <$ self.report.doubleColon <*> wrap self.report.typeTerm}}

starIsTypeMixin :: ExtensionOverlay l g t
starIsTypeMixin self super = super{
   extensions = super.extensions{
      groundTypeKind = super.extensions.groundTypeKind <|> delimiter "*"}}

unicodeStarIsTypeMixin :: ExtensionOverlay l g t
unicodeStarIsTypeMixin self super = super{
   extensions = super.extensions{
      groundTypeKind = super.extensions.groundTypeKind <|> delimiter "★"}}

starIsTypeOperatorsMixin :: ExtensionOverlay l g t
starIsTypeOperatorsMixin self super = super{
   report = super.report{
      aType = parens (Abstract.constructorType
                      <$> wrap (Abstract.constructorReference . Abstract.qualifiedName Nothing
                                <$> token (Report.nameToken $ string "*")))
              <<|> super.report.aType}}

roleAnnotationsMixin :: forall l g t. Abstract.ExtendedWith '[ 'RoleAnnotations ] l => ExtensionOverlay l g t
roleAnnotationsMixin self super = super{
   report = super.report{
      declarationLevel= super.report.declarationLevel {
         topLevelDeclaration = super.report.declarationLevel.topLevelDeclaration
            <|> Abstract.typeRoleDeclaration Abstract.build <$ keyword "type" <* keyword "role"
                <*> self.report.qualifiedTypeConstructor
                <*> some (Abstract.nominalRole Abstract.build <$ keyword "nominal"
                          <|> Abstract.representationalRole Abstract.build <$ keyword "representational"
                          <|> Abstract.phantomRole Abstract.build <$ keyword "phantom"
                          <|> Abstract.inferredRole Abstract.build <$ keyword "_")
         }
      }
   }

inferredTypeVariablesMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
inferredTypeVariablesMixin self super = super{
   extensions = super.extensions{
      typeVarBinder = super.extensions.typeVarBinder
         <|> braces (Abstract.inferredTypeVariable <$> self.report.typeVar
                     <|> Abstract.inferredExplicitlyKindedTypeVariable
                         <$> self.report.typeVar
                         <*> wrap self.extensions.kindSignature)}}

typeApplicationsMixin :: (Abstract.ExtendedHaskell l, Abstract.DeeplyFoldable (Serialization (Down Int) t) l,
                          SpaceMonoid t) => ExtensionOverlay l g t
typeApplicationsMixin self super = super{
   report = super.report{
      bType = super.report.bType
         <|> Abstract.visibleKindApplication
             <$> filter whiteSpaceTrailing (wrap self.report.bType)
             <* typeApplicationDelimiter
             <*> wrap (Abstract.typeKind <$> wrap self.report.aType),
      bareExpression = super.report.bareExpression
         <|> Abstract.visibleTypeApplication
             <$> filter whiteSpaceTrailing self.report.aExpression
             <* typeApplicationDelimiter
             <*> wrap self.report.aType},
   extensions = super.extensions{
      return_type = (super.extensions.return_type)
         <|> Abstract.visibleKindApplication
             <$> filter whiteSpaceTrailing (wrap self.extensions.return_type)
             <* typeApplicationDelimiter
             <*> wrap (Abstract.typeKind <$> wrap self.report.aType)}}
   where typeApplicationDelimiter = notFollowedBy unreservedSymbolLexeme *> delimiter "@"


typeAbstractionsOrApplicationsMixin :: (Abstract.ExtendedHaskell l, SpaceMonoid t,
                                        Abstract.DeeplyFoldable (Serialization (Down Int) t) l)
                                    => ExtensionOverlay l g t
typeAbstractionsOrApplicationsMixin self super = super{
   report = super.report{
      lPattern = super.report.lPattern
         <|> Abstract.constructorPatternWithTypeApplications
             <$> filter whiteSpaceTrailing (wrap self.report.generalConstructor)
             <*> some (typeApplicationDelimiter *> wrap self.report.aType)
             <*> many (wrap self.extensions.conArgPattern)},
   extensions = super.extensions{
      typeVarBinder = super.extensions.typeVarBinder
         <|> Abstract.wildcardTypeBinding <$ keyword "_"
         <|> parens (Abstract.explicitlyKindedWildcardTypeBinding <$ keyword "_"
                     <*> wrap self.extensions.kindSignature),
      instanceDesignatorApplications = super.extensions.instanceDesignatorApplications
         <|> Abstract.classInstanceLHSKindApplication
             <$> filter whiteSpaceTrailing (wrap self.extensions.instanceDesignatorApplications)
             <* typeApplicationDelimiter
             <*> wrap (Abstract.typeKind <$> wrap self.report.aType)}}
   where typeApplicationDelimiter = notFollowedBy unreservedSymbolLexeme *> delimiter "@"

typeAbstractionsMixin :: (Abstract.ExtendedWith '[ 'TypeAbstractions ] l,
                          SpaceMonoid t) => ExtensionOverlay l g t
typeAbstractionsMixin self super = super{
   report = super.report{
      declarationLevel = super.report.declarationLevel{
         simpleType = super.report.declarationLevel.simpleType
            <|> Abstract.typeLHSTypeApplication Abstract.build
                   <$> wrap self.report.declarationLevel.simpleType
                   <* delimiter "@"
                   <*> self.extensions.typeVarBinder},
      aPattern =
         Abstract.invisibleTypePattern Abstract.build
            <$ (filter precededByOpenSpace getInput *> delimiter "@")
            <*> wrap self.report.aType
         <|> notFollowedBy (self.report.variable *> filter precededByOpenSpace getInput *> delimiter "@")
             *> super.report.aPattern}}

linearTypesMixin :: (SpaceMonoid t, Abstract.ExtendedHaskell l) => ExtensionOverlay l g t
linearTypesMixin self super = super{
   report= super.report{
      variableSymbol = notFollowedBy prefixPercent *> super.report.variableSymbol},
   extensions = super.extensions{
      arrowType = super.extensions.arrowType
         <|> Abstract.linearFunctionType
             <$> wrap self.extensions.cType
             <* token prefixPercent
             <* keyword "1"
             <* self.report.rightArrow
             <*> wrap self.extensions.arrowType
         <|> Abstract.multiplicityFunctionType
             <$> wrap self.extensions.cType
             <* token prefixPercent
             <* notFollowedBy (keyword "1")
             <*> wrap self.report.aType
             <* self.report.rightArrow
             <*> wrap self.extensions.arrowType}}
   where prefixPercent =
            filter precededByOpenSpace getInput *> string "%" <* filter (not . followedByCloseSpace) getInput

gadtLinearTypesMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
gadtLinearTypesMixin self super = super{
   extensions = super.extensions{
      prefix_gadt_body = (super.extensions.prefix_gadt_body)
        <|> Abstract.linearFunctionType
            <$> wrap (self.report.bType <|> self.report.declarationLevel.strictType)
            <* delimiter "%"
            <* keyword "1"
            <* self.report.rightArrow
            <*> wrap (self.extensions.prefix_gadt_body)
        <|> Abstract.multiplicityFunctionType
            <$> wrap (self.report.bType <|> self.report.declarationLevel.strictType)
            <* delimiter "%"
            <* notFollowedBy (keyword "1")
            <*> wrap self.report.aType
            <* self.report.rightArrow
            <*> wrap (self.extensions.prefix_gadt_body),
      gadtNewBody = super.extensions.gadtNewBody
        <|> Abstract.linearFunctionType
            <$> wrap self.report.bType
            <* delimiter "%"
            <* keyword "1"
            <* self.report.rightArrow
            <*> wrap (self.extensions.return_type)
        <|> Abstract.multiplicityFunctionType
            <$> wrap (self.report.bType <|> self.report.declarationLevel.strictType)
            <* delimiter "%"
            <* notFollowedBy (keyword "1")
            <*> wrap self.report.aType
            <* self.report.rightArrow
            <*> wrap (self.extensions.return_type)}}

unicodeLinearTypesMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
unicodeLinearTypesMixin self super = super{
   extensions = super.extensions{
      arrowType = super.extensions.arrowType
        <|> Abstract.linearFunctionType
            <$> wrap self.extensions.cType
            <* delimiter "⊸"
            <*> wrap self.extensions.arrowType}}

gadtUnicodeLinearTypesMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
gadtUnicodeLinearTypesMixin self super = super{
   extensions = super.extensions{
      prefix_gadt_body = (super.extensions.prefix_gadt_body)
        <|> Abstract.linearFunctionType
            <$> wrap (self.report.bType <|> self.report.declarationLevel.strictType)
            <* delimiter "⊸"
            <*> wrap (self.extensions.prefix_gadt_body),
      gadtNewBody = super.extensions.gadtNewBody
        <|> Abstract.linearFunctionType
            <$> wrap self.report.bType
            <* delimiter "⊸"
            <*> wrap (self.extensions.return_type)}}

standaloneKindSignaturesMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
standaloneKindSignaturesMixin self super = super{
   report= super.report{
      declarationLevel= super.report.declarationLevel{
         topLevelDeclaration = super.report.declarationLevel.topLevelDeclaration
            <|> Abstract.kindSignature <$ keyword "type"
                  <*> self.report.typeConstructor
                  <*> wrap self.extensions.kindSignature}}}

kindSignaturesMixin :: (OutlineMonoid t, Abstract.ExtendedHaskell l,
                        Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l))
                    => ExtensionOverlay l g t
kindSignaturesMixin self super = super{
   report= super.report{
      declarationLevel= super.report.declarationLevel{
         topLevelDeclaration = super.report.declarationLevel.topLevelDeclaration
            <|> Abstract.kindedDataDeclaration <$ keyword "data"
                   <*> wrap self.report.declarationLevel.optionalContext
                   <*> wrap self.report.declarationLevel.simpleType
                   <*> wrap self.extensions.kindSignature
                   <*> (delimiter "=" *> self.report.declarationLevel.declaredConstructors <|> pure [])
                   <*> moptional self.report.declarationLevel.derivingClause
            <|> Abstract.kindedNewtypeDeclaration <$ keyword "newtype"
                   <*> wrap self.report.declarationLevel.optionalContext
                   <*> wrap self.report.declarationLevel.simpleType
                   <*> wrap self.extensions.kindSignature
                   <* delimiter "="
                   <*> wrap self.report.declarationLevel.newConstructor
                   <*> moptional self.report.declarationLevel.derivingClause},
      typeTerm = super.report.typeTerm <|>
         Abstract.kindedType <$> wrap self.report.typeTerm <*> wrap self.extensions.kindSignature},
   extensions = super.extensions{
      optionallyKindedAndParenthesizedTypeVar =
         Abstract.typeVariable <$> self.extensions.optionallyParenthesizedTypeVar
         <|> parens (Abstract.kindedType
                     <$> wrap (Abstract.typeVariable <$> self.extensions.optionallyParenthesizedTypeVar)
                     <*> wrap self.extensions.kindSignature),
      optionallyKindedTypeVar =
         Abstract.typeVariable <$> self.report.typeVar
         <|> Abstract.kindedType
             <$> wrap (Abstract.typeVariable <$> self.report.typeVar)
             <*> wrap self.extensions.kindSignature,
      typeVarBinder = super.extensions.typeVarBinder
                      <|> parens (Abstract.explicitlyKindedTypeVariable
                                  <$> self.report.typeVar
                                  <*> wrap self.extensions.kindSignature)}}

existentialQuantificationMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
existentialQuantificationMixin self super = super{
   report= super.report{
      declarationLevel= super.report.declarationLevel{
         declaredConstructor = super.report.declarationLevel.declaredConstructor
            <|> Abstract.existentialConstructor
                <$ self.extensions.keywordForall
                <*> many self.extensions.typeVarBinder <* delimiter "."
                <*> wrap self.report.declarationLevel.optionalContext
                <*> wrap super.report.declarationLevel.declaredConstructor
            <|> Abstract.existentialConstructor []
                <$> wrap self.report.declarationLevel.context
                <* self.report.rightDoubleArrow
                <*> wrap super.report.declarationLevel.declaredConstructor}}}

scopedTypeVariablesMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
scopedTypeVariablesMixin self super = super{
   extensions = super.extensions{
      typedPattern = super.extensions.typedPattern
         <|> Abstract.typedPattern
                <$> wrap self.extensions.infixPattern
                <* self.report.doubleColon
                <*> wrap self.report.typeTerm}}

explicitForAllMixin :: (OutlineMonoid t, Abstract.ExtendedHaskell l,
                        Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l))
                    => ExtensionOverlay l g t
explicitForAllMixin self super = super{
      report= super.report{
         declarationLevel= super.report.declarationLevel{
            optionalTypeSignatureContext = pure Abstract.noContext,
            topLevelDeclaration = super.report.declarationLevel.topLevelDeclaration
               <|> Abstract.explicitlyScopedInstanceDeclaration <$ keyword "instance"
                   <* self.extensions.keywordForall
                   <*> many self.extensions.typeVarBinder
                   <* delimiter "."
                   <*> wrap self.report.declarationLevel.optionalContext
                   <*> wrap self.report.declarationLevel.instanceDesignator
                   <*> (keyword "where"
                        *> blockOf (wrap self.report.declarationLevel.inInstanceDeclaration)
                        <|> pure [])},
         typeVar = notFollowedBy (self.extensions.keywordForall) *> super.report.typeVar},
   extensions = super.extensions{
      arrowType = super.extensions.arrowType
         <|> Abstract.forallType <$ self.extensions.keywordForall
             <*> many self.extensions.typeVarBinder <* delimiter "."
             <*> wrap self.extensions.arrowType,
      keywordForall = super.extensions.keywordForall <|> keyword "forall",
      optionalForall = self.extensions.keywordForall *> many self.extensions.typeVarBinder <* delimiter "."
                       <<|> pure []}}

gadtSyntaxMixin :: (OutlineMonoid t, Abstract.ExtendedHaskell l,
                    Deep.Foldable (Serialization (Down Int) t) (Abstract.GADTConstructor l l))
                => ExtensionOverlay l g t
gadtSyntaxMixin self super = super{
   report= super.report{
      declarationLevel= super.report.declarationLevel{
         topLevelDeclaration = super.report.declarationLevel.topLevelDeclaration
            <|> Abstract.gadtDeclaration <$ keyword "data"
                <*> wrap super.report.declarationLevel.simpleType
                <*> optional (wrap self.extensions.kindSignature) <* keyword "where"
                <*> blockOf (wrap self.extensions.gadtConstructors)
                <*> moptional super.report.declarationLevel.derivingClause
            <|> Abstract.gadtNewtypeDeclaration <$ keyword "newtype"
                <*> wrap super.report.declarationLevel.simpleType
                <*> optional (wrap self.extensions.kindSignature) <* keyword "where"
                <*> wrap self.extensions.gadtNewConstructor
                <*> moptional super.report.declarationLevel.derivingClause}},
   extensions = super.extensions{
      optionalForall = self.extensions.keywordForall *> many self.extensions.typeVarBinder <* delimiter "."
                       <<|> pure []}}

gadtSyntaxTypeOperatorsMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
gadtSyntaxTypeOperatorsMixin self super = super{
   extensions = super.extensions{
      prefix_gadt_body = (super.extensions.prefix_gadt_body)
         <|> Abstract.functionType
                <$> wrap (Abstract.infixTypeApplication
                             <$> wrap self.extensions.cType
                             <*> self.report.qualifiedOperator
                             <*> wrap self.report.bType)
                <* self.report.rightArrow
                <*> wrap (self.extensions.prefix_gadt_body),
      return_type = (super.extensions.return_type) <|>
          Abstract.infixTypeApplication <$> wrap (self.extensions.arg_type)
                                        <*> self.report.qualifiedOperator
                                        <*> wrap (self.extensions.arg_type)}}

dataKindsGadtSyntaxTypeOperatorsMixin :: Abstract.ExtendedWith '[ 'DataKinds ] l => ExtensionOverlay l g t
dataKindsGadtSyntaxTypeOperatorsMixin self super = super{
   extensions = super.extensions{
      return_type = (super.extensions.return_type) <|>
         Abstract.promotedInfixTypeApplication Abstract.build
         <$> wrap (self.extensions.arg_type)
         <* terminator "'"
         <*> self.report.qualifiedOperator
         <*> wrap (self.extensions.arg_type)}}

namedFieldPunsMixin :: Abstract.ExtendedWith '[ 'NamedFieldPuns ] l => ExtensionOverlay l g t
namedFieldPunsMixin self super = super{
   report = super.report{
      fieldBinding = super.report.fieldBinding <|>
         Abstract.punnedFieldBinding Abstract.build <$> self.report.qualifiedVariable,
      fieldPattern = super.report.fieldPattern <|>
         Abstract.punnedFieldPattern Abstract.build <$> self.report.qualifiedVariable}}

recordWildCardsMixin :: Abstract.ExtendedWith '[ 'RecordWildCards ] l => ExtensionOverlay l g t
recordWildCardsMixin self super = super{
   extensions = super.extensions{
     conArgPattern = super.extensions.conArgPattern
        <|> Abstract.wildcardRecordPattern Abstract.build <$> self.report.qualifiedConstructor
            <*> braces (wrap self.report.fieldPattern `endBy` comma <* delimiter "..")},
   report = super.report{
      bareExpression = super.report.bareExpression
         <|> Abstract.wildcardRecordExpression Abstract.build <$> self.report.qualifiedConstructor
             <*> braces (wrap self.report.fieldBinding `endBy` comma <* delimiter "..")}}

overloadedRecordDotMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
overloadedRecordDotMixin self super = super{
   report = super.report{
      qualifiedVariableSymbol =
         notFollowedBy (string "." *> satisfyCharInput varStart) *> super.report.qualifiedVariableSymbol,
      bareExpression = super.report.bareExpression <|>
         Abstract.getField <$> self.report.aExpression <* prefixDot <*> self.report.variableIdentifier
         <|>
         Abstract.fieldProjection <$> parens (someNonEmpty $ prefixDot *> self.report.variableIdentifier)}}
   where prefixDot = void (string "."
                           <* lookAhead (satisfyCharInput varStart)
                           <* lift ([[Token Modifier "."]], ()))
                     <?> "prefix ."

implicitParametersMixin :: Abstract.ExtendedWith '[ 'ImplicitParameters ] l => ExtensionOverlay l g t
implicitParametersMixin self super = super{
   extensions = super.extensions{
      implicitParameterConstraint =
         Abstract.implicitParameterConstraint Abstract.build
         <$ delimiter "?"
         <*> self.report.variableIdentifier
         <* self.report.doubleColon
         <*> wrap self.report.typeTerm},
   report = super.report{
       declarationLevel = super.report.declarationLevel{
           declaration = super.report.declarationLevel.declaration
              <|> Abstract.implicitParameterDeclaration Abstract.build
                  <$ delimiter "?"
                  <*> self.report.variableIdentifier
                  <* delimiter "="
                  <*> self.report.expression},
       bareExpression = super.report.bareExpression
          <|> Abstract.implicitParameterExpression Abstract.build
              <$ delimiter "?" <*> self.report.variableIdentifier,
       qualifiedVariableSymbol = notFollowedBy (delimiter "?") *> super.report.qualifiedVariableSymbol}}

strictDataMixin :: (SpaceMonoid t, Abstract.ExtendedWith '[ 'StrictData ] l) => ExtensionOverlay l g t
strictDataMixin self super = super{
   report = super.report{
      declarationLevel = super.report.declarationLevel{
         strictType = super.report.declarationLevel.strictType
            <|> Abstract.lazyType Abstract.build <$ delimiter "~" <*> wrap self.report.aType}}}

strictMixin :: (SpaceMonoid t, Abstract.ExtendedWith '[ 'Strict ] l) => ExtensionOverlay l g t
strictMixin self super = super{
   report = super.report{
      aPattern =
         Abstract.irrefutablePattern
            <$ delimiter "~"
            <*> parens (wrap (Abstract.lazyPattern Abstract.build
                                 <$ delimiter "~"
                                 <*> wrap self.report.aPattern))
         <<|> Abstract.lazyPattern Abstract.build <$ delimiter "~" <*> wrap self.report.aPattern
         <<|> super.report.aPattern}}

bangPatternsMixin :: (SpaceMonoid t, Abstract.ExtendedWith '[ 'BangPatterns ] l) => ExtensionOverlay l g t
bangPatternsMixin self super = super{
   extensions = super.extensions{
      conArgPattern = super.extensions.conArgPattern
         <|> Abstract.bangPattern Abstract.build <$ bang <*> wrap self.extensions.conArgPattern},
   report = super.report{
      variableOperator = notFollowedBy bang *> super.report.variableOperator}}
   where bang = filter precededByOpenSpace getInput
                *> string "!"
                <* notSatisfyChar (\c-> Char.isSpace c || isSymbol c)
                <* notFollowedBy Report.comment
                <* lift ([[Token Delimiter "!"]], ())

orPatternsMixin :: Abstract.ExtendedWith '[ 'OrPatterns ] l => ExtensionOverlay l g t
orPatternsMixin self super = super{
   report = super.report{
      pattern = super.report.pattern
         <|> Abstract.orPattern Abstract.build
             <$> ((:|)
                  <$> wrap self.extensions.typedPattern
                  <*> some (semi *> wrap self.extensions.typedPattern))}}

viewPatternsMixin :: Abstract.ExtendedWith '[ 'ViewPatterns ] l => ExtensionOverlay l g t
viewPatternsMixin self super = super{
   report = super.report{
      pPattern = super.report.pPattern
         <|> Abstract.viewPattern Abstract.build
             <$> self.report.expression
             <* self.report.rightArrow
             <*> wrap self.report.pPattern}}

nPlusKPatternsMixin :: Abstract.ExtendedWith '[ 'NPlusKPatterns ] l => ExtensionOverlay l g t
nPlusKPatternsMixin self super = super{
   report = super.report{
      pPattern = super.report.pPattern
         <|> Abstract.nPlusKPattern Abstract.build
             <$> self.report.variable
             <* delimiter "+"
             <*> self.report.integer}}

patternSynonymsMixin :: forall l g t. (OutlineMonoid t, Abstract.ExtendedWith '[ 'PatternSynonyms ] l,
                                       Deep.Foldable (Serialization (Down Int) t) (Abstract.PatternEquationClause l l),
                                       Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l))
                     => ExtensionOverlay l g t
patternSynonymsMixin self super = super{
   report= super.report{
      moduleLevel= super.report.moduleLevel{
         export = super.report.moduleLevel.export
            <|> Abstract.exportPattern Abstract.build <$ keyword "pattern"
                <*> self.report.qualifiedConstructor,
         importItem = super.report.moduleLevel.importItem
            <|> Abstract.importPattern Abstract.build <$ keyword "pattern"
                <*> self.report.constructor,
         members = super.report.moduleLevel.members
            <|> parens (Abstract.allMembersPlus Abstract.build
                        <$> filter (not . null)
                               (moptional (self.report.moduleLevel.cname `sepBy` comma <* comma)
                                <> ([] <$ delimiter "..")
                                <> moptional (comma *> self.report.moduleLevel.cname `sepEndBy` comma)))},
      declarationLevel= super.report.declarationLevel{
         topLevelDeclaration = super.report.declarationLevel.topLevelDeclaration
            <|> keyword "pattern" *>
            (Abstract.implicitPatternSynonym Abstract.build
                <$> wrap lhsPattern <* delimiter "=" <*> wrap self.report.pattern
             <|> Abstract.unidirectionalPatternSynonym Abstract.build
                    <$> wrap lhsPattern <* self.report.leftArrow <*> wrap self.report.pattern
             <|> Abstract.explicitPatternSynonym Abstract.build
                    <$> wrap lhsPattern
                    <* self.report.leftArrow
                    <*> wrap self.report.pattern
                    <*> patternClauses
             <|> Abstract.patternSynonymSignature Abstract.build
                    <$> self.report.constructor `sepByNonEmpty` comma
                    <* self.report.doubleColon
                    <*> self.extensions.optionalForall
                    <*> wrap (self.report.declarationLevel.context <* self.report.rightDoubleArrow
                              <<|> pure Abstract.noContext)
                    <*> self.extensions.optionalForall
                    <*> wrap self.report.declarationLevel.optionalContext
                    <*> many (wrap self.extensions.cType <* self.report.rightArrow)
                    <*> wrap self.extensions.cType)},
      variableIdentifier = notFollowedBy (keyword "pattern") *> super.report.variableIdentifier}}
   where lhsPattern =
            Abstract.prefixPatternLHS Abstract.build
               <$> self.report.constructor
               <*> many self.report.variableIdentifier
            <|> Abstract.infixPatternLHS Abstract.build
                   <$> self.report.variableIdentifier
                   <*> self.report.constructorOperator
                   <*> self.report.variableIdentifier
            <|> Abstract.recordPatternLHS Abstract.build
                   <$> self.report.constructor
                   <*> braces (self.report.variable `sepBy` comma)
         patternClauses = keyword "where" *> blockOf (wrap patternClause)
         patternClause = Abstract.patternEquationClause @l Abstract.build
                         <$> wrap patternClauseLHS
                         <*> wrap self.report.rhs
                         <*> self.report.declarationLevel.whereClauses
         patternClauseLHS =
            Abstract.prefixPatternEquationLHS Abstract.build
               <$> self.report.constructor
               <*> many (wrap self.report.pattern)
            <|> Abstract.infixPatternEquationLHS Abstract.build
                   <$> wrap self.report.pattern
                   <*> self.report.constructorOperator
                   <*> wrap self.report.pattern

standaloneDerivingMixin :: Abstract.ExtendedWith '[ 'StandaloneDeriving ] l => ExtensionOverlay l g t
standaloneDerivingMixin self super = super{
   report = super.report{
      declarationLevel= super.report.declarationLevel{
         topLevelDeclaration = super.report.declarationLevel.topLevelDeclaration
            <|> Abstract.standaloneDerivingDeclaration Abstract.build <$ keyword "deriving" <* keyword "instance"
                <*> self.extensions.optionalForall
                <*> wrap self.report.declarationLevel.optionalContext
                <*> wrap self.report.declarationLevel.instanceDesignator}}}

derivingStrategiesMixin :: forall l g t. Abstract.ExtendedWith '[ 'DerivingStrategies ] l => ExtensionOverlay l g t
derivingStrategiesMixin self super = super{
   report = super.report{
      declarationLevel= super.report.declarationLevel{
         derivingClause = concatSome self.extensions.singleDerivingClause}},
   extensions = super.extensions{
      singleDerivingClause = super.report.declarationLevel.derivingClause
         <|> takeSome (wrap $
                       Abstract.strategicDerive Abstract.build <$ keyword "deriving"
                       <*> wrap self.extensions.derivingStrategy
                       <*> (pure <$> wrap (Abstract.constructorType <$> wrap self.report.generalConstructor)
                            <<|> parens (wrap self.report.typeTerm `sepBy` comma))),
      derivingStrategy = Abstract.stockStrategy @l Abstract.build <$ keyword "stock"
                         <|> Abstract.anyClassStrategy @l Abstract.build <$ keyword "anyclass"
                         <|> Abstract.newtypeStrategy @l Abstract.build <$ keyword "newtype"}}

standaloneDerivingStrategiesMixin :: (Abstract.ExtendedWith '[ 'StandaloneDeriving ] l,
                                      Abstract.ExtendedWith '[ 'DerivingStrategies ] l)
                                  => ExtensionOverlay l g t
standaloneDerivingStrategiesMixin self super = super{
   report = super.report{
      declarationLevel= super.report.declarationLevel{
         topLevelDeclaration = super.report.declarationLevel.topLevelDeclaration
            <|> Abstract.standaloneStrategicDerivingDeclaration Abstract.build
                   <$ keyword "deriving"
                   <*> wrap self.extensions.derivingStrategy
                   <* keyword "instance"
                   <*> self.extensions.optionalForall
                   <*> wrap self.report.declarationLevel.optionalContext
                   <*> wrap self.report.declarationLevel.instanceDesignator}}}

derivingViaMixin :: forall l g t. Abstract.ExtendedWith '[ 'DerivingVia ] l => ExtensionOverlay l g t
derivingViaMixin self super = super{
   extensions = super.extensions{
      singleDerivingClause =
         super.extensions.singleDerivingClause
         <|> takeSome (wrap $
                       Abstract.deriveVia Abstract.build <$ keyword "deriving"
                       <*> (parens (wrap self.report.typeTerm `sepBy` comma)
                            <<|> pure <$> wrap (Abstract.constructorType <$> wrap self.report.generalConstructor))
                       <* keyword "via"
                       <*> wrap self.report.typeTerm)}}

standaloneDerivingViaMixin :: forall l g t. (Abstract.ExtendedWith '[ 'StandaloneDeriving ] l,
                                             Abstract.ExtendedWith '[ 'DerivingStrategies ] l,
                                             Abstract.ExtendedWith '[ 'DerivingVia ] l)
                           => ExtensionOverlay l g t
standaloneDerivingViaMixin self super = super{
   report = super.report{
      declarationLevel= super.report.declarationLevel{
         topLevelDeclaration = super.report.declarationLevel.topLevelDeclaration
            <|> Abstract.standaloneStrategicDerivingDeclaration Abstract.build
                   <$ keyword "deriving"
                   <*> wrap derivingVia
                   <*  keyword "instance"
                   <*> self.extensions.optionalForall
                   <*> wrap self.report.declarationLevel.optionalContext
                   <*> wrap self.report.declarationLevel.instanceDesignator}}}
   where derivingVia = Abstract.derivingViaStrategy @l Abstract.build <$ keyword "via" <*> wrap self.report.typeTerm

functionalDependenciesMixin :: forall l g t. (OutlineMonoid t, Abstract.ExtendedWith '[ 'FunctionalDependencies ] l,
                                              Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l))
                            => ExtensionOverlay l g t
functionalDependenciesMixin self super = super{
   report= super.report{
      declarationLevel= super.report.declarationLevel{
         topLevelDeclaration = super.report.declarationLevel.topLevelDeclaration
            <|> Abstract.fundepClassDeclaration Abstract.build
                   <$ keyword "class"
                   <*> wrap self.report.declarationLevel.optionalContext
                   <*> wrap self.report.declarationLevel.classLHS
                   <*  delimiter "|"
                   <*> wrap (Abstract.functionalDependency Abstract.build
                                <$> many self.report.typeVar <* self.report.rightArrow <*> many self.report.typeVar)
                      `sepBy` comma
                   <*> moptional (keyword "where" *> blockOf (wrap self.report.declarationLevel.inClassDeclaration))}}}

constraintsAreTypesMixin :: forall l g t. Abstract.ExtendedHaskell l => ExtensionOverlay l g t
constraintsAreTypesMixin self super = super{
   extensions = super.extensions{
      cType = super.extensions.cType <|> Abstract.constraintType <$> wrap self.extensions.equalityConstraint},
   report= super.report{
      typeTerm = super.report.typeTerm
                 <|> Abstract.constraintType <$> wrap self.extensions.implicitParameterConstraint,
      declarationLevel= super.report.declarationLevel{
         context = self.report.declarationLevel.constraint,
         constraint = Abstract.typeConstraint <$> wrap self.extensions.cType}}}

instanceSignaturesMixin :: ExtensionOverlay l g t
instanceSignaturesMixin self super = super{
   report= super.report{
      declarationLevel= super.report.declarationLevel{
         inInstanceDeclaration = super.report.declarationLevel.inInstanceDeclaration
            <|> Abstract.typeSignature <$> self.report.declarationLevel.variables
                                       <*  self.report.doubleColon
                                       <*> wrap self.report.declarationLevel.optionalTypeSignatureContext
                                       <*> wrap self.report.typeTerm}}}

defaultSignaturesMixin :: Abstract.ExtendedWith '[ 'DefaultSignatures ] l => ExtensionOverlay l g t
defaultSignaturesMixin self super = super{
   report= super.report{
      declarationLevel= super.report.declarationLevel{
         inClassDeclaration = super.report.declarationLevel.inClassDeclaration
            <|> Abstract.defaultMethodSignature Abstract.build <$ keyword "default"
                   <*> self.report.variable
                   <*  self.report.doubleColon
                   <*> wrap self.report.declarationLevel.optionalTypeSignatureContext
                   <*> wrap self.report.typeTerm}}}

-- | Not an extension by itself, common to magicHashMixin and negativeLiteralsMixin.
negationConstraintMixin :: Parser g t t -> ExtensionOverlay l g t
negationConstraintMixin prefixMinusFollow self super = super{
   report= super.report{
      variableSymbol = negationGuard *> super.report.variableSymbol,
      qualifiedVariableSymbol = negationGuard *> super.report.qualifiedVariableSymbol,
      prefixNegation = negationGuard *> super.report.prefixNegation}}
   where negationGuard = notFollowedBy (string "-" *> prefixMinusFollow)

nondecreasingIndentationMixin :: (Deep.Foldable (Serialization (Down Int) t) (Abstract.Expression l l),
                                  Deep.Foldable (Serialization (Down Int) t) (Abstract.Statement l l),
                                  OutlineMonoid t)
                              => ExtensionOverlay l g t
nondecreasingIndentationMixin self super = super{
   report = super.report{
      statements = Report.blockWith nonDecreasingIndentLine Report.blockTerminatorKeyword self.report.statement
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
