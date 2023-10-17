{-# Language DataKinds, FlexibleContexts, FlexibleInstances, NamedFieldPuns, OverloadedStrings,
             Rank2Types, RecordWildCards, ScopedTypeVariables,
             TemplateHaskell, TupleSections, TypeApplications, TypeFamilies, TypeOperators, TypeSynonymInstances #-}

-- | Missing syntax extensions:
-- * @QualifiedDo@ requires TemplateHaskell 2.17
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
import Text.Grampa.Combinators (moptional, someNonEmpty)
import Text.Grampa.ContextFree.SortedMemoizing.Transformer.LeftRecursive (autochain, ParserT, lift)
import qualified Transformation.Deep as Deep
import Witherable (filter)

import Language.Haskell.Extensions (Extension(..), ExtensionSwitch(..),
                                    on, partitionContradictory, switchesByName, withImplications)
import qualified Language.Haskell.Extensions.Abstract as Abstract
import qualified Language.Haskell.Grammar as Report
import Language.Haskell.Grammar (HaskellGrammar(..), ModuleLevelGrammar(..), DeclarationGrammar(..),
                                 Parser, OutlineMonoid, NodeWrap,
                                 blockOf, delimiter, terminator, inputColumn, isSymbol,
                                 moduleId, nameQualifier,
                                 oneExtendedLine, rewrap, startSepEndBy, wrap, unwrap, whiteSpace)
import Language.Haskell.Reserializer (Lexeme(..), Serialization, TokenType(..), lexemes)

import Prelude hiding (exponent, filter)

class TextualMonoid t => SpaceMonoid t where
   precededByOpenSpace :: t -> Bool

instance (Eq t, Factorial.StableFactorial t, RightReductive t, TextualMonoid t) => SpaceMonoid (Shadowed t) where
   precededByOpenSpace t = Textual.any isOpenOrSpace (Factorial.primeSuffix $ prefix t) || "-}" `isSuffixOf` prefix t
      where isOpenOrSpace c = Char.isSpace c || c `elem` ("([{,;" :: [Char])

followedByCloseSpace :: TextualMonoid t => t -> Bool
followedByCloseSpace t =
   any isCloseOrSpace (Textual.characterPrefix t) || "{-" `isPrefixOf` t || "--" `isPrefixOf` t
   where isCloseOrSpace c = Char.isSpace c || c `elem` (")]},;" :: [Char])

instance (Eq t, Factorial.StableFactorial t, OutlineMonoid t) => OutlineMonoid (Shadowed t) where
   currentColumn = Report.currentColumn . content

data ExtendedGrammar l t f p = ExtendedGrammar {
   report :: HaskellGrammar l t f p,
   singleDerivingClause :: p [f (Abstract.DerivingClause l l f f)],
   keywordForall :: p (),
   classLHS :: p (Abstract.TypeLHS l l f f),
   kindSignature :: p (Abstract.Kind l l f f),
   groundTypeKind :: p (),
   cType, equalityConstraintType, arrowType :: p (Abstract.Type l l f f),
   promotedLiteral :: p (Abstract.Type l l f f),
   instanceTypeDesignatorInsideParens :: p (Abstract.Type l l f f),
   infixPattern :: p (Abstract.Pattern l l f f),
   gadtNewConstructor, gadtConstructors :: p (Abstract.GADTConstructor l l f f),
   constructorIDs :: p (NonEmpty (Abstract.Name l)),
   derivingStrategy :: p (Abstract.DerivingStrategy l l f f),
   namespacedMember :: p (Abstract.ModuleMember l),
   inClassOrInstanceTypeFamilyDeclaration :: p (Abstract.Declaration l l f f),
   familyInstanceDesignator, familyInstanceDesignatorApplications,
   familyInstanceDesignatorBase, flexibleInstanceDesignator :: p (Abstract.ClassInstanceLHS l l f f),
   optionalForall :: p [Abstract.TypeVarBinding l l f f],
   typeVarBinder :: p (Abstract.TypeVarBinding l l f f),
   optionallyParenthesizedTypeVar :: p (Abstract.Name l),   
   optionallyKindedTypeVar, optionallyKindedAndParenthesizedTypeVar :: p (Abstract.Type l l f f),
   gadtNewBody, gadtBody, prefix_gadt_body, record_gadt_body :: p (Abstract.Type l l f f),
   return_type, base_return_type, arg_type :: p (Abstract.Type l l f f),
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
     (Set.fromList [OverloadedLabels],               [(4, overloadedLabelsMixin)]),
     (Set.fromList [RecursiveDo],                    [(4, recursiveDoMixin)]),
     (Set.fromList [TupleSections],                  [(5, tupleSectionsMixin)]),
     (Set.fromList [EmptyCase],                      [(6, emptyCaseMixin)]),
     (Set.fromList [LambdaCase],                     [(7, lambdaCaseMixin)]),
     (Set.fromList [GratuitouslyParenthesizedTypes], [(7, gratuitouslyParenthesizedTypesMixin)]),
     (Set.fromList [EqualityConstraints],            [(7, equalityConstraintsMixin)]),
     (Set.fromList [MultiWayIf],                     [(8, multiWayIfMixin)]),
     (Set.fromList [KindSignatures],                 [(7, kindSignaturesBaseMixin), (8, kindSignaturesMixin)]),
     (Set.fromList [MultiParameterConstraints],      [(8, multiParameterConstraintsMixin)]),
     (Set.fromList [ParenthesizedTypeOperators],     [(8, parenthesizedTypeOperatorsMixin)]),
     (Set.fromList [TypeOperators],                  [(8, typeOperatorsMixin)]),
     (Set.fromList [ExplicitNamespaces],             [(9, explicitNamespacesMixin)]),
     (Set.fromList [BlockArguments],                 [(9, blockArgumentsMixin)]),
     (Set.fromList [ExistentialQuantification],      [(9, existentialQuantificationMixin)]),
     (Set.fromList [ExplicitForAll],                 [(9, explicitForAllMixin)]),
     (Set.fromList [ScopedTypeVariables],            [(9, scopedTypeVariablesMixin)]),
     (Set.fromList [GADTSyntax],                     [(9, gadtSyntaxMixin)]),
     (Set.fromList [TypeFamilies],                   [(9, typeFamiliesMixin)]),
     (Set.fromList [TypeFamilyDependencies],         [(9, typeFamilyDependenciesMixin)]),
     (Set.fromList [DataKinds],                      [(9, dataKindsMixin)]),
     (Set.fromList [StandaloneKindSignatures],       [(7, kindSignaturesBaseMixin),
                                                      (9, standaloneKindSignaturesMixin)]),
     (Set.fromList [StarIsType],                     [(9, starIsTypeMixin)]),
     (Set.fromList [TypeApplications],               [(9, typeApplicationsMixin)]),
     (Set.fromList [LinearTypes],                    [(9, linearTypesMixin)]),
     (Set.fromList [RoleAnnotations],                [(9, roleAnnotationsMixin)]),
     (Set.fromList [NamedFieldPuns],                 [(9, namedFieldPunsMixin)]),
     (Set.fromList [RecordWildCards],                [(9, recordWildCardsMixin)]),
     (Set.fromList [OverloadedRecordDot],            [(9, overloadedRecordDotMixin)]),
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
     (Set.fromList [MultiParamTypeClasses],          [(9, mptcsMixin)]),
     (Set.fromList [FunctionalDependencies],         [(9, functionalDependenciesMixin)]),
     (Set.fromList [FlexibleInstances],              [(9, flexibleInstancesMixin)]),
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
     (Set.fromList [PolyKinds, ExplicitForAll],      [(9, visibleDependentKindQualificationMixin)]),
     (Set.fromList [FlexibleContexts],               [(9, flexibleContextsMixin)]),
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
         extendWith = overlay reportGrammar . map snd . reverse . List.sortOn fst . fold . map (extensionMixins Map.!)

reportGrammar :: forall l g t. (g ~ ExtendedGrammar l t (NodeWrap t), Abstract.ExtendedHaskell l,
                                LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t,
                                Deep.Foldable (Serialization (Down Int) t) (Abstract.CaseAlternative l l),
                                Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l),
                                Deep.Foldable (Serialization (Down Int) t) (Abstract.Expression l l),
                                Deep.Foldable (Serialization (Down Int) t) (Abstract.Import l l),
                                Deep.Foldable (Serialization (Down Int) t) (Abstract.Statement l l))
              => GrammarBuilder g g (ParserT ((,) [[Lexeme t]])) t
reportGrammar g@ExtendedGrammar{report= r} =
   g{report= r'{
        declarationLevel= (declarationLevel r'){
           simpleType =
              Abstract.simpleTypeLHS <$> nonTerminal (Report.typeConstructor . report) <*> pure []
              <|> Abstract.simpleTypeLHSApplication
                  <$> wrap (nonTerminal $ Report.simpleType . declarationLevel . report)
                  <*> nonTerminal typeVarBinder,
           context = nonTerminal (Report.constraint . declarationLevel . report),
           constraint = Abstract.typeConstraint <$> wrap (nonTerminal cType),
           optionalTypeSignatureContext = pure Abstract.noContext,
           instanceTypeDesignator =
              nonTerminal (Report.generalTypeConstructor . report)
              <|> Abstract.listType <$> brackets (wrap $ nonTerminal optionallyKindedAndParenthesizedTypeVar)
              <|> parens (nonTerminal instanceTypeDesignatorInsideParens)},
        pattern = nonTerminal infixPattern,
        aType = Report.aType r'
                <|> Abstract.typeWildcard <$ keyword "_"
                <|> Abstract.groundType <$ nonTerminal groundTypeKind,
        typeTerm = nonTerminal arrowType},
     classLHS = empty,
     keywordForall = keyword "forall",
     kindSignature = empty,
     groundTypeKind = empty,
     derivingStrategy = empty,
     arrowType = nonTerminal cType
        <|> Abstract.functionType <$> wrap (nonTerminal cType)
                                  <* nonTerminal (Report.rightArrow . report)
                                  <*> wrap (nonTerminal arrowType)
        <|> Abstract.constrainedType <$> wrap (nonTerminal $ Report.context . declarationLevel . report)
                                     <* nonTerminal (Report.rightDoubleArrow . report)
                                     <*> wrap (nonTerminal arrowType),
     cType = nonTerminal (Report.bType . report) <|> nonTerminal equalityConstraintType,
     equalityConstraintType = empty,
     infixPattern = Report.pattern r',
     promotedLiteral =
        Abstract.promotedIntegerLiteral <$> nonTerminal (Report.integer . report)
        <|> Abstract.promotedCharLiteral <$> nonTerminal (Report.charLiteral . report)
        <|> Abstract.promotedStringLiteral <$> nonTerminal (Report.stringLiteral . report),
     namespacedMember =
        Abstract.defaultMember <$> nonTerminal (cname . Report.moduleLevel . report)
        <|> Abstract.typeMember <$ keyword "type" <*> nonTerminal (cname . Report.moduleLevel . report),
     inClassOrInstanceTypeFamilyDeclaration = empty,
     familyInstanceDesignatorBase =
        Abstract.classReferenceInstanceLHS <$> nonTerminal (Report.qualifiedTypeClass . declarationLevel . report)
        <|> parens (nonTerminal familyInstanceDesignator),
     familyInstanceDesignatorApplications =
        nonTerminal familyInstanceDesignatorBase
        <|> Abstract.classInstanceLHSApplication
            <$> wrap (nonTerminal familyInstanceDesignatorApplications)
            <*> wrap (nonTerminal (Report.aType . report)),
     familyInstanceDesignator = nonTerminal familyInstanceDesignatorApplications,
     flexibleInstanceDesignator =
        Abstract.typeClassInstanceLHS
           <$> nonTerminal (Report.qualifiedTypeClass . declarationLevel . report)
           <*> wrap (nonTerminal $ Report.aType . report)
        <|> parens (nonTerminal flexibleInstanceDesignator),
     instanceTypeDesignatorInsideParens =
        nonTerminal (Report.typeVarApplications . declarationLevel . report)
        <|> Abstract.listType <$> brackets (wrap $ nonTerminal optionallyKindedAndParenthesizedTypeVar)
        <|> Abstract.tupleType <$> typeVarTuple
        <|> Abstract.functionType
            <$> wrap (nonTerminal optionallyKindedAndParenthesizedTypeVar)
            <* nonTerminal (Report.rightArrow . report)
            <*> wrap (nonTerminal optionallyKindedAndParenthesizedTypeVar),
     optionalForall = pure [],
     optionallyParenthesizedTypeVar = nonTerminal (Report.typeVar . report),
     optionallyKindedAndParenthesizedTypeVar = Abstract.typeVariable <$> nonTerminal optionallyParenthesizedTypeVar,
     optionallyKindedTypeVar = empty,
     typeVarBinder = Abstract.implicitlyKindedTypeVariable <$> nonTerminal (Report.typeVar . report),
     gadtConstructors =
        Abstract.gadtConstructors <$> nonTerminal constructorIDs <* nonTerminal (Report.doubleColon . report)
                                  <*> nonTerminal optionalForall
                                  <*> wrap optionalContext
                                  <*> wrap (nonTerminal gadtBody),
     gadtNewConstructor =
        Abstract.gadtConstructors <$> ((:|[]) <$> constructor) <* nonTerminal (Report.doubleColon . report)
                                  <*> nonTerminal optionalForall
                                  <*> wrap (pure Abstract.noContext
                                            <|> context *> rightDoubleArrow
                                                *> fail "No context allowed on GADT newtype")
                                  <*> wrap (nonTerminal gadtNewBody),
     constructorIDs = constructor `sepByNonEmpty` comma,
     gadtNewBody =
        parens (nonTerminal gadtNewBody)
        <|> Abstract.functionType
            <$> wrap (bType <|> Abstract.strictType <$ delimiter "!" <*> wrap bType)
            <* nonTerminal (Report.rightArrow . report)
            <*> wrap (nonTerminal return_type)
        <|> Abstract.recordFunctionType
            <$> braces ((:[]) <$> wrap (nonTerminal $ Report.fieldDeclaration . declarationLevel . report))
            <* nonTerminal (Report.rightArrow . report)
            <*> wrap (nonTerminal return_type),
     gadtBody = nonTerminal prefix_gadt_body <|> nonTerminal record_gadt_body,
     prefix_gadt_body =
        parens (nonTerminal prefix_gadt_body)
        <|> nonTerminal return_type
        <|> Abstract.functionType
            <$> wrap (bType <|> Abstract.strictType <$ delimiter "!" <*> wrap bType)
            <* nonTerminal (Report.rightArrow . report)
            <*> wrap (nonTerminal prefix_gadt_body),
     record_gadt_body =
        parens (nonTerminal record_gadt_body)
        <|> Abstract.recordFunctionType
            <$> braces (wrap (nonTerminal $ Report.fieldDeclaration . declarationLevel . report) `sepBy` comma)
            <* nonTerminal (Report.rightArrow . report)
            <*> wrap (nonTerminal return_type),
     return_type = Abstract.typeApplication
                      <$> wrap (nonTerminal return_type <|> parens (nonTerminal return_type))
                      <*> wrap (nonTerminal arg_type)
                   <|> nonTerminal base_return_type,
     base_return_type = Abstract.constructorType <$> wrap (nonTerminal $ Report.generalConstructor . report),
     arg_type = nonTerminal (Report.aType . report),
     binary = empty}
   where r'@HaskellGrammar{declarationLevel= DeclarationGrammar{..}, ..} = Report.grammar r

identifierSyntaxMixin :: ExtensionOverlay l g t
identifierSyntaxMixin self super = super{
   report= (report super){
      variableIdentifier = token (Abstract.name . Text.pack . toString mempty <$> variableLexeme),
      constructorIdentifier = token (Abstract.name . Text.pack . toString mempty <$> constructorLexeme),
      variableSymbol = token (Abstract.name . Text.pack . toString mempty <$> Report.variableSymbolLexeme),
      constructorSymbol = token (Abstract.name . Text.pack . toString mempty <$> Report.constructorSymbolLexeme)}}

overloadedLabelsMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
overloadedLabelsMixin self super = super{
   report= (report super){
      bareExpression = (super & report & bareExpression)
                       <|> Abstract.overloadedLabel . Text.pack . toString mempty
                           <$> token (string "#" *> variableLexeme),
      variableSymbol = notFollowedBy (string "#" *> variableLexeme) *> (super & report & variableSymbol)}}

unicodeSyntaxMixin :: ExtensionOverlay l g t
unicodeSyntaxMixin self super = super{
   keywordForall = keywordForall super <|> delimiter "∀",
   report= (report super){
      doubleColon = (super & report & doubleColon) <|> delimiter "∷",
      rightDoubleArrow = (super & report & rightDoubleArrow) <|> delimiter "⇒",
      rightArrow = (super & report & rightArrow) <|> delimiter "→",
      leftArrow = (super & report & leftArrow) <|> delimiter "←",
      variableSymbol = notSatisfyChar (`elem` ("∀←→⇒∷★" :: [Char])) *> (super & report & variableSymbol)}}

magicHashMixin :: forall l g t. Abstract.ExtendedHaskell l => ExtensionOverlay l g t
magicHashMixin self super =
  let integer', integerHash, integerHash2 :: (LexicalParsing (Parser g t), Show t, TextualMonoid t) => Parser g t Integer
      float', floatHash, floatHash2 :: (LexicalParsing (Parser g t), Show t, TextualMonoid t) => Parser g t Rational
      charLiteral', charHashLiteral :: (LexicalParsing (Parser g t), Show t, TextualMonoid t) => Parser g t Char
      stringLiteral', stringHashLiteral :: (LexicalParsing (Parser g t), Show t, TextualMonoid t) => Parser g t Text

      integer' = token ((self & report & integerLexeme) <* notFollowedBy (string "#"))
      float' = token ((self & report & floatLexeme) <* notFollowedBy (string "#"))
      charLiteral' = token ((self & report & charLexeme) <* notFollowedBy (string "#"))
      stringLiteral' = token ((self & report & stringLexeme) <* notFollowedBy (string "#")) <?> "string literal"

      unsignedIntegerLexeme = notSatisfyChar (== '-') *> (self & report & integerLexeme)
      unsignedFloatLexeme = notSatisfyChar (== '-') *> (self & report & floatLexeme)
      signedIntegerLexeme = unsignedIntegerLexeme <|> negate <$ string "-" <*> unsignedIntegerLexeme
      signedFloatLexeme = unsignedFloatLexeme <|> negate <$ string "-" <*> unsignedFloatLexeme
      integerHash = token (signedIntegerLexeme <* string "#" <* notFollowedBy (string "#"))
      floatHash = token (signedFloatLexeme <* string "#" <* notFollowedBy (string "#"))
      integerHash2 = token (signedIntegerLexeme <* string "##")
      floatHash2 = token (signedFloatLexeme <* string "##")
      charHashLiteral = token ((self & report & charLexeme) <* string "#")
      stringHashLiteral = token ((self & report & stringLexeme) <* string "#")
      prefixMinusFollow = takeCharsWhile1 Char.isDigit *> takeCharsWhile isNumChar *> string "#"
      isNumChar c = Char.isDigit c || c `elem` ("eE.bBoOxX_" :: String)
  in super{report= (report super){
        variableIdentifier =
           token (Abstract.name . Text.pack . toString mempty <$> (variableLexeme <> concatAll (string "#"))),
        constructorIdentifier =
           token (Abstract.name . Text.pack . toString mempty <$> (constructorLexeme <> concatAll (string "#"))),
        lPattern = (self & report & aPattern)
                   <|> Abstract.literalPattern
                       <$> wrap ((Abstract.integerLiteral . negate) <$ delimiter "-" <*> integer')
                   <|> Abstract.literalPattern
                       <$> wrap ((Abstract.floatingLiteral . negate) <$ delimiter "-" <*> float')
                   <|> Abstract.constructorPattern
                       <$> wrap (self & report & generalConstructor)
                       <*> some (wrap $ self & report & aPattern),
         literal = Abstract.integerLiteral <$> integer' <|> Abstract.floatingLiteral <$> float'
                   <|> Abstract.charLiteral <$> charLiteral' <|> Abstract.stringLiteral <$> stringLiteral'
                   <|> Abstract.hashLiteral
                       <$> (Abstract.integerLiteral <$> integerHash <|> Abstract.floatingLiteral <$> floatHash
                            <|> Abstract.charLiteral <$> charHashLiteral
                            <|> Abstract.stringLiteral <$> stringHashLiteral)
                   <|> Abstract.hashLiteral . Abstract.hashLiteral
                       <$> (Abstract.integerLiteral <$> integerHash2 <|> Abstract.floatingLiteral <$> floatHash2)}}
     & negationConstraintMixin prefixMinusFollow self

recursiveDoMixin :: (OutlineMonoid t, Abstract.ExtendedWith 'RecursiveDo l,
                     Abstract.DeeplyFoldable (Serialization (Down Int) t) l)
                 => ExtensionOverlay l g t
recursiveDoMixin self super = super{
   report= (report super){
      closedBlockExpresion = (super & report & closedBlockExpresion)
         <|> Abstract.mdoExpression' Abstract.build <$ keyword "mdo" <*> wrap (self & report & statements),
      statement = (super & report & statement)
                  <|> Deep.InL
                      <$> wrap (Abstract.recursiveStatement' Abstract.build
                                . (either id (rewrap Abstract.expressionStatement) . Deep.eitherFromSum . unwrap <$>)
                                <$ keyword "rec"
                                <*> blockOf (self & report & statement)),
      variableIdentifier = notFollowedBy (keyword "mdo" <|> keyword "rec") *> (super & report & variableIdentifier)}}

parallelListComprehensionsMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
parallelListComprehensionsMixin self@ExtendedGrammar{report= HaskellGrammar{qualifiers, expression}} super = super{
   report= (report super){
      bareExpression = (super & report & bareExpression)
                       <|> brackets (Abstract.parallelListComprehension
                                     <$> expression <*> qualifiers <*> qualifiers <*> many qualifiers)}}

tupleSectionsMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
tupleSectionsMixin self@ExtendedGrammar{report= HaskellGrammar{expression}} super = super{
   report= (report super){
      bareExpression = (super & report & bareExpression)
         <|> Abstract.tupleSectionExpression
             <$> parens (filter (\l-> any isJust l && any isNothing l)
                         $ (:|) <$> optional expression <*> some (comma *> optional expression))}}

lambdaCaseMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
lambdaCaseMixin self super = super{
   report= (report super){
      closedBlockExpresion = (super & report & closedBlockExpresion)
         <|> Abstract.lambdaCaseExpression <$ (delimiter "\\" *> keyword "case")
             <*> (self & report & alternatives)}}

emptyCaseMixin :: (OutlineMonoid t, Abstract.ExtendedHaskell l,
                   Deep.Foldable (Serialization (Down Int) t) (Abstract.CaseAlternative l l))
               => ExtensionOverlay l g t
emptyCaseMixin self super = super{
   report= (report super){
      alternatives = blockOf (alternative $ report super)}}

multiWayIfMixin :: (OutlineMonoid t, Abstract.ExtendedHaskell l,
                    Deep.Foldable (Serialization (Down Int) t) (Abstract.GuardedExpression l l))
                => ExtensionOverlay l g t
multiWayIfMixin self@ExtendedGrammar{report= HaskellGrammar{expression, guards, rightArrow}} super = super{
   report= (report super){
      closedBlockExpresion = (super & report & closedBlockExpresion)
         <|> Abstract.multiWayIfExpression <$ keyword "if"
             <*> blockOf' (Abstract.guardedExpression . toList
                           <$> guards <* rightArrow <*> expression)}}

packageImportsMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
packageImportsMixin self super = super{
   report= (report super){
      moduleLevel= (super & report & moduleLevel){
         importDeclaration = (super & report & moduleLevel & importDeclaration)
                             <|> Abstract.packageQualifiedImportDeclaration <$ keyword "import"
                                 <*> (True <$ keyword "qualified" <|> pure False)
                                 <*> (self & report & stringLiteral)
                                 <*> moduleId
                                 <*> optional (keyword "as" *> moduleId)
                                 <*> optional (wrap $ self & report & moduleLevel & importSpecification)}}}

safeImportsMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
safeImportsMixin self super = super{
   report= (report super){
      moduleLevel= (super & report & moduleLevel){
         importDeclaration = (super & report & moduleLevel & importDeclaration)
                             <|> Abstract.safeImportDeclaration <$ keyword "import" <* keyword "safe"
                                 <*> (True <$ keyword "qualified" <|> pure False)
                                 <*> moduleId
                                 <*> optional (keyword "as" *> moduleId)
                                 <*> optional (wrap $ self & report & moduleLevel & importSpecification)}}}

importQualifiedPostMixin :: ExtensionOverlay l g t
importQualifiedPostMixin self super = super{
   report= (report super){
      moduleLevel= (super & report & moduleLevel){
         importDeclaration = (super & report & moduleLevel & importDeclaration)
                             <|> flip Abstract.importDeclaration <$ keyword "import"
                                 <*> moduleId
                                 <*> (True <$ keyword "qualified" <|> pure False)
                                 <*> optional (keyword "as" *> moduleId)
                                 <*> optional (wrap $ self & report & moduleLevel & importSpecification)}}}

safePackageImportsMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
safePackageImportsMixin self super = super{
   report= (report super){
      moduleLevel= (super & report & moduleLevel){
         importDeclaration = (super & report & moduleLevel & importDeclaration)
                             <|> Abstract.safePackageQualifiedImportDeclaration <$ keyword "import" <* keyword "safe"
                                 <*> (True <$ keyword "qualified" <|> pure False)
                                 <*> (self & report & stringLiteral)
                                 <*> moduleId
                                 <*> optional (keyword "as" *> moduleId)
                                 <*> optional (wrap $ self & report & moduleLevel & importSpecification)}}}

packageImportsQualifiedPostMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
packageImportsQualifiedPostMixin self super = super{
   report= (report super){
      moduleLevel= (super & report & moduleLevel){
         importDeclaration = (super & report & moduleLevel & importDeclaration)
                             <|> Abstract.packageQualifiedImportDeclaration <$ keyword "import"
                                 <**> pure flip
                                 <*> (self & report & stringLiteral)
                                 <**> pure flip
                                 <*> moduleId
                                 <*> (True <$ keyword "qualified" <|> pure False)
                                 <*> optional (keyword "as" *> moduleId)
                                 <*> optional (wrap $ self & report & moduleLevel & importSpecification)}}}

safeImportsQualifiedPostMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
safeImportsQualifiedPostMixin self super = super{
   report= (report super){
      moduleLevel= (super & report & moduleLevel){
         importDeclaration = (super & report & moduleLevel & importDeclaration)
                             <|> flip Abstract.safeImportDeclaration <$ keyword "import" <* keyword "safe"
                                 <*> moduleId
                                 <*> (True <$ keyword "qualified" <|> pure False)
                                 <*> optional (keyword "as" *> moduleId)
                                 <*> optional (wrap $ self & report & moduleLevel & importSpecification)}}}

safePackageImportsQualifiedPostMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
safePackageImportsQualifiedPostMixin self super = super{
   report= (report super){
      moduleLevel= (super & report & moduleLevel){
         importDeclaration = (super & report & moduleLevel & importDeclaration)
                             <|> Abstract.safePackageQualifiedImportDeclaration <$ keyword "import" <* keyword "safe"
                                 <**> pure flip
                                 <*> (self & report & stringLiteral)
                                 <**> pure flip
                                 <*> moduleId
                                 <*> (True <$ keyword "qualified" <|> pure False)
                                 <*> optional (keyword "as" *> moduleId)
                                 <*> optional (wrap $ self & report & moduleLevel & importSpecification)}}}

explicitNamespacesMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
explicitNamespacesMixin self super = super{
   report= (report super){
      moduleLevel= (super & report & moduleLevel){
         export = (super & report & moduleLevel & export)
            <|> Abstract.exportClassOrType <$ keyword "type"
                <*> parens (self & report & qualifiedVariableSymbol)
                <*> optional (self & report & moduleLevel & members),
         importItem = (super & report & moduleLevel & importItem)
            <|> Abstract.importClassOrType <$ keyword "type"
                <*> parens ((self & report & variableSymbol) <|> (self & report & constructorSymbol))
                <*> optional (self & report & moduleLevel & members),
         members = parens (Abstract.allMembers <$ delimiter ".."
                           <|> Abstract.explicitlyNamespacedMemberList
                               <$> namespacedMember self `sepEndBy` comma)}}}

blockArgumentsMixin :: ExtensionOverlay l g t
blockArgumentsMixin self super = super{
   report= (report super){
      lExpression = (super & report & lExpression)
         <|> wrap (Abstract.applyExpression <$> (self & report & fExpression)
                                            <*> wrap (self & report & openBlockExpression)),
      dExpression = (self & report & fExpression),
      bareExpression = (super & report & bareExpression) <|> (self & report & closedBlockExpresion)}}

spaceSensitiveOperatorsMixin :: SpaceMonoid t => ExtensionOverlay l g t
spaceSensitiveOperatorsMixin self super = super{
   report= (report super){
      aPattern = Abstract.variablePattern <$> (super & report & variable) <* lookAhead unreservedSymbolLexeme
                 <<|> notFollowedBy unreservedSymbolLexeme *> (super & report & aPattern),
      variableSymbol = (super & report & variableSymbol) <|> Report.nameToken unreservedSymbolLexeme}}

unreservedSymbolLexeme :: (Rank2.Apply g, Ord t, SpaceMonoid t) => Parser g t t
unreservedSymbolLexeme =
   filter precededByOpenSpace getInput
      *> (string "@" <|> string "~") <* filter followedByCloseSpace getInput
   <|> filter (not . precededByOpenSpace) getInput *> string "~"

lexicalNegationMixin :: SpaceMonoid t => ExtensionOverlay l g t
lexicalNegationMixin self super = super{
   report= (report super){
      qualifiedVariableSymbol = notFollowedBy (filter precededByOpenSpace getInput
                                               *> string "-"
                                               *> satisfyCharInput (\c-> Char.isAlphaNum c || c == '(' || c == '['))
                                *> token (nameQualifier <*> (self & report & variableSymbol)),
      prefixNegation = empty,
      bareExpression = (super & report & bareExpression)
         <|> Abstract.applyExpression <$> wrap (Abstract.negate <$ prefixMinus) <*> (self & report & aExpression)
         <|> parens (Abstract.rightSectionExpression
                     <$> (notFollowedBy prefixMinus *> (self & report & qualifiedOperator))
                     <*> (self & report & infixExpression))}}
   where prefixMinus = void (filter precededByOpenSpace getInput
                             *> string "-"
                             <* lookAhead (satisfyCharInput $ \c-> Char.isAlphaNum c || c == '(' || c == '[')
                             <* lift ([[Token Modifier "-"]], ()))
                       <?> "prefix -"

negativeLiteralsMixin :: ExtensionOverlay l g t
negativeLiteralsMixin self super =
   super{
      report= (report super){
         integerLexeme = (negate <$ string "-" <|> pure id) <*> (super & report & integerLexeme),
         floatLexeme = (negate <$ string "-" <|> pure id) <*> (super & report & floatLexeme)}}
   & negationConstraintMixin (satisfyCharInput Char.isDigit) self

binaryLiteralsMixin :: ExtensionOverlay l g t
binaryLiteralsMixin self super = super{
   binary = (string "0b" <|> string "0B") *> (takeCharsWhile1 (\c-> c == '0' || c == '1') <?> "binary number"),
   report = (report super){
      integerLexeme = List.foldl' addBinary 0 . toString mempty <$> (self & binary)
                      <<|> (super & report & integerLexeme)}}
   where addBinary n '0' = 2*n
         addBinary n '1' = 2*n + 1
         addBinary _ _ = error "non-binary"

hexFloatLiteralsMixin :: ExtensionOverlay l g t
hexFloatLiteralsMixin self@ExtendedGrammar{report= HaskellGrammar{decimal, hexadecimal}} super = super{
   report= (report super){
      integerLexeme = notFollowedBy ((string "0x" <|> string "0X")
                                    *> hexadecimal *> satisfyCharInput (`elem` ['.', 'p', 'P']))
                      *> (super & report & integerLexeme),
      floatLexeme = (string "0x" <|> string "0X")
                    *> (readHexFloat <$> hexadecimal <* string "." <*> hexadecimal <*> (hexExponent <<|> pure 0)
                       <|> readHexFloat <$> hexadecimal <*> pure mempty <*> hexExponent)
                    <|> (super & report & floatLexeme)}}
   where hexExponent =
           (string "p" <|> string "P")
           *> (id <$ string "+" <|> negate <$ string "-" <|> pure id)
           <*> (fst . head . Numeric.readDec . toString mempty <$> decimal)
         readHexFloat whole fraction magnitude =
           fst (head $ Numeric.readHex $ toString mempty $ whole <> fraction)
           * 2 ^^ (magnitude - Factorial.length fraction)

numericUnderscoresMixin :: ExtensionOverlay l g t
numericUnderscoresMixin self super = super{
   report= (report super){
      decimal = takeCharsWhile1 Char.isDigit <> concatAll (char '_' *> takeCharsWhile1 Char.isDigit)
                <?> "decimal number",
      octal = takeCharsWhile1 Char.isOctDigit <> concatAll (char '_' *> takeCharsWhile1 Char.isOctDigit)
              <?> "octal number",
      hexadecimal = takeCharsWhile1 Char.isHexDigit <> concatAll (char '_' *> takeCharsWhile1 Char.isHexDigit)
                    <?> "hexadecimal number"}}

binaryUnderscoresMixin :: ExtensionOverlay l g t
binaryUnderscoresMixin self super = super{
   binary = (string "0b" <|> string "0B") *> (binaryDigits <> concatAll (char '_' *> binaryDigits) <?> "binary number")}
   where binaryDigits = takeCharsWhile1 (\c-> c == '0' || c == '1')

parenthesizedTypeOperatorsMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
parenthesizedTypeOperatorsMixin self super =
   super{
      report= (report super){
         moduleLevel= (super & report & moduleLevel){
            export = Abstract.exportVar <$> (self & report & qualifiedVariable)
               <|> Abstract.exportClassOrType
                   <$> ((self & report & qualifiedConstructorIdentifier)
                        <|> parens (self & report & qualifiedConstructorSymbol))
                   <*> pure Nothing
               <|> Abstract.exportClassOrType
                   <$> (self & report & qualifiedTypeConstructor)
                   <*> (Just <$> (self & report & moduleLevel & members))
               <|> Abstract.reExportModule <$ keyword "module" <*> Report.moduleId},
         qualifiedTypeConstructor = (self & report & qualifiedConstructorIdentifier) <|> parens anyQualifiedOperator,
         generalTypeConstructor = (super & report & generalTypeConstructor)
           <|> Abstract.constructorType
               <$> wrap (Abstract.constructorReference <$> parens (self & report & qualifiedVariableSymbol)),
         declarationLevel= (super & report & declarationLevel){
            qualifiedTypeClass =
               (super & report & declarationLevel & qualifiedTypeClass) <|> parens anyQualifiedOperator}}}
   where anyQualifiedOperator =
            (self & report & qualifiedConstructorOperator) <|> (self & report & qualifiedVariableOperator)

typeOperatorsMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
typeOperatorsMixin self super =
   super{
      report= (report super){
         moduleLevel= (super & report & moduleLevel){
            importItem = Abstract.importVar <$> (self & report & variable)
               <|> Abstract.importClassOrType
                   <$> ((self & report & constructorIdentifier) <|> parens (self & report & constructorSymbol))
                   <*> pure Nothing
               <|> Abstract.importClassOrType
                   <$> (self & report & typeConstructor)
                   <*> (Just <$> (self & report & moduleLevel & members))},
         declarationLevel= (super & report & declarationLevel){
            typeClass = (super & report & declarationLevel & typeClass) <|> parens anyOperator,
            simpleType = (super & report & declarationLevel & simpleType)
               <|> Abstract.simpleInfixTypeLHSApplication
                            <$> typeVarBinder self
                            <*> anyOperator
                            <*> typeVarBinder self
               <|> parens ((self & report & declarationLevel & simpleType))},
         typeConstructor = (self & report & constructorIdentifier) <|> parens anyOperator},
      equalityConstraintType = empty,
      cType = (super & cType)
         <|> Abstract.infixTypeApplication
                <$> wrap (self & cType)
                <*> (self & report & qualifiedOperator)
                <*> wrap (self & report & bType),
      instanceTypeDesignatorInsideParens = (super & instanceTypeDesignatorInsideParens)
         <|> Abstract.infixTypeApplication
             <$> wrap (optionallyKindedAndParenthesizedTypeVar self)
             <*> (self & report & qualifiedOperator)
             <*> wrap (optionallyKindedAndParenthesizedTypeVar self),
      classLHS = classLHS super
         <|> Abstract.simpleInfixTypeLHSApplication
             <$> typeVarBinder self
             <*> anyOperator
             <*> typeVarBinder self,
      familyInstanceDesignator = familyInstanceDesignator super
         <|> Abstract.infixTypeClassInstanceLHS
                <$> wrap (super & report & bType)
                <*> (self & report & qualifiedOperator)
                <*> wrap (cType super)}
   where anyOperator = (self & report & constructorOperator) <|> (self & report & variableOperator)

equalityConstraintsMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
equalityConstraintsMixin self super = super{
   equalityConstraintType =
      Abstract.typeEquality <$> wrap (self & report & bType) <* delimiter "~" <*> wrap ((self & report & bType))}

multiParameterConstraintsMixin :: forall l g t. Abstract.ExtendedHaskell l => ExtensionOverlay l g t
multiParameterConstraintsMixin self super = super{
   report= (report super){
      declarationLevel= (super & report & declarationLevel){
         derivingClause =
            keyword "deriving"
            *> (pure <$> wrap (parens (Abstract.strategicDerive Abstract.build
                                       <$> wrap (pure $ Abstract.defaultStrategy @l Abstract.build)
                                       <*> wrap (self & report & typeTerm) `sepBy` comma)
                               <<|> Abstract.simpleDerive <$> (self & report & declarationLevel & qualifiedTypeClass))),
         instanceDesignator =
            Abstract.classReferenceInstanceLHS <$> (self & report & declarationLevel & qualifiedTypeClass)
            <|> Abstract.classInstanceLHSApplication
                <$> wrap (self & report & declarationLevel & instanceDesignator)
                <*> wrap ((self & report & declarationLevel & instanceTypeDesignator)
                          <|> Abstract.typeVariable <$> (self & optionallyParenthesizedTypeVar))}},
   flexibleInstanceDesignator =
      Abstract.classReferenceInstanceLHS <$> (self & report & declarationLevel & qualifiedTypeClass)
      <|> Abstract.classInstanceLHSApplication
          <$> wrap (self & flexibleInstanceDesignator)
          <*> wrap (self & report & aType)
      <|> parens (self & flexibleInstanceDesignator)}

gratuitouslyParenthesizedTypesMixin :: (OutlineMonoid t, Abstract.ExtendedHaskell l,
                                        Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l),
                                        Deep.Foldable (Serialization (Down Int) t) (Abstract.GADTConstructor l l))
                                    => ExtensionOverlay l g t
gratuitouslyParenthesizedTypesMixin self super = super{
   report= (report super){
      declarationLevel = (super & report & declarationLevel){
         topLevelDeclaration = (super & report & declarationLevel & topLevelDeclaration)
            <|> Abstract.classDeclaration <$ keyword "class"
                <*> wrap (self & report & declarationLevel & optionalContext)
                <*> wrap (Abstract.simpleKindedTypeLHS
                          <$> (self & report & declarationLevel & typeClass)
                          <*> ((:[]) <$> parens (typeVarBinder self)))
                <*> (keyword "where" *> blockOf (self & report & declarationLevel & inClassDeclaration) <|> pure []),
         qualifiedTypeClass = (super & report & declarationLevel & qualifiedTypeClass) <|> parens qtc,
         typeVarApplications =
            (self & report & generalTypeConstructor)
            <|> Abstract.typeApplication
                <$> wrap ((self & report & declarationLevel & typeVarApplications)
                          <|> parens (self & report & declarationLevel & typeVarApplications))
                <*> wrap (optionallyKindedAndParenthesizedTypeVar self),
         instanceDesignator = (super & report & declarationLevel & instanceDesignator)
            <|> parens (self & report & declarationLevel & instanceDesignator)}},
   gadtConstructors = (super & gadtConstructors)
      <|> Abstract.gadtConstructors <$> nonTerminal constructorIDs
                                    <* (self & report & doubleColon)
                                    <**> pure uncurry3
                                    <*> (parens forallAndContextAndBody <|> forallAndParenContextBody),
   gadtNewConstructor = (super & gadtNewConstructor)
      <|> Abstract.gadtConstructors <$> ((:|[]) <$> (self & report & constructor))
                                    <* (self & report & doubleColon)
                                    <**> pure uncurry3
                                    <*> parens forallAndNewBody,
   gadtNewBody = (super & gadtNewBody)
      <|> Abstract.functionType
          <$> wrap ((self & report & bType)
                    <|> Abstract.strictType <$ delimiter "!" <*> wrap (self & report & bType))
          <* (self & report & Report.rightArrow)
          <*> wrap paren_return_type
      <|> Abstract.recordFunctionType
          <$> braces ((:[]) <$> wrap (self & report & declarationLevel & fieldDeclaration))
          <* (self & report & Report.rightArrow)
          <*> wrap paren_return_type,
   record_gadt_body = (super & record_gadt_body)
      <|> Abstract.recordFunctionType
          <$> braces (wrap (self & report & declarationLevel & fieldDeclaration) `sepBy` comma)
          <* (self & report & Report.rightArrow)
          <*> wrap paren_return_type,
   instanceTypeDesignatorInsideParens = (super & instanceTypeDesignatorInsideParens)
      <|> parens (self & instanceTypeDesignatorInsideParens),
   optionallyParenthesizedTypeVar = (self & report & typeVar)
                                    <|> parens (optionallyParenthesizedTypeVar self),
   typeVarBinder = Abstract.implicitlyKindedTypeVariable <$> optionallyParenthesizedTypeVar self}
   where qtc = (self & report & declarationLevel & qualifiedTypeClass)
         paren_return_type = parens ((self & return_type) <|> parens paren_return_type)
         optionalContextAndGadtBody =
            contextAndGadtBody <|> (,) <$> wrap (pure Abstract.noContext) <*> wrap (self & gadtBody)
         contextAndGadtBody =
            (,) <$> wrap (self & report & declarationLevel & context)
                <*  (self & report & rightDoubleArrow)
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
                           <|> (self & report & declarationLevel & context)
                               *> (self & report & rightDoubleArrow)
                               *> fail "No context allowed on GADT newtype")
                 <*> wrap (self & gadtNewBody)
            <|> parens forallAndNewBody
         uncurry3 f (a, b, c) = f a b c

flexibleInstancesMixin :: ExtensionOverlay l g t
flexibleInstancesMixin self super = super{
   report= (report super){
             declarationLevel= (super & report & declarationLevel){
                instanceDesignator = flexibleInstanceDesignator self}}}

typeFamiliesMixin :: forall l g t. (OutlineMonoid t, Abstract.ExtendedHaskell l,
                                    Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l),
                                    Deep.Foldable (Serialization (Down Int) t) (Abstract.GADTConstructor l l))
                  => ExtensionOverlay l g t
typeFamiliesMixin self@ExtendedGrammar
                  {report= HaskellGrammar{
                     declarationLevel= DeclarationGrammar{optionalContext, simpleType, derivingClause,
                                                          declaredConstructors, newConstructor}}}
                  super =
  super{
    report= (report super){
      declarationLevel= (super & report & declarationLevel){
         topLevelDeclaration = (super & report & declarationLevel & topLevelDeclaration)
            <|> Abstract.dataFamilyDeclaration <$ keyword "data" <* keyword "family"
                <*> wrap simpleType <*> optional (wrap $ kindSignature self)
            <|> Abstract.openTypeFamilyDeclaration <$ keyword "type" <* keyword "family"
                <*> wrap simpleType <*> optional (wrap $ kindSignature self)
            <|> Abstract.closedTypeFamilyDeclaration <$ keyword "type" <* keyword "family"
                <*> wrap simpleType <*> optional (wrap $ kindSignature self) <* keyword "where"
                <*> blockOf (Abstract.typeFamilyInstance
                             <$> optionalForall self
                             <*> wrap (familyInstanceDesignator self) <* delimiter "="
                             <*> wrap (self & report & typeTerm))
            <|> Abstract.dataFamilyInstance <$ (keyword "data" *> keyword "instance")
                <*> optionalForall self
                <*> wrap optionalContext
                <*> wrap (familyInstanceDesignator self)
                <*> optional (wrap $ kindSignature self)
                <*> moptional (delimiter "=" *> declaredConstructors)
                <*> moptional derivingClause
            <|> Abstract.newtypeFamilyInstance <$ (keyword "newtype" *> keyword "instance")
                <*> optionalForall self
                <*> wrap optionalContext
                <*> wrap (familyInstanceDesignator self)
                <*> optional (wrap $ kindSignature self)
                <* delimiter "="
                <*> wrap newConstructor
                <*> moptional derivingClause
            <|> Abstract.gadtDataFamilyInstance <$ (keyword "data" *> keyword "instance")
                <*> optionalForall self
                <*> wrap (familyInstanceDesignator self)
                <*> optional (wrap $ kindSignature self)
                <* keyword "where"
                <*> blockOf (gadtConstructors self)
                <*> moptional derivingClause
            <|> Abstract.gadtNewtypeFamilyInstance <$ (keyword "newtype" *> keyword "instance")
                <*> optionalForall self
                <*> wrap (familyInstanceDesignator self)
                <*> optional (wrap $ kindSignature self)
                <* keyword "where"
                <*> wrap (gadtNewConstructor self)
                <*> moptional derivingClause
            <|> Abstract.typeFamilyInstance <$ (keyword "type" *> keyword "instance")
                <*> optionalForall self
                <*> wrap (familyInstanceDesignator self)
                <* delimiter "="
                <*> wrap (self & report & typeTerm),
         inClassDeclaration = (super & report & declarationLevel & inClassDeclaration)
            <|> Abstract.dataFamilyDeclaration <$ keyword "data" <* optional (keyword "family")
                <*> wrap simpleType <*> optional (wrap $ kindSignature self)
            <|> Abstract.openTypeFamilyDeclaration <$ keyword "type" <* optional (keyword "family")
                <*> wrap simpleType <*> optional (wrap $ kindSignature self)
            <|> inClassOrInstanceTypeFamilyDeclaration self,
         inInstanceDeclaration = (super & report & declarationLevel & inInstanceDeclaration)
            <|> Abstract.dataFamilyInstance <$ keyword "data" <* optional (keyword "instance")
                <*> optionalForall self
                <*> wrap optionalContext
                <*> wrap (familyInstanceDesignator self)
                <*> optional (wrap $ kindSignature self)
                <*> moptional (delimiter "=" *> declaredConstructors)
                <*> moptional derivingClause
            <|> Abstract.newtypeFamilyInstance <$ keyword "newtype" <* optional (keyword "instance")
                <*> optionalForall self
                <*> wrap optionalContext
                <*> wrap (familyInstanceDesignator self)
                <*> optional (wrap $ kindSignature self)
                <* delimiter "="
                <*> wrap newConstructor
                <*> moptional derivingClause
            <|> Abstract.gadtDataFamilyInstance <$ (keyword "data" *> optional (keyword "instance"))
                <*> optionalForall self
                <*> wrap (familyInstanceDesignator self)
                <*> optional (wrap $ kindSignature self)
                <* keyword "where"
                <*> blockOf (gadtConstructors self)
                <*> moptional derivingClause
            <|> Abstract.gadtNewtypeFamilyInstance <$ (keyword "newtype" *> optional (keyword "instance"))
                <*> optionalForall self
                <*> wrap (familyInstanceDesignator self)
                <*> optional (wrap $ kindSignature self)
                <* keyword "where"
                <*> wrap (gadtNewConstructor self)
                <*> moptional derivingClause
            <|> inClassOrInstanceTypeFamilyDeclaration self}},
    inClassOrInstanceTypeFamilyDeclaration =
       Abstract.typeFamilyInstance <$ keyword "type" <* optional (keyword "instance")
           <*> optionalForall self
           <*> wrap (familyInstanceDesignator self)
           <* delimiter "="
           <*> wrap (self & report & typeTerm)}

typeFamilyDependenciesMixin :: (OutlineMonoid t, Abstract.ExtendedHaskell l,
                                Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l),
                                Deep.Foldable (Serialization (Down Int) t) (Abstract.GADTConstructor l l))
                            => ExtensionOverlay l g t
typeFamilyDependenciesMixin
  self@ExtendedGrammar{report= HaskellGrammar{declarationLevel= DeclarationGrammar{simpleType}}}
  super =
  super{
    report= (report super){
      declarationLevel= (super & report & declarationLevel){
         topLevelDeclaration = (super & report & declarationLevel & topLevelDeclaration)
            <|> Abstract.injectiveOpenTypeFamilyDeclaration <$ keyword "type" <* keyword "family"
                <*> wrap simpleType <* delimiter "="
                <*> typeVarBinder self
                <*> optional dependencies
            <|> Abstract.injectiveClosedTypeFamilyDeclaration <$ keyword "type" <* keyword "family"
                <*> wrap simpleType <* delimiter "="
                <*> typeVarBinder self
                <*> optional dependencies
                <* keyword "where"
                <*> blockOf (Abstract.typeFamilyInstance
                             <$> optionalForall self
                             <*> wrap (familyInstanceDesignator self) <* delimiter "="
                             <*> wrap (self & report & typeTerm)),
         inClassDeclaration = (super & report & declarationLevel & inClassDeclaration)
            <|> Abstract.injectiveOpenTypeFamilyDeclaration <$ keyword "type" <* optional (keyword "family")
                <*> wrap simpleType <* delimiter "="
                <*> typeVarBinder self
                <*> (Just <$> dependencies)}}}
   where dependencies = (,) <$> (delimiter "|" *> (self & report & typeVar)) <* (self & report & rightArrow)
                            <*> someNonEmpty (self & report & typeVar)

dataKindsMixin :: forall l g t. (Abstract.ExtendedHaskell l, TextualMonoid t) => ExtensionOverlay l g t
dataKindsMixin self super = super{
   report= (report super){
      aType = (super & report & aType)
         <|> promotedLiteral self
         <|> Abstract.promotedTupleType <$ terminator "'"
             <*> parens ((:|) <$> wrap (self & report & typeTerm)
                              <*> some (comma *> wrap (self & report & typeTerm)))
         <|> Abstract.promotedListType <$ terminator "'"
                                       <*> brackets (wrap (self & report & typeTerm) `sepBy1` comma)
         <|> Abstract.promotedListType
             <$> brackets ((:) <$> wrap (self & report & typeTerm) <* comma
                               <*> wrap (self & report & typeTerm) `sepBy1` comma),
      generalTypeConstructor = (super & report & generalTypeConstructor)
         <|> Abstract.promotedConstructorType <$ terminator "'" <*> wrap (self & report & generalConstructor),
      declarationLevel= (super & report & declarationLevel){
         instanceTypeDesignator = (super & report & declarationLevel & instanceTypeDesignator)
            <|> promotedLiteral self}}}

dataKindsTypeOperatorsMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
dataKindsTypeOperatorsMixin self super = super{
   instanceTypeDesignatorInsideParens = (super & instanceTypeDesignatorInsideParens)
      <|> Abstract.promotedInfixTypeApplication
          <$> wrap (optionallyKindedAndParenthesizedTypeVar self)
          <* terminator "'"
          <*> (self & report & qualifiedOperator)
          <*> wrap (optionallyKindedAndParenthesizedTypeVar self),
   cType = (super & cType)
      <|> Abstract.promotedInfixTypeApplication
          <$> wrap (cType self)
          <* terminator "'"
          <*> (self & report & qualifiedOperator)
          <*> wrap (self & report & bType),
   familyInstanceDesignator = familyInstanceDesignator super
      <|> Abstract.infixTypeClassInstanceLHS
          <$> wrap (self & report & bType)
          <* terminator "'"
          <*> (self & report & qualifiedOperator)
          <*> wrap (cType self)}

visibleDependentKindQualificationMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
visibleDependentKindQualificationMixin self super = super{
   arrowType = arrowType super
      <|> Abstract.visibleDependentType
          <$ keywordForall self
          <*> many (typeVarBinder self)
          <* (self & report & rightArrow)
          <*> wrap (arrowType self)}

kindSignaturesBaseMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
kindSignaturesBaseMixin self super = super{
   kindSignature = Abstract.typeKind <$ (self & report & doubleColon) <*> wrap (self & report & typeTerm)}

starIsTypeMixin :: ExtensionOverlay l g t
starIsTypeMixin self super = super{
   groundTypeKind = groundTypeKind super <|> delimiter "*"}

unicodeStarIsTypeMixin :: ExtensionOverlay l g t
unicodeStarIsTypeMixin self super = super{
   groundTypeKind = groundTypeKind super <|> delimiter "★"}

starIsTypeOperatorsMixin :: ExtensionOverlay l g t
starIsTypeOperatorsMixin self super = super{
   report = (report super) {
      aType = parens (Abstract.constructorType
                      <$> wrap (Abstract.constructorReference . Abstract.qualifiedName Nothing
                                <$> token (Report.nameToken $ string "*")))
              <<|> (super & report & aType)}}

roleAnnotationsMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
roleAnnotationsMixin self super = super{
   report = (report super) {
      declarationLevel= (super & report & declarationLevel) {
         topLevelDeclaration = (super & report & declarationLevel & topLevelDeclaration)
            <|> Abstract.typeRoleDeclaration <$ keyword "type" <* keyword "role"
                <*> (self & report & qualifiedTypeConstructor)
                <*> some (Abstract.nominalRole <$ keyword "nominal"
                          <|> Abstract.representationalRole <$ keyword "representational"
                          <|> Abstract.phantomRole <$ keyword "phantom"
                          <|> Abstract.inferredRole <$ keyword "_")
         }
      }
   }

typeApplicationsMixin :: (Abstract.ExtendedHaskell l, Abstract.DeeplyFoldable (Serialization (Down Int) t) l,
                          SpaceMonoid t) => ExtensionOverlay l g t
typeApplicationsMixin self super = super{
   report = (report super){
      bType = (super & report & bType)
         <|> Abstract.visibleKindApplication
             <$> filter whiteSpaceTrailing (wrap $ self & report & bType)
             <* typeApplicationDelimiter
             <*> wrap (Abstract.typeKind <$> wrap (self & report & aType)),
      bareExpression = (super & report & bareExpression)
         <|> Abstract.visibleTypeApplication
             <$> filter whiteSpaceTrailing (self & report & aExpression)
             <* typeApplicationDelimiter
             <*> wrap (self & report & aType),
      lPattern = (super & report & lPattern)
         <|> Abstract.constructorPatternWithTypeApplications
             <$> filter whiteSpaceTrailing (wrap $ self & report & generalConstructor)
             <*> some (typeApplicationDelimiter *> wrap (self & report & aType))
             <*> many (wrap $ self & report & aPattern)
      },
   return_type = (super & return_type)
      <|> Abstract.visibleKindApplication
          <$> filter whiteSpaceTrailing (wrap $ self & return_type)
          <* typeApplicationDelimiter
          <*> wrap (Abstract.typeKind <$> wrap (self & report & aType)),
   typeVarBinder = typeVarBinder super
      <|> Abstract.inferredTypeVariable <$> braces (self & report & typeVar),
   flexibleInstanceDesignator = flexibleInstanceDesignator super
      <|> Abstract.classInstanceLHSKindApplication
          <$> filter whiteSpaceTrailing (wrap $ self & flexibleInstanceDesignator)
          <* typeApplicationDelimiter
          <*> wrap (Abstract.typeKind <$> wrap (self & report & aType)),
   familyInstanceDesignatorApplications = familyInstanceDesignatorApplications super
      <|> Abstract.classInstanceLHSKindApplication
          <$> filter whiteSpaceTrailing (wrap $ self & familyInstanceDesignatorApplications)
          <* typeApplicationDelimiter
          <*> wrap (Abstract.typeKind <$> wrap (self & report & aType))}
   where typeApplicationDelimiter = notFollowedBy unreservedSymbolLexeme *> delimiter "@"

linearTypesMixin :: (SpaceMonoid t, Abstract.ExtendedHaskell l) => ExtensionOverlay l g t
linearTypesMixin self super = super{
   report= (report super){
      variableSymbol = notFollowedBy prefixPercent *> (super & report & variableSymbol)},
   arrowType = (super & arrowType)
      <|> Abstract.linearFunctionType
          <$> wrap (self & cType)
          <* token prefixPercent
          <* keyword "1"
          <* (self & report & rightArrow)
          <*> wrap (self & arrowType)
      <|> Abstract.multiplicityFunctionType
          <$> wrap (self & cType)
          <* token prefixPercent
          <* notFollowedBy (keyword "1")
          <*> wrap (self & report & aType)
          <* (self & report & rightArrow)
          <*> wrap (self & arrowType)}
   where prefixPercent =
            filter precededByOpenSpace getInput *> string "%" <* filter (not . followedByCloseSpace) getInput

gadtLinearTypesMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
gadtLinearTypesMixin self super = super{
  prefix_gadt_body = (super & prefix_gadt_body)
    <|> Abstract.linearFunctionType
        <$> wrap ((self & report & bType) <|> Abstract.strictType <$ delimiter "!" <*> wrap (self & report & bType))
        <* delimiter "%"
        <* keyword "1"
        <* (self & report & rightArrow)
        <*> wrap (self & prefix_gadt_body)
    <|> Abstract.multiplicityFunctionType
        <$> wrap ((self & report & bType) <|> Abstract.strictType <$ delimiter "!" <*> wrap (self & report & bType))
        <* delimiter "%"
        <* notFollowedBy (keyword "1")
        <*> wrap (self & report & aType)
        <* (self & report & rightArrow)
        <*> wrap (self & prefix_gadt_body),
  gadtNewBody = (super & gadtNewBody)
    <|> Abstract.linearFunctionType
        <$> wrap ((self & report & bType) <|> Abstract.strictType <$ delimiter "!" <*> wrap (self & report & bType))
        <* delimiter "%"
        <* keyword "1"
        <* (self & report & rightArrow)
        <*> wrap (self & return_type)
    <|> Abstract.multiplicityFunctionType
        <$> wrap ((self & report & bType) <|> Abstract.strictType <$ delimiter "!" <*> wrap (self & report & bType))
        <* delimiter "%"
        <* notFollowedBy (keyword "1")
        <*> wrap (self & report & aType)
        <* (self & report & rightArrow)
        <*> wrap (self & return_type)}

unicodeLinearTypesMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
unicodeLinearTypesMixin self super = super{
  arrowType = (super & arrowType)
    <|> Abstract.linearFunctionType
        <$> wrap (self & cType)
        <* delimiter "⊸"
        <*> wrap (self & arrowType)}

gadtUnicodeLinearTypesMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
gadtUnicodeLinearTypesMixin self super = super{
  prefix_gadt_body = (super & prefix_gadt_body)
    <|> Abstract.linearFunctionType
        <$> wrap ((self & report & bType) <|> Abstract.strictType <$ delimiter "!" <*> wrap (self & report & bType))
        <* delimiter "⊸"
        <*> wrap (self & prefix_gadt_body),
  gadtNewBody = (super & gadtNewBody)
    <|> Abstract.linearFunctionType
        <$> wrap ((self & report & bType) <|> Abstract.strictType <$ delimiter "!" <*> wrap (self & report & bType))
        <* delimiter "⊸"
        <*> wrap (self & return_type)}

standaloneKindSignaturesMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
standaloneKindSignaturesMixin self super = super{
   report= (report super){
      declarationLevel= (super & report & declarationLevel){
         topLevelDeclaration = (super & report & declarationLevel & topLevelDeclaration)
            <|> Abstract.kindSignature <$ keyword "type"
                  <*> (self & report & typeConstructor)
                  <*> wrap (self & kindSignature)}}}

kindSignaturesMixin :: (OutlineMonoid t, Abstract.ExtendedHaskell l,
                        Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l))
                    => ExtensionOverlay l g t
kindSignaturesMixin
   self@ExtendedGrammar{
     report= HaskellGrammar{
        declarationLevel= DeclarationGrammar{optionalContext, declaredConstructors, newConstructor,
                                             simpleType, typeClass, derivingClause, inClassDeclaration}}}
   super =
   super{
      report= (report super){
         declarationLevel= (super & report & declarationLevel){
            topLevelDeclaration = (super & report & declarationLevel & topLevelDeclaration)
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
                      <*> moptional derivingClause
               <|> Abstract.classDeclaration
                      <$ keyword "class"
                      <*> wrap optionalContext
                      <*> wrap (Abstract.simpleTypeLHSApplication
                                   <$> wrap (Abstract.simpleTypeLHS <$> typeClass <*> pure [])
                                   <*> parens (Abstract.explicitlyKindedTypeVariable
                                               <$> (self & report & typeVar)
                                               <*> wrap (kindSignature self)))
                      <*> (keyword "where"
                           *> blockOf inClassDeclaration
                           <|> pure []),
            typeVarTuple = (:|) <$> wrap (optionallyKindedTypeVar self)
                                <*> some (comma *> wrap (optionallyKindedTypeVar self))},
         typeTerm = (super & report & typeTerm) <|>
            Abstract.kindedType <$> wrap (self & report & typeTerm) <*> wrap (kindSignature self)},
      instanceTypeDesignatorInsideParens = (super & instanceTypeDesignatorInsideParens)
         <|> Abstract.kindedType
             <$> wrap (self & instanceTypeDesignatorInsideParens)
             <*> wrap (kindSignature self),
      optionallyKindedAndParenthesizedTypeVar =
         Abstract.typeVariable <$> optionallyParenthesizedTypeVar self
         <|> parens (Abstract.kindedType
                     <$> wrap (Abstract.typeVariable <$> optionallyParenthesizedTypeVar self)
                     <*> wrap (kindSignature self)),
      optionallyKindedTypeVar =
         Abstract.typeVariable <$> (self & report & typeVar)
         <|> Abstract.kindedType
             <$> wrap (Abstract.typeVariable <$> (self & report & typeVar))
             <*> wrap (kindSignature self),
      typeVarBinder = typeVarBinder super
                      <|> parens (Abstract.explicitlyKindedTypeVariable
                                  <$> (self & report & typeVar)
                                  <*> wrap (kindSignature self))}

existentialQuantificationMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
existentialQuantificationMixin self super = super{
   report= (report super){
      declarationLevel= (super & report & declarationLevel){
         declaredConstructor = (super & report & declarationLevel & declaredConstructor)
            <|> Abstract.existentialConstructor
                <$ keywordForall self
                <*> many (typeVarBinder self) <* delimiter "."
                <*> wrap (self & report & declarationLevel & optionalContext)
                <*> wrap (super & report & declarationLevel & declaredConstructor)
            <|> Abstract.existentialConstructor []
                <$> wrap (self & report & declarationLevel & context)
                <* (self & report & rightDoubleArrow)
                <*> wrap (super & report & declarationLevel & declaredConstructor)}}}

scopedTypeVariablesMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
scopedTypeVariablesMixin self super = super{
   report= (report super){
      pattern = (super & report & pattern)
         <|> Abstract.typedPattern
                <$> wrap (self & infixPattern)
                <* (self & report & doubleColon)
                <*> wrap (self & report & typeTerm)}}

explicitForAllMixin :: (OutlineMonoid t, Abstract.ExtendedHaskell l,
                        Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l))
                    => ExtensionOverlay l g t
explicitForAllMixin
   self@ExtendedGrammar{
      report= HaskellGrammar{
         declarationLevel= DeclarationGrammar{context, optionalContext, instanceDesignator}}}
   super = super{
      report= (report super){
         declarationLevel= (super & report & declarationLevel){
            optionalTypeSignatureContext = pure Abstract.noContext,
            topLevelDeclaration = (super & report & declarationLevel & topLevelDeclaration)
               <|> Abstract.explicitlyScopedInstanceDeclaration <$ keyword "instance"
                   <* keywordForall self
                   <*> many (typeVarBinder self)
                   <* delimiter "."
                   <*> wrap optionalContext
                   <*> wrap instanceDesignator
                   <*> (keyword "where"
                        *> blockOf (self & report & declarationLevel & inInstanceDeclaration)
                        <|> pure [])},
         typeVar = notFollowedBy (keywordForall self) *> (super & report & typeVar)},
      arrowType = arrowType super
         <|> Abstract.forallType <$ keywordForall self
             <*> many (typeVarBinder self) <* delimiter "."
             <*> wrap (arrowType self),
      optionalForall = keywordForall self *> many (typeVarBinder self) <* delimiter "." <<|> pure []}

gadtSyntaxMixin :: (OutlineMonoid t, Abstract.ExtendedHaskell l,
                    Deep.Foldable (Serialization (Down Int) t) (Abstract.GADTConstructor l l))
                => ExtensionOverlay l g t
gadtSyntaxMixin
   self@ExtendedGrammar{report= HaskellGrammar{declarationLevel= DeclarationGrammar{simpleType, derivingClause}}}
   super = super{
      report= (report super){
         declarationLevel= (super & report & declarationLevel){
            topLevelDeclaration = (super & report & declarationLevel & topLevelDeclaration)
               <|> Abstract.gadtDeclaration <$ keyword "data"
                   <*> wrap simpleType
                   <*> optional (wrap $ kindSignature self) <* keyword "where"
                   <*> blockOf (gadtConstructors self)
                   <*> moptional derivingClause
               <|> Abstract.gadtNewtypeDeclaration <$ keyword "newtype"
                   <*> wrap simpleType
                   <*> optional (wrap $ kindSignature self) <* keyword "where"
                   <*> wrap (gadtNewConstructor self)
                   <*> moptional derivingClause}},
      optionalForall = keywordForall self *> many (typeVarBinder self) <* delimiter "." <<|> pure []}

gadtSyntaxTypeOperatorsMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
gadtSyntaxTypeOperatorsMixin self super = super{
   prefix_gadt_body = (super & prefix_gadt_body)
      <|> Abstract.functionType
             <$> wrap (Abstract.infixTypeApplication
                          <$> wrap (self & cType)
                          <*> (self & report & qualifiedOperator)
                          <*> wrap (self & report & bType))
             <* nonTerminal (Report.rightArrow . report)
             <*> wrap (nonTerminal prefix_gadt_body),
   return_type = return_type super <|>
       Abstract.infixTypeApplication <$> wrap (arg_type self)
                                     <*> (self & report & qualifiedOperator)
                                     <*> wrap (arg_type self)}

dataKindsGadtSyntaxTypeOperatorsMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
dataKindsGadtSyntaxTypeOperatorsMixin self super =
   super{
      return_type = return_type super <|>
         Abstract.promotedInfixTypeApplication
         <$> wrap (arg_type self)
         <* terminator "'"
         <*> (self & report & qualifiedOperator)
         <*> wrap (arg_type self)}

namedFieldPunsMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
namedFieldPunsMixin self super =
   super{
      report = (report super){
         fieldBinding = (super & report & fieldBinding) <|>
            Abstract.punnedFieldBinding <$> (self & report & qualifiedVariable),
         fieldPattern = (super & report & fieldPattern) <|>
            Abstract.punnedFieldPattern <$> (self & report & qualifiedVariable)}}

recordWildCardsMixin :: Abstract.ExtendedWith 'RecordWildCards l => ExtensionOverlay l g t
recordWildCardsMixin self super =
   super{
      report = (report super){
         bareExpression = (super & report & bareExpression)
            <|> Abstract.wildcardRecordExpression' Abstract.build <$> (self & report & qualifiedConstructor)
                <*> braces (wrap (self & report & fieldBinding) `endBy` comma <* delimiter ".."),
         aPattern = (super & report & aPattern)
            <|> Abstract.wildcardRecordPattern' Abstract.build <$> (self & report & qualifiedConstructor)
                <*> braces (wrap (self & report & fieldPattern) `endBy` comma <* delimiter "..")}}

overloadedRecordDotMixin :: Abstract.ExtendedHaskell l => ExtensionOverlay l g t
overloadedRecordDotMixin self super =
   super{
      report = (report super){
         qualifiedVariableSymbol =
            notFollowedBy (string "." *> satisfyCharInput varStart) *> (super & report & qualifiedVariableSymbol),
         bareExpression = (super & report & bareExpression) <|>
            Abstract.getField <$> (self & report & aExpression) <* prefixDot <*> (self & report & variableIdentifier)
            <|>
            Abstract.fieldProjection <$> parens (someNonEmpty $ prefixDot *> (self & report & variableIdentifier))}}
   where prefixDot = void (string "."
                           <* lookAhead (satisfyCharInput varStart)
                           <* lift ([[Token Modifier "."]], ()))
                     <?> "prefix ."

bangPatternsMixin :: (SpaceMonoid t, Abstract.ExtendedWith 'BangPatterns l) => ExtensionOverlay l g t
bangPatternsMixin self super =
   super{
      report = (report super){
         aPattern = (super & report & aPattern)
            <|> Abstract.bangPattern Abstract.build <$ bang <*> wrap (self & report & aPattern),
         variableOperator = notFollowedBy bang *> (super & report & variableOperator)}}
   where bang = filter precededByOpenSpace getInput
                *> string "!"
                <* notSatisfyChar Char.isSpace
                <* notFollowedBy Report.comment
                <* lift ([[Token Delimiter "!"]], ())

viewPatternsMixin :: Abstract.ExtendedWith 'ViewPatterns l => ExtensionOverlay l g t
viewPatternsMixin self super =
   super{
      report = (report super){
         pPattern = (super & report & pPattern)
            <|> Abstract.viewPattern Abstract.build
                <$> (self & report & expression)
                <* (self & report & rightArrow)
                <*> wrap (self & report & pPattern)}}

nPlusKPatternsMixin :: Abstract.ExtendedWith 'NPlusKPatterns l => ExtensionOverlay l g t
nPlusKPatternsMixin self super =
   super{
      report = (report super){
         pPattern = (super & report & pPattern)
            <|> Abstract.nPlusKPattern Abstract.build
                <$> (self & report & variable)
                <* delimiter "+"
                <*> (self & report & integer)}}

patternSynonymsMixin :: forall l g t. (OutlineMonoid t, Abstract.ExtendedWith 'PatternSynonyms l,
                                       Deep.Foldable (Serialization (Down Int) t) (Abstract.PatternEquationClause l l),
                                       Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l))
                     => ExtensionOverlay l g t
patternSynonymsMixin self super =
   super{
      report= (report super){
         moduleLevel= (super & report & moduleLevel){
            export = (super & report & moduleLevel & export)
               <|> Abstract.exportPattern Abstract.build <$ keyword "pattern"
                   <*> (self & report & qualifiedConstructor),
            importItem = (super & report & moduleLevel & importItem)
               <|> Abstract.importPattern Abstract.build <$ keyword "pattern"
                   <*> (self & report & constructor),
            members = (super & report & moduleLevel & members)
               <|> parens (Abstract.allMembersPlus Abstract.build
                           <$> filter (not . null)
                                  (moptional ((self & report & moduleLevel & cname) `sepBy` comma <* comma)
                                   <> ([] <$ delimiter "..")
                                   <> moptional (comma *> (self & report & moduleLevel & cname) `sepEndBy` comma)))},
         declarationLevel= (super & report & declarationLevel){
            topLevelDeclaration = (super & report & declarationLevel & topLevelDeclaration)
               <|> keyword "pattern" *>
               (Abstract.implicitPatternSynonym Abstract.build
                   <$> wrap lhsPattern <* delimiter "=" <*> wrap (self & report & pattern)
                <|> Abstract.unidirectionalPatternSynonym Abstract.build
                       <$> wrap lhsPattern <* (self & report & leftArrow) <*> wrap (self & report & pattern)
                <|> Abstract.explicitPatternSynonym Abstract.build
                       <$> wrap lhsPattern
                       <* (self & report & leftArrow)
                       <*> wrap (self & report & pattern)
                       <*> patternClauses
                <|> Abstract.patternSynonymSignature Abstract.build
                       <$> (self & report & constructor) `sepByNonEmpty` comma
                       <* (self & report & doubleColon)
                       <*> optionalForall self
                       <*> wrap ((self & report & declarationLevel & context) <* (self & report & rightDoubleArrow)
                                 <<|> pure Abstract.noContext)
                       <*> optionalForall self
                       <*> wrap (self & report & declarationLevel & optionalContext)
                       <*> many (wrap (self & cType) <* (self & report & rightArrow))
                       <*> wrap (self & cType))},
         variableIdentifier = notFollowedBy (keyword "pattern") *> (super & report & variableIdentifier)}}
   where lhsPattern =
            Abstract.prefixPatternLHS Abstract.build
               <$> (self & report & constructor)
               <*> many (self & report & variableIdentifier)
            <|> Abstract.infixPatternLHS Abstract.build
                   <$> (self & report & variableIdentifier)
                   <*> (self & report & constructorOperator)
                   <*> (self & report & variableIdentifier)
            <|> Abstract.recordPatternLHS Abstract.build
                   <$> (self & report & constructor)
                   <*> braces ((self & report & variableIdentifier) `sepBy` comma)
         patternClauses = keyword "where" *> blockOf patternClause
         patternClause = Abstract.patternEquationClause @l Abstract.build
                         <$> wrap patternClauseLHS
                         <*> wrap (self & report & rhs)
                         <*> (self & report & declarationLevel & whereClauses)
         patternClauseLHS =
            Abstract.prefixPatternEquationLHS Abstract.build
               <$> (self & report & constructor)
               <*> many (wrap (self & report & pattern))
            <|> Abstract.infixPatternEquationLHS Abstract.build
                   <$> wrap (self & report & pattern)
                   <*> (self & report & constructor)
                   <*> wrap (self & report & pattern)

standaloneDerivingMixin :: Abstract.ExtendedWith 'StandaloneDeriving l => ExtensionOverlay l g t
standaloneDerivingMixin self@ExtendedGrammar{
                           report= HaskellGrammar{
                              declarationLevel= DeclarationGrammar{optionalContext, instanceDesignator}}}
                        super@ExtendedGrammar{report= report@HaskellGrammar{declarationLevel}} =
   super{
      report = report{
         declarationLevel= declarationLevel{
            topLevelDeclaration = (declarationLevel & topLevelDeclaration)
               <|> Abstract.standaloneDerivingDeclaration Abstract.build <$ keyword "deriving" <* keyword "instance"
                   <*> wrap optionalContext
                   <*> wrap instanceDesignator}}}

derivingStrategiesMixin :: forall l g t. Abstract.ExtendedWith 'DerivingStrategies l => ExtensionOverlay l g t
derivingStrategiesMixin self@ExtendedGrammar{
                           report= HaskellGrammar{generalConstructor, typeTerm}}
                        super@ExtendedGrammar{report= HaskellGrammar{declarationLevel}} =
   super{
      report = (report super){
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

standaloneDerivingStrategiesMixin :: (Abstract.ExtendedWith 'StandaloneDeriving l,
                                      Abstract.ExtendedWith 'DerivingStrategies l)
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
                      <*> wrap optionalContext
                      <*> wrap instanceDesignator}}}

derivingViaMixin :: forall l g t. Abstract.ExtendedWith 'DerivingVia l => ExtensionOverlay l g t
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

standaloneDerivingViaMixin :: forall l g t. (Abstract.ExtendedWith 'StandaloneDeriving l,
                                             Abstract.ExtendedWith 'DerivingStrategies l,
                                             Abstract.ExtendedWith 'DerivingVia l)
                           => ExtensionOverlay l g t
standaloneDerivingViaMixin self@ExtendedGrammar{
                              report= HaskellGrammar{
                                 declarationLevel= DeclarationGrammar{optionalContext, instanceDesignator}}}
                           super@ExtendedGrammar{report= HaskellGrammar{declarationLevel}} =
   super{
      report = (report super){
         declarationLevel= declarationLevel{
            topLevelDeclaration = (declarationLevel & topLevelDeclaration)
               <|> Abstract.standaloneStrategicDerivingDeclaration Abstract.build
                      <$ keyword "deriving"
                      <*> wrap derivingVia
                      <* keyword "instance"
                      <*> wrap optionalContext
                      <*> wrap instanceDesignator}}}
   where derivingVia = Abstract.derivingViaStrategy @l Abstract.build <$ keyword "via"
                       <*> wrap (self & report & typeTerm)

mptcsMixin :: forall l g t. (OutlineMonoid t, Abstract.ExtendedHaskell l,
                             Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l))
           => ExtensionOverlay l g t
mptcsMixin
   self@ExtendedGrammar{
     typeVarBinder,
     report= HaskellGrammar{
        declarationLevel= DeclarationGrammar{optionalContext, typeClass, inClassDeclaration}}}
   super =
   super{
      report= (report super){
         declarationLevel= (super & report & declarationLevel){
            topLevelDeclaration = (super & report & declarationLevel & topLevelDeclaration)
               <|> Abstract.classDeclaration
                      <$ keyword "class"
                      <*> wrap optionalContext
                      <*> wrap (Abstract.simpleTypeLHS <$> typeClass <*> pure [] <|> classLHS self)
                      <*> moptional (keyword "where" *> blockOf inClassDeclaration)}},
      classLHS = classLHS super
         <|> Abstract.simpleTypeLHSApplication
                <$> wrap (Abstract.simpleTypeLHSApplication
                             <$> wrap (Abstract.simpleTypeLHS <$> typeClass <*> pure [])
                             <*> typeVarBinder)
                <*> typeVarBinder
         <|> Abstract.simpleInfixTypeLHSApplication
             <$> typeVarBinder
             <* terminator "`"
             <*> typeClass
             <* terminator "`"
             <*> typeVarBinder
         <|> parens (classLHS self)
         <|> Abstract.simpleTypeLHSApplication <$> wrap (classLHS self) <*> typeVarBinder}

functionalDependenciesMixin :: forall l g t. (OutlineMonoid t, Abstract.ExtendedWith 'FunctionalDependencies l,
                                              Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l))
                            => ExtensionOverlay l g t
functionalDependenciesMixin
   self@ExtendedGrammar{
     classLHS, typeVarBinder,
     report= HaskellGrammar{
        rightArrow, typeVar,
        declarationLevel= DeclarationGrammar{optionalContext, inClassDeclaration}}}
   super =
   super{
      report= (report super){
         declarationLevel= (super & report & declarationLevel){
            topLevelDeclaration = (super & report & declarationLevel & topLevelDeclaration)
               <|> Abstract.fundepClassDeclaration Abstract.build
                      <$ keyword "class"
                      <*> wrap optionalContext
                      <*> wrap classLHS
                      <* delimiter "|"
                      <*> wrap (Abstract.functionalDependency Abstract.build
                                   <$> someNonEmpty typeVar <* rightArrow <*> someNonEmpty typeVar)
                         `sepBy` comma
                      <*> moptional (keyword "where" *> blockOf inClassDeclaration)}}}

flexibleContextsMixin :: forall l g t. Abstract.ExtendedHaskell l => ExtensionOverlay l g t
flexibleContextsMixin self super =
   super{
      report= (report super){
         declarationLevel= (super & report & declarationLevel){
            context = Abstract.typeConstraint <$> wrap (self & cType)}}}

instanceSignaturesMixin :: ExtensionOverlay l g t
instanceSignaturesMixin
   self@ExtendedGrammar{
      report= HaskellGrammar{doubleColon, typeTerm,
                             declarationLevel= DeclarationGrammar{optionalTypeSignatureContext, variables}}}
   super =
   super{
      report= (report super){
         declarationLevel= (super & report & declarationLevel){
            inInstanceDeclaration = (super & report & declarationLevel & inInstanceDeclaration)
               <|> Abstract.typeSignature <$> variables <* doubleColon <*> wrap optionalTypeSignatureContext
                                          <*> wrap typeTerm}}}

defaultSignaturesMixin :: Abstract.ExtendedWith 'DefaultSignatures l => ExtensionOverlay l g t
defaultSignaturesMixin
   self@ExtendedGrammar{
      report= HaskellGrammar{doubleColon, typeTerm, variable,
                             declarationLevel= DeclarationGrammar{optionalTypeSignatureContext}}}
   super =
   super{
      report= (report super){
         declarationLevel= (super & report & declarationLevel){
            inClassDeclaration = (super & report & declarationLevel & inClassDeclaration)
               <|> Abstract.defaultMethodSignature Abstract.build <$ keyword "default"
                      <*> variable <* doubleColon <*> wrap optionalTypeSignatureContext <*> wrap typeTerm}}}

-- | Not an extension by itself, common to magicHashMixin and negativeLiteralsMixin.
negationConstraintMixin :: Parser g t t -> ExtensionOverlay l g t
negationConstraintMixin prefixMinusFollow
                        self@ExtendedGrammar{
                           report= HaskellGrammar{
                              dExpression, infixExpression, leftInfixExpression, qualifiedOperator}}
                        super = super{
   report= (report super){
      variableSymbol = negationGuard *> (super & report & variableSymbol),
      qualifiedVariableSymbol = negationGuard *> (super & report & qualifiedVariableSymbol),
      prefixNegation = negationGuard *> (super & report & prefixNegation)}}
   where negationGuard = notFollowedBy (string "-" *> prefixMinusFollow)

nondecreasingIndentationMixin :: (Deep.Foldable (Serialization (Down Int) t) (Abstract.Expression l l),
                                  Deep.Foldable (Serialization (Down Int) t) (Abstract.Statement l l),
                                  OutlineMonoid t)
                              => ExtensionOverlay l g t
nondecreasingIndentationMixin self super =
   super{
      report = (report super){
         statements = Report.blockWith nonDecreasingIndentLine Report.blockTerminatorKeyword (self & report & statement)
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
         => Parser g t (node (NodeWrap t) (NodeWrap t))
         -> Parser g t [NodeWrap t (node (NodeWrap t) (NodeWrap t))]
blockOf' p = braces (many (many semi *> wrap p) <* many semi) <|> (inputColumn >>= alignedBlock pure)
   where alignedBlock cont indent =
            do rest <- getInput
               item <- filter (oneExtendedLine indent rest) (wrap p)
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
