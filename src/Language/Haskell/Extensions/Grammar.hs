{-# Language FlexibleContexts, FlexibleInstances, NamedFieldPuns, OverloadedStrings,
             Rank2Types, RecordWildCards, ScopedTypeVariables,
             TemplateHaskell, TupleSections, TypeApplications, TypeFamilies, TypeSynonymInstances #-}

-- | Missing syntax extensions:
-- * @QualifiedDo@ requires TemplateHaskell 2.17
-- * @TransformListComp@ is not supported by TemplateHaskell
-- * @Arrows@ is not supported by TemplateHaskell
-- * @LexicalNegation@ ignores the presence or absence of whitespace preceding the minus

module Language.Haskell.Extensions.Grammar (extendedGrammar, parseModule, report, module Report) where

import Debug.Trace

import Control.Applicative
import Control.Monad (void)
import qualified Data.Char as Char
import Data.Foldable (fold, toList)
import Data.Function ((&))
import Data.Functor.Compose (Compose(getCompose))
import Data.List (foldl', null, sortOn)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Ord (Down)
import Data.Maybe (isJust, isNothing)
import Data.Monoid (Dual(..), Endo(..))
import Data.Monoid.Instances.Positioned (LinePositioned, column)
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
import Text.Parser.Combinators (eof, sepBy, sepBy1, sepByNonEmpty)
import Text.Parser.Token (braces, brackets, comma, parens)
import Text.Grampa
import Text.Grampa.Combinators (someNonEmpty)
import Text.Grampa.ContextFree.LeftRecursive.Transformer (ParserT, lift)
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

import Prelude hiding (exponent, filter, null)

data ExtendedGrammar l t f p = ExtendedGrammar {
   original_report :: HaskellGrammar l t f p,
   report :: HaskellGrammar l t f p,
   keywordForall :: p (),
   kindSignature, kind, bKind, aKind :: p (Abstract.Kind l l f f),
   kindWithWildCards, bKindWithWildcards, aKindWithWildcards :: p (Abstract.Kind l l f f),
   groundTypeKind :: p (),
   cType, arrowType :: p (Abstract.Type l l f f),
   typeWithWildcards, arrowTypeWithWildcards :: p (Abstract.Type l l f f),
   cTypeWithWildcards, bTypeWithWildcards, aTypeWithWildcards :: p (Abstract.Type l l f f),
   promotedLiteral :: p (Abstract.Type l l f f),
   instanceTypeDesignatorInsideParens :: p (Abstract.Type l l f f),
   kindVar :: p (Abstract.Name l),
   gadtNewConstructor, gadtConstructors :: p (Abstract.GADTConstructor l l f f),
   constructorIDs :: p (NonEmpty (Abstract.Name l)),
   namespacedMember :: p (Abstract.ModuleMember l),
   inClassOrInstanceTypeFamilyDeclaration :: p (Abstract.Declaration l l f f),
   familyInstanceDesignator, familyInstanceDesignatorApplications,
   familyInstanceDesignatorBase, flexibleInstanceDesignator :: p (Abstract.ClassInstanceLHS l l f f),
   optionalForall :: p [Abstract.TypeVarBinding l l f f],
   typeVarBinder :: p (Abstract.TypeVarBinding l l f f),
   optionallyParenthesizedTypeVar :: p (Abstract.Name l),   
   optionallyKindedTypeVar, optionallyKindedAndParenthesizedTypeVar :: p (Abstract.Type l l f f),
   gadtNewBody, gadtBody, prefix_gadt_body, record_gadt_body :: p (Abstract.Type l l f f),
   return_type, base_return_type, arg_type :: p (Abstract.Type l l f f)}

$(Rank2.TH.deriveAll ''ExtendedGrammar)

extensionMixins :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser (ExtendedGrammar l t (NodeWrap t)) t),
                              Ord t, Show t, OutlineMonoid t, Abstract.DeeplyFoldable (Serialization (Down Int) t) l,
                              g ~ ExtendedGrammar l t (NodeWrap t))
                => Map (Set Extension) [(Int, GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t))]
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
     (Set.fromList [MultiWayIf],                     [(8, multiWayIfMixin)]),
     (Set.fromList [KindSignatures],                 [(7, kindSignaturesBaseMixin), (8, kindSignaturesMixin)]),
     (Set.fromList [TypeOperators],                  [(8, typeOperatorsMixin)]),
     (Set.fromList [EqualityConstraints],            [(8, equalityConstraintsMixin)]),
     (Set.fromList [ExplicitNamespaces],             [(9, explicitNamespacesMixin)]),
     (Set.fromList [BlockArguments],                 [(9, blockArgumentsMixin)]),
     (Set.fromList [ExistentialQuantification],      [(9, existentialQuantificationMixin)]),
     (Set.fromList [ExplicitForAll],                 [(9, explicitForAllMixin)]),
     (Set.fromList [GADTSyntax],                     [(9, gadtSyntaxMixin)]),
     (Set.fromList [FlexibleInstances],              [(9, flexibleInstancesMixin)]),
     (Set.fromList [TypeFamilies],                   [(9, typeFamiliesMixin)]),
     (Set.fromList [TypeFamilyDependencies],         [(9, typeFamilyDependenciesMixin)]),
     (Set.fromList [DataKinds],                      [(9, dataKindsMixin)]),
     (Set.fromList [MultiParameterConstraints],      [(9, multiParameterConstraintsMixin)]),
     (Set.fromList [PolyKinds],                      [(9, polyKindsMixin)]),
     (Set.fromList [StandaloneKindSignatures],       [(7, kindSignaturesBaseMixin),
                                                      (9, standaloneKindSignaturesMixin)]),
     (Set.fromList [StarIsType],                     [(9, starIsTypeMixin)]),
     (Set.fromList [TypeApplications],               [(9, typeApplicationsMixin)]),
     (Set.fromList [LinearTypes],                    [(9, linearTypesMixin)]),
     (Set.fromList [RoleAnnotations],                [(9, roleAnnotationsMixin)]),
     (Set.fromList [TraditionalRecordSyntax],        [(9, traditionalRecordSyntaxMixin)]),
     (Set.fromList [LinearTypes, GADTSyntax],        [(9, gadtLinearTypesMixin)]),
     (Set.fromList [LinearTypes, UnicodeSyntax],     [(9, unicodeLinearTypesMixin)]),
     (Set.fromList [GADTSyntax, LinearTypes,
                    UnicodeSyntax],                  [(9, gadtUnicodeLinearTypesMixin)]),
     (Set.fromList [StarIsType, UnicodeSyntax],      [(9, unicodeStarIsTypeMixin)]),
     (Set.fromList [GADTSyntax, TypeOperators],      [(9, gadtSyntaxTypeOperatorsMixin)]),
     (Set.fromList [DataKinds, TypeOperators],       [(9, dataKindsTypeOperatorsMixin)]),
     (Set.fromList [MultiParameterConstraints,
                    TypeOperators],                  [(9, multiParameterConstraintsTypeOperatorsMixin)]),
     (Set.fromList [DataKinds, TypeOperators,
                    GADTSyntax],                     [(9, dataKindsGadtSyntaxTypeOperatorsMixin)]),
     (Set.fromList [PolyKinds, ExplicitForAll],      [(9, visibleDependentKindQualificationMixin)])]

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
                        Ord t, Show t, OutlineMonoid t,
                        Abstract.DeeplyFoldable (Serialization (Down Int) t) l)
            => Map Extension Bool -> t
            -> ParseResults t [NodeWrap t (Abstract.Module l l (NodeWrap t) (NodeWrap t))]
parseModule extensions source = case moduleExtensions of
   Left err -> Left err
   Right [extensions']
      | let (contradictions, extensionMap) = partitionContradictory (Set.fromList extensions') ->
        if Set.null contradictions then
           (if null extensions' then id else fmap $ fmap $ rewrap $ Abstract.withLanguagePragma extensions')
           $ parseResults $ Report.haskellModule $ report
           $ parseComplete (extendedGrammar $ withImplications $ extensionMap <> extensions) source
        else Left mempty{errorAlternatives= [StaticDescription
                                             $ "Contradictory extension switches " <> show (toList contradictions)]}
   Right extensionses -> error (show extensionses)
   where moduleExtensions = parseResults $ fmap snd $ getCompose $ simply parsePrefix languagePragmas source
         parseResults = getCompose . fmap snd . getCompose
         getSwitch (ExtensionSwitch s) = s

extendedGrammar :: (Abstract.ExtendedHaskell l, LexicalParsing (Parser (ExtendedGrammar l t (NodeWrap t)) t),
                    Ord t, Show t, OutlineMonoid t,
                    Abstract.DeeplyFoldable (Serialization (Down Int) t) l)
                 => Map Extension Bool -> Grammar (ExtendedGrammar l t (NodeWrap t)) (ParserT ((,) [[Lexeme t]])) t
extendedGrammar extensions =
   overlay reportGrammar (map snd $ reverse $ sortOn fst $ fold $ Map.filterWithKey isIncluded extensionMixins)
   where isIncluded required _ = all (\e-> Map.findWithDefault False e extensions) required

reportGrammar :: forall l g t. (g ~ ExtendedGrammar l t (NodeWrap t), Abstract.ExtendedHaskell l,
                                LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t,
                                Deep.Foldable (Serialization (Down Int) t) (Abstract.CaseAlternative l l),
                                Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l),
                                Deep.Foldable (Serialization (Down Int) t) (Abstract.Expression l l),
                                Deep.Foldable (Serialization (Down Int) t) (Abstract.Import l l),
                                Deep.Foldable (Serialization (Down Int) t) (Abstract.Statement l l))
              => GrammarBuilder g g (ParserT ((,) [[Lexeme t]])) t
reportGrammar g@ExtendedGrammar{report= r} =
   g{original_report = r',
     report= r'{
        declarationLevel= (declarationLevel r'){
           simpleType =
              Abstract.simpleTypeLHS <$> nonTerminal (Report.typeConstructor . report) <*> pure []
              <|> Abstract.simpleTypeLHSApplication
                  <$> wrap (nonTerminal $ Report.simpleType . declarationLevel . report)
                  <*> nonTerminal typeVarBinder,
           instanceTypeDesignator =
              nonTerminal (Report.generalTypeConstructor . report)
              <|> Abstract.listType <$> brackets (wrap $ nonTerminal optionallyKindedAndParenthesizedTypeVar)
              <|> parens (nonTerminal instanceTypeDesignatorInsideParens),
           fieldDeclaration = empty,
           newRecordConstructor = empty},
        fieldBinding = empty,
        fieldPattern = empty,
        typeTerm = nonTerminal arrowType},
     keywordForall = keyword "forall",
     kindSignature = empty,
     kindVar = nonTerminal (Report.typeVar . report),
     kind = empty,
     bKind = empty,
     aKind = empty,
     kindWithWildCards = empty,
     bKindWithWildcards = empty,
     aKindWithWildcards = empty,
     groundTypeKind = empty,
     arrowType = nonTerminal cType
        <|> Abstract.functionType <$> wrap (nonTerminal cType)
                                  <* nonTerminal (Report.rightArrow . report)
                                  <*> wrap (nonTerminal arrowType),
     cType = nonTerminal (Report.bType . report),
     typeWithWildcards =
        Abstract.kindedType <$> wrap (nonTerminal arrowTypeWithWildcards) <*> wrap (nonTerminal kindSignature)
        <|> nonTerminal arrowTypeWithWildcards,
     arrowTypeWithWildcards =
        Abstract.functionType <$> wrap (nonTerminal cTypeWithWildcards) <* rightArrow
                              <*> wrap (nonTerminal arrowTypeWithWildcards)
        <|> nonTerminal cTypeWithWildcards,
     cTypeWithWildcards = nonTerminal bTypeWithWildcards,
     bTypeWithWildcards =
        Abstract.typeApplication <$> wrap (nonTerminal bTypeWithWildcards) <*> wrap (nonTerminal aTypeWithWildcards)
        <|> nonTerminal aTypeWithWildcards,
     aTypeWithWildcards =
        nonTerminal (Report.generalTypeConstructor . report)
        <|> Abstract.typeVariable <$> nonTerminal (Report.typeVar . report)
        <|> Abstract.typeWildcard <$ keyword "_"
        <|> Abstract.tupleType
            <$> parens ((:|) <$> wrap (nonTerminal typeWithWildcards)
                             <*> some (comma *> wrap (nonTerminal typeWithWildcards)))
        <|> Abstract.listType <$> brackets (wrap $ nonTerminal typeWithWildcards)
        <|> parens (nonTerminal typeWithWildcards),
     promotedLiteral =
        Abstract.promotedIntegerLiteral <$> nonTerminal (Report.integer . report)
        <|> Abstract.promotedCharLiteral <$> nonTerminal (Report.charLiteral . report)
        <|> Abstract.promotedStringLiteral <$> nonTerminal (Report.stringLiteral . report),
     namespacedMember =
        Abstract.defaultMember <$> nonTerminal (cname . Report.moduleLevel . report)
        <|> Abstract.typeMember <$ keyword "type" <*> nonTerminal (cname . Report.moduleLevel . report)
        <|> Abstract.patternMember <$ keyword "pattern" <*> nonTerminal (cname . Report.moduleLevel . report),
     inClassOrInstanceTypeFamilyDeclaration = empty,
     familyInstanceDesignatorBase =
        Abstract.classReferenceInstanceLHS <$> nonTerminal (Report.qualifiedTypeClass . declarationLevel . report)
        <|> parens (nonTerminal familyInstanceDesignator),
     familyInstanceDesignatorApplications =
        nonTerminal familyInstanceDesignatorBase
        <|> Abstract.classInstanceLHSApplication
            <$> wrap (nonTerminal familyInstanceDesignatorApplications)
            <*> wrap (nonTerminal aTypeWithWildcards),
     familyInstanceDesignator = nonTerminal familyInstanceDesignatorApplications,
     flexibleInstanceDesignator =
        Abstract.typeClassInstanceLHS <$> qualifiedTypeClass <*> wrap (nonTerminal $ Report.aType . report)
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
     gadtBody = nonTerminal prefix_gadt_body,
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
     arg_type = nonTerminal (Report.aType . report)}
   where r'@HaskellGrammar{declarationLevel= DeclarationGrammar{..}, ..} = Report.grammar r

identifierSyntaxMixin :: forall l g t. (Abstract.Haskell l, LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t,
                                    g ~ ExtendedGrammar l t (NodeWrap t))
                      => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
identifierSyntaxMixin self super = super{
   report= (report super){
      variableIdentifier = token (Abstract.name . Text.pack . toString mempty <$> variableLexeme),
      constructorIdentifier = token (Abstract.name . Text.pack . toString mempty <$> constructorLexeme),
      variableSymbol = token (Abstract.name . Text.pack . toString mempty <$> Report.variableSymbolLexeme),
      constructorSymbol = token (Abstract.name . Text.pack . toString mempty <$> Report.constructorSymbolLexeme)}}

overloadedLabelsMixin :: forall l g t. (Abstract.Haskell l, LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t,
                                    g ~ ExtendedGrammar l t (NodeWrap t))
                      => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
overloadedLabelsMixin self super = super{
   report= (report super){
      variableIdentifier = (super & report & variableIdentifier)
         <|> token (Abstract.name . Text.pack . toString mempty <$> (string "#" <> variableLexeme)),
      variableSymbol = notFollowedBy (string "#" *> variableLexeme) *> (super & report & variableSymbol)}}

unicodeSyntaxMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                 Ord t, Show t, OutlineMonoid t, g ~ ExtendedGrammar l t (NodeWrap t))
                   => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
unicodeSyntaxMixin self super = super{
   keywordForall = keywordForall super <|> delimiter "∀",
   report= (report super){
      doubleColon = (super & report & doubleColon) <|> delimiter "∷",
      rightDoubleArrow = (super & report & rightDoubleArrow) <|> delimiter "⇒",
      rightArrow = (super & report & rightArrow) <|> delimiter "→",
      leftArrow = (super & report & leftArrow) <|> delimiter "←",
      variableSymbol = notSatisfyChar (`elem` ("∀←→⇒∷★" :: [Char])) *> (super & report & variableSymbol)}}

magicHashMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t,
                             g ~ ExtendedGrammar l t (NodeWrap t))
               => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
magicHashMixin self super =
  let integer', integerHash, integerHash2 :: (LexicalParsing (Parser g t), Show t, TextualMonoid t) => Parser g t Integer
      float', floatHash, floatHash2 :: (LexicalParsing (Parser g t), Show t, TextualMonoid t) => Parser g t Rational
      charLiteral', charHashLiteral :: (LexicalParsing (Parser g t), Show t, TextualMonoid t) => Parser g t Char
      stringLiteral', stringHashLiteral :: (LexicalParsing (Parser g t), Show t, TextualMonoid t) => Parser g t Text

      integer' = token ((self & report & integerLexeme) <* notFollowedBy (string "#"))
      float' = token ((self & report & floatLexeme) <* notFollowedBy (string "#"))
      charLiteral' = token ((self & report & charLexeme) <* notFollowedBy (string "#"))
      stringLiteral' = token ((self & report & stringLexeme) <* notFollowedBy (string "#")) <?> "string literal"

      integerHash = token ((self & report & integerLexeme) <* string "#" <* notFollowedBy (string "#"))
      floatHash = token ((self & report & floatLexeme) <* string "#" <* notFollowedBy (string "#"))
      integerHash2 = token ((self & report & integerLexeme) <* string "##")
      floatHash2 = token ((self & report & floatLexeme) <* string "##")
      charHashLiteral = token ((self & report & charLexeme) <* string "#")
      stringHashLiteral = token ((self & report & stringLexeme) <* string "#")
  in super{report= (report super){
        variableIdentifier =
           token (Abstract.name . Text.pack . toString mempty <$> (variableLexeme <> concatAll (string "#"))),
        constructorIdentifier =
           token (Abstract.name . Text.pack . toString mempty <$> (constructorLexeme <> concatAll (string "#"))),
        lPattern = (self & report & aPattern)
                   <|> Abstract.literalPattern
                       <$> wrap ((Abstract.integerLiteral . negate) <$ delimiter "-" <*> integer'
                                 <|> (Abstract.hashLiteral . Abstract.integerLiteral . negate)
                                     <$ delimiter "-" <*> integerHash
                                 <|> (Abstract.hashLiteral . Abstract.hashLiteral . Abstract.integerLiteral . negate)
                                     <$ delimiter "-" <*> integerHash2)
                   <|> Abstract.literalPattern
                       <$> wrap ((Abstract.floatingLiteral . negate) <$ delimiter "-" <*> float'
                                 <|> (Abstract.hashLiteral . Abstract.floatingLiteral . negate)
                                     <$ delimiter "-" <*> floatHash
                                 <|> (Abstract.hashLiteral . Abstract.hashLiteral . Abstract.floatingLiteral . negate)
                                     <$ delimiter "-" <*> floatHash2)
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

recursiveDoMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t,
                               Abstract.DeeplyFoldable (Serialization (Down Int) t) l,
                               g ~ ExtendedGrammar l t (NodeWrap t))
                 => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
recursiveDoMixin self super = super{
   report= (report super){
      closedBlockExpresion = (super & report & closedBlockExpresion)
         <|> Abstract.mdoExpression <$ keyword "mdo" <*> wrap (self & report & statements),
      statement = (super & report & statement)
                  <|> Deep.InL
                      <$> wrap (Abstract.recursiveStatement
                                . (either id (rewrap Abstract.expressionStatement) . Deep.eitherFromSum . unwrap <$>)
                                <$ keyword "rec"
                                <*> blockOf (self & report & statement)),
      variableIdentifier = notFollowedBy (keyword "mdo" <|> keyword "rec") *> (super & report & variableIdentifier)}}

parallelListComprehensionsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                              Ord t, Show t, OutlineMonoid t, g ~ ExtendedGrammar l t (NodeWrap t))
                                => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
parallelListComprehensionsMixin self@ExtendedGrammar{report= HaskellGrammar{qualifiers, expression}} super = super{
   report= (report super){
      bareExpression = (super & report & bareExpression)
                       <|> brackets (Abstract.parallelListComprehension
                                     <$> expression <*> qualifiers <*> qualifiers <*> many qualifiers)}}

tupleSectionsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                 Ord t, Show t, OutlineMonoid t, g ~ ExtendedGrammar l t (NodeWrap t))
                   => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
tupleSectionsMixin self@ExtendedGrammar{report= HaskellGrammar{expression}} super = super{
   report= (report super){
      bareExpression = (super & report & bareExpression)
         <|> Abstract.tupleSectionExpression
             <$> parens (filter (\l-> any isJust l && any isNothing l)
                         $ (:|) <$> optional expression <*> some (comma *> optional expression))}}

lambdaCaseMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t), Ord t, Show t, TextualMonoid t,
                              g ~ ExtendedGrammar l t (NodeWrap t))
                => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
lambdaCaseMixin self super = super{
   report= (report super){
      closedBlockExpresion = (super & report & closedBlockExpresion)
         <|> Abstract.lambdaCaseExpression <$ (delimiter "\\" *> keyword "case")
             <*> (self & report & alternatives)}}

emptyCaseMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t,
                             Deep.Foldable (Serialization (Down Int) t) (Abstract.CaseAlternative l l),
                             g ~ ExtendedGrammar l t (NodeWrap t))
                 => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
emptyCaseMixin self super = super{
   report= (report super){
      alternatives = blockOf (alternative $ report super)}}

multiWayIfMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t,
                              Deep.Foldable (Serialization (Down Int) t) (Abstract.GuardedExpression l l),
                              g ~ ExtendedGrammar l t (NodeWrap t))
                => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
multiWayIfMixin self@ExtendedGrammar{report= HaskellGrammar{expression, guards, rightArrow}} super = super{
   report= (report super){
      closedBlockExpresion = (super & report & closedBlockExpresion)
         <|> Abstract.multiWayIfExpression <$ keyword "if"
             <*> blockOf' (Abstract.guardedExpression . toList
                           <$> guards <* rightArrow <*> expression)}}

packageImportsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t), Ord t, Show t,
                                  OutlineMonoid t, g ~ ExtendedGrammar l t (NodeWrap t))
                      => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
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

safeImportsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t,
                               g ~ ExtendedGrammar l t (NodeWrap t))
                 => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
safeImportsMixin self super = super{
   report= (report super){
      moduleLevel= (super & report & moduleLevel){
         importDeclaration = (super & report & moduleLevel & importDeclaration)
                             <|> Abstract.safeImportDeclaration <$ keyword "import" <* keyword "safe"
                                 <*> (True <$ keyword "qualified" <|> pure False)
                                 <*> moduleId
                                 <*> optional (keyword "as" *> moduleId)
                                 <*> optional (wrap $ self & report & moduleLevel & importSpecification)}}}

importQualifiedPostMixin :: forall l g t. (Abstract.Haskell l, LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t,
                                       g ~ ExtendedGrammar l t (NodeWrap t))
                         => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
importQualifiedPostMixin self super = super{
   report= (report super){
      moduleLevel= (super & report & moduleLevel){
         importDeclaration = (super & report & moduleLevel & importDeclaration)
                             <|> flip Abstract.importDeclaration <$ keyword "import"
                                 <*> moduleId
                                 <*> (True <$ keyword "qualified" <|> pure False)
                                 <*> optional (keyword "as" *> moduleId)
                                 <*> optional (wrap $ self & report & moduleLevel & importSpecification)}}}

safePackageImportsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t), Ord t, Show t,
                                      OutlineMonoid t, g ~ ExtendedGrammar l t (NodeWrap t))
                        => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
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

packageImportsQualifiedPostMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                               Ord t, Show t, OutlineMonoid t, g ~ ExtendedGrammar l t (NodeWrap t))
                                 => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
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

safeImportsQualifiedPostMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                            Ord t, Show t, OutlineMonoid t, g ~ ExtendedGrammar l t (NodeWrap t))
                              => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
safeImportsQualifiedPostMixin self super = super{
   report= (report super){
      moduleLevel= (super & report & moduleLevel){
         importDeclaration = (super & report & moduleLevel & importDeclaration)
                             <|> flip Abstract.safeImportDeclaration <$ keyword "import" <* keyword "safe"
                                 <*> moduleId
                                 <*> (True <$ keyword "qualified" <|> pure False)
                                 <*> optional (keyword "as" *> moduleId)
                                 <*> optional (wrap $ self & report & moduleLevel & importSpecification)}}}

safePackageImportsQualifiedPostMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                                   Ord t, Show t, OutlineMonoid t, g ~ ExtendedGrammar l t (NodeWrap t))
                                     => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
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

explicitNamespacesMixin :: forall l g t. (g ~ ExtendedGrammar l t (NodeWrap t), Abstract.ExtendedHaskell l,
                                      LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t,
                                      g ~ ExtendedGrammar l t (NodeWrap t))
                        => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
explicitNamespacesMixin self super = super{
   report= (report super){
      moduleLevel= (super & report & moduleLevel){
         export = (super & report & moduleLevel & export)
            <|> Abstract.exportClassOrType <$ keyword "type"
                <*> parens (self & report & qualifiedVariableSymbol)
                <*> optional (self & report & moduleLevel & members),
         importItem = (super & report & moduleLevel & importItem)
            <|> Abstract.importClassOrType <$ keyword "type"
                <*> parens (self & report & variableSymbol)
                <*> optional (self & report & moduleLevel & members),
         members = parens (Abstract.allMembers <$ delimiter ".."
                           <|> Abstract.explicitlyNamespacedMemberList
                               <$> (namespacedMember self `sepBy` comma) <* optional comma)}}}

blockArgumentsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                  Ord t, Show t, OutlineMonoid t,
                                  Deep.Foldable (Serialization (Down Int) t) (Abstract.GuardedExpression l l),
                                  g ~ ExtendedGrammar l t (NodeWrap t))
                    => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
blockArgumentsMixin self super = super{
   report= (report super){
      lExpression = (super & report & lExpression)
         <|> wrap (Abstract.applyExpression <$> (self & report & fExpression)
                                            <*> wrap (self & report & openBlockExpression)),
      dExpression = (self & report & fExpression),
      bareExpression = (super & report & bareExpression) <|> (self & report & closedBlockExpresion)}}

lexicalNegationMixin :: forall l g t. (Abstract.Haskell l, LexicalParsing (Parser (ExtendedGrammar l t (NodeWrap t)) t),
                                   Ord t, Show t, TextualMonoid t, g ~ ExtendedGrammar l t (NodeWrap t))
                     => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
lexicalNegationMixin self super = super{
   report= (report super){
      qualifiedVariableSymbol = notFollowedBy (string "-"
                                               *> satisfyCharInput (\c-> Char.isAlphaNum c || c == '(' || c == '['))
                                *> token (nameQualifier <*> (self & report & variableSymbol)),
      infixExpression = wrap (Abstract.infixExpression
                                 <$> (self & report & dExpression)
                                 <*> wrap (Abstract.referenceExpression <$> (self & report & qualifiedOperator))
                                 <*> (self & report & infixExpression))
                        <|> (self & report & lExpression),
      leftInfixExpression =
         wrap (Abstract.infixExpression
                  <$> (self & report & dExpression)
                  <*> wrap (Abstract.referenceExpression <$> (self & report & qualifiedOperator))
                  <*> (self & report & leftInfixExpression))
         <|> (self & report & dExpression),
      bareExpression = (super & report & bareExpression)
         <|> Abstract.applyExpression <$> wrap (Abstract.negate <$ prefixMinus) <*> (self & report & aExpression)
         <|> parens (Abstract.rightSectionExpression
                     <$> (notFollowedBy prefixMinus *> (self & report & qualifiedOperator))
                     <*> (self & report & infixExpression))}}
   where prefixMinus = void (string "-"
                             <* lookAhead (satisfyCharInput $ \c-> Char.isAlphaNum c || c == '(' || c == '[')
                             <* lift ([[Token Modifier "-"]], ()))
                       <?> "prefix -"

negativeLiteralsMixin :: forall l g t. (Abstract.Haskell l, LexicalParsing (Parser (ExtendedGrammar l t (NodeWrap t)) t),
                                    Ord t, Show t, TextualMonoid t, g ~ ExtendedGrammar l t (NodeWrap t))
                      => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
negativeLiteralsMixin self@ExtendedGrammar
                      {report= HaskellGrammar{dExpression, infixExpression, leftInfixExpression, qualifiedOperator}}
                      super = super{
   report= (report super){
      qualifiedVariableSymbol =
         notFollowedBy (string "-" *> satisfyCharInput Char.isDigit) *> (super & report & qualifiedVariableSymbol),
      infixExpression =
         wrap (Abstract.infixExpression
                  <$> dExpression
                  <*> wrap (Abstract.referenceExpression <$> qualifiedOperator)
                  <*> infixExpression
               <|> Abstract.applyExpression <$> wrap (Abstract.negate <$ prefixMinus) <*> infixExpression)
         <|> (self & report & lExpression),
      leftInfixExpression =
         wrap (Abstract.infixExpression
                  <$> dExpression
                  <*> wrap (Abstract.referenceExpression <$> qualifiedOperator)
                  <*> leftInfixExpression
               <|> Abstract.applyExpression <$> wrap (Abstract.negate <$ prefixMinus) <*> leftInfixExpression)
         <|> dExpression,
      integerLexeme = (negate <$ string "-" <|> pure id) <*> (super & report & integerLexeme),
      floatLexeme = (negate <$ string "-" <|> pure id) <*> (super & report & floatLexeme)}}
   where prefixMinus = void (token $ string "-" <* notSatisfyChar Char.isDigit) <?> "prefix -"

binaryLiteralsMixin :: forall l g t. (Abstract.Haskell l, LexicalParsing (Parser g t), Ord t, Show t, TextualMonoid t,
                                  g ~ ExtendedGrammar l t (NodeWrap t))
                      => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
binaryLiteralsMixin self super = super{
   report= (report super){
      integerLexeme =
         (string "0b" <|> string "0B")
         *> (foldl' binary 0 . toString mempty <$> takeCharsWhile1 (\c-> c == '0' || c == '1') <?> "binary number")
         <<|> (super & report & integerLexeme)}}
   where binary n '0' = 2*n
         binary n '1' = 2*n + 1
         binary _ _ = error "non-binary"

hexFloatLiteralsMixin :: forall l g t. (Abstract.Haskell l, LexicalParsing (Parser g t), Ord t, Show t, TextualMonoid t,
                                    g ~ ExtendedGrammar l t (NodeWrap t))
                      => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
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

numericUnderscoresMixin :: forall l g t. (Abstract.Haskell l, LexicalParsing (Parser g t), Ord t, Show t, TextualMonoid t,
                                      g ~ ExtendedGrammar l t (NodeWrap t))
                        => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
numericUnderscoresMixin self super = super{
   report= (report super){
      decimal = takeCharsWhile1 Char.isDigit <> concatAll (char '_' *> takeCharsWhile1 Char.isDigit)
                <?> "decimal number",
      octal = takeCharsWhile1 Char.isOctDigit <> concatAll (char '_' *> takeCharsWhile1 Char.isOctDigit)
              <?> "octal number",
      hexadecimal = takeCharsWhile1 Char.isHexDigit <> concatAll (char '_' *> takeCharsWhile1 Char.isHexDigit)
                    <?> "hexadecimal number"}}

binaryUnderscoresMixin :: forall l g t. (Abstract.Haskell l, LexicalParsing (Parser g t), Ord t, Show t, TextualMonoid t,
                                     g ~ ExtendedGrammar l t (NodeWrap t))
                        => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
binaryUnderscoresMixin self super = super{
   report= (report super){
      integerLexeme =
         (string "0b" <|> string "0B")
         *> (foldl' binary 0 . toString mempty
             <$> (binaryDigits <> concatAll (char '_' *> binaryDigits)) <?> "binary number")
         <<|> (super & report & integerLexeme)}}
   where binary n '0' = 2*n
         binary n '1' = 2*n + 1
         binary _ _ = error "non-binary"
         binaryDigits = takeCharsWhile1 (\c-> c == '0' || c == '1')

typeOperatorsMixin :: forall l g t. (g ~ ExtendedGrammar l t (NodeWrap t), Abstract.ExtendedHaskell l,
                                 LexicalParsing (Parser g t), Ord t, Show t, TextualMonoid t)
                   => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
typeOperatorsMixin self super =
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
               <|> Abstract.reExportModule <$ keyword "module" <*> Report.moduleId,
            importItem = Abstract.importVar <$> (self & report & variable)
               <|> Abstract.importClassOrType
                   <$> ((self & report & constructorIdentifier) <|> parens (self & report & constructorSymbol))
                   <*> pure Nothing
               <|> Abstract.importClassOrType
                   <$> (self & report & typeConstructor)
                   <*> (Just <$> (self & report & moduleLevel & members))},
         declarationLevel= (super & report & declarationLevel){
            simpleType = (super & report & declarationLevel & simpleType)
               <|> Abstract.simpleInfixTypeLHSApplication
                            <$> typeVarBinder self
                            <*> anySymbol
                            <*> typeVarBinder self
               <|> parens ((self & report & declarationLevel & simpleType)),
            qualifiedTypeClass =
               (super & report & declarationLevel & qualifiedTypeClass) <|> parens anyQualifiedSymbol},
         typeConstructor = (self & report & constructorIdentifier) <|> parens anySymbol,
         qualifiedTypeConstructor = (self & report & qualifiedConstructorIdentifier) <|> parens anyQualifiedSymbol,
         generalTypeConstructor = (super & report & generalTypeConstructor)
           <|> Abstract.constructorType
               <$> wrap (Abstract.constructorReference <$> parens (self & report & qualifiedVariableSymbol))},
        cType = (super & cType)
           <|> Abstract.infixTypeApplication
                  <$> wrap (self & cType)
                  <*> (self & report & qualifiedOperator)
                  <*> wrap (self & report & bType),
        cTypeWithWildcards = cTypeWithWildcards super
           <|> Abstract.infixTypeApplication
                  <$> wrap (bTypeWithWildcards self)
                  <*> (self & report & qualifiedOperator)
                  <*> wrap (cTypeWithWildcards self),
        instanceTypeDesignatorInsideParens = (super & instanceTypeDesignatorInsideParens)
           <|> Abstract.infixTypeApplication
               <$> wrap (optionallyKindedAndParenthesizedTypeVar self)
               <*> (self & report & qualifiedOperator)
               <*> wrap (optionallyKindedAndParenthesizedTypeVar self),
        familyInstanceDesignator = familyInstanceDesignator super
           <|> Abstract.infixTypeClassInstanceLHS
                  <$> wrap (bTypeWithWildcards super)
                  <*> (self & report & qualifiedOperator)
                  <*> wrap (cTypeWithWildcards super)}
   where anySymbol = (self & report & constructorSymbol) <|> (self & report & variableSymbol)
         anyQualifiedSymbol = (self & report & qualifiedConstructorSymbol) <|> (self & report & qualifiedVariableSymbol)

equalityConstraintsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                       g ~ ExtendedGrammar l t (NodeWrap t), Ord t, Show t, TextualMonoid t)
                       => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
equalityConstraintsMixin self super = super{
   report= (report super){
      declarationLevel= (super & report & declarationLevel){
         constraint = (super & report & declarationLevel & constraint) <|> equalityConstraint},
      typeTerm = (super & report & typeTerm) <|> Abstract.constraintType <$> wrap equalityConstraint}}
   where equalityConstraint =
            Abstract.typeEqualityConstraint <$> wrap (self & report & bType)
            <* delimiter "~" <*> wrap ((self & report & bType))

multiParameterConstraintsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                             g ~ ExtendedGrammar l t (NodeWrap t), Ord t, Show t, TextualMonoid t)
                               => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
multiParameterConstraintsMixin self super = super{
   report= (report super){
      declarationLevel= (super & report & declarationLevel){
         constraint= (super & report & declarationLevel & constraint)
            <|> Abstract.multiParameterClassConstraint <$> (self & report & declarationLevel & qualifiedTypeClass)
                <*> filter
                       ((1 /=) . length)
                       (many $ wrap $
                        optionallyKindedAndParenthesizedTypeVar self
                        <|> parens (self & report & declarationLevel & typeApplications))}}}

multiParameterConstraintsTypeOperatorsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                                          g ~ ExtendedGrammar l t (NodeWrap t),
                                                          Ord t, Show t, TextualMonoid t)
                                            => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
multiParameterConstraintsTypeOperatorsMixin self super = super{
   report= (report super){
      declarationLevel= (super & report & declarationLevel){
         constraint = (super & report & declarationLevel & constraint)
            <|> Abstract.infixConstraint
                <$> wrap ((self & report & bType))
                <*> (self & report & qualifiedOperator)
                <*> wrap ((self & report & bType))}}}

gratuitouslyParenthesizedTypesMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                                  g ~ ExtendedGrammar l t (NodeWrap t),
                                                  OutlineMonoid t, Ord t, Show t, TextualMonoid t,
                                                  Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l),
                                                  Deep.Foldable (Serialization (Down Int) t)
                                                                (Abstract.GADTConstructor l l))
                            => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
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
         constraint = (super & report & declarationLevel & constraint)
            <|> parens (self & report & declarationLevel & constraint),
         context = (self & report & declarationLevel & constraint)
            <|> Abstract.constraints
                <$> parens (filter ((1 /=) . length)
                            $ wrap ((self & report & declarationLevel & constraint)) `sepBy` comma),
         classConstraint = (super & report & declarationLevel & classConstraint)
            <|> Abstract.simpleConstraint
                <$> (self & report & declarationLevel & qualifiedTypeClass)
                <*> parens (optionallyParenthesizedTypeVar self),
         qualifiedTypeClass = (super & report & declarationLevel & qualifiedTypeClass) <|> parens qtc,
         typeApplications =
            Abstract.typeApplication
            <$> wrap (Abstract.typeVariable <$> optionallyParenthesizedTypeVar self
                      <|> (self & report & declarationLevel & typeApplications))
            <*> wrap (self & report & aType),
         typeVarApplications =
            (self & report & generalTypeConstructor)
            <|> Abstract.typeApplication
                <$> wrap ((self & report & declarationLevel & typeVarApplications)
                          <|> parens (self & report & declarationLevel & typeVarApplications))
                <*> wrap (optionallyKindedAndParenthesizedTypeVar self),
         instanceDesignator = (super & report & declarationLevel & instanceDesignator)
            <|> parens (self & report & declarationLevel & instanceDesignator),
         derivingClause = keyword "deriving"
                          *> (pure <$> wrap (Abstract.simpleDerive <$> qtc)
                              <|> parens (filter ((/= 1) . length)
                                          $ wrap (Abstract.simpleDerive <$> qtc) `sepBy` comma))
                          <|> pure []}},
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

flexibleInstancesMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                     g ~ ExtendedGrammar l t (NodeWrap t),
                                     Ord t, Show t, TextualMonoid t)
                       => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
flexibleInstancesMixin self super = super{
   report= (report super){
             declarationLevel= (super & report & declarationLevel){
                instanceDesignator = flexibleInstanceDesignator self}}}

typeFamiliesMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                    g ~ ExtendedGrammar l t (NodeWrap t),
                                    Ord t, Show t, TextualMonoid t, OutlineMonoid t,
                                    Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l),
                                    Deep.Foldable (Serialization (Down Int) t) (Abstract.GADTConstructor l l))
                  => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
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
                <*> (delimiter "=" *> declaredConstructors <|> pure [])
                <*> derivingClause
            <|> Abstract.newtypeFamilyInstance <$ (keyword "newtype" *> keyword "instance")
                <*> optionalForall self
                <*> wrap optionalContext
                <*> wrap (familyInstanceDesignator self)
                <*> optional (wrap $ kindSignature self)
                <* delimiter "="
                <*> wrap newConstructor
                <*> derivingClause
            <|> Abstract.gadtDataFamilyInstance <$ (keyword "data" *> keyword "instance")
                <*> optionalForall self
                <*> wrap (familyInstanceDesignator self)
                <*> optional (wrap $ kindSignature self)
                <* keyword "where"
                <*> blockOf (gadtConstructors self)
                <*> derivingClause
            <|> Abstract.gadtNewtypeFamilyInstance <$ (keyword "newtype" *> keyword "instance")
                <*> optionalForall self
                <*> wrap (familyInstanceDesignator self)
                <*> optional (wrap $ kindSignature self)
                <* keyword "where"
                <*> wrap (gadtNewConstructor self)
                <*> derivingClause
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
            <|> inClassOrInstanceTypeFamilyDeclaration self
        }
    },
    inClassOrInstanceTypeFamilyDeclaration =
       Abstract.dataFamilyInstance <$ keyword "data" <* optional (keyword "instance")
           <*> optionalForall self
           <*> wrap optionalContext
           <*> wrap (familyInstanceDesignator self)
           <*> optional (wrap $ kindSignature self)
           <* delimiter "="
           <*> declaredConstructors
           <*> derivingClause
       <|> Abstract.newtypeFamilyInstance <$ keyword "newtype" <* optional (keyword "instance")
           <*> optionalForall self
           <*> wrap optionalContext
           <*> wrap (familyInstanceDesignator self)
           <*> optional (wrap $ kindSignature self)
           <* delimiter "="
           <*> wrap newConstructor
           <*> derivingClause
       <|> Abstract.gadtDataFamilyInstance <$ (keyword "data" *> optional (keyword "instance"))
           <*> optionalForall self
           <*> wrap (familyInstanceDesignator self)
           <*> optional (wrap $ kindSignature self)
           <* keyword "where"
           <*> blockOf (gadtConstructors self)
           <*> derivingClause
       <|> Abstract.gadtNewtypeFamilyInstance <$ (keyword "newtype" *> optional (keyword "instance"))
           <*> optionalForall self
           <*> wrap (familyInstanceDesignator self)
           <*> optional (wrap $ kindSignature self)
           <* keyword "where"
           <*> wrap (gadtNewConstructor self)
           <*> derivingClause
       <|> Abstract.typeFamilyInstance <$ keyword "type" <* optional (keyword "instance")
           <*> optionalForall self
           <*> wrap (familyInstanceDesignator self)
           <* delimiter "="
           <*> wrap (self & report & typeTerm)}

typeFamilyDependenciesMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                          g ~ ExtendedGrammar l t (NodeWrap t),
                                          Ord t, Show t, TextualMonoid t, OutlineMonoid t,
                                          Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l),
                                          Deep.Foldable (Serialization (Down Int) t) (Abstract.GADTConstructor l l))
                            => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
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

dataKindsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                  g ~ ExtendedGrammar l t (NodeWrap t),
                                  Ord t, Show t, TextualMonoid t, OutlineMonoid t,
                                  Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l))
                    => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
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
            <|> promotedLiteral self}},
   aTypeWithWildcards = aTypeWithWildcards super
      <|> promotedLiteral self
      <|> Abstract.promotedTupleType <$ terminator "'"
          <*> parens ((:|) <$> wrap (typeWithWildcards self)
                           <*> some (comma *> wrap (typeWithWildcards self)))
      <|> Abstract.promotedListType <$ terminator "'" <*> brackets (wrap (typeWithWildcards self) `sepBy1` comma)
      <|> Abstract.promotedListType
          <$> brackets ((:) <$> wrap (typeWithWildcards self) <* comma
                            <*> wrap (typeWithWildcards self) `sepBy1` comma),
   bKind = bKind super
     <|> Abstract.typeRepresentationKind
         <$ (Report.nameQualifier <*> Report.nameToken (string "TYPE") :: Report.Parser g t (Abstract.QualifiedName l))
         <* notFollowedBy (fst <$> match (aKind self))
         <*> wrap (self & report & aType),
   aKind = aKind super
     <|> Abstract.tupleKind <$> parens ((:|) <$> wrap (kind self) <*> some (comma *> wrap (kind self)))
     <|> Abstract.listKind <$> brackets (wrap $ kind self)}

dataKindsTypeOperatorsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                          g ~ ExtendedGrammar l t (NodeWrap t),
                                          Ord t, Show t, TextualMonoid t, OutlineMonoid t,
                                          Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l))
                            => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
dataKindsTypeOperatorsMixin self super = super{
   instanceTypeDesignatorInsideParens = (super & instanceTypeDesignatorInsideParens)
      <|> Abstract.promotedInfixTypeApplication
          <$> wrap (optionallyKindedAndParenthesizedTypeVar self)
          <* terminator "'"
          <*> (super & report & qualifiedOperator)
          <*> wrap (optionallyKindedAndParenthesizedTypeVar self),
   bKind = bKind super
           <|> Abstract.infixKindApplication
           <$> wrap (bKind self)
           <*> (super & report & qualifiedOperator)
           <*> wrap (aKind self),
   cType = (super & cType)
      <|> Abstract.promotedInfixTypeApplication
          <$> wrap (self & cType)
          <* terminator "'"
          <*> (super & report & qualifiedOperator)
             <*> wrap (self & report & bType),
   cTypeWithWildcards = cTypeWithWildcards super
      <|> Abstract.infixTypeApplication
          <$> wrap (bTypeWithWildcards self)
          <* terminator "'"
          <*> (super & report & qualifiedOperator)
          <*> wrap (cTypeWithWildcards self),
   familyInstanceDesignator = familyInstanceDesignator super
      <|> Abstract.infixTypeClassInstanceLHS
          <$> wrap (bTypeWithWildcards super)
          <* terminator "'"
          <*> (super & report & qualifiedOperator)
          <*> wrap (cTypeWithWildcards super)}

polyKindsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                             g ~ ExtendedGrammar l t (NodeWrap t),
                             Ord t, Show t, TextualMonoid t, OutlineMonoid t,
                             Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l))
               => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
polyKindsMixin self super = super{
   report= (report super){
      aType = (super & report & aType)
          <|> Abstract.groundType <$ groundTypeKind self
      },
   aTypeWithWildcards = aTypeWithWildcards super
       <|> Abstract.groundType <$ groundTypeKind self,
   kind = Abstract.typeKind <$> wrap (self & report & typeTerm),
   kindWithWildCards = Abstract.typeKind <$> wrap (self & typeWithWildcards),
   aKind = Abstract.typeKind <$> wrap (self & report & aType),
   aKindWithWildcards = Abstract.typeKind <$> wrap (self & aTypeWithWildcards)}

visibleDependentKindQualificationMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                                     g ~ ExtendedGrammar l t (NodeWrap t),
                                                     Ord t, Show t, TextualMonoid t, OutlineMonoid t,
                                                     Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l))
                                       => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
visibleDependentKindQualificationMixin self super = super{
   arrowType = arrowType super
     <|> Abstract.visibleDependentType
         <$ keywordForall self
         <*> many (typeVarBinder self)
         <* (self & report & rightArrow)
         <*> wrap (arrowType self),
   arrowTypeWithWildcards = arrowTypeWithWildcards super
     <|> Abstract.visibleDependentType
         <$ keywordForall self
         <*> many (typeVarBinder self)
         <* (self & report & rightArrow)
         <*> wrap (arrowTypeWithWildcards self)}

kindSignaturesBaseMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                      g ~ ExtendedGrammar l t (NodeWrap t),
                                      Ord t, Show t, TextualMonoid t, OutlineMonoid t,
                                      Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l))
                        => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
kindSignaturesBaseMixin self super = super{
   kindSignature = (self & report & doubleColon) *> kind self,
   kind = Abstract.functionKind <$> wrap (bKind self) <* (self & report & rightArrow) <*> wrap (kind self)
          <|> bKind self,
   bKind = Abstract.kindApplication <$> wrap (bKind self) <*> wrap (aKind self)
           <|> aKind self,
   aKind = Abstract.constructorKind <$> wrap (self & report & generalConstructor)
           <|> Abstract.groundTypeKind <$ groundTypeKind self
           <|> Abstract.kindVariable <$> kindVar self
           <|> parens (kind self),
   kindWithWildCards =
      Abstract.functionKind
               <$> wrap (bKindWithWildcards self)
               <* (self & report & rightArrow)
               <*> wrap (kindWithWildCards self)
      <|> bKindWithWildcards self,
   bKindWithWildcards =
      Abstract.kindApplication <$> wrap (bKindWithWildcards self) <*> wrap (aKindWithWildcards self)
      <|> aKindWithWildcards self,
   aKindWithWildcards = Abstract.constructorKind <$> wrap (self & report & generalConstructor)
      <|> Abstract.groundTypeKind <$ groundTypeKind self
      <|> Abstract.kindVariable <$> kindVar self
      <|> parens (kindWithWildCards self)}

starIsTypeMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                              g ~ ExtendedGrammar l t (NodeWrap t),
                              Ord t, Show t, TextualMonoid t, OutlineMonoid t,
                              Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l))
                => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
starIsTypeMixin self super = super{
   groundTypeKind = groundTypeKind super <|> delimiter "*"}

unicodeStarIsTypeMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                     g ~ ExtendedGrammar l t (NodeWrap t),
                                     Ord t, Show t, TextualMonoid t, OutlineMonoid t,
                                     Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l))
                       => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
unicodeStarIsTypeMixin self super = super{
   groundTypeKind = groundTypeKind super <|> delimiter "★"}

roleAnnotationsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                       g ~ ExtendedGrammar l t (NodeWrap t),
                                       Ord t, Show t, TextualMonoid t, OutlineMonoid t,
                                       Abstract.DeeplyFoldable (Serialization (Down Int) t) l)
                     => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
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

typeApplicationsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                        g ~ ExtendedGrammar l t (NodeWrap t),
                                        Ord t, Show t, TextualMonoid t, OutlineMonoid t,
                                        Abstract.DeeplyFoldable (Serialization (Down Int) t) l)
                      => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
typeApplicationsMixin self super = super{
   report = (report super){
      bType = (super & report & bType)
         <|> Abstract.visibleKindApplication
             <$> filter whiteSpaceTrailing (wrap $ self & report & bType)
             <* delimiter "@"
             <*> wrap (aKindWithWildcards self),
      bareExpression = (super & report & bareExpression)
         <|> Abstract.visibleTypeApplication
             <$> filter whiteSpaceTrailing (self & report & aExpression)
             <* delimiter "@"
             <*> wrap (aTypeWithWildcards self),
      lPattern = (super & report & lPattern)
         <|> Abstract.constructorPatternWithTypeApplications
             <$> filter whiteSpaceTrailing (wrap $ self & report & generalConstructor)
             <*> some (delimiter "@" *> wrap (aTypeWithWildcards self))
             <*> many (wrap $ self & report & aPattern)
      },
   return_type = (super & return_type)
      <|> Abstract.visibleKindApplication
          <$> filter whiteSpaceTrailing (wrap $ self & return_type)
          <* delimiter "@"
          <*> wrap (aKindWithWildcards self),
   bTypeWithWildcards = (super & bTypeWithWildcards)
      <|> Abstract.visibleKindApplication
          <$> filter whiteSpaceTrailing (wrap $ self & bTypeWithWildcards)
          <* delimiter "@"
          <*> wrap (aKindWithWildcards self),
   bKind = (super & bKind)
      <|> Abstract.visibleKindKindApplication
          <$> filter whiteSpaceTrailing (wrap $ self & bKind)
          <* delimiter "@"
          <*> wrap (aKindWithWildcards self),
   bKindWithWildcards = (super & bKindWithWildcards)
      <|> Abstract.visibleKindKindApplication
          <$> filter whiteSpaceTrailing (wrap $ self & bKindWithWildcards)
          <* delimiter "@"
          <*> wrap (aKindWithWildcards self),
   typeVarBinder = typeVarBinder super
      <|> Abstract.inferredTypeVariable <$> braces (self & report & typeVar),
   familyInstanceDesignatorApplications = familyInstanceDesignatorApplications super
      <|> Abstract.classInstanceLHSKindApplication
          <$> filter whiteSpaceTrailing (wrap $ self & familyInstanceDesignatorApplications)
          <* delimiter "@"
          <*> wrap (self & aKindWithWildcards)
   }

linearTypesMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                   g ~ ExtendedGrammar l t (NodeWrap t),
                                   Ord t, Show t, TextualMonoid t, OutlineMonoid t,
                                   Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l))
                 => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
linearTypesMixin self super = super{
  arrowType = (super & arrowType)
    <|> Abstract.linearFunctionType
        <$> wrap (self & cType)
        <* delimiter "%"
        <* keyword "1"
        <* (self & report & rightArrow)
        <*> wrap (self & arrowType)
    <|> Abstract.multiplicityFunctionType
        <$> wrap (self & cType)
        <* delimiter "%"
        <* notFollowedBy (keyword "1")
        <*> wrap (self & report & aType)
        <* (self & report & rightArrow)
        <*> wrap (self & arrowType)}

gadtLinearTypesMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                       g ~ ExtendedGrammar l t (NodeWrap t),
                                       Ord t, Show t, TextualMonoid t, OutlineMonoid t,
                                       Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l))
                     => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
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

unicodeLinearTypesMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                          g ~ ExtendedGrammar l t (NodeWrap t),
                                          Ord t, Show t, TextualMonoid t, OutlineMonoid t,
                                          Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l))
                        => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
unicodeLinearTypesMixin self super = super{
  arrowType = (super & arrowType)
    <|> Abstract.linearFunctionType
        <$> wrap (self & cType)
        <* delimiter "⊸"
        <*> wrap (self & arrowType)}

gadtUnicodeLinearTypesMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                              g ~ ExtendedGrammar l t (NodeWrap t),
                                              Ord t, Show t, TextualMonoid t, OutlineMonoid t,
                                              Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l))
                            => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
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

standaloneKindSignaturesMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                            g ~ ExtendedGrammar l t (NodeWrap t),
                                            Ord t, Show t, TextualMonoid t, OutlineMonoid t,
                                            Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l))
                              => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
standaloneKindSignaturesMixin self super = super{
   report= (report super){
      declarationLevel= (super & report & declarationLevel){
         topLevelDeclaration = (super & report & declarationLevel & topLevelDeclaration)
            <|> Abstract.kindSignature <$ keyword "type"
                  <*> (self & report & typeConstructor)
                  <* (self & report & doubleColon)
                  <*> wrap (self & report & declarationLevel & optionalContext)
                  <*> wrap (kind self)}}}

kindSignaturesMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                  g ~ ExtendedGrammar l t (NodeWrap t),
                                  Ord t, Show t, TextualMonoid t, OutlineMonoid t,
                                  Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l))
                    => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
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
                      <*> derivingClause
               <|> Abstract.kindedNewtypeDeclaration <$ keyword "newtype"
                      <*> wrap optionalContext
                      <*> wrap simpleType
                      <*> wrap (kindSignature self)
                      <* delimiter "="
                      <*> wrap newConstructor
                      <*> derivingClause
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
                                <*> some (comma *> wrap (optionallyKindedTypeVar self)),
           classConstraint = (super & report & declarationLevel & classConstraint)
              <|> Abstract.multiParameterClassConstraint
                  <$> (self & report & declarationLevel & qualifiedTypeClass)
                  <*> parens (pure
                              <$> wrap (Abstract.kindedType
                                        <$> wrap (Abstract.typeVariable <$> optionallyParenthesizedTypeVar self)
                                        <*> wrap (kindSignature self)))},
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

existentialQuantificationMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                             g ~ ExtendedGrammar l t (NodeWrap t),
                                             Ord t, Show t, TextualMonoid t)
                               => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
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

explicitForAllMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                  g ~ ExtendedGrammar l t (NodeWrap t),
                                  Ord t, Show t, OutlineMonoid t, TextualMonoid t,
                                  Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l))
                    => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
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
             <*> wrap (arrowType self)
         <|> Abstract.constrainedType
             <$> wrap context
             <* (self & report & rightDoubleArrow)
             <*> wrap (arrowType self),
      arrowTypeWithWildcards = arrowTypeWithWildcards super
         <|> Abstract.forallType <$ keywordForall self
             <*> many (typeVarBinder self) <* delimiter "."
             <*> wrap (arrowTypeWithWildcards self)
         <|> Abstract.constrainedType
             <$> wrap context
             <* (self & report & rightDoubleArrow)
             <*> wrap (arrowType self),
      optionalForall = keywordForall self *> many (typeVarBinder self) <* delimiter "." <|> pure [],
      kind = kind super
         <|> Abstract.forallKind <$ keywordForall self
             <*> many (typeVarBinder self) <* delimiter "."
             <*> wrap (kind self),
      kindVar = notFollowedBy (keywordForall self) *> kindVar super}

gadtSyntaxMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                              g ~ ExtendedGrammar l t (NodeWrap t),
                              Ord t, Show t, OutlineMonoid t, TextualMonoid t,
                              Deep.Foldable (Serialization (Down Int) t) (Abstract.GADTConstructor l l))
                => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
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
                   <*> derivingClause
               <|> Abstract.gadtNewtypeDeclaration <$ keyword "newtype"
                   <*> wrap simpleType
                   <*> optional (wrap $ kindSignature self) <* keyword "where"
                   <*> wrap (gadtNewConstructor self)
                   <*> derivingClause}},
      optionalForall = keywordForall self *> many (typeVarBinder self) <* delimiter "." <|> pure []}

gadtSyntaxTypeOperatorsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                           g ~ ExtendedGrammar l t (NodeWrap t),
                                           Ord t, Show t, OutlineMonoid t, TextualMonoid t,
                                           Deep.Foldable (Serialization (Down Int) t) (Abstract.GADTConstructor l l))
                => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
gadtSyntaxTypeOperatorsMixin self super = super{
   return_type = return_type super <|>
      Abstract.infixTypeApplication <$> wrap (arg_type self)
                                    <*> (self & report & qualifiedOperator)
                                    <*> wrap (arg_type self)}

dataKindsGadtSyntaxTypeOperatorsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                           g ~ ExtendedGrammar l t (NodeWrap t),
                                           Ord t, Show t, OutlineMonoid t, TextualMonoid t,
                                           Deep.Foldable (Serialization (Down Int) t) (Abstract.GADTConstructor l l))
                => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
dataKindsGadtSyntaxTypeOperatorsMixin self super =
   super{
      return_type = return_type super <|>
         Abstract.promotedInfixTypeApplication
         <$> wrap (arg_type self)
         <* terminator "'"
         <*> (self & report & qualifiedOperator)
         <*> wrap (arg_type self)}

traditionalRecordSyntaxMixin :: (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                 g ~ ExtendedGrammar l t (NodeWrap t),
                                 Ord t, Show t, OutlineMonoid t, TextualMonoid t,
                                 Deep.Foldable (Serialization (Down Int) t) (Abstract.GADTConstructor l l))
                             => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
traditionalRecordSyntaxMixin self super =
   super{
      report = (super & report){
         declarationLevel = (super & report & declarationLevel){
            fieldDeclaration = (super & original_report & declarationLevel & fieldDeclaration),
            newRecordConstructor = (super & original_report & declarationLevel & newRecordConstructor)},
         fieldBinding = (super & original_report & fieldBinding),
         fieldPattern = (super & original_report & fieldPattern)},
      gadtBody = gadtBody super <|> record_gadt_body self}

variableLexeme, constructorLexeme, identifierTail :: (Rank2.Apply g, Ord t, Show t, TextualMonoid t) => Parser g t t
variableLexeme = filter (`Set.notMember` Report.reservedWords) (satisfyCharInput varStart <> identifierTail)
                 <?> "variable"
   where varStart c = (Char.isLetter c && not (Char.isUpper c)) ||  c == '_'
constructorLexeme = satisfyCharInput Char.isUpper <> identifierTail <?> "constructor"
identifierTail = takeCharsWhile isNameTailChar

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
