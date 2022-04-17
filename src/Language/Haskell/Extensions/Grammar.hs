{-# Language FlexibleContexts, FlexibleInstances, NamedFieldPuns, OverloadedStrings,
             Rank2Types, RecordWildCards, ScopedTypeVariables,
             TemplateHaskell, TupleSections, TypeApplications, TypeFamilies, TypeSynonymInstances #-}

-- | Missing syntax extensions:
-- * @QualifiedDo@ requires TemplateHaskell 2.17
-- * @TransformListComp@ is not supported by TemplateHaskell
-- * @Arrows@ is not supported by TemplateHaskell
-- * @LexicalNegation@ ignores the presence or absence of whitespace preceding the minus

module Language.Haskell.Extensions.Grammar (extendedGrammar, parseModule, report, module Report) where

import Control.Applicative
import Control.Monad (void)
import qualified Data.Char as Char
import Data.Foldable (fold, toList)
import Data.Functor.Compose (Compose(getCompose))
import Data.List (foldl', null, sortOn)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Ord (Down)
import Data.Maybe (isJust)
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
                                 oneExtendedLine, rewrap, startSepEndBy, wrap, unwrap, whiteSpace)
import Language.Haskell.Reserializer (Lexeme(..), Serialization, TokenType(..))

import Prelude hiding (exponent, filter, null)

data ExtendedGrammar l t f p = ExtendedGrammar {
   report :: HaskellGrammar l t f p,
   keywordForall :: p (),
   kindSignature, kind, bKind, aKind, groundTypeKind :: p (Abstract.Kind l l f f),
   cType, forallType :: p (Abstract.Type l l f f),
   typeWithWildcards, cTypeWithWildcards, bTypeWithWildcards, aTypeWithWildcards :: p (Abstract.Type l l f f),
   promotedType :: p (Abstract.Type l l f f),
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
   return_type, arg_type :: p (Abstract.Type l l f f)}

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
     (Set.fromList [ExplicitNamespaces],             [(0, explicitNamespacesMixin)]),
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
     (Set.fromList [StarIsType, UnicodeSyntax],      [(9, unicodeStarIsTypeMixin)]),
     (Set.fromList [GADTSyntax, TypeOperators],      [(9, gadtSyntaxTypeOperatorsMixin)]),
     (Set.fromList [DataKinds, TypeOperators],       [(9, dataKindsTypeOperatorsMixin)]),
     (Set.fromList [MultiParameterConstraints,
                    TypeOperators],                  [(9, multiParameterConstraintsTypeOperatorsMixin)]),
     (Set.fromList [DataKinds, TypeOperators,
                    GADTSyntax],                     [(9, dataKindsGadtSyntaxTypeOperatorsMixin)]),
     (Set.fromList [PolyKinds, ExplicitForAll],      [(9, visibleDependentKindQualificationMixin)])]

languagePragmas :: (Ord t, Show t, TextualMonoid t, LexicalParsing (Parser g t)) => Parser g t [ExtensionSwitch]
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
   where isIncluded required _ = all (`Map.member` extensions) required

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
          instanceTypeDesignator =
             generalTypeConstructor
             <|> Abstract.listType <$> brackets (wrap $ nonTerminal optionallyKindedAndParenthesizedTypeVar)
             <|> parens (nonTerminal (Report.instanceTypeDesignator . declarationLevel . report)
                         <|> typeVarApplications
                         <|> Abstract.tupleType <$> typeVarTuple
                         <|> Abstract.functionType
                             <$> wrap (nonTerminal optionallyKindedAndParenthesizedTypeVar)
                             <* nonTerminal (Report.rightArrow . report)
                             <*> wrap (nonTerminal optionallyKindedAndParenthesizedTypeVar))},
        typeTerm = nonTerminal forallType},
     keywordForall = keyword "forall",
     kindSignature = empty,
     kindVar = nonTerminal (Report.typeVar . report),
     kind = empty,
     bKind = empty,
     aKind = empty,
     groundTypeKind = empty,
     forallType = nonTerminal cType,
     cType = Abstract.functionType <$> wrap (nonTerminal $ Report.bType . report)
                                   <* nonTerminal (Report.rightArrow . report)
                                   <*> wrap (nonTerminal cType)
             <|> nonTerminal (Report.bType . report),
     typeWithWildcards =
        Abstract.functionType <$> wrap (nonTerminal cTypeWithWildcards) <* rightArrow
                              <*> wrap (nonTerminal typeWithWildcards)
        <|> Abstract.kindedType <$> wrap (nonTerminal typeWithWildcards) <*> wrap (nonTerminal kindSignature)
        <|> nonTerminal cTypeWithWildcards,
     cTypeWithWildcards = nonTerminal bTypeWithWildcards,
     bTypeWithWildcards =
        Abstract.typeApplication <$> wrap (nonTerminal bTypeWithWildcards) <*> wrap (nonTerminal aTypeWithWildcards)
        <|> nonTerminal aTypeWithWildcards,
     aTypeWithWildcards =
        generalTypeConstructor
        <|> Abstract.typeVariable <$> nonTerminal (Report.typeVar . report)
        <|> Abstract.typeWildcard <$ keyword "_"
        <|> Abstract.tupleType
            <$> parens ((:|) <$> wrap (nonTerminal typeWithWildcards)
                             <*> some (comma *> wrap (nonTerminal typeWithWildcards)))
        <|> Abstract.listType <$> brackets (wrap $ nonTerminal typeWithWildcards)
        <|> parens (nonTerminal typeWithWildcards),
     promotedType =
        Abstract.promotedConstructorType <$ terminator "'" <*> wrap (nonTerminal $ Report.generalConstructor . report)
        <|> Abstract.promotedIntegerLiteral <$> nonTerminal (Report.integer . report)
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
                                  <*> wrap optionalContext
                                  <*> wrap (nonTerminal gadtNewBody),
     constructorIDs = constructor `sepByNonEmpty` comma,
     gadtNewBody =
        parens (nonTerminal gadtNewBody)
        <|> Abstract.functionType
            <$> wrap (bType <|> Abstract.strictType <$ delimiter "!" <*> wrap bType)
            <* nonTerminal (Report.rightArrow . report)
            <*> wrap (nonTerminal return_type)
        <|> Abstract.recordFunctionType
            <$> braces ((:[]) <$> wrap fieldDeclaration)
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
            <$> braces (wrap fieldDeclaration `sepBy` comma)
            <* nonTerminal (Report.rightArrow . report)
            <*> wrap (nonTerminal return_type),
     return_type = Abstract.typeApplication
                      <$> wrap (nonTerminal return_type <|> parens (nonTerminal return_type))
                      <*> wrap (nonTerminal arg_type)
                   <|> Abstract.constructorType <$> wrap generalConstructor,
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
overloadedLabelsMixin self super@ExtendedGrammar{report= HaskellGrammar{..}} = super{
   report= (report super){
      variableIdentifier = token (Abstract.name . Text.pack . toString mempty <$> (string "#" <> variableLexeme))
                           <|> variableIdentifier,
      variableSymbol = notFollowedBy (string "#" *> variableLexeme) *> variableSymbol}}

unicodeSyntaxMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                 Ord t, Show t, OutlineMonoid t, g ~ ExtendedGrammar l t (NodeWrap t))
                   => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
unicodeSyntaxMixin self super@ExtendedGrammar{report= HaskellGrammar{..}} = super{
   keywordForall = keywordForall super <|> delimiter "∀",
   report= (report super){
      doubleColon = doubleColon <|> delimiter "∷",
      rightDoubleArrow = rightDoubleArrow <|> delimiter "⇒",
      rightArrow = rightArrow <|> delimiter "→",
      leftArrow = leftArrow <|> delimiter "←",
      variableSymbol = notSatisfyChar (`elem` ("∀←→⇒∷★" :: [Char])) *> variableSymbol}}

magicHashMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t,
                             g ~ ExtendedGrammar l t (NodeWrap t))
               => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
magicHashMixin self super@ExtendedGrammar{report= HaskellGrammar{..}} =
  let integer', integerHash, integerHash2 :: (LexicalParsing (Parser g t), Show t, TextualMonoid t) => Parser g t Integer
      float', floatHash, floatHash2 :: (LexicalParsing (Parser g t), Show t, TextualMonoid t) => Parser g t Rational
      charLiteral', charHashLiteral :: (LexicalParsing (Parser g t), Show t, TextualMonoid t) => Parser g t Char
      stringLiteral', stringHashLiteral :: (LexicalParsing (Parser g t), Show t, TextualMonoid t) => Parser g t Text

      integer' = token (integerLexeme <* notFollowedBy (string "#"))
      float' = token (floatLexeme <* notFollowedBy (string "#"))
      charLiteral' = token (charLexeme <* notFollowedBy (string "#"))
      stringLiteral' = token (stringLexeme <* notFollowedBy (string "#")) <?> "string literal"

      integerHash = token (integerLexeme <* string "#" <* notFollowedBy (string "#"))
      floatHash = token (floatLexeme <* string "#" <* notFollowedBy (string "#"))
      integerHash2 = token (integerLexeme <* string "##")
      floatHash2 = token (floatLexeme <* string "##")
      charHashLiteral = token (charLexeme <* string "#")
      stringHashLiteral = token (stringLexeme <* string "#")
  in super{report= (report super){
        variableIdentifier =
           token (Abstract.name . Text.pack . toString mempty <$> (variableLexeme <> concatAll (string "#"))),
        constructorIdentifier =
           token (Abstract.name . Text.pack . toString mempty <$> (constructorLexeme <> concatAll (string "#"))),
        lPattern = aPattern
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
                    <|> Abstract.constructorPattern <$> wrap generalConstructor <*> some (wrap aPattern),
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
recursiveDoMixin self super@ExtendedGrammar{report= HaskellGrammar{..}} = super{
   report= (report super){
      closedBlockExpresion = closedBlockExpresion <|> Abstract.mdoExpression <$ keyword "mdo" <*> wrap statements,
      statement = statement
                  <|> Deep.InL
                      <$> wrap (Abstract.recursiveStatement
                                . (either id (rewrap Abstract.expressionStatement) . Deep.eitherFromSum . unwrap <$>)
                                <$ keyword "rec"
                                <*> blockOf statement),
      variableIdentifier = notFollowedBy (keyword "mdo" <|> keyword "rec") *> variableIdentifier}}

parallelListComprehensionsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                              Ord t, Show t, OutlineMonoid t, g ~ ExtendedGrammar l t (NodeWrap t))
                                => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
parallelListComprehensionsMixin self super@ExtendedGrammar{report= HaskellGrammar{..}} = super{
   report= (report super){
      bareExpression = bareExpression
                       <|> brackets (Abstract.parallelListComprehension
                                     <$> expression <*> qualifiers <*> qualifiers <*> many qualifiers)}}

tupleSectionsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                 Ord t, Show t, OutlineMonoid t, g ~ ExtendedGrammar l t (NodeWrap t))
                   => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
tupleSectionsMixin self super@ExtendedGrammar{report= HaskellGrammar{..}} = super{
   report= (report super){
      bareExpression = bareExpression
                       <|> Abstract.tupleSectionExpression
                              <$> parens (filter (any isJust)
                                          $ (:|) <$> optional expression <*> some (comma *> optional expression))}}

lambdaCaseMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t), Ord t, Show t, TextualMonoid t,
                              g ~ ExtendedGrammar l t (NodeWrap t))
                => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
lambdaCaseMixin self super@ExtendedGrammar{report= HaskellGrammar{..}} = super{
   report= (report super){
      closedBlockExpresion = closedBlockExpresion
                             <|> Abstract.lambdaCaseExpression <$ (delimiter "\\" *> keyword "case")
                                                               <*> alternatives}}

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
multiWayIfMixin self super@ExtendedGrammar{report= HaskellGrammar{..}} = super{
   report= (report super){
      closedBlockExpresion = closedBlockExpresion
                             <|> Abstract.multiWayIfExpression <$ keyword "if"
                                     <*> blockOf'
                                            (Abstract.guardedExpression . toList
                                                <$> guards <* rightArrow
                                                <*> expression)}}

packageImportsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t), Ord t, Show t,
                                  OutlineMonoid t, g ~ ExtendedGrammar l t (NodeWrap t))
                      => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
packageImportsMixin self super@ExtendedGrammar{report= HaskellGrammar{moduleLevel= ModuleLevelGrammar{..}}} = super{
   report= (report super){
      moduleLevel= (moduleLevel . report $ super){
         importDeclaration = importDeclaration
                             <|> Abstract.packageQualifiedImportDeclaration <$ keyword "import"
                                 <*> (True <$ keyword "qualified" <|> pure False)
                                 <*> (stringLiteral . report $ self)
                                 <*> Report.moduleId
                                 <*> optional (keyword "as" *> Report.moduleId)
                                 <*> optional (wrap importSpecification)}}}

safeImportsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t,
                               g ~ ExtendedGrammar l t (NodeWrap t))
                 => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
safeImportsMixin self super@ExtendedGrammar{report= HaskellGrammar {moduleLevel= ModuleLevelGrammar{..}}} = super{
   report= (report super){
      moduleLevel= (moduleLevel . report $ super){
         importDeclaration = importDeclaration
                             <|> Abstract.safeImportDeclaration <$ keyword "import" <* keyword "safe"
                                 <*> (True <$ keyword "qualified" <|> pure False)
                                 <*> Report.moduleId
                                 <*> optional (keyword "as" *> Report.moduleId)
                                 <*> optional (wrap importSpecification)}}}

importQualifiedPostMixin :: forall l g t. (Abstract.Haskell l, LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t,
                                       g ~ ExtendedGrammar l t (NodeWrap t))
                         => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
importQualifiedPostMixin self super@ExtendedGrammar{report= HaskellGrammar{moduleLevel= ModuleLevelGrammar{..}}} = super{
   report= (report super){
      moduleLevel= (moduleLevel . report $ super){
         importDeclaration = importDeclaration
                             <|> flip Abstract.importDeclaration <$ keyword "import"
                                 <*> Report.moduleId
                                 <*> (True <$ keyword "qualified" <|> pure False)
                                 <*> optional (keyword "as" *> Report.moduleId)
                                 <*> optional (wrap importSpecification)}}}

safePackageImportsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t), Ord t, Show t,
                                      OutlineMonoid t, g ~ ExtendedGrammar l t (NodeWrap t))
                        => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
safePackageImportsMixin self super@ExtendedGrammar{report= HaskellGrammar{moduleLevel= ModuleLevelGrammar{..}}} = super{
   report= (report super){
      moduleLevel= (moduleLevel . report $ super){
         importDeclaration = importDeclaration
                             <|> Abstract.safePackageQualifiedImportDeclaration <$ keyword "import" <* keyword "safe"
                                 <*> (True <$ keyword "qualified" <|> pure False)
                                 <*> (stringLiteral . report $ self)
                                 <*> Report.moduleId
                                 <*> optional (keyword "as" *> Report.moduleId)
                                 <*> optional (wrap importSpecification)}}}

packageImportsQualifiedPostMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                               Ord t, Show t, OutlineMonoid t, g ~ ExtendedGrammar l t (NodeWrap t))
                                 => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
packageImportsQualifiedPostMixin self super@ExtendedGrammar{report= HaskellGrammar{moduleLevel= ModuleLevelGrammar{..}}} = super{
   report= (report super){
      moduleLevel= (moduleLevel . report $ super){
         importDeclaration = importDeclaration
                             <|> Abstract.packageQualifiedImportDeclaration <$ keyword "import"
                                 <**> pure flip
                                 <*> (stringLiteral . report $ self)
                                 <**> pure flip
                                 <*> Report.moduleId
                                 <*> (True <$ keyword "qualified" <|> pure False)
                                 <*> optional (keyword "as" *> Report.moduleId)
                                 <*> optional (wrap importSpecification)}}}

safeImportsQualifiedPostMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                            Ord t, Show t, OutlineMonoid t, g ~ ExtendedGrammar l t (NodeWrap t))
                              => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
safeImportsQualifiedPostMixin self super@ExtendedGrammar{report= HaskellGrammar{moduleLevel= ModuleLevelGrammar{..}}} = super{
   report= (report super){
      moduleLevel= (moduleLevel . report $ super){
         importDeclaration = importDeclaration
                             <|> flip Abstract.safeImportDeclaration <$ keyword "import" <* keyword "safe"
                                 <*> Report.moduleId
                                 <*> (True <$ keyword "qualified" <|> pure False)
                                 <*> optional (keyword "as" *> Report.moduleId)
                                 <*> optional (wrap importSpecification)}}}

safePackageImportsQualifiedPostMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                                   Ord t, Show t, OutlineMonoid t, g ~ ExtendedGrammar l t (NodeWrap t))
                                     => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
safePackageImportsQualifiedPostMixin self super@ExtendedGrammar{report= HaskellGrammar{moduleLevel= ModuleLevelGrammar{..}}} = super{
   report= (report super){
      moduleLevel= (moduleLevel . report $ super){
         importDeclaration = importDeclaration
                             <|> Abstract.safePackageQualifiedImportDeclaration <$ keyword "import" <* keyword "safe"
                                 <**> pure flip
                                 <*> (stringLiteral . report $ self)
                                 <**> pure flip
                                 <*> Report.moduleId
                                 <*> (True <$ keyword "qualified" <|> pure False)
                                 <*> optional (keyword "as" *> Report.moduleId)
                                 <*> optional (wrap importSpecification)}}}

explicitNamespacesMixin :: forall l g t. (g ~ ExtendedGrammar l t (NodeWrap t), Abstract.ExtendedHaskell l,
                                      LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t,
                                      g ~ ExtendedGrammar l t (NodeWrap t))
                        => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
explicitNamespacesMixin self super@ExtendedGrammar{report= HaskellGrammar{moduleLevel= ModuleLevelGrammar{..}}} = super{
   report= (report super){
      moduleLevel= (moduleLevel . report $ super){
         export = export
            <|> Abstract.exportClassOrType <$ keyword "type"
                <*> parens (qualifiedVariableSymbol . report $ self)
                <*> pure Nothing,
         importItem = importItem
            <|> Abstract.importClassOrType <$ keyword "type"
                <*> parens (variableSymbol . report $ self)
                <*> pure Nothing,
         members = parens (Abstract.allMembers <$ delimiter ".."
                           <|> Abstract.explicitlyNamespacedMemberList
                               <$> (namespacedMember self `sepBy` comma) <* optional comma)}}}

blockArgumentsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                  Ord t, Show t, OutlineMonoid t,
                                  Deep.Foldable (Serialization (Down Int) t) (Abstract.GuardedExpression l l),
                                  g ~ ExtendedGrammar l t (NodeWrap t))
                    => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
blockArgumentsMixin self super@ExtendedGrammar{report= HaskellGrammar{..}} = super{
   report= (report super){
      lExpression = lExpression <|> wrap (Abstract.applyExpression <$> fExpression <*> wrap openBlockExpression),
      dExpression = fExpression,
      bareExpression = bareExpression <|> closedBlockExpresion}}

lexicalNegationMixin :: forall l g t. (Abstract.Haskell l, LexicalParsing (Parser (ExtendedGrammar l t (NodeWrap t)) t),
                                   Ord t, Show t, TextualMonoid t, g ~ ExtendedGrammar l t (NodeWrap t))
                     => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
lexicalNegationMixin self super@ExtendedGrammar{report= HaskellGrammar{..}} = super{
   report= (report super){
      qualifiedVariableSymbol = notFollowedBy (string "-"
                                               *> satisfyCharInput (\c-> Char.isAlphaNum c || c == '(' || c == '['))
                                *> token (Report.nameQualifier <*> variableSymbol),
      infixExpression = wrap (Abstract.infixExpression
                                 <$> (Report.dExpression . report $ self)
                                 <*> wrap (Abstract.referenceExpression <$> qualifiedOperator)
                                 <*> (Report.infixExpression . report $ self))
                        <|> lExpression,
      leftInfixExpression =
         wrap (Abstract.infixExpression
                  <$> (Report.dExpression . report $ self)
                  <*> wrap (Abstract.referenceExpression <$> qualifiedOperator)
                  <*> (Report.leftInfixExpression . report $ self))
         <|> dExpression,
      bareExpression =
         Abstract.applyExpression <$> wrap (Abstract.negate <$ prefixMinus) <*> aExpression
         <|> parens (Abstract.rightSectionExpression
                     <$> (notFollowedBy prefixMinus *> qualifiedOperator)
                     <*> infixExpression)
         <|> bareExpression}}
   where prefixMinus = void (string "-"
                             <* lookAhead (satisfyCharInput $ \c-> Char.isAlphaNum c || c == '(' || c == '[')
                             <* lift ([[Token Modifier "-"]], ()))
                       <?> "prefix -"

negativeLiteralsMixin :: forall l g t. (Abstract.Haskell l, LexicalParsing (Parser (ExtendedGrammar l t (NodeWrap t)) t),
                                    Ord t, Show t, TextualMonoid t, g ~ ExtendedGrammar l t (NodeWrap t))
                      => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
negativeLiteralsMixin self super@ExtendedGrammar{report= HaskellGrammar{..}} = super{
   report= (report super){
      qualifiedVariableSymbol = notFollowedBy (string "-" *> satisfyCharInput Char.isDigit) *> qualifiedVariableSymbol,
      infixExpression =
         wrap (Abstract.infixExpression
                  <$> (Report.dExpression . report $ self)
                  <*> wrap (Abstract.referenceExpression <$> qualifiedOperator)
                  <*> (Report.infixExpression . report $ self)
               <|> Abstract.applyExpression <$> wrap (Abstract.negate <$ prefixMinus) <*> infixExpression)
         <|> lExpression,
      leftInfixExpression =
         wrap (Abstract.infixExpression
                  <$> (Report.dExpression . report $ self)
                  <*> wrap (Abstract.referenceExpression <$> qualifiedOperator)
                  <*> (Report.leftInfixExpression . report $ self)
                  <|> Abstract.applyExpression <$> wrap (Abstract.negate <$ prefixMinus) <*> leftInfixExpression)
         <|> dExpression,
      integerLexeme = (negate <$ string "-" <|> pure id) <*> integerLexeme,
      floatLexeme = (negate <$ string "-" <|> pure id) <*> floatLexeme}}
   where prefixMinus = void (token $ string "-" <* notSatisfyChar Char.isDigit) <?> "prefix -"

binaryLiteralsMixin :: forall l g t. (Abstract.Haskell l, LexicalParsing (Parser g t), Ord t, Show t, TextualMonoid t,
                                  g ~ ExtendedGrammar l t (NodeWrap t))
                      => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
binaryLiteralsMixin self super@ExtendedGrammar{report= HaskellGrammar{..}} = super{
   report= (report super){
      integerLexeme =
         (string "0b" <|> string "0B")
         *> (foldl' binary 0 . toString mempty <$> takeCharsWhile1 (\c-> c == '0' || c == '1') <?> "binary number")
         <<|> integerLexeme}}
   where binary n '0' = 2*n
         binary n '1' = 2*n + 1
         binary _ _ = error "non-binary"

hexFloatLiteralsMixin :: forall l g t. (Abstract.Haskell l, LexicalParsing (Parser g t), Ord t, Show t, TextualMonoid t,
                                    g ~ ExtendedGrammar l t (NodeWrap t))
                      => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
hexFloatLiteralsMixin self super@ExtendedGrammar{report= HaskellGrammar{..}} = super{
   report= (report super){
      integerLexeme = notFollowedBy ((string "0x" <|> string "0X")
                                    *> hexadecimal *> satisfyCharInput (`elem` ['.', 'p', 'P']))
                      *> integerLexeme,
      floatLexeme = (string "0x" <|> string "0X")
                    *> (readHexFloat <$> hexadecimal <* string "." <*> hexadecimal <*> (hexExponent <<|> pure 0)
                       <|> readHexFloat <$> hexadecimal <*> pure mempty <*> hexExponent)
                    <|> floatLexeme}}
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
numericUnderscoresMixin self super@ExtendedGrammar{report= HaskellGrammar{..}} = super{
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
binaryUnderscoresMixin self super@ExtendedGrammar{report= HaskellGrammar{..}} = super{
   report= (report super){
      integerLexeme =
         (string "0b" <|> string "0B")
         *> (foldl' binary 0 . toString mempty
             <$> (binaryDigits <> concatAll (char '_' *> binaryDigits)) <?> "binary number")
         <<|> integerLexeme}}
   where binary n '0' = 2*n
         binary n '1' = 2*n + 1
         binary _ _ = error "non-binary"
         binaryDigits = takeCharsWhile1 (\c-> c == '0' || c == '1')

typeOperatorsMixin :: forall l g t. (g ~ ExtendedGrammar l t (NodeWrap t), Abstract.ExtendedHaskell l,
                                 LexicalParsing (Parser g t), Ord t, Show t, TextualMonoid t)
                   => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
typeOperatorsMixin self super@ExtendedGrammar{report= HaskellGrammar{declarationLevel= DeclarationGrammar{..}}} =
   super{
      report= (report super){
                declarationLevel= (declarationLevel . report $ super){
                  simpleType = simpleType
                     <|> Abstract.simpleInfixTypeLHSApplication
                                  <$> typeVarBinder self
                                  <*> anySymbol
                                  <*> typeVarBinder self
                     <|> parens ((Report.simpleType . Report.declarationLevel . report $ self)),
                  instanceTypeDesignator = instanceTypeDesignator
                     <|> parens (Abstract.infixTypeApplication
                                 <$> wrap (optionallyKindedAndParenthesizedTypeVar self)
                                 <*> (qualifiedOperator . report $ self)
                                 <*> wrap (optionallyKindedAndParenthesizedTypeVar self)),
                  qualifiedTypeClass =
                     (qualifiedConstructor . report $ self) <|> parens (qualifiedVariableSymbol . report $ self)},
                typeConstructor = (constructorIdentifier . report $ self) <|> parens anySymbol,
                generalTypeConstructor = (generalTypeConstructor . report $ super)
                  <|> Abstract.constructorType
                      <$> wrap (Abstract.constructorReference <$> parens (qualifiedVariableSymbol . report $ self)),
                bType = (bType . report $ super)
                   <|> Abstract.infixTypeApplication
                          <$> wrap (Report.bType . report $ self)
                          <*> (qualifiedOperator . report $ self)
                          <*> wrap (Report.aType . report $ self)},
        cTypeWithWildcards = cTypeWithWildcards super
           <|> Abstract.infixTypeApplication
                  <$> wrap (bTypeWithWildcards self)
                  <*> (qualifiedOperator . report $ self)
                  <*> wrap (cTypeWithWildcards self),
        familyInstanceDesignator = familyInstanceDesignator super
           <|> Abstract.infixTypeClassInstanceLHS
                  <$> wrap (bTypeWithWildcards super)
                  <*> (qualifiedOperator . report $ self)
                  <*> wrap (cTypeWithWildcards super)}
   where anySymbol = (constructorSymbol . report $ self) <|> (variableSymbol . report $ self)

equalityConstraintsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                       g ~ ExtendedGrammar l t (NodeWrap t), Ord t, Show t, TextualMonoid t)
                       => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
equalityConstraintsMixin self super@ExtendedGrammar
                              {report= HaskellGrammar{declarationLevel= DeclarationGrammar{..}}} = super{
   report= (report super){
      declarationLevel= (declarationLevel . report $ super){
         constraint = constraint <|> equalityConstraint},
      typeTerm = (typeTerm . report $ super) <|> Abstract.constraintType <$> wrap equalityConstraint}}
   where equalityConstraint =
            Abstract.typeEqualityConstraint <$> wrap (Report.bType . report $ self)
            <* delimiter "~" <*> wrap ((Report.bType . report $ self))

multiParameterConstraintsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                             g ~ ExtendedGrammar l t (NodeWrap t), Ord t, Show t, TextualMonoid t)
                               => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
multiParameterConstraintsMixin self super@ExtendedGrammar
                               {report= HaskellGrammar{declarationLevel= DeclarationGrammar{..}}} = super{
   report= (report super){
      declarationLevel= (declarationLevel . report $ super){
         constraint= constraint
            <|> Abstract.multiParameterClassConstraint <$> qualifiedTypeClass
                <*> filter
                       ((1 /=) . length)
                       (many $ wrap $
                        optionallyKindedAndParenthesizedTypeVar self
                        <|> parens (Report.typeApplications . declarationLevel . report $ self))}}}

multiParameterConstraintsTypeOperatorsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                                          g ~ ExtendedGrammar l t (NodeWrap t),
                                                          Ord t, Show t, TextualMonoid t)
                                            => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
multiParameterConstraintsTypeOperatorsMixin
   self
   super@ExtendedGrammar {report= HaskellGrammar{declarationLevel= DeclarationGrammar{..}}} = super{
   report= (report super){
      declarationLevel= (declarationLevel . report $ super){
         constraint = constraint
            <|> Abstract.infixConstraint
                <$> wrap ((Report.bType . report $ self))
                <*> (qualifiedOperator . report $ self)
                <*> wrap ((Report.bType . report $ self))}}}

gratuitouslyParenthesizedTypesMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                                  g ~ ExtendedGrammar l t (NodeWrap t),
                                                  OutlineMonoid t, Ord t, Show t, TextualMonoid t,
                                                  Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l),
                                                  Deep.Foldable (Serialization (Down Int) t)
                                                                (Abstract.GADTConstructor l l))
                            => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
gratuitouslyParenthesizedTypesMixin
   self
   super@ExtendedGrammar
               {report= HaskellGrammar{declarationLevel= DeclarationGrammar{..}}} = super{
   report= (report super){
      declarationLevel = (declarationLevel . report $ super){
         topLevelDeclaration = topLevelDeclaration
            <|> Abstract.classDeclaration <$ keyword "class"
                <*> wrap optionalContext
                <*> wrap (Abstract.simpleKindedTypeLHS
                          <$> typeClass
                          <*> ((:[]) <$> parens (typeVarBinder self)))
                <*> (keyword "where" *> blockOf inClassDeclaration <|> pure []),
         constraint = constraint <|> parens (Report.constraint . declarationLevel . report $ self),
         context = (Report.constraint . declarationLevel . report $ self)
            <|> Abstract.constraints
                <$> parens (filter ((1 /=) . length)
                            $ wrap ((Report.constraint . declarationLevel . report $ self)) `sepBy` comma),
         classConstraint = classConstraint
            <|> Abstract.simpleConstraint
                <$> (Report.qualifiedTypeClass . declarationLevel . report $ self)
                <*> parens (optionallyParenthesizedTypeVar self),
         qualifiedTypeClass = qualifiedTypeClass <|> parens qtc,
         typeApplications =
            Abstract.typeApplication
            <$> wrap (Abstract.typeVariable <$> optionallyParenthesizedTypeVar self <|> typeApplications)
            <*> wrap (aType . report $ self),
         typeVarApplications =
            (generalTypeConstructor . report $ self)
            <|> Abstract.typeApplication
                <$> wrap ((Report.typeVarApplications . declarationLevel . report $ self)
                          <|> parens (Report.typeVarApplications . declarationLevel . report $ self))
                <*> wrap (optionallyKindedAndParenthesizedTypeVar self),
         instanceDesignator = instanceDesignator
            <|> parens (Report.instanceDesignator . declarationLevel . report $ self),
         derivingClause = keyword "deriving"
                          *> (pure <$> wrap (Abstract.simpleDerive <$> qtc)
                              <|> parens (filter ((/= 1) . length)
                                          $ wrap (Abstract.simpleDerive <$> qtc) `sepBy` comma))
                          <|> pure []}},
   optionallyParenthesizedTypeVar = (Report.typeVar . report $ self)
                                    <|> parens (optionallyParenthesizedTypeVar self),
   typeVarBinder = Abstract.implicitlyKindedTypeVariable <$> optionallyParenthesizedTypeVar self}
   where qtc = (Report.qualifiedTypeClass . declarationLevel . report $ self)

flexibleInstancesMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                     g ~ ExtendedGrammar l t (NodeWrap t),
                                     Ord t, Show t, TextualMonoid t)
                       => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
flexibleInstancesMixin self super = super{
   report= (report super){
             declarationLevel= (declarationLevel . report $ super){
                instanceDesignator = flexibleInstanceDesignator self}}}

typeFamiliesMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                    g ~ ExtendedGrammar l t (NodeWrap t),
                                    Ord t, Show t, TextualMonoid t, OutlineMonoid t,
                                    Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l),
                                    Deep.Foldable (Serialization (Down Int) t) (Abstract.GADTConstructor l l))
                  => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
typeFamiliesMixin self super@ExtendedGrammar{report= HaskellGrammar{declarationLevel= DeclarationGrammar{..}}} =
  super{
    report= (report super){
      declarationLevel= (declarationLevel . report $ super){
         topLevelDeclaration = topLevelDeclaration
            <|> Abstract.dataFamilyDeclaration <$ keyword "data" <* keyword "family"
                <*> wrap simpleType <*> optional (wrap $ kindSignature self)
            <|> Abstract.openTypeFamilyDeclaration <$ keyword "type" <* keyword "family"
                <*> wrap simpleType <*> optional (wrap $ kindSignature self)
            <|> Abstract.closedTypeFamilyDeclaration <$ keyword "type" <* keyword "family"
                <*> wrap simpleType <*> optional (wrap $ kindSignature self) <* keyword "where"
                <*> blockOf (Abstract.typeFamilyInstance
                             <$> optionalForall self
                             <*> wrap (familyInstanceDesignator self) <* delimiter "="
                             <*> wrap (typeTerm . report $ self))
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
                <*> wrap (typeTerm . report $ self),
         inClassDeclaration = inClassDeclaration
            <|> Abstract.dataFamilyDeclaration <$ keyword "data" <* optional (keyword "family")
                <*> wrap simpleType <*> optional (wrap $ kindSignature self)
            <|> Abstract.openTypeFamilyDeclaration <$ keyword "type" <* optional (keyword "family")
                <*> wrap simpleType <*> optional (wrap $ kindSignature self)
            <|> inClassOrInstanceTypeFamilyDeclaration self,
         inInstanceDeclaration = inInstanceDeclaration <|> inClassOrInstanceTypeFamilyDeclaration self
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
           <*> wrap (typeTerm . report $ self)}

typeFamilyDependenciesMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                          g ~ ExtendedGrammar l t (NodeWrap t),
                                          Ord t, Show t, TextualMonoid t, OutlineMonoid t,
                                          Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l),
                                          Deep.Foldable (Serialization (Down Int) t) (Abstract.GADTConstructor l l))
                            => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
typeFamilyDependenciesMixin
  self
  super@ExtendedGrammar{report= HaskellGrammar{declarationLevel= DeclarationGrammar{..}}} =
  super{
    report= (report super){
      declarationLevel= (declarationLevel . report $ super){
         topLevelDeclaration = topLevelDeclaration
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
                             <*> wrap (typeTerm . report $ self)),
         inClassDeclaration = inClassDeclaration
            <|> Abstract.injectiveOpenTypeFamilyDeclaration <$ keyword "type" <* optional (keyword "family")
                <*> wrap simpleType <* delimiter "="
                <*> typeVarBinder self
                <*> (Just <$> dependencies)}}}
   where dependencies = (,) <$> (delimiter "|" *> (Report.typeVar . report $ self)) <* (rightArrow . report $ self)
                            <*> someNonEmpty (Report.typeVar . report $ self)

dataKindsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                  g ~ ExtendedGrammar l t (NodeWrap t),
                                  Ord t, Show t, TextualMonoid t, OutlineMonoid t,
                                  Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l))
                    => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
dataKindsMixin self super@ExtendedGrammar{report= HaskellGrammar{declarationLevel= baseDeclarations}} = super{
   report= (report super){
      aType = (aType . report $ super)
         <|> promotedType self
         <|> Abstract.promotedTupleType <$ terminator "'"
             <*> parens ((:|) <$> wrap (typeTerm . report $ self)
                              <*> some (comma *> wrap (typeTerm . report $ self)))
         <|> Abstract.promotedListType <$ terminator "'"
                                       <*> brackets (wrap (typeTerm . report $ self) `sepBy1` comma)
         <|> Abstract.promotedListType
             <$> brackets ((:) <$> wrap (typeTerm . report $ self) <* comma
                               <*> wrap (typeTerm . report $ self) `sepBy1` comma),
      declarationLevel= (declarationLevel . report $ super){
         instanceTypeDesignator = instanceTypeDesignator baseDeclarations
            <|> promotedType self}},
   aTypeWithWildcards = aTypeWithWildcards super
      <|> promotedType self
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
         <*> wrap (Report.aType . report $ self),
   aKind = aKind super
     <|> Abstract.tupleKind <$> parens ((:|) <$> wrap (kind self) <*> some (comma *> wrap (kind self)))
     <|> Abstract.listKind <$> brackets (wrap $ kind self)}

dataKindsTypeOperatorsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                          g ~ ExtendedGrammar l t (NodeWrap t),
                                          Ord t, Show t, TextualMonoid t, OutlineMonoid t,
                                          Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l))
                            => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
dataKindsTypeOperatorsMixin self super@ExtendedGrammar
                                 {report= HaskellGrammar{declarationLevel= baseDeclarations}} = super{
   report= (report super){
      declarationLevel= (declarationLevel . report $ super){
         instanceTypeDesignator = instanceTypeDesignator baseDeclarations
            <|> parens (Abstract.promotedInfixTypeApplication
                        <$> wrap (optionallyKindedAndParenthesizedTypeVar self)
                        <* terminator "'"
                        <*> (qualifiedOperator . report $ super)
                        <*> wrap (optionallyKindedAndParenthesizedTypeVar self))},
      bType = (bType . report $ super)
         <|> Abstract.promotedInfixTypeApplication
             <$> wrap (bType . report $ self)
             <* terminator "'"
             <*> (qualifiedOperator . report $ super)
             <*> wrap (aType . report $ self)},
   bKind = bKind super
           <|> Abstract.infixKindApplication
           <$> wrap (bKind self)
           <*> (qualifiedOperator . report $ super)
           <*> wrap (aKind self),
   cTypeWithWildcards = cTypeWithWildcards super
      <|> Abstract.infixTypeApplication
          <$> wrap (bTypeWithWildcards self)
          <* terminator "'"
          <*> (qualifiedOperator . report $ super)
          <*> wrap (cTypeWithWildcards self),
   familyInstanceDesignator = familyInstanceDesignator super
      <|> Abstract.infixTypeClassInstanceLHS
          <$> wrap (bTypeWithWildcards super)
          <* terminator "'"
          <*> (qualifiedOperator . report $ super)
          <*> wrap (cTypeWithWildcards super)}

polyKindsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                             g ~ ExtendedGrammar l t (NodeWrap t),
                             Ord t, Show t, TextualMonoid t, OutlineMonoid t,
                             Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l))
               => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
polyKindsMixin self super = super{
   kind = kind super
      <|> Abstract.typeKind
          <$> wrap (Abstract.forallType <$ keywordForall self
                    <*> many (typeVarBinder self) <* delimiter "."
                    <*> wrap (Report.context . declarationLevel . report $ self)
                    <* (rightDoubleArrow . report $ self)
                    <*> wrap (forallType self)),
   aKind = groundTypeKind self
      <<|> parens (kind self)
      <<|> Abstract.typeKind <$> wrap (Report.aType . report $ self)}

visibleDependentKindQualificationMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                                     g ~ ExtendedGrammar l t (NodeWrap t),
                                                     Ord t, Show t, TextualMonoid t, OutlineMonoid t,
                                                     Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l))
                                       => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
visibleDependentKindQualificationMixin self super = super{
   kind = kind super
     <|> Abstract.visibleDependentKind
         <$ keywordForall self
         <*> many (typeVarBinder self)
         <* (Report.rightArrow . report $ self)
         <*> wrap (kind self)}

kindSignaturesBaseMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                      g ~ ExtendedGrammar l t (NodeWrap t),
                                      Ord t, Show t, TextualMonoid t, OutlineMonoid t,
                                      Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l))
                        => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
kindSignaturesBaseMixin self super@ExtendedGrammar{report= HaskellGrammar{..}} = super{
   kindSignature = doubleColon *> kind self,
   kind = Abstract.functionKind <$> wrap (bKind self) <* rightArrow <*> wrap (kind self)
          <|> bKind self,
   bKind = Abstract.kindApplication <$> wrap (bKind self) <*> wrap (aKind self)
           <|> aKind self,
   aKind = Abstract.constructorKind <$> wrap generalConstructor
           <|> groundTypeKind self
           <|> Abstract.kindVariable <$> kindVar self
           <|> parens (kind self)}

starIsTypeMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                              g ~ ExtendedGrammar l t (NodeWrap t),
                              Ord t, Show t, TextualMonoid t, OutlineMonoid t,
                              Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l))
                => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
starIsTypeMixin self super = super{
   groundTypeKind = groundTypeKind super <|> Abstract.groundTypeKind <$ delimiter "*"}

unicodeStarIsTypeMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                     g ~ ExtendedGrammar l t (NodeWrap t),
                                     Ord t, Show t, TextualMonoid t, OutlineMonoid t,
                                     Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l))
                       => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
unicodeStarIsTypeMixin self super = super{
   groundTypeKind = groundTypeKind super <|> Abstract.groundTypeKind <$ delimiter "★"}

standaloneKindSignaturesMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                            g ~ ExtendedGrammar l t (NodeWrap t),
                                            Ord t, Show t, TextualMonoid t, OutlineMonoid t,
                                            Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l))
                              => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
standaloneKindSignaturesMixin self super@ExtendedGrammar
                              {report= HaskellGrammar{declarationLevel= DeclarationGrammar{..}}} = super{
   report= (report super){
      declarationLevel= (declarationLevel . report $ super){
         topLevelDeclaration = topLevelDeclaration
            <|> Abstract.kindSignature <$ keyword "type"
                  <*> (typeConstructor . report $ self)
                  <* (doubleColon . report $ self)
                  <*> wrap (Report.optionalContext . declarationLevel . report $ self)
                  <*> wrap (kind self)}}}

kindSignaturesMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                  g ~ ExtendedGrammar l t (NodeWrap t),
                                  Ord t, Show t, TextualMonoid t, OutlineMonoid t,
                                  Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l))
                    => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
kindSignaturesMixin self super@ExtendedGrammar
                    {report= HaskellGrammar{declarationLevel= DeclarationGrammar{..}}} = super{
   report= (report super){
      declarationLevel= (declarationLevel . report $ super){
         topLevelDeclaration = topLevelDeclaration
            <|> Abstract.kindedDataDeclaration <$ keyword "data"
                   <*> wrap optionalContext
                   <*> wrap (Report.simpleType . Report.declarationLevel . report $ self)
                   <*> wrap (kindSignature self)
                   <*> (delimiter "=" *> (Report.declaredConstructors . Report.declarationLevel . report $ self)
                        <|> pure [])
                   <*> (Report.derivingClause . declarationLevel . report $ super)
            <|> Abstract.kindedNewtypeDeclaration <$ keyword "newtype"
                   <*> wrap optionalContext
                   <*> wrap (Report.simpleType . Report.declarationLevel . report $ self)
                   <*> wrap (kindSignature self)
                   <* delimiter "="
                   <*> wrap (Report.newConstructor . Report.declarationLevel . report $ self)
                   <*> (Report.derivingClause . declarationLevel . report $ super)
            <|> Abstract.classDeclaration
                   <$ keyword "class"
                   <*> wrap optionalContext
                   <*> wrap (Abstract.simpleTypeLHSApplication
                                <$> wrap (Abstract.simpleTypeLHS <$> typeClass <*> pure [])
                                <*> parens (Abstract.explicitlyKindedTypeVariable
                                            <$> (Report.typeVar . report $ self)
                                            <*> wrap (kindSignature self)))
                   <*> (keyword "where"
                        *> blockOf (Report.inClassDeclaration . Report.declarationLevel . report $ self)
                        <|> pure []),
         instanceTypeDesignator = instanceTypeDesignator
            <|> Abstract.kindedType
                <$> wrap (Report.instanceTypeDesignator . declarationLevel . report $ self)
                <*> wrap (kindSignature self),
         typeVarTuple = (:|) <$> wrap (optionallyKindedTypeVar self)
                             <*> some (comma *> wrap (optionallyKindedTypeVar self))},
      typeTerm = (typeTerm . report $ super) <|>
         Abstract.kindedType <$> wrap (Report.typeTerm . report $ self) <*> wrap (kindSignature self)},
   optionallyKindedAndParenthesizedTypeVar =
      Abstract.typeVariable <$> optionallyParenthesizedTypeVar self
      <|> parens (Abstract.kindedType
                  <$> wrap (Abstract.typeVariable <$> optionallyParenthesizedTypeVar self)
                  <*> wrap (kindSignature self)),
   optionallyKindedTypeVar =
      Abstract.typeVariable <$> (Report.typeVar . report $ self)
      <|> Abstract.kindedType
          <$> wrap (Abstract.typeVariable <$> (Report.typeVar . report $ self))
          <*> wrap (kindSignature self),
   typeVarBinder = typeVarBinder super
                   <|> parens (Abstract.explicitlyKindedTypeVariable
                               <$> (Report.typeVar . report $ self)
                               <*> wrap (kindSignature self))}

existentialQuantificationMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                             g ~ ExtendedGrammar l t (NodeWrap t),
                                             Ord t, Show t, TextualMonoid t)
                               => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
existentialQuantificationMixin self super = super{
   report= (report super){
      declarationLevel= (declarationLevel . report $ super){
         declaredConstructor = (declaredConstructor . declarationLevel . report $ super)
            <|> Abstract.existentialConstructor
                <$ keywordForall self
                <*> many (typeVarBinder self) <* delimiter "."
                <*> wrap (Report.optionalContext . declarationLevel . report $ self)
                <*> wrap (declaredConstructor . declarationLevel . report $ super)
            <|> Abstract.existentialConstructor []
                <$> wrap (context . declarationLevel . report $ self)
                <* (rightDoubleArrow . report $ self)
                <*> wrap (declaredConstructor . declarationLevel . report $ super)}}}

explicitForAllMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                  g ~ ExtendedGrammar l t (NodeWrap t),
                                  Ord t, Show t, OutlineMonoid t, TextualMonoid t,
                                  Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l))
                    => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
explicitForAllMixin self super@ExtendedGrammar
                    {report= HaskellGrammar{declarationLevel= DeclarationGrammar{..}}} = super{
   report= (report super){
             declarationLevel= (declarationLevel . report $ super){
               topLevelDeclaration = topLevelDeclaration
                  <|> Abstract.explicitlyScopedInstanceDeclaration <$ keyword "instance"
                      <* keywordForall self
                      <*> many (typeVarBinder self)
                      <* delimiter "."
                      <*> wrap optionalContext
                      <*> wrap instanceDesignator
                      <*> (keyword "where"
                           *> blockOf (Report.inInstanceDeclaration . declarationLevel . report $ self)
                           <|> pure [])},
      aType = (aType . report $ super)
         <|> parens (Abstract.forallType []
                     <$> wrap (Report.context . declarationLevel . report $ self)
                     <* (Report.rightDoubleArrow . report $ self)
                     <*> wrap (Report.typeTerm . report $ self)),
      typeVar = notFollowedBy (keywordForall self) *> (typeVar . report $ super)},
   forallType = forallType super
      <|> Abstract.forallType <$ keywordForall self
          <*> many (typeVarBinder self) <* delimiter "."
          <*> wrap optionalContext
          <*> wrap (forallType self),
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
gadtSyntaxMixin self super@ExtendedGrammar
                {report= HaskellGrammar{declarationLevel= DeclarationGrammar{..}}} = super{
   report= (report super){
             declarationLevel= (declarationLevel . report $ super){
               topLevelDeclaration = topLevelDeclaration
                  <|> Abstract.gadtDeclaration <$ keyword "data"
                      <*> wrap simpleType <*> optional (wrap $ kindSignature self) <* keyword "where"
                      <*> blockOf (gadtConstructors self)
                      <*> derivingClause
                  <|> Abstract.gadtNewtypeDeclaration <$ keyword "newtype"
                      <*> wrap simpleType <*> optional (wrap $ kindSignature self) <* keyword "where"
                      <*> wrap (gadtNewConstructor self)
                      <*> derivingClause}},
   optionalForall = keywordForall self *> many (typeVarBinder self) <* delimiter "." <|> pure []}

gadtSyntaxTypeOperatorsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                           g ~ ExtendedGrammar l t (NodeWrap t),
                                           Ord t, Show t, OutlineMonoid t, TextualMonoid t,
                                           Deep.Foldable (Serialization (Down Int) t) (Abstract.GADTConstructor l l))
                => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
gadtSyntaxTypeOperatorsMixin self super@ExtendedGrammar{report= HaskellGrammar{..}} = super{
   return_type = return_type super <|>
      Abstract.infixTypeApplication <$> wrap (arg_type self)
                                    <*> qualifiedOperator
                                    <*> wrap (arg_type self)}

dataKindsGadtSyntaxTypeOperatorsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                           g ~ ExtendedGrammar l t (NodeWrap t),
                                           Ord t, Show t, OutlineMonoid t, TextualMonoid t,
                                           Deep.Foldable (Serialization (Down Int) t) (Abstract.GADTConstructor l l))
                => GrammarOverlay g (ParserT ((,) [[Lexeme t]]) g t)
dataKindsGadtSyntaxTypeOperatorsMixin self super@ExtendedGrammar{report= HaskellGrammar{..}} =
   super{
      return_type = return_type super <|>
         Abstract.promotedInfixTypeApplication
         <$> wrap (arg_type self)
         <* terminator "'"
         <*> qualifiedOperator
         <*> wrap (arg_type self)}

variableLexeme, constructorLexeme, identifierTail :: (Ord t, Show t, TextualMonoid t) => Parser g t t
variableLexeme = filter (`Set.notMember` Report.reservedWords) (satisfyCharInput varStart <> identifierTail)
                 <?> "variable"
   where varStart c = (Char.isLetter c && not (Char.isUpper c)) ||  c == '_'
constructorLexeme = satisfyCharInput Char.isUpper <> identifierTail <?> "constructor"
identifierTail = takeCharsWhile isNameTailChar

isNameTailChar :: Char -> Bool
isNameTailChar c = Report.isNameTailChar c || Char.isMark c

blockOf' :: (Ord t, Show t, OutlineMonoid t, LexicalParsing (Parser g t),
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
