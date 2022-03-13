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
import Data.Foldable (toList)
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
import qualified Text.Grampa.ContextFree.SortedMemoizing as P (Parser)
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
                                 oneExtendedLine, rewrap, startSepEndBy, wrap, unwrap)
import Language.Haskell.Reserializer (Lexeme(..), Serialization, TokenType(..))

import Prelude hiding (exponent, filter, null)

data ExtendedGrammar l t f p = ExtendedGrammar {
   report :: HaskellGrammar l t f p,
   keywordForall :: p (),
   kindSignature, kind, bKind, aKind :: p (Abstract.Kind l l f f),
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

extensionMixins :: forall l t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser (ExtendedGrammar l t (NodeWrap t)) t),
                            Ord t, Show t, OutlineMonoid t,
                            Abstract.DeeplyFoldable (Serialization (Down Int) t) l)
                => Map (Set Extension)
                       (Int,
                        GrammarBuilder (ExtendedGrammar l t (NodeWrap t))
                                       (ExtendedGrammar l t (NodeWrap t))
                                       (ParserT ((,) [[Lexeme t]]))
                                       t)
extensionMixins =
  Map.fromList [
     (Set.fromList [IdentifierSyntax],               (0, identifierSyntaxMixin)),
     (Set.fromList [PackageImports],                 (0, packageImportsMixin)),
     (Set.fromList [SafeImports],                    (0, safeImportsMixin)),
     (Set.fromList [ImportQualifiedPost],            (0, importQualifiedPostMixin)),
     (Set.fromList [ExplicitNamespaces],             (0, explicitNamespacesMixin)),
     (Set.fromList [UnicodeSyntax],                  (1, unicodeSyntaxMixin)),
     (Set.fromList [BinaryLiterals],                 (1, binaryLiteralsMixin)),
     (Set.fromList [HexFloatLiterals],               (1, hexFloatLiteralsMixin)),
     (Set.fromList [NumericUnderscores],             (1, numericUnderscoresMixin)),
     (Set.fromList [BinaryLiterals,
                    NumericUnderscores],             (9, binaryUnderscoresMixin)),
     (Set.fromList [PackageImports,
                    SafeImports],                    (9, safePackageImportsMixin)),
     (Set.fromList [PackageImports,
                    ImportQualifiedPost],            (9, packageImportsQualifiedPostMixin)),
     (Set.fromList [SafeImports,
                    ImportQualifiedPost],            (9, safeImportsQualifiedPostMixin)),
     (Set.fromList [PackageImports,
                    SafeImports,
                    ImportQualifiedPost],            (9, safePackageImportsQualifiedPostMixin)),
     (Set.fromList [NegativeLiterals],               (2, negativeLiteralsMixin)),
     (Set.fromList [LexicalNegation],                (3, lexicalNegationMixin)),
     (Set.fromList [MagicHash],                      (3, magicHashMixin)),
     (Set.fromList [ParallelListComprehensions],     (3, parallelListComprehensionsMixin)),
     (Set.fromList [OverloadedLabels],               (4, overloadedLabelsMixin)),
     (Set.fromList [RecursiveDo],                    (4, recursiveDoMixin)),
     (Set.fromList [TupleSections],                  (5, tupleSectionsMixin)),
     (Set.fromList [EmptyCase],                      (6, emptyCaseMixin)),
     (Set.fromList [LambdaCase],                     (7, lambdaCaseMixin)),
     (Set.fromList [GratuitouslyParenthesizedTypes], (7, gratuitouslyParenthesizedTypesMixin)),
     (Set.fromList [MultiWayIf],                     (8, multiWayIfMixin)),
     (Set.fromList [KindSignatures],                 (8, kindSignaturesMixin)),
     (Set.fromList [TypeOperators],                  (8, typeOperatorsMixin)),
     (Set.fromList [EqualityConstraints],            (8, equalityConstraintsMixin)),
     (Set.fromList [BlockArguments],                 (9, blockArgumentsMixin)),
     (Set.fromList [ExistentialQuantification],      (9, existentialQuantificationMixin)),
     (Set.fromList [ExplicitForAll],                 (9, explicitForAllMixin)),
     (Set.fromList [GADTSyntax],                     (9, gadtSyntaxMixin)),
     (Set.fromList [FlexibleInstances],              (9, flexibleInstancesMixin)),
     (Set.fromList [TypeFamilies],                   (9, typeFamiliesMixin)),
     (Set.fromList [TypeFamilyDependencies],         (9, typeFamilyDependenciesMixin)),
     (Set.fromList [DataKinds],                      (9, dataKindsMixin)),
     (Set.fromList [MultiParameterConstraints],      (9, multiParameterConstraintsMixin)),
     (Set.fromList [GADTSyntax, TypeOperators],      (9, gadtSyntaxTypeOperatorsMixin)),
     (Set.fromList [DataKinds, TypeOperators],       (9, dataKindsTypeOperatorsMixin)),
     (Set.fromList [MultiParameterConstraints,
                    TypeOperators],                  (9, multiParameterConstraintsTypeOperatorsMixin)),
     (Set.fromList [DataKinds, TypeOperators,
                    GADTSyntax],                     (9, dataKindsGadtSyntaxTypeOperatorsMixin))]

languagePragmas :: (Ord t, Show t, TextualMonoid t) => P.Parser g t [ExtensionSwitch]
languagePragmas = spaceChars
                 *> admit (string "{-#" *> spaceChars *> filter isLanguagePragma (takeCharsWhile Char.isAlphaNum)
                           *> commit (spaceChars
                                      *> liftA2 (<>)
                                            (extension `sepBy` (string "," *> spaceChars) <* string "#-}")
                                            languagePragmas)
                           <<|> comment *> commit languagePragmas
                           <<|> commit (pure mempty))
   where spaceChars = takeCharsWhile Char.isSpace
         isLanguagePragma pragmaName = Text.toUpper (Textual.toText mempty pragmaName) == "LANGUAGE"
         extension = do extensionName <- takeCharsWhile Char.isAlphaNum
                        void spaceChars
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
   where moduleExtensions = getCompose . fmap snd . getCompose $ simply parsePrefix languagePragmas source
         parseResults = getCompose . fmap snd . getCompose

extendedGrammar :: (Abstract.ExtendedHaskell l, LexicalParsing (Parser (ExtendedGrammar l t (NodeWrap t)) t),
                    Ord t, Show t, OutlineMonoid t,
                    Abstract.DeeplyFoldable (Serialization (Down Int) t) l)
                 => Map Extension Bool -> Grammar (ExtendedGrammar l t (NodeWrap t)) (ParserT ((,) [[Lexeme t]])) t
extendedGrammar extensions = fixGrammar (extended . reportGrammar)
   where extended = appEndo $ getDual $ foldMap (Dual . Endo) $ map snd $ sortOn fst
                    $ Map.elems $ Map.restrictKeys extensionMixins
                    $ Set.powerSet (Map.keysSet $ Map.filter id extensions)

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
               declarationLevel= baseDeclarations{
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
                                    <$> wrap (nonTerminal optionallyKindedAndParenthesizedTypeVar) <* rightArrow
                                    <*> wrap (nonTerminal optionallyKindedAndParenthesizedTypeVar))}},
     keywordForall = keyword "forall",
     kindSignature = empty,
     kindVar = variableIdentifier,
     kind = empty,
     bKind = empty,
     aKind = empty,
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
        <|> Abstract.typeVariable <$> typeVar
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
        Abstract.defaultMember <$> cname
        <|> Abstract.typeMember <$ keyword "type" <*> cname
        <|> Abstract.patternMember <$ keyword "pattern" <*> cname,
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
     typeVarBinder = Abstract.implicitlyKindedTypeVariable <$> typeVar,
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
   where r'@HaskellGrammar{moduleLevel= ModuleLevelGrammar{..},
                           declarationLevel= baseDeclarations@DeclarationGrammar{..},
                           ..}
                          = Report.grammar r
identifierSyntaxMixin :: forall l g t. (Abstract.Haskell l, LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t)
                      => GrammarBuilder (ExtendedGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
identifierSyntaxMixin baseGrammar = baseGrammar{
   report= (report baseGrammar){
      variableIdentifier = token (Abstract.name . Text.pack . toString mempty <$> variableLexeme),
      constructorIdentifier = token (Abstract.name . Text.pack . toString mempty <$> constructorLexeme),
      variableSymbol = token (Abstract.name . Text.pack . toString mempty <$> Report.variableSymbolLexeme),
      constructorSymbol = token (Abstract.name . Text.pack . toString mempty <$> Report.constructorSymbolLexeme)}}

overloadedLabelsMixin :: forall l g t. (Abstract.Haskell l, LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t)
                      => GrammarBuilder (ExtendedGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
overloadedLabelsMixin baseGrammar@ExtendedGrammar{report= baseReport@HaskellGrammar{..}} = baseGrammar{
   report= baseReport{
      variableIdentifier = token (Abstract.name . Text.pack . toString mempty <$> (string "#" <> variableLexeme))
                           <|> variableIdentifier,
      variableSymbol = notFollowedBy (string "#" *> variableLexeme) *> variableSymbol}}

unicodeSyntaxMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                 Ord t, Show t, OutlineMonoid t)
                   => GrammarBuilder (ExtendedGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
unicodeSyntaxMixin baseGrammar@ExtendedGrammar{report= baseReport@HaskellGrammar{..}, ..} = baseGrammar{
   keywordForall = keywordForall <|> delimiter "∀",
   aKind = aKind <|> Abstract.groundTypeKind <$ delimiter "★",
   report= baseReport{
      doubleColon = doubleColon <|> delimiter "∷",
      rightDoubleArrow = rightDoubleArrow <|> delimiter "⇒",
      rightArrow = rightArrow <|> delimiter "→",
      leftArrow = leftArrow <|> delimiter "←",
      variableSymbol = notSatisfyChar (`elem` ("∀←→⇒∷★" :: [Char])) *> variableSymbol}}

magicHashMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t)
               => GrammarBuilder (ExtendedGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
magicHashMixin baseGrammar@ExtendedGrammar{report= baseReport@HaskellGrammar{..}} =
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
  in baseGrammar{report= baseReport{
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
                               Abstract.DeeplyFoldable (Serialization (Down Int) t) l)
                 => GrammarBuilder (ExtendedGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
recursiveDoMixin baseGrammar@ExtendedGrammar{report= baseReport@HaskellGrammar{..}} = baseGrammar{
   report= baseReport{
      closedBlockExpresion = closedBlockExpresion <|> Abstract.mdoExpression <$ keyword "mdo" <*> wrap statements,
      statement = statement
                  <|> Deep.InL
                      <$> wrap (Abstract.recursiveStatement
                                . (either id (rewrap Abstract.expressionStatement) . Deep.eitherFromSum . unwrap <$>)
                                <$ keyword "rec"
                                <*> blockOf statement),
      variableIdentifier = notFollowedBy (keyword "mdo" <|> keyword "rec") *> variableIdentifier}}

parallelListComprehensionsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                              Ord t, Show t, OutlineMonoid t)
                                => GrammarBuilder (ExtendedGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
parallelListComprehensionsMixin baseGrammar@ExtendedGrammar{report= baseReport@HaskellGrammar{..}} = baseGrammar{
   report= baseReport{
      bareExpression = bareExpression
                       <|> brackets (Abstract.parallelListComprehension
                                     <$> expression <*> qualifiers <*> qualifiers <*> many qualifiers)}}

tupleSectionsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                 Ord t, Show t, OutlineMonoid t)
                   => GrammarBuilder (ExtendedGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
tupleSectionsMixin baseGrammar@ExtendedGrammar{report= baseReport@HaskellGrammar{..}} = baseGrammar{
   report= baseReport{
      bareExpression = bareExpression
                       <|> Abstract.tupleSectionExpression
                              <$> parens (filter (any isJust)
                                          $ (:|) <$> optional expression <*> some (comma *> optional expression))}}

lambdaCaseMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t), Ord t, Show t, TextualMonoid t)
                => GrammarBuilder (ExtendedGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
lambdaCaseMixin baseGrammar@ExtendedGrammar{report= baseReport@HaskellGrammar{..}} = baseGrammar{
   report= baseReport{
      closedBlockExpresion = closedBlockExpresion
                             <|> Abstract.lambdaCaseExpression <$ (delimiter "\\" *> keyword "case")
                                                               <*> alternatives}}

emptyCaseMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t,
                             Deep.Foldable (Serialization (Down Int) t) (Abstract.CaseAlternative l l))
                 => GrammarBuilder (ExtendedGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
emptyCaseMixin baseGrammar = baseGrammar{
   report= (report baseGrammar){
      alternatives = blockOf (alternative $ report baseGrammar)}}

multiWayIfMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t,
                             Deep.Foldable (Serialization (Down Int) t) (Abstract.GuardedExpression l l))
                => GrammarBuilder (ExtendedGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
multiWayIfMixin baseGrammar@ExtendedGrammar{report= baseReport@HaskellGrammar{..}} = baseGrammar{
   report= baseReport{
      closedBlockExpresion = closedBlockExpresion
                             <|> Abstract.multiWayIfExpression <$ keyword "if"
                                     <*> blockOf'
                                            (Abstract.guardedExpression . toList
                                                <$> guards <* rightArrow
                                                <*> expression)}}

packageImportsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t), Ord t, Show t,
                                  OutlineMonoid t)
                      => GrammarBuilder (ExtendedGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
packageImportsMixin baseGrammar@ExtendedGrammar
                    {report= baseReport@HaskellGrammar
                                       {moduleLevel= baseModuleLevel@ModuleLevelGrammar{..}, ..}} = baseGrammar{
   report= baseReport{
      moduleLevel= baseModuleLevel{
         importDeclaration = importDeclaration
                             <|> Abstract.packageQualifiedImportDeclaration <$ keyword "import"
                                 <*> (True <$ keyword "qualified" <|> pure False)
                                 <*> stringLiteral
                                 <*> Report.moduleId
                                 <*> optional (keyword "as" *> Report.moduleId)
                                 <*> optional (wrap importSpecification)}}}

safeImportsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t)
                 => GrammarBuilder (ExtendedGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
safeImportsMixin baseGrammar@ExtendedGrammar
                 {report= baseReport@HaskellGrammar
                                    {moduleLevel= baseModule@ModuleLevelGrammar{..}, ..}} = baseGrammar{
   report= baseReport{
      moduleLevel= baseModule{
         importDeclaration = importDeclaration
                             <|> Abstract.safeImportDeclaration <$ keyword "import" <* keyword "safe"
                                 <*> (True <$ keyword "qualified" <|> pure False)
                                 <*> Report.moduleId
                                 <*> optional (keyword "as" *> Report.moduleId)
                                 <*> optional (wrap importSpecification)}}}

importQualifiedPostMixin :: forall l g t. (Abstract.Haskell l, LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t)
                         => GrammarBuilder (ExtendedGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
importQualifiedPostMixin baseGrammar@ExtendedGrammar
                         {report= baseReport@HaskellGrammar
                                            {moduleLevel= baseModule@ModuleLevelGrammar{..}, ..}} = baseGrammar{
   report= baseReport{
      moduleLevel= baseModule{
         importDeclaration = importDeclaration
                             <|> flip Abstract.importDeclaration <$ keyword "import"
                                 <*> Report.moduleId
                                 <*> (True <$ keyword "qualified" <|> pure False)
                                 <*> optional (keyword "as" *> Report.moduleId)
                                 <*> optional (wrap importSpecification)}}}

safePackageImportsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t), Ord t, Show t,
                                          OutlineMonoid t)
                        => GrammarBuilder (ExtendedGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
safePackageImportsMixin baseGrammar@ExtendedGrammar
                        {report= baseReport@HaskellGrammar
                                           {moduleLevel= baseModule@ModuleLevelGrammar{..}, ..}} = baseGrammar{
   report= baseReport{
      moduleLevel= baseModule{
         importDeclaration = importDeclaration
                             <|> Abstract.safePackageQualifiedImportDeclaration <$ keyword "import" <* keyword "safe"
                                 <*> (True <$ keyword "qualified" <|> pure False)
                                 <*> stringLiteral
                                 <*> Report.moduleId
                                 <*> optional (keyword "as" *> Report.moduleId)
                                 <*> optional (wrap importSpecification)}}}

packageImportsQualifiedPostMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                                   Ord t, Show t, OutlineMonoid t)
                                 => GrammarBuilder (ExtendedGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
packageImportsQualifiedPostMixin baseGrammar@ExtendedGrammar
                                 {report= baseReport@HaskellGrammar
                                          {moduleLevel= baseModule@ModuleLevelGrammar{..}, ..}} = baseGrammar{
   report= baseReport{
      moduleLevel= baseModule{
         importDeclaration = importDeclaration
                             <|> Abstract.packageQualifiedImportDeclaration <$ keyword "import"
                                 <**> pure flip
                                 <*> stringLiteral
                                 <**> pure flip
                                 <*> Report.moduleId
                                 <*> (True <$ keyword "qualified" <|> pure False)
                                 <*> optional (keyword "as" *> Report.moduleId)
                                 <*> optional (wrap importSpecification)}}}

safeImportsQualifiedPostMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                                Ord t, Show t, OutlineMonoid t)
                              => GrammarBuilder (ExtendedGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
safeImportsQualifiedPostMixin baseGrammar@ExtendedGrammar
                              {report= baseReport@HaskellGrammar
                                                 {moduleLevel= baseModule@ModuleLevelGrammar{..}, ..}} = baseGrammar{
   report= baseReport{
      moduleLevel= baseModule{
         importDeclaration = importDeclaration
                             <|> flip Abstract.safeImportDeclaration <$ keyword "import" <* keyword "safe"
                                 <*> Report.moduleId
                                 <*> (True <$ keyword "qualified" <|> pure False)
                                 <*> optional (keyword "as" *> Report.moduleId)
                                 <*> optional (wrap importSpecification)}}}

safePackageImportsQualifiedPostMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                                       Ord t, Show t, OutlineMonoid t)
                                     => GrammarBuilder (ExtendedGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
safePackageImportsQualifiedPostMixin baseGrammar@ExtendedGrammar
                                     {report= baseReport@HaskellGrammar
                                              {moduleLevel= baseModule@ModuleLevelGrammar{..}, ..}} = baseGrammar{
   report= baseReport{
      moduleLevel= baseModule{
         importDeclaration = importDeclaration
                             <|> Abstract.safePackageQualifiedImportDeclaration <$ keyword "import" <* keyword "safe"
                                 <**> pure flip
                                 <*> stringLiteral
                                 <**> pure flip
                                 <*> Report.moduleId
                                 <*> (True <$ keyword "qualified" <|> pure False)
                                 <*> optional (keyword "as" *> Report.moduleId)
                                 <*> optional (wrap importSpecification)}}}

explicitNamespacesMixin :: forall l g t. (g ~ ExtendedGrammar l t (NodeWrap t), Abstract.ExtendedHaskell l,
                                      LexicalParsing (Parser g t), Ord t, Show t,
                                      OutlineMonoid t)
                        => GrammarBuilder g g (ParserT ((,) [[Lexeme t]])) t
explicitNamespacesMixin baseGrammar@ExtendedGrammar
                        {report= baseReport@HaskellGrammar
                                           {moduleLevel= baseModule@ModuleLevelGrammar{..}, ..}} = baseGrammar{
   report= baseReport{
      moduleLevel= baseModule{
         export = export
            <|> keyword "type" *> (Abstract.exportClassOrType <$> parens qualifiedVariableSymbol <*> pure Nothing),
         importItem = importItem
            <|> keyword "type" *> (Abstract.importClassOrType <$> parens variableSymbol <*> pure Nothing),
         members = parens (Abstract.allMembers <$ delimiter ".."
                           <|> Abstract.explicitlyNamespacedMemberList
                               <$> (nonTerminal namespacedMember `sepBy` comma) <* optional comma)}}}
blockArgumentsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                  Ord t, Show t, OutlineMonoid t,
                                  Deep.Foldable (Serialization (Down Int) t) (Abstract.GuardedExpression l l))
                    => GrammarBuilder (ExtendedGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
blockArgumentsMixin baseGrammar@ExtendedGrammar{report= baseReport@HaskellGrammar{..}} = baseGrammar{
   report= baseReport{
      lExpression = lExpression <|> wrap (Abstract.applyExpression <$> fExpression <*> wrap openBlockExpression),
      dExpression = fExpression,
      bareExpression = bareExpression <|> closedBlockExpresion}}

lexicalNegationMixin :: forall l t. (Abstract.Haskell l, LexicalParsing (Parser (ExtendedGrammar l t (NodeWrap t)) t),
                                 Ord t, Show t, TextualMonoid t)
                     => GrammarBuilder (ExtendedGrammar l t (NodeWrap t))
                                       (ExtendedGrammar l t (NodeWrap t))
                                       (ParserT ((,) [[Lexeme t]]))
                                       t
lexicalNegationMixin baseGrammar@ExtendedGrammar{report= baseReport@HaskellGrammar{..}} = baseGrammar{
   report= baseReport{
      qualifiedVariableSymbol = notFollowedBy (string "-"
                                               *> satisfyCharInput (\c-> Char.isAlphaNum c || c == '(' || c == '['))
                                *> token (Report.nameQualifier <*> variableSymbol),
      infixExpression = wrap (Abstract.infixExpression
                                 <$> nonTerminal (Report.dExpression . report)
                                 <*> wrap (Abstract.referenceExpression <$> qualifiedOperator)
                                 <*> nonTerminal (Report.infixExpression . report))
                        <|> lExpression,
      leftInfixExpression =
         wrap (Abstract.infixExpression
                  <$> nonTerminal (Report.dExpression . report)
                  <*> wrap (Abstract.referenceExpression <$> qualifiedOperator)
                  <*> nonTerminal (Report.leftInfixExpression . report))
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

negativeLiteralsMixin :: forall l t. (Abstract.Haskell l, LexicalParsing (Parser (ExtendedGrammar l t (NodeWrap t)) t),
                                  Ord t, Show t, TextualMonoid t)
                      => GrammarBuilder (ExtendedGrammar l t (NodeWrap t))
                                        (ExtendedGrammar l t (NodeWrap t))
                                        (ParserT ((,) [[Lexeme t]]))
                                        t
negativeLiteralsMixin baseGrammar@ExtendedGrammar{report= baseReport@HaskellGrammar{..}} = baseGrammar{
   report= baseReport{
      qualifiedVariableSymbol = notFollowedBy (string "-" *> satisfyCharInput Char.isDigit) *> qualifiedVariableSymbol,
      infixExpression =
         wrap (Abstract.infixExpression
                  <$> nonTerminal (Report.dExpression . report)
                  <*> wrap (Abstract.referenceExpression <$> qualifiedOperator)
                  <*> nonTerminal (Report.infixExpression . report)
               <|> Abstract.applyExpression <$> wrap (Abstract.negate <$ prefixMinus) <*> infixExpression)
         <|> lExpression,
      leftInfixExpression =
         wrap (Abstract.infixExpression
                  <$> nonTerminal (Report.dExpression . report)
                  <*> wrap (Abstract.referenceExpression <$> qualifiedOperator)
                  <*> nonTerminal (Report.leftInfixExpression . report)
                  <|> Abstract.applyExpression <$> wrap (Abstract.negate <$ prefixMinus) <*> leftInfixExpression)
         <|> dExpression,
      integerLexeme = (negate <$ string "-" <|> pure id) <*> integerLexeme,
      floatLexeme = (negate <$ string "-" <|> pure id) <*> floatLexeme}}
   where prefixMinus = void (token $ string "-" <* notSatisfyChar Char.isDigit) <?> "prefix -"

binaryLiteralsMixin :: forall l g t. (Abstract.Haskell l, LexicalParsing (Parser g t), Ord t, Show t, TextualMonoid t)
                      => GrammarBuilder (ExtendedGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
binaryLiteralsMixin baseGrammar@ExtendedGrammar{report= baseReport@HaskellGrammar{..}} = baseGrammar{
   report= baseReport{
      integerLexeme =
         (string "0b" <|> string "0B")
         *> (foldl' binary 0 . toString mempty <$> takeCharsWhile1 (\c-> c == '0' || c == '1') <?> "binary number")
         <<|> integerLexeme}}
   where binary n '0' = 2*n
         binary n '1' = 2*n + 1
         binary _ _ = error "non-binary"

hexFloatLiteralsMixin :: forall l g t. (Abstract.Haskell l, LexicalParsing (Parser g t), Ord t, Show t, TextualMonoid t)
                      => GrammarBuilder (ExtendedGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
hexFloatLiteralsMixin baseGrammar@ExtendedGrammar{report= baseReport@HaskellGrammar{..}} = baseGrammar{
   report= baseReport{
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

numericUnderscoresMixin :: forall l g t. (Abstract.Haskell l, LexicalParsing (Parser g t), Ord t, Show t, TextualMonoid t)
                        => GrammarBuilder (ExtendedGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
numericUnderscoresMixin baseGrammar@ExtendedGrammar{report= baseReport@HaskellGrammar{..}} = baseGrammar{
   report= baseReport{
      decimal = takeCharsWhile1 Char.isDigit <> concatAll (char '_' *> takeCharsWhile1 Char.isDigit)
                <?> "decimal number",
      octal = takeCharsWhile1 Char.isOctDigit <> concatAll (char '_' *> takeCharsWhile1 Char.isOctDigit)
              <?> "octal number",
      hexadecimal = takeCharsWhile1 Char.isHexDigit <> concatAll (char '_' *> takeCharsWhile1 Char.isHexDigit)
                    <?> "hexadecimal number"}}

binaryUnderscoresMixin :: forall l g t. (Abstract.Haskell l, LexicalParsing (Parser g t), Ord t, Show t, TextualMonoid t)
                        => GrammarBuilder (ExtendedGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
binaryUnderscoresMixin baseGrammar@ExtendedGrammar{report= baseReport@HaskellGrammar{..}} = baseGrammar{
   report= baseReport{
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
                   => GrammarBuilder g g (ParserT ((,) [[Lexeme t]])) t
typeOperatorsMixin baseGrammar@ExtendedGrammar
                   {report= baseReport@HaskellGrammar
                                      {declarationLevel= baseDeclarations@DeclarationGrammar{..}, ..}} = baseGrammar{
   report= baseReport{
             declarationLevel= baseDeclarations{
               simpleType = simpleType
                  <|> Abstract.simpleInfixTypeLHSApplication
                               <$> nonTerminal typeVarBinder
                               <*> anySymbol
                               <*> nonTerminal typeVarBinder
                  <|> parens (nonTerminal (Report.simpleType . Report.declarationLevel . report)),
               instanceTypeDesignator = instanceTypeDesignator
                  <|> parens (Abstract.infixTypeApplication
                              <$> wrap (nonTerminal optionallyKindedAndParenthesizedTypeVar)
                              <*> qualifiedOperator
                              <*> wrap (nonTerminal optionallyKindedAndParenthesizedTypeVar)),
               qualifiedTypeClass = qualifiedConstructor <|> parens qualifiedVariableSymbol},
             typeConstructor = constructorIdentifier <|> parens anySymbol,
             generalTypeConstructor = generalTypeConstructor
               <|> Abstract.constructorType <$> wrap (Abstract.constructorReference <$> parens qualifiedVariableSymbol),
             bType = bType
                <|> Abstract.infixTypeApplication <$> wrap (nonTerminal (Report.bType . report))
                                                  <*> qualifiedOperator
                                                  <*> wrap (nonTerminal (Report.aType . report))},
     cTypeWithWildcards = cTypeWithWildcards baseGrammar
        <|> Abstract.infixTypeApplication <$> wrap (nonTerminal bTypeWithWildcards)
                                          <*> qualifiedOperator
                                          <*> wrap (nonTerminal cTypeWithWildcards),
     familyInstanceDesignator = familyInstanceDesignator baseGrammar
        <|> Abstract.infixTypeClassInstanceLHS
               <$> wrap (bTypeWithWildcards baseGrammar)
               <*> qualifiedOperator
               <*> wrap (cTypeWithWildcards baseGrammar)}
   where anySymbol = constructorSymbol <|> variableSymbol

equalityConstraintsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                       g ~ ExtendedGrammar l t (NodeWrap t), Ord t, Show t, TextualMonoid t)
                       => GrammarBuilder g g (ParserT ((,) [[Lexeme t]])) t
equalityConstraintsMixin baseGrammar@ExtendedGrammar
                         {report= baseReport@HaskellGrammar
                                  {declarationLevel= baseDeclarations@DeclarationGrammar{..}, ..}} = baseGrammar{
   report= baseReport{
      declarationLevel= baseDeclarations{
         constraint = constraint <|> equalityConstraint},
      typeTerm = typeTerm <|> Abstract.constraintType <$> wrap equalityConstraint}}
   where equalityConstraint =
            Abstract.typeEqualityConstraint <$> wrap (nonTerminal (Report.bType . report))
            <* delimiter "~" <*> wrap (nonTerminal (Report.bType . report))

multiParameterConstraintsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                             g ~ ExtendedGrammar l t (NodeWrap t), Ord t, Show t, TextualMonoid t)
                               => GrammarBuilder g g (ParserT ((,) [[Lexeme t]])) t
multiParameterConstraintsMixin baseGrammar@ExtendedGrammar
                               {report= baseReport@HaskellGrammar
                                        {declarationLevel= baseDeclarations@DeclarationGrammar{..}, ..}} = baseGrammar{
   report= baseReport{
      declarationLevel= baseDeclarations{
         constraint= constraint
            <|> Abstract.multiParameterClassConstraint <$> qualifiedTypeClass
                <*> filter
                       ((1 /=) . length)
                       (many $ wrap $
                        nonTerminal optionallyKindedAndParenthesizedTypeVar
                        <|> parens (nonTerminal $ Report.typeApplications . declarationLevel . report))}}}

multiParameterConstraintsTypeOperatorsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                                          g ~ ExtendedGrammar l t (NodeWrap t),
                                                          Ord t, Show t, TextualMonoid t)
                                            => GrammarBuilder g g (ParserT ((,) [[Lexeme t]])) t
multiParameterConstraintsTypeOperatorsMixin
   baseGrammar@ExtendedGrammar {report= baseReport@HaskellGrammar
                                        {declarationLevel= baseDeclarations@DeclarationGrammar{..}, ..}} = baseGrammar{
   report= baseReport{
      declarationLevel= baseDeclarations{
         constraint = constraint
            <|> Abstract.infixConstraint
                <$> wrap (nonTerminal (Report.bType . report))
                <*> qualifiedOperator
                <*> wrap (nonTerminal (Report.bType . report))}}}

gratuitouslyParenthesizedTypesMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                                  g ~ ExtendedGrammar l t (NodeWrap t),
                                                  OutlineMonoid t, Ord t, Show t, TextualMonoid t,
                                                  Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l),
                                                  Deep.Foldable (Serialization (Down Int) t)
                                                                (Abstract.GADTConstructor l l))
                            => GrammarBuilder g g (ParserT ((,) [[Lexeme t]])) t
gratuitouslyParenthesizedTypesMixin
   baseGrammar@ExtendedGrammar
               {report= baseReport@HaskellGrammar
                                   {declarationLevel= baseDeclarations@DeclarationGrammar{..}, ..}} = baseGrammar{
   report= baseReport{
      declarationLevel = baseDeclarations{
         topLevelDeclaration = topLevelDeclaration
            <|> Abstract.classDeclaration <$ keyword "class"
                <*> wrap optionalContext
                <*> wrap (Abstract.simpleKindedTypeLHS
                          <$> typeClass
                          <*> ((:[]) <$> parens (nonTerminal typeVarBinder)))
                <*> (keyword "where" *> blockOf inClassDeclaration <|> pure []),
         constraint = constraint <|> parens (nonTerminal $ Report.constraint . declarationLevel . report),
         context = nonTerminal (Report.constraint . declarationLevel . report)
            <|> Abstract.constraints
                <$> parens (filter ((1 /=) . length)
                            $ wrap (nonTerminal (Report.constraint . declarationLevel . report)) `sepBy` comma),
         classConstraint = classConstraint
            <|> Abstract.simpleConstraint
                <$> nonTerminal (Report.qualifiedTypeClass . declarationLevel . report)
                <*> parens (nonTerminal optionallyParenthesizedTypeVar),
         qualifiedTypeClass = qualifiedTypeClass <|> parens qtc,
         typeApplications =
            Abstract.typeApplication
            <$> wrap (Abstract.typeVariable <$> nonTerminal optionallyParenthesizedTypeVar <|> typeApplications)
            <*> wrap aType,
         typeVarApplications =
            generalTypeConstructor
            <|> Abstract.typeApplication
                <$> wrap (nonTerminal (Report.typeVarApplications . declarationLevel . report)
                          <|> parens (nonTerminal $ Report.typeVarApplications . declarationLevel . report))
                <*> wrap (nonTerminal optionallyKindedAndParenthesizedTypeVar),
         instanceDesignator = instanceDesignator
            <|> parens (nonTerminal $ Report.instanceDesignator . declarationLevel . report),
         derivingClause = keyword "deriving"
                          *> (pure <$> wrap (Abstract.simpleDerive <$> qtc)
                              <|> parens (filter ((/= 1) . length)
                                          $ wrap (Abstract.simpleDerive <$> qtc) `sepBy` comma))
                          <|> pure []}},
   optionallyParenthesizedTypeVar = typeVar <|> parens (nonTerminal optionallyParenthesizedTypeVar),
   typeVarBinder = Abstract.implicitlyKindedTypeVariable <$> nonTerminal optionallyParenthesizedTypeVar}
   where qtc = nonTerminal (Report.qualifiedTypeClass . declarationLevel . report)

flexibleInstancesMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                     g ~ ExtendedGrammar l t (NodeWrap t),
                                     Ord t, Show t, TextualMonoid t)
                       => GrammarBuilder g g (ParserT ((,) [[Lexeme t]])) t
flexibleInstancesMixin baseGrammar@ExtendedGrammar
                       {report= baseReport@HaskellGrammar
                                {declarationLevel= baseDeclarations@DeclarationGrammar{..}, ..}, ..} = baseGrammar{
   report= baseReport{
             declarationLevel= baseDeclarations{
                instanceDesignator = flexibleInstanceDesignator}}}

typeFamiliesMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                    g ~ ExtendedGrammar l t (NodeWrap t),
                                    Ord t, Show t, TextualMonoid t, OutlineMonoid t,
                                    Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l),
                                    Deep.Foldable (Serialization (Down Int) t) (Abstract.GADTConstructor l l))
                  => GrammarBuilder g g (ParserT ((,) [[Lexeme t]])) t
typeFamiliesMixin baseGrammar@ExtendedGrammar{report= baseReport@HaskellGrammar
                                                      {declarationLevel= baseDeclarations@DeclarationGrammar{..}, ..}} =
  baseGrammar{
    report= baseReport{
      declarationLevel= baseDeclarations{
         topLevelDeclaration = topLevelDeclaration
            <|> Abstract.dataFamilyDeclaration <$ keyword "data" <* keyword "family"
                <*> wrap simpleType <*> optional (wrap $ nonTerminal kindSignature)
            <|> Abstract.openTypeFamilyDeclaration <$ keyword "type" <* keyword "family"
                <*> wrap simpleType <*> optional (wrap $ nonTerminal kindSignature)
            <|> Abstract.closedTypeFamilyDeclaration <$ keyword "type" <* keyword "family"
                <*> wrap simpleType <*> optional (wrap $ nonTerminal kindSignature) <* keyword "where"
                <*> blockOf (Abstract.typeFamilyInstance
                             <$> nonTerminal optionalForall
                             <*> wrap (nonTerminal familyInstanceDesignator) <* delimiter "="
                             <*> wrap typeTerm)
            <|> Abstract.dataFamilyInstance <$ (keyword "data" *> keyword "instance")
                <*> nonTerminal optionalForall
                <*> wrap optionalContext
                <*> wrap (nonTerminal familyInstanceDesignator)
                <*> optional (wrap $ nonTerminal kindSignature)
                <*> (delimiter "=" *> declaredConstructors <|> pure [])
                <*> derivingClause
            <|> Abstract.newtypeFamilyInstance <$ (keyword "newtype" *> keyword "instance")
                <*> nonTerminal optionalForall
                <*> wrap optionalContext
                <*> wrap (nonTerminal familyInstanceDesignator)
                <*> optional (wrap $ nonTerminal kindSignature)
                <* delimiter "="
                <*> wrap newConstructor
                <*> derivingClause
            <|> Abstract.gadtDataFamilyInstance <$ (keyword "data" *> keyword "instance")
                <*> nonTerminal optionalForall
                <*> wrap (nonTerminal familyInstanceDesignator)
                <*> optional (wrap $ nonTerminal kindSignature)
                <* keyword "where"
                <*> blockOf (nonTerminal gadtConstructors)
                <*> derivingClause
            <|> Abstract.gadtNewtypeFamilyInstance <$ (keyword "newtype" *> keyword "instance")
                <*> nonTerminal optionalForall
                <*> wrap (nonTerminal familyInstanceDesignator)
                <*> optional (wrap $ nonTerminal kindSignature)
                <* keyword "where"
                <*> wrap (nonTerminal gadtNewConstructor)
                <*> derivingClause
            <|> Abstract.typeFamilyInstance <$ (keyword "type" *> keyword "instance")
                <*> nonTerminal optionalForall
                <*> wrap (nonTerminal familyInstanceDesignator)
                <* delimiter "="
                <*> wrap typeTerm,
         inClassDeclaration = inClassDeclaration
            <|> Abstract.dataFamilyDeclaration <$ keyword "data" <* optional (keyword "family")
                <*> wrap simpleType <*> optional (wrap $ nonTerminal kindSignature)
            <|> Abstract.openTypeFamilyDeclaration <$ keyword "type" <* optional (keyword "family")
                <*> wrap simpleType <*> optional (wrap $ nonTerminal kindSignature)
            <|> nonTerminal inClassOrInstanceTypeFamilyDeclaration,
         inInstanceDeclaration = inInstanceDeclaration <|> nonTerminal inClassOrInstanceTypeFamilyDeclaration
        }
    },
    inClassOrInstanceTypeFamilyDeclaration =
       Abstract.dataFamilyInstance <$ keyword "data" <* optional (keyword "instance")
           <*> nonTerminal optionalForall
           <*> wrap optionalContext
           <*> wrap (nonTerminal familyInstanceDesignator)
           <*> optional (wrap $ nonTerminal kindSignature)
           <* delimiter "="
           <*> declaredConstructors
           <*> derivingClause
       <|> Abstract.newtypeFamilyInstance <$ keyword "newtype" <* optional (keyword "instance")
           <*> nonTerminal optionalForall
           <*> wrap optionalContext
           <*> wrap (nonTerminal familyInstanceDesignator)
           <*> optional (wrap $ nonTerminal kindSignature)
           <* delimiter "="
           <*> wrap newConstructor
           <*> derivingClause
       <|> Abstract.gadtDataFamilyInstance <$ (keyword "data" *> optional (keyword "instance"))
           <*> nonTerminal optionalForall
           <*> wrap (nonTerminal familyInstanceDesignator)
           <*> optional (wrap $ nonTerminal kindSignature)
           <* keyword "where"
           <*> blockOf (nonTerminal gadtConstructors)
           <*> derivingClause
       <|> Abstract.gadtNewtypeFamilyInstance <$ (keyword "newtype" *> optional (keyword "instance"))
           <*> nonTerminal optionalForall
           <*> wrap (nonTerminal familyInstanceDesignator)
           <*> optional (wrap $ nonTerminal kindSignature)
           <* keyword "where"
           <*> wrap (nonTerminal gadtNewConstructor)
           <*> derivingClause
       <|> Abstract.typeFamilyInstance <$ keyword "type" <* optional (keyword "instance")
           <*> nonTerminal optionalForall
           <*> wrap (nonTerminal familyInstanceDesignator)
           <* delimiter "="
           <*> wrap typeTerm}

typeFamilyDependenciesMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                          g ~ ExtendedGrammar l t (NodeWrap t),
                                          Ord t, Show t, TextualMonoid t, OutlineMonoid t,
                                          Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l),
                                          Deep.Foldable (Serialization (Down Int) t) (Abstract.GADTConstructor l l))
                            => GrammarBuilder g g (ParserT ((,) [[Lexeme t]])) t
typeFamilyDependenciesMixin
  baseGrammar@ExtendedGrammar{report= baseReport@HaskellGrammar
                                                {declarationLevel= baseDeclarations@DeclarationGrammar{..}, ..}} =
  baseGrammar{
    report= baseReport{
      declarationLevel= baseDeclarations{
         topLevelDeclaration = topLevelDeclaration
            <|> Abstract.injectiveOpenTypeFamilyDeclaration <$ keyword "type" <* keyword "family"
                <*> wrap simpleType <* delimiter "="
                <*> nonTerminal typeVarBinder
                <*> optional ((,) <$> (delimiter "|" *> typeVar) <* rightArrow <*> someNonEmpty typeVar)
            <|> Abstract.injectiveClosedTypeFamilyDeclaration <$ keyword "type" <* keyword "family"
                <*> wrap simpleType <* delimiter "="
                <*> nonTerminal typeVarBinder
                <*> optional ((,) <$> (delimiter "|" *> typeVar) <* rightArrow <*> someNonEmpty typeVar)
                <* keyword "where"
                <*> blockOf (Abstract.typeFamilyInstance
                             <$> nonTerminal optionalForall
                             <*> wrap (nonTerminal familyInstanceDesignator) <* delimiter "="
                             <*> wrap typeTerm),
         inClassDeclaration = inClassDeclaration
            <|> Abstract.injectiveOpenTypeFamilyDeclaration <$ keyword "type" <* optional (keyword "family")
                <*> wrap simpleType <* delimiter "="
                <*> nonTerminal typeVarBinder
                <*> (Just <$> ((,) <$> (delimiter "|" *> typeVar) <* rightArrow <*> someNonEmpty typeVar))}}}

dataKindsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                  g ~ ExtendedGrammar l t (NodeWrap t),
                                  Ord t, Show t, TextualMonoid t, OutlineMonoid t,
                                  Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l))
                    => GrammarBuilder g g (ParserT ((,) [[Lexeme t]])) t
dataKindsMixin baseGrammar@ExtendedGrammar{report= baseReport@HaskellGrammar
                                                             {declarationLevel= baseDeclarations}} = baseGrammar{
   report= baseReport{
      aType = aType baseReport
         <|> nonTerminal promotedType
         <|> Abstract.promotedTupleType <$ terminator "'"
             <*> parens ((:|) <$> wrap (nonTerminal $ typeTerm . report)
                              <*> some (comma *> wrap (nonTerminal $ typeTerm . report)))
         <|> Abstract.promotedListType <$ terminator "'"
                                       <*> brackets (wrap (nonTerminal $ typeTerm . report) `sepBy1` comma)
         <|> Abstract.promotedListType
             <$> brackets ((:) <$> wrap (nonTerminal $ typeTerm . report) <* comma
                               <*> wrap (nonTerminal $ typeTerm . report) `sepBy1` comma),
      declarationLevel= baseDeclarations{
         instanceTypeDesignator = instanceTypeDesignator baseDeclarations
            <|> nonTerminal promotedType}},
   aTypeWithWildcards = aTypeWithWildcards baseGrammar
      <|> nonTerminal promotedType
      <|> Abstract.promotedTupleType <$ terminator "'"
          <*> parens ((:|) <$> wrap (nonTerminal typeWithWildcards)
                           <*> some (comma *> wrap (nonTerminal typeWithWildcards)))
      <|> Abstract.promotedListType <$ terminator "'" <*> brackets (wrap (nonTerminal typeWithWildcards) `sepBy1` comma)
      <|> Abstract.promotedListType
          <$> brackets ((:) <$> wrap (nonTerminal typeWithWildcards) <* comma
                            <*> wrap (nonTerminal typeWithWildcards) `sepBy1` comma),
   bKind = bKind baseGrammar
     <|> Abstract.typeRepresentationKind
         <$ (Report.nameQualifier <*> Report.nameToken (string "TYPE") :: Report.Parser g t (Abstract.QualifiedName l))
         <* notFollowedBy (fst <$> match (nonTerminal aKind))
         <*> wrap (nonTerminal $ Report.aType . report),
   aKind = aKind baseGrammar
     <|> Abstract.tupleKind <$> parens ((:|) <$> wrap (nonTerminal kind) <*> some (comma *> wrap (nonTerminal kind)))
     <|> Abstract.listKind <$> brackets (wrap $ nonTerminal kind)}

dataKindsTypeOperatorsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                          g ~ ExtendedGrammar l t (NodeWrap t),
                                          Ord t, Show t, TextualMonoid t, OutlineMonoid t,
                                          Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l))
                            => GrammarBuilder g g (ParserT ((,) [[Lexeme t]])) t
dataKindsTypeOperatorsMixin baseGrammar@ExtendedGrammar{report= baseReport@HaskellGrammar
                                                                {declarationLevel= baseDeclarations}} = baseGrammar{
   report= baseReport{
      declarationLevel= baseDeclarations{
         instanceTypeDesignator = instanceTypeDesignator baseDeclarations
            <|> parens (Abstract.promotedInfixTypeApplication
                        <$> wrap (nonTerminal optionallyKindedAndParenthesizedTypeVar)
                        <* terminator "'"
                        <*> qualifiedOperator baseReport
                        <*> wrap (nonTerminal optionallyKindedAndParenthesizedTypeVar))},
      bType = bType baseReport
         <|> Abstract.promotedInfixTypeApplication
             <$> wrap (nonTerminal $ bType . report)
             <* terminator "'"
             <*> qualifiedOperator baseReport
             <*> wrap (nonTerminal $ aType . report)},
   bKind = bKind baseGrammar
           <|> Abstract.infixKindApplication
           <$> wrap (nonTerminal bKind)
           <*> qualifiedOperator baseReport
           <*> wrap (nonTerminal aKind),
   cTypeWithWildcards = cTypeWithWildcards baseGrammar
      <|> Abstract.infixTypeApplication
          <$> wrap (nonTerminal bTypeWithWildcards)
          <* terminator "'"
          <*> qualifiedOperator baseReport
          <*> wrap (nonTerminal cTypeWithWildcards),
   familyInstanceDesignator = familyInstanceDesignator baseGrammar
      <|> Abstract.infixTypeClassInstanceLHS
          <$> wrap (bTypeWithWildcards baseGrammar)
          <* terminator "'"
          <*> qualifiedOperator baseReport
          <*> wrap (cTypeWithWildcards baseGrammar)}

kindSignaturesMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                  g ~ ExtendedGrammar l t (NodeWrap t),
                                  Ord t, Show t, TextualMonoid t, OutlineMonoid t,
                                  Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l))
                    => GrammarBuilder g g (ParserT ((,) [[Lexeme t]])) t
kindSignaturesMixin baseGrammar@ExtendedGrammar
                    {report= baseReport@HaskellGrammar
                             {declarationLevel= baseDeclarations@DeclarationGrammar{..}, ..}} = baseGrammar{
   report= baseReport{
      declarationLevel= baseDeclarations{
         topLevelDeclaration = topLevelDeclaration
            <|> Abstract.kindedDataDeclaration <$ keyword "data"
                   <*> wrap optionalContext
                   <*> wrap (nonTerminal $ Report.simpleType . Report.declarationLevel . report)
                   <*> wrap (nonTerminal kindSignature)
                   <*> (delimiter "=" *> nonTerminal (Report.declaredConstructors . Report.declarationLevel . report)
                        <|> pure [])
                   <*> Report.derivingClause baseDeclarations
            <|> Abstract.kindedNewtypeDeclaration <$ keyword "newtype"
                   <*> wrap optionalContext
                   <*> wrap (nonTerminal $ Report.simpleType . Report.declarationLevel . report)
                   <*> wrap (nonTerminal kindSignature)
                   <* delimiter "="
                   <*> wrap (nonTerminal $ Report.newConstructor . Report.declarationLevel . report)
                   <*> Report.derivingClause baseDeclarations
            <|> Abstract.classDeclaration
                   <$ keyword "class"
                   <*> wrap optionalContext
                   <*> wrap (Abstract.simpleTypeLHSApplication
                                <$> wrap (Abstract.simpleTypeLHS <$> typeClass <*> pure [])
                                <*> parens (Abstract.explicitlyKindedTypeVariable <$> typeVar
                                            <*> wrap (nonTerminal kindSignature)))
                   <*> (keyword "where"
                        *> blockOf (nonTerminal $ Report.inClassDeclaration . Report.declarationLevel . report)
                        <|> pure []),
         instanceTypeDesignator = instanceTypeDesignator
            <|> Abstract.kindedType
                <$> wrap (nonTerminal $ Report.instanceTypeDesignator . declarationLevel . report)
                <*> wrap (nonTerminal kindSignature),
         typeVarTuple = (:|) <$> wrap (nonTerminal optionallyKindedTypeVar)
                             <*> some (comma *> wrap (nonTerminal optionallyKindedTypeVar))},
      typeTerm = typeTerm <|>
         Abstract.kindedType <$> wrap (nonTerminal $ Report.typeTerm . report) <*> wrap (nonTerminal kindSignature)},
   kindSignature = doubleColon *> nonTerminal kind,
   kind = Abstract.functionKind <$> wrap (nonTerminal bKind) <* rightArrow <*> wrap (nonTerminal kind)
          <|> nonTerminal bKind,
   bKind = Abstract.kindApplication <$> wrap (nonTerminal bKind) <*> wrap (nonTerminal aKind)
           <|> nonTerminal aKind,
   aKind = Abstract.constructorKind <$> wrap generalConstructor
           <|> Abstract.groundTypeKind <$ delimiter "*"
           <|> Abstract.kindVariable <$> nonTerminal kindVar
           <|> parens (nonTerminal kind),
   optionallyKindedAndParenthesizedTypeVar =
      Abstract.typeVariable <$> nonTerminal optionallyParenthesizedTypeVar
      <|> parens (Abstract.kindedType
                  <$> wrap (Abstract.typeVariable <$> nonTerminal optionallyParenthesizedTypeVar)
                  <*> wrap (nonTerminal kindSignature)),
   optionallyKindedTypeVar =
      Abstract.typeVariable <$> variableIdentifier
      <|> Abstract.kindedType
          <$> wrap (Abstract.typeVariable <$> variableIdentifier)
          <*> wrap (nonTerminal kindSignature),
   typeVarBinder = typeVarBinder baseGrammar
                   <|> parens (Abstract.explicitlyKindedTypeVariable <$> typeVar <*> wrap (nonTerminal kindSignature))}

existentialQuantificationMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                             g ~ ExtendedGrammar l t (NodeWrap t),
                                             Ord t, Show t, TextualMonoid t)
                               => GrammarBuilder g g (ParserT ((,) [[Lexeme t]])) t
existentialQuantificationMixin baseGrammar@ExtendedGrammar
                               {report= baseReport@HaskellGrammar
                                        {declarationLevel= baseDeclarations@DeclarationGrammar{..}, ..}} = baseGrammar{
   report= baseReport{
      declarationLevel= baseDeclarations{
         declaredConstructor = declaredConstructor
            <|> Abstract.existentialConstructor
                <$ nonTerminal keywordForall
                <*> many (nonTerminal typeVarBinder) <* delimiter "."
                <*> wrap (context <* nonTerminal (Report.rightDoubleArrow . report) <|> pure Abstract.noContext)
                <*> wrap declaredConstructor
            <|> Abstract.existentialConstructor [] <$> wrap (context <* rightDoubleArrow)
                                                   <*> wrap declaredConstructor}}}

explicitForAllMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                  g ~ ExtendedGrammar l t (NodeWrap t),
                                  Ord t, Show t, OutlineMonoid t, TextualMonoid t,
                                  Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l))
                    => GrammarBuilder g g (ParserT ((,) [[Lexeme t]])) t
explicitForAllMixin baseGrammar@ExtendedGrammar
                    {report= baseReport@HaskellGrammar
                             {declarationLevel= baseDeclarations@DeclarationGrammar{..}, ..},
                     keywordForall} = baseGrammar{
   report= baseReport{
             declarationLevel= baseDeclarations{
               topLevelDeclaration = topLevelDeclaration
                  <|> Abstract.explicitlyScopedInstanceDeclaration <$ keyword "instance"
                      <* keywordForall
                      <*> many (nonTerminal typeVarBinder)
                      <* delimiter "."
                      <*> wrap optionalContext
                      <*> wrap instanceDesignator
                      <*> (keyword "where"
                           *> blockOf (nonTerminal $ Report.inInstanceDeclaration . declarationLevel . report)
                           <|> pure [])},
      typeTerm = typeTerm
         <|> Abstract.forallType <$ keywordForall
             <*> many (nonTerminal typeVarBinder) <* delimiter "."
             <*> wrap optionalContext
             <*> wrap (nonTerminal $ Report.typeTerm . report),
      typeVar = notFollowedBy keywordForall *> typeVar},
   optionalForall = keywordForall *> many (nonTerminal typeVarBinder) <* delimiter "." <|> pure [],
   kind = kind baseGrammar
      <|> Abstract.forallKind <$ keywordForall
          <*> many (nonTerminal typeVarBinder) <* delimiter "."
          <*> wrap (nonTerminal kind),
   kindVar = notFollowedBy keywordForall *> kindVar baseGrammar}

gadtSyntaxMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                              g ~ ExtendedGrammar l t (NodeWrap t),
                              Ord t, Show t, OutlineMonoid t, TextualMonoid t,
                              Deep.Foldable (Serialization (Down Int) t) (Abstract.GADTConstructor l l))
                => GrammarBuilder g g (ParserT ((,) [[Lexeme t]])) t
gadtSyntaxMixin baseGrammar@ExtendedGrammar
                {report= baseReport@HaskellGrammar
                                   {declarationLevel= baseDeclarations@DeclarationGrammar{..}, ..}} = baseGrammar{
   report= baseReport{
             declarationLevel= baseDeclarations{
               topLevelDeclaration = topLevelDeclaration
                  <|> Abstract.gadtDeclaration <$ keyword "data"
                      <*> wrap simpleType <*> optional (wrap $ nonTerminal kindSignature) <* keyword "where"
                      <*> blockOf (nonTerminal gadtConstructors)
                      <*> derivingClause
                  <|> Abstract.gadtNewtypeDeclaration <$ keyword "newtype"
                      <*> wrap simpleType <*> optional (wrap $ nonTerminal kindSignature) <* keyword "where"
                      <*> wrap (nonTerminal gadtNewConstructor)
                      <*> derivingClause}},
   optionalForall = keywordForall baseGrammar *> many (nonTerminal typeVarBinder) <* delimiter "." <|> pure []}

gadtSyntaxTypeOperatorsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                           g ~ ExtendedGrammar l t (NodeWrap t),
                                           Ord t, Show t, OutlineMonoid t, TextualMonoid t,
                                           Deep.Foldable (Serialization (Down Int) t) (Abstract.GADTConstructor l l))
                => GrammarBuilder g g (ParserT ((,) [[Lexeme t]])) t
gadtSyntaxTypeOperatorsMixin baseGrammar@ExtendedGrammar{report= baseReport@HaskellGrammar{..}, ..} = baseGrammar{
   return_type = return_type <|>
      Abstract.infixTypeApplication <$> wrap arg_type
                                    <*> qualifiedOperator
                                    <*> wrap arg_type}

dataKindsGadtSyntaxTypeOperatorsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                           g ~ ExtendedGrammar l t (NodeWrap t),
                                           Ord t, Show t, OutlineMonoid t, TextualMonoid t,
                                           Deep.Foldable (Serialization (Down Int) t) (Abstract.GADTConstructor l l))
                => GrammarBuilder g g (ParserT ((,) [[Lexeme t]])) t
dataKindsGadtSyntaxTypeOperatorsMixin baseGrammar@ExtendedGrammar{report= baseReport@HaskellGrammar{..}, ..} =
   baseGrammar{
      return_type = return_type <|>
         Abstract.promotedInfixTypeApplication
         <$> wrap arg_type
         <* terminator "'"
         <*> qualifiedOperator
         <*> wrap arg_type}

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

instance TokenParsing (Parser (ExtendedGrammar l t f) (LinePositioned Text)) where
   someSpace = someLexicalSpace
   token = lexicalToken

instance LexicalParsing (Parser (ExtendedGrammar l t f) (LinePositioned Text)) where
   lexicalComment = Report.comment
   lexicalWhiteSpace = Report.whiteSpace
   lexicalToken p = Report.storeToken p <* lexicalWhiteSpace
   keyword = Report.keyword
