{-# Language FlexibleContexts, FlexibleInstances, OverloadedStrings,
             Rank2Types, RecordWildCards, ScopedTypeVariables,
             TemplateHaskell, TupleSections, TypeApplications, TypeFamilies, TypeSynonymInstances #-}

-- | Missing syntax extensions:
-- * @QualifiedDo@ requires TemplateHaskell 2.17
-- * @TransformListComp@ is not supported by TemplateHaskell
-- * @Arrows@ is not supported by TemplateHaskell
-- * @LexicalNegation@ ignores the presence or absence of whitespace preceding the minus

module Language.Haskell.Extensions.Grammar (grammar, extendedGrammar, parseModule, report, module Report) where

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
import Text.Parser.Combinators (eof, sepBy, sepByNonEmpty)
import Text.Parser.Token (braces, brackets, comma, parens)
import Text.Grampa
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
   kindVar :: p (Abstract.Name l),
   gadtConstructors :: p (Abstract.GADTConstructor l l f f),
   constructorIDs :: p (NonEmpty (Abstract.Name l)),
   optionalForall :: p [Abstract.TypeVarBinding l l f f],
   typeVarBinder :: p (Abstract.TypeVarBinding l l f f),
   optionallyKindedTypeVar, optionallyKindedAndParenthesizedTypeVar :: p (Abstract.Type l l f f),
   gadtBody, prefix_gadt_body, record_gadt_body :: p (Abstract.Type l l f f),
   return_type :: p (Abstract.Type l l f f)}

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
     (Set.fromList [IdentifierSyntax],           (0, identifierSyntaxMixin)),
     (Set.fromList [PackageImports],             (0, packageImportsMixin)),
     (Set.fromList [SafeImports],                (0, safeImportsMixin)),
     (Set.fromList [ImportQualifiedPost],        (0, importQualifiedPostMixin)),
     (Set.fromList [ExplicitNamespaces],         (0, explicitNamespacesMixin)),
     (Set.fromList [UnicodeSyntax],              (1, unicodeSyntaxMixin)),
     (Set.fromList [BinaryLiterals],             (1, binaryLiteralsMixin)),
     (Set.fromList [HexFloatLiterals],           (1, hexFloatLiteralsMixin)),
     (Set.fromList [NumericUnderscores],         (1, numericUnderscoresMixin)),
     (Set.fromList [BinaryLiterals,
                    NumericUnderscores],         (9, binaryUnderscoresMixin)),
     (Set.fromList [PackageImports,
                    SafeImports],                (9, safePackageImportsMixin)),
     (Set.fromList [PackageImports,
                    ImportQualifiedPost],        (9, packageImportsQualifiedPostMixin)),
     (Set.fromList [SafeImports,
                    ImportQualifiedPost],        (9, safeImportsQualifiedPostMixin)),
     (Set.fromList [PackageImports,
                    SafeImports,
                    ImportQualifiedPost],        (9, safePackageImportsQualifiedPostMixin)),
     (Set.fromList [NegativeLiterals],           (2, negativeLiteralsMixin)),
     (Set.fromList [LexicalNegation],            (3, lexicalNegationMixin)),
     (Set.fromList [MagicHash],                  (3, magicHashMixin)),
     (Set.fromList [ParallelListComprehensions], (3, parallelListComprehensionsMixin)),
     (Set.fromList [OverloadedLabels],           (4, overloadedLabelsMixin)),
     (Set.fromList [RecursiveDo],                (4, recursiveDoMixin)),
     (Set.fromList [TupleSections],              (5, tupleSectionsMixin)),
     (Set.fromList [EmptyCase],                  (6, emptyCaseMixin)),
     (Set.fromList [LambdaCase],                 (7, lambdaCaseMixin)),
     (Set.fromList [MultiWayIf],                 (8, multiWayIfMixin)),
     (Set.fromList [KindSignatures],             (8, kindSignaturesMixin)),
     (Set.fromList [BlockArguments],             (9, blockArgumentsMixin)),
     (Set.fromList [TypeOperators],              (9, typeOperatorsMixin)),
     (Set.fromList [ExistentialQuantification],  (9, existentialQuantificationMixin)),
     (Set.fromList [ExplicitForAll],             (9, explicitForAllMixin)),
     (Set.fromList [GADTSyntax],                 (9, gadtSyntaxMixin)),
     (Set.fromList [FlexibleInstances],          (9, flexibleInstancesMixin))]

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

grammar :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t,
                     Abstract.DeeplyFoldable (Serialization (Down Int) t) l)
        => GrammarBuilder (ExtendedGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
grammar = blockArgumentsMixin . unicodeSyntaxMixin . identifierSyntaxMixin . magicHashMixin
          . parallelListComprehensionsMixin . recursiveDoMixin . tupleSectionsMixin . lambdaCaseMixin . emptyCaseMixin
          . multiWayIfMixin . reportGrammar

reportGrammar :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t,
                            Deep.Foldable (Serialization (Down Int) t) (Abstract.CaseAlternative l l),
                            Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l),
                            Deep.Foldable (Serialization (Down Int) t) (Abstract.Expression l l),
                            Deep.Foldable (Serialization (Down Int) t) (Abstract.Import l l),
                            Deep.Foldable (Serialization (Down Int) t) (Abstract.Statement l l))
              => GrammarBuilder (ExtendedGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
reportGrammar g@ExtendedGrammar{report= r@HaskellGrammar{..}} =
   g{report= Report.grammar r,
     keywordForall = keyword "forall",
     kindSignature = empty,
     kindVar = variableIdentifier,
     kind = empty,
     bKind = empty,
     aKind = empty,
     optionallyKindedAndParenthesizedTypeVar = empty,
     optionallyKindedTypeVar = empty,
     typeVarBinder = Abstract.implicitlyKindedTypeVariable <$> typeVar}

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
      leftArrow = leftArrow <|> delimiter "←"}}

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
                      <|> Abstract.charLiteral <$> charHashLiteral <|> Abstract.stringLiteral <$> stringHashLiteral)
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

explicitNamespacesMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t), Ord t, Show t,
                                      OutlineMonoid t)
                        => GrammarBuilder (ExtendedGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
explicitNamespacesMixin baseGrammar@ExtendedGrammar
                        {report= baseReport@HaskellGrammar
                                           {moduleLevel= baseModule@ModuleLevelGrammar{..}, ..}} = baseGrammar{
   report= baseReport{
      moduleLevel= baseModule{
         importItem = importItem
                      <|> keyword "type" *> (Abstract.importClassOrType <$> parens variableSymbol <*> pure Nothing)}}}

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
               simpleType =
                  simpleType
                  <|> Abstract.simpleInfixTypeLHSApplication
                               <$> nonTerminal typeVarBinder
                               <*> anySymbol
                               <*> nonTerminal typeVarBinder
                  <|> parens (nonTerminal (Report.simpleType . Report.declarationLevel . report))
                  <|> Abstract.simpleTypeLHSApplication
                               <$> wrap (parens $ nonTerminal (Report.simpleType . Report.declarationLevel . report))
                               <*> nonTerminal typeVarBinder},
             typeConstructor = constructorIdentifier <|> parens anySymbol,
             bType = bType
                <|> Abstract.infixTypeApplication <$> wrap (nonTerminal (Report.bType . report))
                                                  <*> qualifiedOperator
                                                  <*> wrap aType}}
   where anySymbol = constructorSymbol <|> variableSymbol

flexibleInstancesMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                     g ~ ExtendedGrammar l t (NodeWrap t),
                                     Ord t, Show t, TextualMonoid t)
                       => GrammarBuilder g g (ParserT ((,) [[Lexeme t]])) t
flexibleInstancesMixin baseGrammar@ExtendedGrammar
                       {report= baseReport@HaskellGrammar
                                {declarationLevel= baseDeclarations@DeclarationGrammar{..}, ..}} = baseGrammar{
   report= baseReport{
             declarationLevel= baseDeclarations{
                instanceDesignator =
                   Abstract.typeClassInstanceLHS <$> qualifiedTypeClass <*> wrap aType
                   <|> parens (nonTerminal $ Report.instanceDesignator . Report.declarationLevel . report)}}}

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
         simpleType = Abstract.simpleKindedTypeLHS <$> typeConstructor <*> many (nonTerminal typeVarBinder),
         topLevelDeclaration = topLevelDeclaration
            <|> Abstract.kindedDataDeclaration <$ keyword "data"
                   <*> wrap optionalContext
                   <*> wrap (nonTerminal $ Report.simpleType . Report.declarationLevel . report)
                   <*> wrap (nonTerminal kindSignature)
                   <*> (delimiter "=" *> declaredConstructors <|> pure [])
                   <*> Report.derivingClause baseDeclarations
            <|> Abstract.classDeclaration
                   <$ keyword "class"
                   <*> wrap optionalContext
                   <*> wrap (Abstract.simpleTypeLHSApplication
                                <$> wrap (Abstract.simpleTypeLHS <$> typeClass <*> pure [])
                                <*> parens (Abstract.explicitlyKindedTypeVariable <$> typeVar
                                            <*> wrap (nonTerminal kindSignature)))
                   <*> (keyword "where" *> blockOf inClassDeclaration <|> pure []),
         instanceDesignator =
            Abstract.typeClassInstanceLHS <$> qualifiedTypeClass
               <*> wrap (generalTypeConstructor
                         <|> parens (typeVarApplications <|> Abstract.tupleType <$> typeVarTuple)
                         <|> Abstract.listType <$> brackets (wrap $ nonTerminal optionallyKindedTypeVar)
                         <|> parens (Abstract.functionType
                                     <$> wrap (nonTerminal optionallyKindedAndParenthesizedTypeVar) <* rightArrow
                                     <*> wrap (nonTerminal optionallyKindedAndParenthesizedTypeVar))),
         typeVarApplications = generalTypeConstructor
                               <|> Abstract.typeApplication
                                   <$> wrap typeVarApplications
                                   <*> wrap (nonTerminal optionallyKindedAndParenthesizedTypeVar),
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
      Abstract.typeVariable <$> variableIdentifier
      <|> parens (Abstract.kindedType
                  <$> wrap (Abstract.typeVariable <$> variableIdentifier)
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
                declaredConstructor =
                   Abstract.existentialConstructor <$ nonTerminal keywordForall
                                                   <*> some (nonTerminal typeVarBinder) <* delimiter "."
                                                   <*> wrap (context <* rightDoubleArrow <|> pure Abstract.noContext)
                                                   <*> wrap declaredConstructor
                   <|> Abstract.existentialConstructor [] <$> wrap (context <* rightDoubleArrow)
                                                          <*> wrap declaredConstructor
                   <|> declaredConstructor}}}

explicitForAllMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                  g ~ ExtendedGrammar l t (NodeWrap t),
                                  Ord t, Show t, OutlineMonoid t, TextualMonoid t,
                                  Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l))
                    => GrammarBuilder g g (ParserT ((,) [[Lexeme t]])) t
explicitForAllMixin baseGrammar@ExtendedGrammar
                    {report= baseReport@HaskellGrammar
                             {declarationLevel= baseDeclarations@DeclarationGrammar{..}, ..}, ..} = baseGrammar{
   report= baseReport{
             declarationLevel= baseDeclarations{
               topLevelDeclaration = topLevelDeclaration
                  <|> Abstract.explicitlyScopedInstanceDeclaration <$ keyword "instance"
                      <* keywordForall <*> ((:|) <$> typeVarBinder <*> many typeVarBinder) <* delimiter "."
                      <*> wrap optionalContext
                      <*> wrap instanceDesignator
                      <*> (keyword "where" *> blockOf inInstanceDeclaration <|> pure [])},
      typeTerm = typeTerm
         <|> Abstract.forallType <$ keywordForall
             <*> some typeVarBinder <* delimiter "."
             <*> wrap optionalContext
             <*> wrap (nonTerminal $ Report.typeTerm . report),
      typeVar = notFollowedBy keywordForall *> typeVar}}

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
                      <*> derivingClause}},
   gadtConstructors=
      Abstract.gadtConstructors <$> nonTerminal constructorIDs <* doubleColon
                                <*> nonTerminal optionalForall
                                <*> wrap optionalContext
                                <*> wrap (nonTerminal gadtBody),
   constructorIDs = constructor `sepByNonEmpty` comma,
   optionalForall = nonTerminal keywordForall *> some (nonTerminal typeVarBinder) <|> pure [],
   gadtBody = nonTerminal prefix_gadt_body <|> nonTerminal record_gadt_body,
   prefix_gadt_body =
      parens (nonTerminal prefix_gadt_body)
      <|> nonTerminal return_type
      <|> Abstract.functionType <$> wrap (bType <|> Abstract.strictType <$ delimiter "!" <*> wrap bType) <* rightArrow
                                <*> wrap (nonTerminal prefix_gadt_body),
   record_gadt_body =
      Abstract.recordFunctionType
      <$> braces (wrap fieldDeclaration `sepBy` comma) <* rightArrow
      <*> wrap (nonTerminal return_type),
   return_type = bType}
 
variableLexeme, constructorLexeme, identifierTail :: (Ord t, Show t, TextualMonoid t) => Parser g t t
variableLexeme = filter (`Set.notMember` Report.reservedWords) (satisfyCharInput varStart <> identifierTail)
                 <?> "variable"
   where varStart c = (Char.isLetter c && not (Char.isUpper c)) ||  c == '_'
constructorLexeme = satisfyCharInput Char.isUpper <> identifierTail <?> "constructor"
identifierTail = takeCharsWhile isNameTailChar <> concatAll (string "#") -- MagicHash

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
