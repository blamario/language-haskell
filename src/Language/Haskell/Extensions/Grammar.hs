{-# Language FlexibleContexts, FlexibleInstances, OverloadedStrings,
             Rank2Types, RecordWildCards,
             ScopedTypeVariables, TemplateHaskell, TupleSections, TypeSynonymInstances #-}

-- | Missing syntax extensions:
-- * @QualifiedDo@ requires TemplateHaskell 2.17
-- * @TransformListComp@ is not supported by TemplateHaskell
-- * @Arrows@ is not supported by TemplateHaskell
-- * @LexicalNegation@ ignores the presence or absence of whitespace preceding the minus

module Language.Haskell.Extensions.Grammar (grammar, extendedGrammar, parseModule, module Report) where

import Control.Applicative
import Control.Monad (void)
import qualified Data.Char as Char
import Data.Foldable (toList)
import Data.Functor.Compose (Compose(getCompose))
import Data.List (foldl', null, sortOn)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Ord (Down)
import Data.Monoid (Dual(..), Endo(..))
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
import qualified Text.Parser.Char
import Text.Parser.Combinators (eof, sepBy)
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
import Language.Haskell.Grammar (HaskellGrammar(..), Parser, OutlineMonoid, DisambiguatorTrans, NodeWrap,
                                 blockOf, delimiter, inputColumn, isSymbol,
                                 oneExtendedLine, rewrap, startSepEndBy, wrap, unwrap)
import Language.Haskell.Reserializer (Lexeme(..), Serialization, TokenType(..))

import Prelude hiding (exponent, filter, null)

extensionMixins :: forall l t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser (HaskellGrammar l t (NodeWrap t)) t),
                            Ord t, Show t, OutlineMonoid t,
                            Abstract.DeeplyFoldable (Serialization (Down Int) t) l,
                            Abstract.DeeplyFunctor (DisambiguatorTrans t) l)
                => Map (Set Extension)
                       (Int,
                        GrammarBuilder (HaskellGrammar l t (NodeWrap t))
                                       (HaskellGrammar l t (NodeWrap t))
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
     (Set.fromList [BlockArguments],             (9, blockArgumentsMixin))]

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

parseModule :: forall l t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser (HaskellGrammar l t (NodeWrap t)) t),
                        Ord t, Show t, OutlineMonoid t,
                        Abstract.DeeplyFoldable (Serialization (Down Int) t) l,
                        Abstract.DeeplyFunctor (DisambiguatorTrans t) l)
            => Map Extension Bool -> t
            -> ParseResults t [NodeWrap t (Abstract.Module l l (NodeWrap t) (NodeWrap t))]
parseModule extensions source = case moduleExtensions of
   Left err -> Left err
   Right [extensions']
      | let (contradictions, extensionMap) = partitionContradictory (Set.fromList extensions') ->
        if Set.null contradictions then
           (if null extensions' then id else fmap $ fmap $ rewrap $ Abstract.withLanguagePragma extensions')
           $ parseResults $ Report.haskellModule
           $ parseComplete (extendedGrammar $ withImplications $ extensionMap <> extensions) source
        else Left mempty{errorAlternatives= [StaticDescription
                                             $ "Contradictory extension switches " <> show (toList contradictions)]}
   Right extensionses -> error (show extensionses)
   where moduleExtensions = getCompose . fmap snd . getCompose $ simply parsePrefix languagePragmas source
         parseResults = getCompose . fmap snd . getCompose

extendedGrammar :: (Abstract.ExtendedHaskell l, LexicalParsing (Parser (HaskellGrammar l t (NodeWrap t)) t),
                    Ord t, Show t, OutlineMonoid t,
                    Abstract.DeeplyFoldable (Serialization (Down Int) t) l,
                    Abstract.DeeplyFunctor (DisambiguatorTrans t) l)
                 => Map Extension Bool -> Grammar (HaskellGrammar l t (NodeWrap t)) (ParserT ((,) [[Lexeme t]])) t
extendedGrammar extensions = fixGrammar (extended . Report.grammar)
   where extended = appEndo $ getDual $ foldMap (Dual . Endo) $ map snd $ sortOn fst
                    $ Map.elems $ Map.restrictKeys extensionMixins
                    $ Set.powerSet (Map.keysSet $ Map.filter id extensions)

grammar :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t,
                     Abstract.DeeplyFoldable (Serialization (Down Int) t) l,
                     Abstract.DeeplyFunctor (DisambiguatorTrans t) l)
        => GrammarBuilder (HaskellGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
grammar = blockArgumentsMixin . unicodeSyntaxMixin . identifierSyntaxMixin . magicHashMixin
          . parallelListComprehensionsMixin . recursiveDoMixin . tupleSectionsMixin . lambdaCaseMixin . emptyCaseMixin
          . multiWayIfMixin . Report.grammar

identifierSyntaxMixin :: forall l g t. (Abstract.Haskell l, LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t)
                      => GrammarBuilder (HaskellGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
identifierSyntaxMixin baseGrammar = baseGrammar{
   variableIdentifier = token (Abstract.name . Text.pack . toString mempty <$> variableLexeme),
   constructorIdentifier = token (Abstract.name . Text.pack . toString mempty <$> constructorLexeme),
   variableSymbol = token (Abstract.name . Text.pack . toString mempty <$> Report.variableSymbolLexeme),
   constructorSymbol = token (Abstract.name . Text.pack . toString mempty <$> Report.constructorSymbolLexeme)}

overloadedLabelsMixin :: forall l g t. (Abstract.Haskell l, LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t)
                      => GrammarBuilder (HaskellGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
overloadedLabelsMixin baseGrammar@HaskellGrammar{..} = baseGrammar{
   variableIdentifier = token (Abstract.name . Text.pack . toString mempty <$> (string "#" <> variableLexeme))
                        <|> variableIdentifier,
   variableSymbol = notFollowedBy (string "#" *> variableLexeme) *> variableSymbol}

unicodeSyntaxMixin :: forall l g t. (LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t)
                   => GrammarBuilder (HaskellGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
unicodeSyntaxMixin baseGrammar = baseGrammar{
   doubleColon = doubleColon baseGrammar <|> delimiter "∷",
   rightDoubleArrow = rightDoubleArrow baseGrammar <|> delimiter "⇒",
   rightArrow = rightArrow baseGrammar <|> delimiter "→",
   leftArrow = leftArrow baseGrammar <|> delimiter "←"}

magicHashMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t)
               => GrammarBuilder (HaskellGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
magicHashMixin baseGrammar@HaskellGrammar{..} =
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
  in baseGrammar{
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
                 <$> (Abstract.integerLiteral <$> integerHash2 <|> Abstract.floatingLiteral <$> floatHash2)}

recursiveDoMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t,
                               Abstract.DeeplyFoldable (Serialization (Down Int) t) l,
                               Abstract.DeeplyFunctor (DisambiguatorTrans t) l)
                 => GrammarBuilder (HaskellGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
recursiveDoMixin baseGrammar = baseGrammar{
   closedBlockExpresion = closedBlockExpresion baseGrammar
                          <|> Abstract.mdoExpression <$ keyword "mdo" <*> wrap (statements baseGrammar),
   statement = statement baseGrammar
               <|> Deep.InL
                   <$> wrap (Abstract.recursiveStatement
                             . (either id (rewrap Abstract.expressionStatement) . Deep.eitherFromSum . unwrap <$>)
                             <$ keyword "rec"
                             <*> blockOf (statement baseGrammar))}

parallelListComprehensionsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                              Ord t, Show t, OutlineMonoid t)
                                => GrammarBuilder (HaskellGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
parallelListComprehensionsMixin baseGrammar@HaskellGrammar{..} = baseGrammar{
   bareExpression = bareExpression
                    <|> brackets (Abstract.parallelListComprehension
                                  <$> expression <*> qualifiers <*> qualifiers <*> many qualifiers)}

tupleSectionsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                 Ord t, Show t, OutlineMonoid t)
                   => GrammarBuilder (HaskellGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
tupleSectionsMixin baseGrammar@HaskellGrammar{..} = baseGrammar{
   bareExpression = bareExpression
                    <|> Abstract.tupleSectionExpression
                           <$> parens ((:|) <$> optional expression <*> some (comma *> optional expression))}

lambdaCaseMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t), Ord t, Show t, TextualMonoid t)
                => GrammarBuilder (HaskellGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
lambdaCaseMixin baseGrammar = baseGrammar{
   closedBlockExpresion = closedBlockExpresion baseGrammar
                          <|> Abstract.lambdaCaseExpression <$ (delimiter "\\" *> keyword "case")
                                                            <*> alternatives baseGrammar}

emptyCaseMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t,
                             Deep.Foldable (Serialization (Down Int) t) (Abstract.CaseAlternative l l),
                             Deep.Functor (DisambiguatorTrans t) (Abstract.CaseAlternative l l))
                 => GrammarBuilder (HaskellGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
emptyCaseMixin baseGrammar = baseGrammar{
   alternatives = blockOf (alternative baseGrammar)}

multiWayIfMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t,
                             Deep.Foldable (Serialization (Down Int) t) (Abstract.GuardedExpression l l),
                             Deep.Functor (DisambiguatorTrans t) (Abstract.GuardedExpression l l))
                => GrammarBuilder (HaskellGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
multiWayIfMixin baseGrammar@HaskellGrammar{..} = baseGrammar{
   closedBlockExpresion = closedBlockExpresion
                          <|> Abstract.multiWayIfExpression <$ keyword "if"
                                  <*> blockOf'
                                         (Abstract.guardedExpression . toList
                                             <$> guards <* rightArrow
                                             <*> expression)}

packageImportsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t), Ord t, Show t,
                                  OutlineMonoid t)
                      => GrammarBuilder (HaskellGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
packageImportsMixin baseGrammar@HaskellGrammar{..} = baseGrammar{
   importDeclaration = importDeclaration
                       <|> Abstract.packageQualifiedImportDeclaration <$ keyword "import"
                           <*> (True <$ keyword "qualified" <|> pure False)
                           <*> stringLiteral
                           <*> Report.moduleId
                           <*> optional (keyword "as" *> Report.moduleId) <*> optional (wrap importSpecification)}

safeImportsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t)
                 => GrammarBuilder (HaskellGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
safeImportsMixin baseGrammar@HaskellGrammar{..} = baseGrammar{
   importDeclaration = importDeclaration
                       <|> Abstract.safeImportDeclaration <$ keyword "import" <* keyword "safe"
                           <*> (True <$ keyword "qualified" <|> pure False)
                           <*> Report.moduleId
                           <*> optional (keyword "as" *> Report.moduleId) <*> optional (wrap importSpecification)}

importQualifiedPostMixin :: forall l g t. (Abstract.Haskell l, LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t)
                         => GrammarBuilder (HaskellGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
importQualifiedPostMixin baseGrammar@HaskellGrammar{..} = baseGrammar{
   importDeclaration = importDeclaration
                       <|> flip Abstract.importDeclaration <$ keyword "import"
                           <*> Report.moduleId
                           <*> (True <$ keyword "qualified" <|> pure False)
                           <*> optional (keyword "as" *> Report.moduleId) <*> optional (wrap importSpecification)}

safePackageImportsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t), Ord t, Show t,
                                          OutlineMonoid t)
                        => GrammarBuilder (HaskellGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
safePackageImportsMixin baseGrammar@HaskellGrammar{..} = baseGrammar{
   importDeclaration = importDeclaration
                       <|> Abstract.safePackageQualifiedImportDeclaration <$ keyword "import" <* keyword "safe"
                           <*> (True <$ keyword "qualified" <|> pure False)
                           <*> stringLiteral
                           <*> Report.moduleId
                           <*> optional (keyword "as" *> Report.moduleId) <*> optional (wrap importSpecification)}

packageImportsQualifiedPostMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                                   Ord t, Show t, OutlineMonoid t)
                                 => GrammarBuilder (HaskellGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
packageImportsQualifiedPostMixin baseGrammar@HaskellGrammar{..} = baseGrammar{
   importDeclaration = importDeclaration
                       <|> Abstract.packageQualifiedImportDeclaration <$ keyword "import"
                           <**> pure flip
                           <*> stringLiteral
                           <**> pure flip
                           <*> Report.moduleId
                           <*> (True <$ keyword "qualified" <|> pure False)
                           <*> optional (keyword "as" *> Report.moduleId) <*> optional (wrap importSpecification)}

safeImportsQualifiedPostMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                                Ord t, Show t, OutlineMonoid t)
                              => GrammarBuilder (HaskellGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
safeImportsQualifiedPostMixin baseGrammar@HaskellGrammar{..} = baseGrammar{
   importDeclaration = importDeclaration
                       <|> flip Abstract.safeImportDeclaration <$ keyword "import" <* keyword "safe"
                           <*> Report.moduleId
                           <*> (True <$ keyword "qualified" <|> pure False)
                           <*> optional (keyword "as" *> Report.moduleId) <*> optional (wrap importSpecification)}

safePackageImportsQualifiedPostMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                                       Ord t, Show t, OutlineMonoid t)
                                     => GrammarBuilder (HaskellGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
safePackageImportsQualifiedPostMixin baseGrammar@HaskellGrammar{..} = baseGrammar{
   importDeclaration = importDeclaration
                       <|> Abstract.safePackageQualifiedImportDeclaration <$ keyword "import" <* keyword "safe"
                           <**> pure flip
                           <*> stringLiteral
                           <**> pure flip
                           <*> Report.moduleId
                           <*> (True <$ keyword "qualified" <|> pure False)
                           <*> optional (keyword "as" *> Report.moduleId) <*> optional (wrap importSpecification)}

explicitNamespacesMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t), Ord t, Show t,
                                      OutlineMonoid t)
                        => GrammarBuilder (HaskellGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
explicitNamespacesMixin baseGrammar@HaskellGrammar{..} = baseGrammar{
   importItem = importItem
                <|> keyword "type" *> (Abstract.importClassOrType <$> parens variableSymbol <*> pure Nothing)}

blockArgumentsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                  Ord t, Show t, OutlineMonoid t,
                                  Deep.Foldable (Serialization (Down Int) t) (Abstract.GuardedExpression l l),
                                  Deep.Functor (DisambiguatorTrans t) (Abstract.GuardedExpression l l))
                    => GrammarBuilder (HaskellGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
blockArgumentsMixin baseGrammar@HaskellGrammar{..} = baseGrammar{
   lExpression = lExpression <|> wrap (Abstract.applyExpression <$> fExpression <*> wrap openBlockExpression),
   dExpression = fExpression,
   bareExpression = bareExpression <|> closedBlockExpresion}

lexicalNegationMixin :: forall l t. (Abstract.Haskell l, LexicalParsing (Parser (HaskellGrammar l t (NodeWrap t)) t),
                                 Ord t, Show t, TextualMonoid t)
                     => GrammarBuilder (HaskellGrammar l t (NodeWrap t))
                                       (HaskellGrammar l t (NodeWrap t))
                                       (ParserT ((,) [[Lexeme t]]))
                                       t
lexicalNegationMixin baseGrammar@HaskellGrammar{..} = baseGrammar{
   qualifiedVariableSymbol = notFollowedBy (string "-"
                                            *> satisfyCharInput (\c-> Char.isAlphaNum c || c == '(' || c == '['))
                             *> token (Report.nameQualifier <*> variableSymbol),
   infixExpression = wrap (Abstract.infixExpression
                              <$> nonTerminal Report.leftInfixExpression
                              <*> wrap (Abstract.referenceExpression <$> qualifiedOperator)
                              <*> nonTerminal Report.infixExpression)
                     <|> lExpression,
   leftInfixExpression =
      wrap (Abstract.infixExpression
               <$> nonTerminal Report.leftInfixExpression
               <*> wrap (Abstract.referenceExpression <$> qualifiedOperator)
               <*> nonTerminal Report.leftInfixExpression)
      <|> dExpression,
   bareExpression =
      Abstract.applyExpression <$> wrap (Abstract.negate <$ prefixMinus) <*> aExpression
      <|> parens (Abstract.rightSectionExpression
                  <$> (notFollowedBy prefixMinus *> qualifiedOperator)
                  <*> infixExpression)
      <|> bareExpression}
   where prefixMinus = void (string "-"
                             <* lookAhead (satisfyCharInput $ \c-> Char.isAlphaNum c || c == '(' || c == '[')
                             <* lift ([[Token Modifier "-"]], ()))
                       <?> "prefix -"

negativeLiteralsMixin :: forall l t. (Abstract.Haskell l, LexicalParsing (Parser (HaskellGrammar l t (NodeWrap t)) t),
                                  Ord t, Show t, TextualMonoid t)
                      => GrammarBuilder (HaskellGrammar l t (NodeWrap t))
                                        (HaskellGrammar l t (NodeWrap t))
                                        (ParserT ((,) [[Lexeme t]]))
                                        t
negativeLiteralsMixin baseGrammar@HaskellGrammar{..} = baseGrammar{
   qualifiedVariableSymbol = notFollowedBy (string "-" *> satisfyCharInput Char.isDigit) *> qualifiedVariableSymbol,
   infixExpression = wrap (Abstract.infixExpression
                              <$> nonTerminal Report.leftInfixExpression
                              <*> wrap (Abstract.referenceExpression <$> qualifiedOperator)
                              <*> nonTerminal Report.infixExpression
                           <|> Abstract.applyExpression <$> wrap (Abstract.negate <$ prefixMinus) <*> infixExpression)
                     <|> lExpression,
   leftInfixExpression =
      wrap (Abstract.infixExpression
               <$> nonTerminal Report.leftInfixExpression
               <*> wrap (Abstract.referenceExpression <$> qualifiedOperator)
               <*> nonTerminal Report.leftInfixExpression
               <|> Abstract.applyExpression <$> wrap (Abstract.negate <$ prefixMinus) <*> leftInfixExpression)
      <|> dExpression,
   integerLexeme = (negate <$ string "-" <|> pure id) <*> integerLexeme,
   floatLexeme = (negate <$ string "-" <|> pure id) <*> floatLexeme}
   where prefixMinus = void (token $ string "-" <* notSatisfyChar Char.isDigit) <?> "prefix -"

binaryLiteralsMixin :: forall l g t. (Abstract.Haskell l, LexicalParsing (Parser g t), Ord t, Show t, TextualMonoid t)
                      => GrammarBuilder (HaskellGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
binaryLiteralsMixin baseGrammar@HaskellGrammar{..} = baseGrammar{
   integerLexeme =
      (string "0b" <|> string "0B")
      *> (foldl' binary 0 . toString mempty <$> takeCharsWhile1 (\c-> c == '0' || c == '1') <?> "binary number")
      <<|> integerLexeme}
   where binary n '0' = 2*n
         binary n '1' = 2*n + 1
         binary _ _ = error "non-binary"

hexFloatLiteralsMixin :: forall l g t. (Abstract.Haskell l, LexicalParsing (Parser g t), Ord t, Show t, TextualMonoid t)
                      => GrammarBuilder (HaskellGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
hexFloatLiteralsMixin baseGrammar@HaskellGrammar{..} = baseGrammar{
   integerLexeme = notFollowedBy ((string "0x" <|> string "0X")
                                 *> hexadecimal *> satisfyCharInput (`elem` ['.', 'p', 'P']))
                   *> integerLexeme,
   floatLexeme = (string "0x" <|> string "0X")
                 *> (readHexFloat <$> hexadecimal <* string "." <*> hexadecimal <*> (hexExponent <<|> pure 0)
                    <|> readHexFloat <$> hexadecimal <*> pure mempty <*> hexExponent)
                 <|> floatLexeme}
   where hexExponent =
           (string "p" <|> string "P")
           *> (id <$ string "+" <|> negate <$ string "-" <|> pure id)
           <*> (fst . head . Numeric.readDec . toString mempty <$> decimal)
         readHexFloat whole fraction magnitude =
           fst (head $ Numeric.readHex $ toString mempty $ whole <> fraction)
           * 2 ^^ (magnitude - Factorial.length fraction)

numericUnderscoresMixin :: forall l g t. (Abstract.Haskell l, LexicalParsing (Parser g t), Ord t, Show t, TextualMonoid t)
                        => GrammarBuilder (HaskellGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
numericUnderscoresMixin baseGrammar@HaskellGrammar{} = baseGrammar{
   decimal = takeCharsWhile1 Char.isDigit <> concatAll (char '_' *> takeCharsWhile1 Char.isDigit) <?> "decimal number",
   octal = takeCharsWhile1 Char.isOctDigit <> concatAll (char '_' *> takeCharsWhile1 Char.isOctDigit)
           <?> "octal number",
   hexadecimal = takeCharsWhile1 Char.isHexDigit <> concatAll (char '_' *> takeCharsWhile1 Char.isHexDigit)
                 <?> "hexadecimal number"}

binaryUnderscoresMixin :: forall l g t. (Abstract.Haskell l, LexicalParsing (Parser g t), Ord t, Show t, TextualMonoid t)
                        => GrammarBuilder (HaskellGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
binaryUnderscoresMixin baseGrammar@HaskellGrammar{..} = baseGrammar{
   integerLexeme =
      (string "0b" <|> string "0B")
      *> (foldl' binary 0 . toString mempty
          <$> (binaryDigits <> concatAll (char '_' *> binaryDigits)) <?> "binary number")
      <<|> integerLexeme}
   where binary n '0' = 2*n
         binary n '1' = 2*n + 1
         binary _ _ = error "non-binary"
         binaryDigits = takeCharsWhile1 (\c-> c == '0' || c == '1')

variableLexeme, constructorLexeme, identifierTail :: (Ord t, Show t, TextualMonoid t) => Parser g t t
variableLexeme = filter (`Set.notMember` Report.reservedWords) (satisfyCharInput varStart <> identifierTail)
                 <?> "variable"
   where varStart c = (Char.isLetter c && not (Char.isUpper c)) ||  c == '_'
constructorLexeme = satisfyCharInput Char.isUpper <> identifierTail <?> "constructor"
identifierTail = takeCharsWhile isNameTailChar <> concatAll (string "#") -- MagicHash

isNameTailChar :: Char -> Bool
isNameTailChar c = Report.isNameTailChar c || Char.isMark c

blockOf' :: (Ord t, Show t, OutlineMonoid t, LexicalParsing (Parser g t),
             Deep.Foldable (Serialization (Down Int) t) node, Deep.Functor (DisambiguatorTrans t) node)
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
