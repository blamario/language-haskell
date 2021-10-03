{-# Language FlexibleContexts, FlexibleInstances, OverloadedStrings,
             Rank2Types, RecordWildCards,
             ScopedTypeVariables, TemplateHaskell, TupleSections, TypeSynonymInstances #-}

-- | Missing syntax extensions:
-- * @QualifiedDo@ requires TemplateHaskell 2.17
-- * @TransformListComp@ is not supported by TemplateHaskell
-- * @Arrows@ is not supported by TemplateHaskell
-- * @LexicalNegation@ awaits

module Language.Haskell.Extensions.Grammar (grammar, extendedGrammar, parseModule, module Report) where

import Control.Applicative
import Control.Monad (void)
import qualified Data.Char as Char
import Data.List (foldl', null, sortOn)
import Data.List.NonEmpty (NonEmpty((:|)), toList)
import Data.Functor.Compose (Compose(Compose, getCompose))
import Data.Ord (Down)
import Data.String (IsString)
import Data.Monoid (Dual(..), Endo(..))
import Data.Monoid.Instances.Positioned (LinePositioned, extract)
import Data.Monoid.Textual (TextualMonoid, characterPrefix, toString)
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
import qualified Text.Parser.Char
import Text.Parser.Combinators (eof, sepBy)
import Text.Parser.Token (braces, brackets, comma, parens)
import Text.Grampa
import qualified Text.Grampa.ContextFree.SortedMemoizing as P (Parser)
import Text.Grampa.ContextFree.LeftRecursive.Transformer (ParserT, lift)
import qualified Transformation.Deep as Deep
import Witherable (filter, mapMaybe)

import Language.Haskell.Extensions (Extension(..), ExtensionSwitch(..),
                                    allExtensions, switchesByName, implications) 
import qualified Language.Haskell.Extensions.Abstract as Abstract
import qualified Language.Haskell.Extensions.AST as AST (Language, Value(..))
import qualified Language.Haskell.Grammar as Report
import Language.Haskell.Grammar (HaskellGrammar(..), Parser, OutlineMonoid, DisambiguatorTrans, NodeWrap,
                                 blockOf, delimiter, inputColumn, isSymbol,
                                 oneExtendedLine, rewrap, startSepEndBy, wrap, unwrap)
import qualified Language.Haskell.Disambiguator as Disambiguator
import Language.Haskell.Reserializer (Lexeme(..), Serialization, TokenType(..))

import Prelude hiding (exponent, filter, null)

extensionMixins :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser (HaskellGrammar l t (NodeWrap t)) t),
                              Ord t, Show t, OutlineMonoid t,
                              Deep.Foldable (Serialization (Down Int) t) (Abstract.CaseAlternative l l),
                              Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l),
                              Deep.Foldable (Serialization (Down Int) t) (Abstract.Expression l l),
                              Deep.Foldable (Serialization (Down Int) t) (Abstract.GuardedExpression l l),
                              Deep.Foldable (Serialization (Down Int) t) (Abstract.Import l l),
                              Deep.Foldable (Serialization (Down Int) t) (Abstract.Statement l l),
                              Deep.Functor (DisambiguatorTrans t) (Abstract.CaseAlternative l l),
                              Deep.Functor (DisambiguatorTrans t) (Abstract.Declaration l l),
                              Deep.Functor (DisambiguatorTrans t) (Abstract.Expression l l),
                              Deep.Functor (DisambiguatorTrans t) (Abstract.GuardedExpression l l),
                              Deep.Functor (DisambiguatorTrans t) (Abstract.Import l l),
                              Deep.Functor (DisambiguatorTrans t) (Abstract.Statement l l))
                => Map (Set ExtensionSwitch)
                       (Int,
                        GrammarBuilder (HaskellGrammar l t (NodeWrap t))
                                       (HaskellGrammar l t (NodeWrap t))
                                       (ParserT ((,) [[Lexeme t]]))
                                       t)
extensionMixins =
  Map.fromList [
     (Set.fromList [Yes IdentifierSyntax],           (0, identifierSyntaxMixin)),
     (Set.fromList [Yes UnicodeSyntax],              (1, unicodeSyntaxMixin)),
     (Set.fromList [Yes BinaryLiterals],             (1, binaryLiteralsMixin)),
     (Set.fromList [Yes HexFloatLiterals],           (1, hexFloatLiteralsMixin)),
     (Set.fromList [Yes NumericUnderscores],         (1, numericUnderscoresMixin)),
     (Set.fromList [Yes BinaryLiterals,
                    Yes NumericUnderscores],         (9, binaryUnderscoresMixin)),
     (Set.fromList [Yes NegativeLiterals],           (2, negativeLiteralsMixin)),
     (Set.fromList [Yes LexicalNegation],            (3, lexicalNegationMixin)),
     (Set.fromList [Yes MagicHash],                  (3, magicHashMixin)),
     (Set.fromList [Yes ParallelListComprehensions], (3, parallelListComprehensionsMixin)),
     (Set.fromList [Yes OverloadedLabels],           (4, overloadedLabelsMixin)),
     (Set.fromList [Yes RecursiveDo],                (4, recursiveDoMixin)),
     (Set.fromList [Yes TupleSections],              (5, tupleSectionsMixin)),
     (Set.fromList [Yes EmptyCase],                  (6, emptyCaseMixin)),
     (Set.fromList [Yes LambdaCase],                 (7, lambdaCaseMixin)),
     (Set.fromList [Yes MultiWayIf],                 (8, multiWayIfMixin)),
     (Set.fromList [Yes BlockArguments],             (9, blockArgumentsMixin))]

pragma :: (Show t, TextualMonoid t) => Parser g t t
pragma = do open <- string "{-#" <> takeCharsWhile Char.isSpace
            content <- concatMany ((notFollowedBy (string "#-}") *> anyToken) <> takeCharsWhile (/= '#'))
            close <- string "#-}"
            lift ([[Comment $ open <> content <> close]], content)

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
                        spaceChars
                        case Map.lookup extensionName switchesByName of
                           Just ext -> pure ext
                           Nothing -> fail ("Unknown language extension " <> toString mempty extensionName)
         comment = string "--" <* takeCharsWhile Report.isLineChar <|> blockComment
         blockComment = string "{-"
                        *> skipMany (blockComment
                                     <|> notFollowedBy (string "-}") *> anyToken <> takeCharsWhile  (/= '-'))
                        *> string "-}"

parseModule :: forall l t p. (Abstract.ExtendedHaskell l, LexicalParsing (Parser (HaskellGrammar l t (NodeWrap t)) t),
                Ord t, Show t, OutlineMonoid t,
                Deep.Foldable (Serialization (Down Int) t) (Abstract.CaseAlternative l l),
                Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l),
                Deep.Foldable (Serialization (Down Int) t) (Abstract.Expression l l),
                Deep.Foldable (Serialization (Down Int) t) (Abstract.GuardedExpression l l),
                Deep.Foldable (Serialization (Down Int) t) (Abstract.Import l l),
                Deep.Foldable (Serialization (Down Int) t) (Abstract.Statement l l),
                Deep.Functor (DisambiguatorTrans t) (Abstract.CaseAlternative l l),
                Deep.Functor (DisambiguatorTrans t) (Abstract.Declaration l l),
                Deep.Functor (DisambiguatorTrans t) (Abstract.Expression l l),
                Deep.Functor (DisambiguatorTrans t) (Abstract.GuardedExpression l l),
                Deep.Functor (DisambiguatorTrans t) (Abstract.Import l l),
                Deep.Functor (DisambiguatorTrans t) (Abstract.Statement l l))
            => Set ExtensionSwitch -> t
            -> ParseResults t [NodeWrap t (Abstract.Module l l (NodeWrap t) (NodeWrap t))]
parseModule extensions source = case moduleExtensions of
  Left err -> Left err
  Right [extensions'] ->
     (if null extensions' then id
      else fmap $ fmap (rewrap $ Abstract.withLanguagePragma extensions'))
    $ parseResults $ Report.haskellModule
    $ parseComplete (extendedGrammar $ extensions <> Set.fromList extensions') source
  Right extensionses -> error (show extensionses)
  where moduleExtensions = getCompose . fmap snd . getCompose $ simply parsePrefix languagePragmas source
        parseResults = getCompose . fmap snd . getCompose

extendedGrammar :: (Abstract.ExtendedHaskell l, LexicalParsing (Parser (HaskellGrammar l t (NodeWrap t)) t),
                    Ord t, Show t, OutlineMonoid t,
                    Deep.Foldable (Serialization (Down Int) t) (Abstract.CaseAlternative l l),
                    Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l),
                    Deep.Foldable (Serialization (Down Int) t) (Abstract.Expression l l),
                    Deep.Foldable (Serialization (Down Int) t) (Abstract.GuardedExpression l l),
                    Deep.Foldable (Serialization (Down Int) t) (Abstract.Import l l),
                    Deep.Foldable (Serialization (Down Int) t) (Abstract.Statement l l),
                    Deep.Functor (DisambiguatorTrans t) (Abstract.CaseAlternative l l),
                    Deep.Functor (DisambiguatorTrans t) (Abstract.Declaration l l),
                    Deep.Functor (DisambiguatorTrans t) (Abstract.Expression l l),
                    Deep.Functor (DisambiguatorTrans t) (Abstract.GuardedExpression l l),
                    Deep.Functor (DisambiguatorTrans t) (Abstract.Import l l),
                    Deep.Functor (DisambiguatorTrans t) (Abstract.Statement l l))
                 => Set ExtensionSwitch -> Grammar (HaskellGrammar l t (NodeWrap t)) (ParserT ((,) [[Lexeme t]])) t
extendedGrammar extensions = fixGrammar (extended . Report.grammar)
   where extended = appEndo $ getDual $ foldMap (Dual . Endo) $ map snd $ sortOn fst
                    $ Map.elems $ Map.restrictKeys extensionMixins
                    $ Set.powerSet $ foldMap withImplications extensions
         withImplications :: ExtensionSwitch -> Set ExtensionSwitch
         withImplications switch@(Yes extension) = Set.insert switch (Map.findWithDefault mempty extension implications)
         withImplications switch = Set.singleton switch
         

grammar :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t,
                      Deep.Foldable (Serialization (Down Int) t) (Abstract.CaseAlternative l l),
                      Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l),
                      Deep.Foldable (Serialization (Down Int) t) (Abstract.Expression l l),
                      Deep.Foldable (Serialization (Down Int) t) (Abstract.GuardedExpression l l),
                      Deep.Foldable (Serialization (Down Int) t) (Abstract.Import l l),
                      Deep.Foldable (Serialization (Down Int) t) (Abstract.Statement l l),
                      Deep.Functor (DisambiguatorTrans t) (Abstract.CaseAlternative l l),
                      Deep.Functor (DisambiguatorTrans t) (Abstract.Declaration l l),
                      Deep.Functor (DisambiguatorTrans t) (Abstract.Expression l l),
                      Deep.Functor (DisambiguatorTrans t) (Abstract.GuardedExpression l l),
                      Deep.Functor (DisambiguatorTrans t) (Abstract.Import l l),
                      Deep.Functor (DisambiguatorTrans t) (Abstract.Statement l l))
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
                        <|> variableIdentifier}

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
  let integer, integerHash, integerHash2 :: (LexicalParsing (Parser g t), Show t, TextualMonoid t) => Parser g t Integer
      float, floatHash, floatHash2 :: (LexicalParsing (Parser g t), Show t, TextualMonoid t) => Parser g t Rational
      charLiteral, charHashLiteral :: (LexicalParsing (Parser g t), Show t, TextualMonoid t) => Parser g t Char
      stringLiteral, stringHashLiteral :: (LexicalParsing (Parser g t), Show t, TextualMonoid t) => Parser g t Text

      integer = token (integerLexeme <* notFollowedBy (string "#"))
      float = token (floatLexeme <* notFollowedBy (string "#"))
      charLiteral = token (charLexeme <* notFollowedBy (string "#"))
      stringLiteral = token (stringLexeme <* notFollowedBy (string "#")) <?> "string literal"

      integerHash = token (integerLexeme <* string "#" <* notFollowedBy (string "#"))
      floatHash = token (floatLexeme <* string "#" <* notFollowedBy (string "#"))
      integerHash2 = token (integerLexeme <* string "##")
      floatHash2 = token (floatLexeme <* string "##")
      charHashLiteral = token (charLexeme <* string "#")
      stringHashLiteral = token (stringLexeme <* string "#")
  in baseGrammar{
  lPattern = aPattern
              <|> Abstract.literalPattern
                  <$> wrap ((Abstract.integerLiteral . negate) <$ delimiter "-" <*> integer
                            <|> (Abstract.hashLiteral . Abstract.integerLiteral . negate)
                                <$ delimiter "-" <*> integerHash
                            <|> (Abstract.hashLiteral . Abstract.hashLiteral . Abstract.integerLiteral . negate)
                                <$ delimiter "-" <*> integerHash2)
              <|> Abstract.literalPattern
                  <$> wrap ((Abstract.floatingLiteral . negate) <$ delimiter "-" <*> float
                            <|> (Abstract.hashLiteral . Abstract.floatingLiteral . negate)
                                <$ delimiter "-" <*> floatHash
                            <|> (Abstract.hashLiteral . Abstract.hashLiteral . Abstract.floatingLiteral . negate)
                                <$ delimiter "-" <*> floatHash2)
              <|> Abstract.constructorPattern <$> wrap generalConstructor <*> some (wrap aPattern),
   literal = Abstract.integerLiteral <$> integer <|> Abstract.floatingLiteral <$> float
             <|> Abstract.charLiteral <$> charLiteral <|> Abstract.stringLiteral <$> stringLiteral
             <|> Abstract.hashLiteral
                 <$> (Abstract.integerLiteral <$> integerHash <|> Abstract.floatingLiteral <$> floatHash
                      <|> Abstract.charLiteral <$> charHashLiteral <|> Abstract.stringLiteral <$> stringHashLiteral)
             <|> Abstract.hashLiteral . Abstract.hashLiteral
                 <$> (Abstract.integerLiteral <$> integerHash2 <|> Abstract.floatingLiteral <$> floatHash2)}

recursiveDoMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t,
                               Deep.Foldable (Serialization (Down Int) t) (Abstract.Expression l l),
                               Deep.Foldable (Serialization (Down Int) t) (Abstract.Statement l l),
                               Deep.Functor (DisambiguatorTrans t) (Abstract.Expression l l),
                               Deep.Functor (DisambiguatorTrans t) (Abstract.Statement l l))
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
                             <* lift ([[Token Delimiter "-"]], ()))
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

hexFloatLiteralsMixin :: forall l g t. (Abstract.Haskell l, LexicalParsing (Parser g t), Ord t, Show t, TextualMonoid t)
                      => GrammarBuilder (HaskellGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
hexFloatLiteralsMixin baseGrammar@HaskellGrammar{..} = baseGrammar{
   integerLexeme = notFollowedBy ((string "0x" <|> string "0X")
                                 *> hexadecimal *> satisfyCharInput (`elem` ['.', 'p', 'P']))
                   *> integerLexeme,
   floatLexeme = (string "0x" <|> string "0X")
                 *> (readHexFloat <$> hexadecimal <* string "." <*> hexadecimal <*> (exponent <<|> pure 0)
                    <|> readHexFloat <$> hexadecimal <*> pure mempty <*> exponent)
                 <|> floatLexeme}
   where exponent =
           (string "p" <|> string "P")
           *> (id <$ string "+" <|> negate <$ string "-" <|> pure id)
           <*> (fst . head . Numeric.readDec . toString mempty <$> decimal)
         readHexFloat whole fraction exponent =
           fst (head $ Numeric.readHex $ toString mempty $ whole <> fraction)
           * 2 ^^ (exponent - Factorial.length fraction)

numericUnderscoresMixin :: forall l g t. (Abstract.Haskell l, LexicalParsing (Parser g t), Ord t, Show t, TextualMonoid t)
                        => GrammarBuilder (HaskellGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
numericUnderscoresMixin baseGrammar@HaskellGrammar{..} = baseGrammar{
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
