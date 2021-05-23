{-# Language FlexibleContexts, FlexibleInstances, OverloadedStrings,
             Rank2Types, RecordWildCards,
             ScopedTypeVariables, TemplateHaskell, TupleSections, TypeSynonymInstances #-}

-- | Missing syntax extensions:
-- * @QualifiedDo@ requires TemplateHaskell 2.17
-- * @TransformListComp@ is not supported by TemplateHaskell
-- * @MonadComprehensions@ implies @TransformListComp@ and is not syntactic by itself
-- * @MonadFailDesugaring@ is not syntactic
-- * @OverloadedLists@ is not syntactic
-- * @NoImplicitPrelude@ is not syntactic
-- * @RebindableSyntax@ is not syntactic
-- * @PostfixOperators@ is not syntactic

module Language.Haskell.Extensions.Grammar (grammar, extendedGrammar, module Report) where

import Control.Applicative
import qualified Data.Char as Char
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Ord (Down)
import Data.Monoid.Textual (TextualMonoid, characterPrefix, toString)
import qualified Data.Monoid.Textual as Textual
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Parser.Token (brackets, comma, parens)
import Text.Grampa
import Text.Grampa.ContextFree.LeftRecursive.Transformer (ParserT)
import qualified Transformation.Deep as Deep
import Witherable (filter, mapMaybe)

import qualified Language.Haskell.Extensions.Abstract as Abstract
import qualified Language.Haskell.Extensions.AST as AST (Language, Value(..))
import qualified Language.Haskell.Grammar as Report
import Language.Haskell.Grammar (HaskellGrammar(..), Parser, OutlineMonoid, DisambiguatorTrans, NodeWrap,
                                 blockOf, delimiter, rewrap, wrap, unwrap)
import Language.Haskell.Reserializer (Lexeme, Serialization)

import Prelude hiding (exponent, filter, null)

extendedGrammar :: (Abstract.ExtendedHaskell l, LexicalParsing (Parser (HaskellGrammar l (NodeWrap t)) t),
                    Ord t, Show t, OutlineMonoid t,
                    Deep.Foldable (Serialization (Down Int) t) (Abstract.CaseAlternative l l),
                    Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l),
                    Deep.Foldable (Serialization (Down Int) t) (Abstract.Expression l l),
                    Deep.Foldable (Serialization (Down Int) t) (Abstract.Import l l),
                    Deep.Foldable (Serialization (Down Int) t) (Abstract.Statement l l),
                    Deep.Functor (DisambiguatorTrans t) (Abstract.CaseAlternative l l),
                    Deep.Functor (DisambiguatorTrans t) (Abstract.Declaration l l),
                    Deep.Functor (DisambiguatorTrans t) (Abstract.Expression l l),
                    Deep.Functor (DisambiguatorTrans t) (Abstract.Import l l),
                    Deep.Functor (DisambiguatorTrans t) (Abstract.Statement l l))
                 => Grammar (HaskellGrammar l (NodeWrap t)) (ParserT ((,) [[Lexeme t]])) t
extendedGrammar = fixGrammar grammar

grammar :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t,
                      Deep.Foldable (Serialization (Down Int) t) (Abstract.CaseAlternative l l),
                      Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l),
                      Deep.Foldable (Serialization (Down Int) t) (Abstract.Expression l l),
                      Deep.Foldable (Serialization (Down Int) t) (Abstract.Import l l),
                      Deep.Foldable (Serialization (Down Int) t) (Abstract.Statement l l),
                      Deep.Functor (DisambiguatorTrans t) (Abstract.CaseAlternative l l),
                      Deep.Functor (DisambiguatorTrans t) (Abstract.Declaration l l),
                      Deep.Functor (DisambiguatorTrans t) (Abstract.Expression l l),
                      Deep.Functor (DisambiguatorTrans t) (Abstract.Import l l),
                      Deep.Functor (DisambiguatorTrans t) (Abstract.Statement l l))
        => GrammarBuilder (HaskellGrammar l (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
grammar = unicodeSyntaxMixin . identifierSyntaxMixin . magicHashMixin . parallelListComprehensionsMixin
          . recursiveDoMixin . tupleSectionsMixin . lambdaCaseMixin . emptyCaseMixin . Report.grammar

identifierSyntaxMixin :: forall l g t. (Abstract.Haskell l, LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t)
                      => GrammarBuilder (HaskellGrammar l (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
identifierSyntaxMixin baseGrammar = baseGrammar{
   variableIdentifier = token (Abstract.name . Text.pack . toString mempty <$> variableLexeme),
   constructorIdentifier = token (Abstract.name . Text.pack . toString mempty <$> constructorLexeme),
   variableSymbol = token (Abstract.name . Text.pack . toString mempty <$> Report.variableSymbolLexeme),
   constructorSymbol = token (Abstract.name . Text.pack . toString mempty <$> Report.constructorSymbolLexeme)}

unicodeSyntaxMixin :: forall l g t. (LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t)
                   => GrammarBuilder (HaskellGrammar l (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
unicodeSyntaxMixin baseGrammar = baseGrammar{
   doubleColon = doubleColon baseGrammar <|> delimiter "∷",
   rightDoubleArrow = rightDoubleArrow baseGrammar <|> delimiter "⇒",
   rightArrow = rightArrow baseGrammar <|> delimiter "→",
   leftArrow = leftArrow baseGrammar <|> delimiter "←"}

magicHashMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t)
               => GrammarBuilder (HaskellGrammar l (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
magicHashMixin baseGrammar@HaskellGrammar{..} = baseGrammar{
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
                 => GrammarBuilder (HaskellGrammar l (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
recursiveDoMixin baseGrammar = baseGrammar{
   dExpression = dExpression baseGrammar
                 <|> wrap (Abstract.mdoExpression <$ keyword "mdo" <*> wrap (statements baseGrammar)),
   statement = statement baseGrammar
               <|> Deep.InL
                   <$> wrap (Abstract.recursiveStatement
                             . (either id (rewrap Abstract.expressionStatement) . Deep.eitherFromSum . unwrap <$>)
                             <$ keyword "rec"
                             <*> blockOf (statement baseGrammar))}

parallelListComprehensionsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                              Ord t, Show t, OutlineMonoid t)
                                => GrammarBuilder (HaskellGrammar l (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
parallelListComprehensionsMixin baseGrammar@HaskellGrammar{..} = baseGrammar{
   bareExpression = Report.bareExpression baseGrammar
                    <|> brackets (Abstract.parallelListComprehension
                                  <$> expression <*> qualifiers <*> qualifiers <*> many qualifiers)}

tupleSectionsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                 Ord t, Show t, OutlineMonoid t)
                   => GrammarBuilder (HaskellGrammar l (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
tupleSectionsMixin baseGrammar@HaskellGrammar{..} = baseGrammar{
   bareExpression = Report.bareExpression baseGrammar
                    <|> Abstract.tupleSectionExpression
                           <$> parens ((:|) <$> optional expression <*> some (comma *> optional expression))}

lambdaCaseMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t), Ord t, Show t, TextualMonoid t)
                => GrammarBuilder (HaskellGrammar l (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
lambdaCaseMixin baseGrammar = baseGrammar{
   dExpression = dExpression baseGrammar
                 <|> wrap (Abstract.lambdaCaseExpression <$ (delimiter "\\" *> keyword "case")
                              <*> alternatives baseGrammar)}

emptyCaseMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t,
                             Deep.Foldable (Serialization (Down Int) t) (Abstract.CaseAlternative l l),
                             Deep.Functor (DisambiguatorTrans t) (Abstract.CaseAlternative l l))
                 => GrammarBuilder (HaskellGrammar l (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
emptyCaseMixin baseGrammar = baseGrammar{
   alternatives = blockOf (alternative baseGrammar)}

variableLexeme, constructorLexeme, identifierTail :: (Ord t, Show t, TextualMonoid t) => Parser g t t
variableLexeme = filter (`Set.notMember` Report.reservedWords) (satisfyCharInput varStart <> identifierTail)
                 <?> "variable"
   where varStart c = (Char.isLetter c && not (Char.isUpper c)) ||  c == '_'
constructorLexeme = satisfyCharInput Char.isUpper <> identifierTail <?> "constructor"
identifierTail = takeCharsWhile isNameTailChar <> concatAll (string "#") -- MagicHash

isNameTailChar :: Char -> Bool
isNameTailChar c = Report.isNameTailChar c || Char.isMark c

integer, integerHash, integerHash2 :: (LexicalParsing (Parser g t), Show t, TextualMonoid t) => Parser g t Integer
float, floatHash, floatHash2 :: (LexicalParsing (Parser g t), Show t, TextualMonoid t) => Parser g t Rational
charLiteral, charHashLiteral :: (LexicalParsing (Parser g t), Show t, TextualMonoid t) => Parser g t Char
stringLiteral, stringHashLiteral :: (LexicalParsing (Parser g t), Show t, TextualMonoid t) => Parser g t Text

integer = token (Report.integerLexeme <* notFollowedBy (string "#"))
float = token (Report.floatLexeme <* notFollowedBy (string "#"))
charLiteral = token (Report.charLexeme <* notFollowedBy (string "#"))
stringLiteral = token (Report.stringLexeme <* notFollowedBy (string "#")) <?> "string literal"

integerHash = token (Report.integerLexeme <* string "#" <* notFollowedBy (string "#"))
floatHash = token (Report.floatLexeme <* string "#" <* notFollowedBy (string "#"))
integerHash2 = token (Report.integerLexeme <* string "##")
floatHash2 = token (Report.floatLexeme <* string "##")
charHashLiteral = token (Report.charLexeme <* string "#")
stringHashLiteral = token (Report.stringLexeme <* string "#")


