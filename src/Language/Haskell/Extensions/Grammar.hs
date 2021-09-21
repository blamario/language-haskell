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
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty((:|)), toList)
import Data.Functor.Compose (Compose(Compose, getCompose))
import Data.Ord (Down)
import Data.String (IsString)
import Data.Monoid (Dual(..), Endo(..))
import Data.Monoid.Instances.Positioned (LinePositioned, extract)
import Data.Monoid.Textual (TextualMonoid, characterPrefix, toString)
import qualified Data.Monoid.Textual as Textual
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Rank2
import qualified Text.Parser.Char
import Text.Parser.Combinators (eof, sepBy)
import Text.Parser.Token (braces, brackets, comma, parens)
import Text.Grampa
import qualified Text.Grampa.ContextFree.SortedMemoizing as P (Parser)
import Text.Grampa.ContextFree.LeftRecursive.Transformer (ParserT, lift)
import qualified Transformation.Deep as Deep
import Witherable (filter, mapMaybe)

import Language.Haskell.Extensions (Extension(..), allExtensions, extensionsByName) 
import qualified Language.Haskell.Extensions.Abstract as Abstract
import qualified Language.Haskell.Extensions.AST as AST (Language, Value(..))
import qualified Language.Haskell.Grammar as Report
import Language.Haskell.Grammar (HaskellGrammar(..), Parser, OutlineMonoid, DisambiguatorTrans, NodeWrap,
                                 blockOf, delimiter, inputColumn, isSymbol,
                                 oneExtendedLine, rewrap, startSepEndBy, wrap, unwrap)
import qualified Language.Haskell.Disambiguator as Disambiguator
import Language.Haskell.Reserializer (Lexeme(..), Serialization)

import Prelude hiding (exponent, filter, null)

extensionMixins :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t,
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
                => Map Extension (Int, GrammarBuilder (HaskellGrammar l (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t)
extensionMixins =
  Map.fromList [
     (IdentifierSyntax,           (0, identifierSyntaxMixin)),
     (UnicodeSyntax,              (1, unicodeSyntaxMixin)),
     (MagicHash,                  (2, magicHashMixin)),
     (ParallelListComprehensions, (3, parallelListComprehensionsMixin)),
     (RecursiveDo,                (4, recursiveDoMixin)),
     (TupleSections,              (5, tupleSectionsMixin)),
     (EmptyCase,                  (6, emptyCaseMixin)),
     (LambdaCase,                 (7, lambdaCaseMixin)),
     (MultiWayIf,                 (8, multiWayIfMixin)),
     (BlockArguments,             (9, blockArgumentsMixin))]

pragma :: (Show t, TextualMonoid t) => Parser g t t
pragma = do open <- string "{-#" <> takeCharsWhile Char.isSpace
            content <- concatMany ((notFollowedBy (string "#-}") *> anyToken) <> takeCharsWhile (/= '#'))
            close <- string "#-}"
            lift ([[Comment $ open <> content <> close]], content)

languagePragma :: (Ord t, Show t, TextualMonoid t) => P.Parser g t (Set Extension)
languagePragma = spaceChars
                 *> admit (string "{-#" *> spaceChars *> filter isLanguagePragma (takeCharsWhile Char.isAlphaNum)
                           *> commit (spaceChars
                                      *> (Set.fromList <$> extension `sepBy` (string "," *> spaceChars) <* string "#-}")
                                      <* spaceChars)
                           <<|> comment *> commit languagePragma
                           <<|> commit (pure mempty))
   where spaceChars = takeCharsWhile Char.isSpace
         isLanguagePragma pragmaName = Text.toUpper (Textual.toText mempty pragmaName) == "LANGUAGE"
         extension = do extensionName <- takeCharsWhile Char.isAlphaNum
                        spaceChars
                        case Map.lookup extensionName extensionsByName of
                           Just ext -> pure ext
                           Nothing -> fail ("Unknown language extension " <> toString mempty extensionName)
         comment = string "--" <* takeCharsWhile Report.isLineChar <|> blockComment
         blockComment = string "{-"
                        *> skipMany (blockComment
                                     <|> notFollowedBy (string "-}") *> anyToken <> takeCharsWhile  (/= '-'))
                        *> string "-}"

parseModule :: forall l t p. (Abstract.ExtendedHaskell l, LexicalParsing (Parser (HaskellGrammar l (NodeWrap t)) t),
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
            => Set Extension -> t
            -> ParseResults t [NodeWrap t (Abstract.Module l l (NodeWrap t) (NodeWrap t))]
parseModule extensions source = case moduleExtensions of
               Left err -> Left err
               Right [extensions'] -> parseResults $ Report.haskellModule
                                      $ parseComplete (extendedGrammar $ extensions <> extensions') source
               Right extensionses -> error (show extensionses)
   where moduleExtensions = getCompose . fmap snd . getCompose $ simply parsePrefix languagePragma source
         parseResults = getCompose . fmap snd . getCompose

extendedGrammar :: (Abstract.ExtendedHaskell l, LexicalParsing (Parser (HaskellGrammar l (NodeWrap t)) t),
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
                 => Set Extension -> Grammar (HaskellGrammar l (NodeWrap t)) (ParserT ((,) [[Lexeme t]])) t
extendedGrammar extensions = fixGrammar (extended . Report.grammar)
   where extended = appEndo $ getDual $ foldMap (Dual . Endo) $ map snd $ sortOn fst
                    $ Map.elems $ Map.restrictKeys extensionMixins extensions

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
        => GrammarBuilder (HaskellGrammar l (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
grammar = blockArgumentsMixin . unicodeSyntaxMixin . identifierSyntaxMixin . magicHashMixin
          . parallelListComprehensionsMixin . recursiveDoMixin . tupleSectionsMixin . lambdaCaseMixin . emptyCaseMixin
          . multiWayIfMixin . Report.grammar

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
   closedBlockExpresion = closedBlockExpresion baseGrammar
                          <|> Abstract.lambdaCaseExpression <$ (delimiter "\\" *> keyword "case")
                                                            <*> alternatives baseGrammar}

emptyCaseMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t,
                             Deep.Foldable (Serialization (Down Int) t) (Abstract.CaseAlternative l l),
                             Deep.Functor (DisambiguatorTrans t) (Abstract.CaseAlternative l l))
                 => GrammarBuilder (HaskellGrammar l (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
emptyCaseMixin baseGrammar = baseGrammar{
   alternatives = blockOf (alternative baseGrammar)}

multiWayIfMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t,
                             Deep.Foldable (Serialization (Down Int) t) (Abstract.GuardedExpression l l),
                             Deep.Functor (DisambiguatorTrans t) (Abstract.GuardedExpression l l))
                => GrammarBuilder (HaskellGrammar l (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
multiWayIfMixin baseGrammar@HaskellGrammar{..} = baseGrammar{
   closedBlockExpresion = Report.closedBlockExpresion baseGrammar
                          <|> Abstract.multiWayIfExpression <$ keyword "if"
                                  <*> blockOf'
                                         (Abstract.guardedExpression . toList
                                             <$> guards <* rightArrow
                                             <*> expression)}

blockArgumentsMixin :: forall l g t. (Abstract.ExtendedHaskell l, LexicalParsing (Parser g t),
                                  Ord t, Show t, OutlineMonoid t,
                                  Deep.Foldable (Serialization (Down Int) t) (Abstract.GuardedExpression l l),
                                  Deep.Functor (DisambiguatorTrans t) (Abstract.GuardedExpression l l))
                    => GrammarBuilder (HaskellGrammar l (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
blockArgumentsMixin baseGrammar@HaskellGrammar{..} = baseGrammar{
   lExpression = Report.lExpression baseGrammar
                 <|> wrap (Abstract.applyExpression <$> fExpression <*> wrap openBlockExpression),
   dExpression = fExpression,
   bareExpression = Report.bareExpression baseGrammar <|> closedBlockExpresion}

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
