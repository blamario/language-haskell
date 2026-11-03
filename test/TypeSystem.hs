{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedRecordDot,
             OverloadedStrings, QuantifiedConstraints, ScopedTypeVariables, TypeApplications, TypeFamilies,
             TypeOperators, UndecidableInstances #-}

-- | Tests of 'Language.Haskell.TypeSystem', the type checker and inferencer.
--
-- Every test states the type the checker /ought/ to infer. The tests whose expectation the checker does not meet yet
-- are declared with 'brokenExpression' instead of 'expression'; such a test fails if the checker ever starts meeting
-- the expectation, which is the signal to promote it to a regular test.

module Main where

import Control.Applicative (ZipList(ZipList))
import Control.Exception (SomeException, evaluate, try)
import Data.Functor.Compose (Compose, getCompose)
import Data.Functor.Identity (Identity(Identity))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid.Instances.PrefixMemory (content)
import Data.Monoid.Textual (fromText)
import Data.Ord (Down)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.ZipNonEmpty (ZipNonEmpty(ZipNonEmpty))
import Test.Tasty (TestTree, defaultMain, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import Text.Grampa (ParseResults, failureDescription, parseComplete)

import qualified Transformation.Full as Full
import qualified Transformation.Rank2 as Rank2

import qualified Language.Haskell as Haskell
import Language.Haskell (Input, Parsed, Placed)
import qualified Language.Haskell.Binder as Binder
import qualified Language.Haskell.Extensions as Extensions
import qualified Language.Haskell.Extensions.Abstract as Abstract
import qualified Language.Haskell.Extensions.AST as AST
import Language.Haskell.Extensions.AST (Language)
import qualified Language.Haskell.Extensions.Grammar as Grammar
import qualified Language.Haskell.Grammar as Report (HaskellGrammar(expression))
import qualified Language.Haskell.Reserializer as Reserializer
import qualified Language.Haskell.Template as Template
import qualified Language.Haskell.TypeSystem as TypeSystem
import Language.Haskell.TypeSystem.Transformations (substituteTypeVariables)

main :: IO ()
main = do
   predefinedModules <- Haskell.predefinedModuleBindings
   preludeBindings <- Haskell.preludeBindings
   defaultMain $ localOption (mkTimeout 10_000_000) $ testGroup "type system" $
      testGroup "substitution" substitutionTests
      : expressionTests (inferExpression predefinedModules preludeBindings)

-- * Tests of the type variable substitution underlying the constraint solver

substitutionTests :: [TestTree]
substitutionTests = [
   substitution "a bare variable" [("a", char)] (var "a") "Char",
   substitution "a variable the map does not bind" [("b", char)] (var "a") "a",
   substitution "inside a function type" [("a", char)] (var "a" --> var "a") "Char -> Char",
   substitution "inside a list type" [("a", char)] (listOf $ var "a") "[Char]",
   substitution "inside a tuple type" [("a", char), ("b", int)] (tupleOf [var "a", var "b"])
      "(Char, Int)",
   substitution "inside a type application" [("a", char)] (applyTo (var "f") (var "a")) "f Char",
   substitution "chained through another variable" [("a", listOf $ var "b"), ("b", char)] (var "a")
      "[Char]",
   substitution "cyclic, left in place rather than diverging" [("a", listOf $ var "a")] (var "a") "[a]",
   substitution "not applied to a variable bound by a forall" [("a", char)] (quantify ["a"] $ var "a" --> var "a")
      "forall a . a -> a",
   substitution "applied to a variable free under a forall" [("b", char)] (quantify ["a"] $ var "a" --> var "b")
      "forall a . a -> Char"]

substitution :: String -> [(Text, Ty)] -> Ty -> Expectation -> TestTree
substitution name bindings subject expected = testCase name $ do
   let substituted = Template.showViaTH
                     $ substituteTypeVariables (Map.fromList [(Abstract.name v, t) | (v, t) <- bindings]) subject
   if substituted == expected then pure ()
   else assertFailure ("expected " <> show expected <> ", substituted into " <> show substituted)

-- * The test cases

expressionTests :: (Text -> IO String) -> [TestTree]
expressionTests infer = [
   testGroup "literals" [
      expression infer "'x'" "Prelude.Char",
      expression infer "\"abc\"" "Prelude.String",
      expression infer "1" "Prelude.Num a => a",
      expression infer "1.5" "Prelude.Fractional a => a"],
   testGroup "lambda" [
      expression infer "\\x -> x" "aa -> aa",
      expression infer "\\x -> \\y -> x" "aa -> axa -> aa",
      brokenExpression "the occurs check is not performed, so the infinite type is accepted"
         infer "\\x -> x x" typeError],
   testGroup "references" [
      expression infer "not" "Bool -> Bool",
      expression infer "undefinedName" typeError],
   testGroup "constraint solving" [
      brokenExpression "the equality constraints collected by unification are never solved"
         infer "not True" "Bool",
      brokenExpression "the equality constraints collected by unification are never solved"
         infer "\\x -> not x" "Bool -> Bool",
      brokenExpression "the equality constraints collected by unification are never solved"
         infer "[1, 2]" "Prelude.Num aa => [aa]",
      brokenExpression "the equality constraints collected by unification are never solved"
         infer "(\\x -> x) (\\y -> y)" "aa -> aa",
      brokenExpression "the equality constraints collected by unification are never solved"
         infer "let f x = x in f" "aa -> aa",
      expression infer "(1, \"a\")" "Prelude.Num aa => (aa, Prelude.String)"],
   testGroup "unimplemented syntax" [
      brokenExpression "AST.Pattern attribution covers only variable, wildcard, and literal patterns"
         infer "\\(x, y) -> x" "(aa, ba) -> aa",
      brokenExpression "AST.Pattern attribution covers only variable, wildcard, and literal patterns"
         infer "\\[x] -> x" "[aa] -> aa",
      brokenExpression "AST.CaseAlternative attribution looks the scrutinee up under the wrong key"
         infer "case 'x' of c -> c" "Prelude.Char",
      brokenExpression "AST.Constructor attribution covers only references and the empty list"
         infer "(,) 'a' 'b'" "(Prelude.Char, Prelude.Char)",
      brokenExpression "AST.Expression attribution has no list comprehension case"
         infer "[x | x <- \"abc\"]" "Prelude.String"],
   testGroup "polymorphic environment" [
      brokenExpression "instantiating a quantified type from the environment calls replaceTypeVar = undefined"
         infer "id" "aa -> aa",
      brokenExpression "instantiating a quantified type from the environment calls replaceTypeVar = undefined"
         infer "id 'x'" "Prelude.Char"]]

-- * The environment the test expressions are checked against

-- | A tiny stand-in for the @Prelude@ type environment.
testValueBindings :: Map (AST.QualifiedName Language) Ty
testValueBindings = Map.fromList $ concat [
   binding "True" bool,
   binding "False" bool,
   binding "not" (bool --> bool),
   binding "ord" (char --> int),
   -- The type constructors are duplicated here because AST.ConstructorType resolves its constructor in the value
   -- namespace; see the AST.Constructor attribution in Language.Haskell.TypeSystem.
   binding "Bool" bool,
   binding "Char" char,
   binding "Int" int,
   binding "id" (forAll ["a"] (var "a" --> var "a"))]

testTypeBindings :: Map (AST.QualifiedName Language) Ty
testTypeBindings = Map.fromList $ concat [binding "Bool" bool, binding "Char" char, binding "Int" int]

type Ty = AST.Type Language Language Identity Identity

-- | Binds the name both unqualified and qualified by @Prelude@, as the checker looks names up as they are written.
binding :: Text -> Ty -> [(AST.QualifiedName Language, Ty)]
binding n t = [(Abstract.qualifiedName Nothing name, t),
               (Abstract.qualifiedName (Just Abstract.preludeName) name, t)]
   where name = Abstract.name n

infixr 1 -->
(-->) :: Ty -> Ty -> Ty
a --> b = AST.FunctionType (Identity a) (Identity b)

var :: Text -> Ty
var = AST.TypeVariable . Abstract.name

listOf :: Ty -> Ty
listOf = AST.ListType . Identity

tupleOf :: [Ty] -> Ty
tupleOf items = AST.TupleType (ZipNonEmpty $ Identity <$> NonEmpty.fromList items)

applyTo :: Ty -> Ty -> Ty
applyTo f a = AST.TypeApplication (Identity f) (Identity a)

-- | Quantifies the type without the context 'forAll' adds.
quantify :: [Text] -> Ty -> Ty
quantify vars body = AST.ForallType (ZipList $ Identity . bind <$> vars) (Identity body)
   where bind v = AST.ImplicitlyKindedTypeVariable False (Abstract.name v)

-- | Quantifies the type the way 'Language.Haskell.TypeSystem.checkModule' does, with an explicit context, so that
-- the checker recognizes it as a type to instantiate.
forAll :: [Text] -> Ty -> Ty
forAll vars body =
   AST.ForallType (ZipList $ Identity . bind <$> vars) (Identity $ AST.ConstrainedType (Identity AST.NoContext) (Identity body))
   where bind v = AST.ImplicitlyKindedTypeVariable False (Abstract.name v)

bool, char, int :: Ty
bool = conType "Bool"
char = conType "Char"
int = conType "Int"

conType :: Text -> Ty
conType =
   AST.ConstructorType . Identity . Abstract.constructorReference . Abstract.qualifiedName Nothing . Abstract.name

-- * The test harness

-- | The expected rendering of an inferred type, or 'typeError' if the checker is expected to reject the expression.
type Expectation = String

typeError :: Expectation
typeError = "<type error>"

-- | A test of the type inferred for the given expression.
expression :: (Text -> IO String) -> Text -> Expectation -> TestTree
expression infer source expected = testCase (Text.unpack source) $ do
   inferred <- infer source
   if inferred == expected then pure ()
   else assertFailure ("expected " <> show expected <> ", inferred " <> show inferred)

-- | A test of an expectation the type checker fails to meet, for the given reason. The test fails once the checker
-- does meet the expectation, which is the signal to turn the test into a regular 'expression' test.
brokenExpression :: String -> (Text -> IO String) -> Text -> Expectation -> TestTree
brokenExpression reason infer source expected = testCase (Text.unpack source) $ do
   inferred <- try @SomeException (infer source >>= evaluate . forceString)
   case inferred of
      Right actual | actual == expected ->
         assertFailure ("The known limitation is gone, promote this test to a regular one.\n"
                        <> "Known limitation: " <> reason)
      _ -> pure ()
   where forceString s = length s `seq` s

-- | Parses, resolves, and type-checks the given expression, rendering either the inferred type or 'typeError'.
inferExpression :: Binder.ModuleEnvironment Language -> Binder.Environment Language -> Text -> IO String
inferExpression modEnv env source = case parseExpression modEnv env source of
   Left err -> assertFailure err
   Right parsed ->
      pure $ either (const typeError) Template.showViaTH
           $ TypeSystem.checkExpression defaultExtensions testTypeBindings testValueBindings parsed

parseExpression :: Binder.ModuleEnvironment Language -> Binder.Environment Language -> Text
                -> Either String (Placed (AST.Expression Language Language Placed Placed))
parseExpression modEnv env source = case parseExpressionSource (fromText source) of
   Left err -> Left (Text.unpack $ failureDescription source (content <$> err) 4)
   Right [parsed] -> Right (unbind $ Haskell.resolvePositions defaultExtensions modEnv env source parsed)
   Right trees -> Left (show (length trees) <> " ambiguous parses of " <> show source)
   where unbind = (Full.Outward (Rank2.Map $ snd . getCompose) Full.<$>)

parseExpressionSource :: forall l w. (Abstract.ExtendedHaskell l,
                                      Abstract.DeeplyFoldable (Reserializer.Serialization (Down Int) Input) l,
                                      w ~ Parsed)
                      => Input -> ParseResults Input [w (Abstract.Expression l l w w)]
parseExpressionSource t = getCompose (snd <$> getCompose grammar.report.expression)
   -- The type of the parsed grammar has to be spelled out because 'parseComplete' returns it under the
   -- non-injective @ResultFunctor@ type family.
   where grammar :: Grammar.ExtendedGrammar l Input w
                       (Compose (Compose (ParseResults Input) []) ((,) [[Reserializer.Lexeme Input]]))
         grammar = parseComplete (Grammar.extendedGrammar @l Extensions.allExtensions) t

defaultExtensions :: Map Extensions.Extension Bool
defaultExtensions = Map.fromSet (const True) Extensions.includedByDefault
