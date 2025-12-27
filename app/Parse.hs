{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
             OverloadedRecordDot, QuantifiedConstraints, RankNTypes, RecordWildCards,
             ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}

module Main where

import Language.Haskell (Bound, Input, Placed)
import qualified Language.Haskell as Haskell
import qualified Language.Haskell.Extensions as Extensions
import Language.Haskell.Extensions.AST (Language)
import qualified Language.Haskell.Extensions.Abstract as Abstract
import qualified Language.Haskell.Extensions.AST as AST
import qualified Language.Haskell.Binder as Binder
import qualified Language.Haskell.Grammar (HaskellGrammar(expression))
import qualified Language.Haskell.Extensions.Grammar as Grammar
import qualified Language.Haskell.Extensions.Reformulator as Reformulator
import Language.Haskell.Extensions.Reformulator (ReformulationOf)
import Language.Haskell.Extensions.Translation (FullyTranslatable)
import qualified Language.Haskell.Extensions.Verifier as Verifier
import qualified Language.Haskell.Reorganizer as Reorganizer
import qualified Language.Haskell.Reserializer as Reserializer
import qualified Language.Haskell.Template as Template

import qualified Rank2
import qualified Transformation
import qualified Transformation.AG as AG
import qualified Transformation.AG.Dimorphic as Di
import qualified Transformation.AG.Generics as AG (Auto)
import qualified Transformation.Rank2 as Rank2
import qualified Transformation.Deep as Deep
import qualified Transformation.Full as Full

import Control.Monad
import Data.Data (Data)
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.Functor.Compose (Compose(..))
import Data.Maybe (fromMaybe)
import Data.Monoid.Instances.PrefixMemory (content)
import Data.Monoid.Textual (fromText)
import Data.Ord (Down)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Text.IO (getLine, readFile, getContents)
import qualified Data.Text.IO as Text
import Data.Typeable (Typeable)
import Options.Applicative
import Text.Grampa (ParseResults, parseComplete, failureDescription)
import Text.Parser.Input.Position (offset)
import ReprTree (reprTreeString)

import Prelude hiding (getLine, getContents, readFile)

data GrammarMode = ModuleMode | ExpressionMode
    deriving Show

data Output = Original | Plain | Pretty | Tree
            deriving Show

data Stage = Parsed | Bound | Resolved | Verified
    deriving Show

data Opts = Opts
    { optsMode         :: GrammarMode
    , optsIndex        :: Int
    , optsReformulate  :: [Extensions.Extension]
    , optsOutput       :: Output
    , optsStage        :: Stage
    , optsInclude      :: Maybe FilePath
    , optsFile         :: Maybe FilePath
    } deriving Show

main :: IO ()
main = execParser opts >>= main'
  where
    opts = info (helper <*> p)
        ( fullDesc
       <> progDesc "Parse a Haskell file, or parse interactively"
       <> header "Haskell parser")

    p :: Parser Opts
    p = Opts
        <$> mode
        <*> (option auto (long "index" <> help "Index of ambiguous parse" <> showDefault <> value 0 <> metavar "INT"))
        <*> many (option auto (long "reformulate" <> help "Reformulate and eliminate given language extensions"))
        <*> (flag' Pretty (long "pretty" <> help "Pretty-print output")
             <|> flag' Tree (long "tree" <> help "Print the output as an abstract syntax tree")
             <|> flag' Original (long "original" <> help "Print the output with the original tokens and whitespace")
             <|> flag' Plain (long "plain" <> help "Print the output as a long Haskell expression")
             <|> pure Plain)
        <*> (flag' Parsed (long "parsed" <> help "show raw ambiguous parse tree")
             <|> flag' Bound (long "bound" <> help "show ambiguous parse tree with identifier bindings")
             <|> flag' Resolved (long "resolved" <> help "show parse tree with all ambiguities resolved")
             <|> flag' Verified (long "verified" <> help "show resolved parse tree with all extensions verified")
             <|> pure Verified)
        <*> optional (strOption (short 'i' <> long "include" <> metavar "DIRECTORY"
                                 <> help "Where to look for imports"))
        <*> optional (strArgument
            ( metavar "FILE"
              <> help "Haskell file to parse"))

    mode :: Parser GrammarMode
    mode = ModuleMode          <$ switch (long "module")
       <|> ExpressionMode      <$ switch (long "expression")

main' :: Opts -> IO ()
main' Opts{..} = do
   (preludeBindings :: Binder.Environment Language) <- Haskell.preludeBindings
   (predefinedModules :: Binder.ModuleEnvironment Language) <- Haskell.predefinedModuleBindings
   includeModules <- traverse (Haskell.directoryModuleBindings preludeBindings predefinedModules) optsInclude
   let importableModules = fromMaybe predefinedModules includeModules
       go :: (Data a, Show a, Template.PrettyViaTH a, Typeable g,
              a ~ g l l Bound Bound, l ~ Language, w ~ Grammar.NodeWrap Input,
              e ~ Binder.WithEnvironment Language w,
              Abstract.ExtendedHaskell l,
              Abstract.QualifiedName l ~ AST.QualifiedName l,
              Show (Di.Atts (Binder.Environment Language) (Binder.LocalEnvironment Language)),
              Data (g l l [] []),
              Show (g Language Language e e), Show (g Language Language w w),
              Transformation.At (Verifier.Verification l Int Text) (g l l Bound Bound),
              Transformation.At (Binder.BindingVerifier l Placed) (g l l Bound Bound),
              forall f. Rank2.Functor (g l l f),
              Rank2.Apply (g l l (AG.Semantics (AG.Keep (AG.Auto (Binder.Binder l w))))),
              Rank2.Traversable (g l l (AG.Semantics (AG.Keep (AG.Auto (Binder.Binder l w))))),
              AG.At (AG.Auto (Binder.Binder l w)) (g l l),
              AG.Atts (AG.Synthesized (Binder.Binder Language (Grammar.NodeWrap Input))) (g l l)
              ~ (x, Binder.LocalEnvironment l),
              Deep.Functor (AG.Knit (AG.Keep (AG.Auto (Binder.Binder l w)))) (g l l),
              Deep.Functor (Rank2.Map (AG.Kept (AG.Auto (Binder.Binder l w))) (Binder.WithEnvironment l w)) (g l l),
              Full.Traversable (Reorganizer.Reorganization l (Down Int) Input) (g l l),
              FullyTranslatable
                 (ReformulationOf
                     (Extensions.On 'Extensions.RecordWildCards) '[ Extensions.On 'Extensions.NamedFieldPuns]
                     Language Language Int Text)
                 g,
              FullyTranslatable
                 (ReformulationOf
                     (Extensions.On 'Extensions.NPlusKPatterns) '[ Extensions.On 'Extensions.ViewPatterns]
                     Language Language Int Text)
                 g,
              FullyTranslatable
                 (ReformulationOf
                     (Extensions.On 'Extensions.OrPatterns)
                     '[ Extensions.On 'Extensions.ViewPatterns, Extensions.On 'Extensions.LambdaCase]
                     Language Language Int Text)
                 g,
              FullyTranslatable
                 (ReformulationOf (Extensions.Off 'Extensions.ListTuplePuns) '[ ] Language Language Int Text)
                 g,
              FullyTranslatable
                 (ReformulationOf (Extensions.On 'Extensions.TupleSections) '[ ] Language Language Int Text)
                 g,
              Deep.Functor (Rank2.Map (Compose ((,) (Binder.Attributes Language)) Placed) []) (g l l),
              Deep.Functor (Rank2.Map (Compose ((,) (Binder.Attributes Language)) (Grammar.NodeWrap Input)) []) (g l l),
              Deep.Functor (Rank2.Map ((,) (Down Int, Reserializer.ParsedLexemes Input, Down Int)) []) (g l l),
              Deep.Functor (Rank2.Map (Reserializer.Wrapped (Down Int) Input)
                                      (Reserializer.Wrapped (Down Int) Text)) (g l l),
              Deep.Functor
                 (Transformation.Mapped
                    ((,) (Di.Atts (Binder.Environment l) (Binder.LocalEnvironment l)))
                    (Rank2.Map (Reserializer.Wrapped (Down Int) Input) Placed))
                 (g l l),
              Deep.Foldable (Binder.BindingVerifier l Placed) (g l l),
              Deep.Foldable (Reserializer.Serialization Int Text) (g l l),
              Deep.Foldable (Reserializer.Serialization (Down Int) Input) (g l l),
              Deep.Foldable (Verifier.Verification l Int Text) (g l l),
              Deep.Foldable
                 (Transformation.Folded
                    ((,) (Di.Atts (Binder.Environment l) (Binder.LocalEnvironment l)))
                    (Reserializer.Serialization Int Text))
                 (g l l))
          => (Input -> ParseResults Input [w (g l l w w)])
          -> String -> Text -> IO ()
       go parser _filename contents = report contents (parser $ fromText contents)
       report :: forall g l a e w x.
                 (Data a, Show a, Template.PrettyViaTH a, Typeable g,
                  a ~ Bound (g l l Bound Bound), l ~ Language, w ~ Grammar.NodeWrap Input,
                  e ~ Binder.WithEnvironment Language w,
                  Abstract.ExtendedHaskell l,
                  Abstract.QualifiedName l ~ AST.QualifiedName l,
                  Show (Di.Atts (Binder.Environment Language) (Binder.LocalEnvironment Language)),
                  Data (g l l [] []),
                  Show (g Language Language e e), Show (g Language Language w w),
                  Transformation.At (Verifier.Verification l Int Text) (g l l Bound Bound),
                  Transformation.At (Binder.BindingVerifier l Placed) (g l l Bound Bound),
                  forall f. Rank2.Functor (g l l f),
                  Rank2.Apply (g l l (AG.Semantics (AG.Keep (AG.Auto (Binder.Binder l w))))),
                  Rank2.Traversable (g l l (AG.Semantics (AG.Keep (AG.Auto (Binder.Binder l w))))),
                  AG.At (AG.Auto (Binder.Binder l w)) (g l l),
                  AG.Atts (AG.Synthesized (AG.Auto (Binder.Binder Language (Grammar.NodeWrap Input)))) (g l l)
                  ~ (x, Binder.LocalEnvironment l),
                  Deep.Functor (AG.Knit (AG.Keep (AG.Auto (Binder.Binder l w)))) (g l l),
                  Deep.Functor (Rank2.Map (AG.Kept (AG.Auto (Binder.Binder l w))) (Binder.WithEnvironment l w)) (g l l),
                  Full.Traversable (Reorganizer.Reorganization l (Down Int) Input) (g l l),
                  FullyTranslatable
                     (ReformulationOf
                         (Extensions.On 'Extensions.RecordWildCards) '[ Extensions.On 'Extensions.NamedFieldPuns]
                         Language Language Int Text)
                     g,
                  FullyTranslatable
                     (ReformulationOf
                         (Extensions.On 'Extensions.NPlusKPatterns) '[ Extensions.On 'Extensions.ViewPatterns]
                         Language Language Int Text)
                     g,
                  FullyTranslatable
                     (ReformulationOf
                         (Extensions.On 'Extensions.OrPatterns)
                         '[ Extensions.On 'Extensions.ViewPatterns, Extensions.On 'Extensions.LambdaCase]
                         Language Language Int Text)
                     g,
                  FullyTranslatable
                     (ReformulationOf (Extensions.Off 'Extensions.ListTuplePuns) '[ ] Language Language Int Text)
                     g,
                  FullyTranslatable
                     (ReformulationOf (Extensions.On 'Extensions.TupleSections) '[ ] Language Language Int Text)
                     g,
                  Deep.Functor (Rank2.Map (Compose ((,) (Binder.Attributes Language)) Placed) []) (g l l),
                  Deep.Functor (Rank2.Map (Compose ((,) (Binder.Attributes Language)) (Grammar.NodeWrap Input)) []) (g l l),
                  Deep.Functor (Rank2.Map ((,) (Down Int, Reserializer.ParsedLexemes Input, Down Int)) []) (g l l),
                  Deep.Functor (Rank2.Map (Reserializer.Wrapped (Down Int) Input)
                                          (Reserializer.Wrapped (Down Int) Text)) (g l l),
                  Deep.Functor
                     (Transformation.Mapped
                        ((,) (Di.Atts (Binder.Environment l) (Binder.LocalEnvironment l)))
                        (Rank2.Map (Reserializer.Wrapped (Down Int) Input) Placed))
                     (g l l),
                  Deep.Foldable (Binder.BindingVerifier l Placed) (g l l),
                  Deep.Foldable (Reserializer.Serialization Int Text) (g l l),
                  Deep.Foldable (Reserializer.Serialization (Down Int) Input) (g l l),
                  Deep.Foldable (Verifier.Verification l Int Text) (g l l),
                  Deep.Foldable
                     (Transformation.Folded
                        ((,) (Di.Atts (Binder.Environment l) (Binder.LocalEnvironment l)))
                        (Reserializer.Serialization Int Text))
                     (g l l))
              => Text -> ParseResults Input [w (g l l w w)] -> IO ()
       report contents (Right [parsed]) = case optsOutput of
          Original -> case optsStage of
             Parsed -> Text.putStr (content $ Reserializer.reserialize parsed)
             Bound -> Text.putStr $ Reserializer.reserializeNested $ Transformation.Mapped (Rank2.Map rewrap) Full.<$> bound
             Resolved -> Text.putStr $ Reserializer.reserializeNested reformulated
             Verified -> verifyBefore (Text.putStr . Reserializer.reserializeNested)
          Plain -> case optsStage of
             Parsed -> print parsed
             Bound -> print bound
             Resolved -> print reformulated
             Verified -> verifyBefore print
          Pretty -> case optsStage of
            Resolved -> putStrLn $ Template.pprint reformulated
            Verified -> verifyBefore (putStrLn . Template.pprint)
            _ -> error "Can't pretty-print an unresolved parse tree."
          Tree -> case optsStage of
             Parsed -> printTree parsed
             Bound -> printTree bound
             Resolved -> printTree reformulated
             Verified -> verifyBefore printTree
          where verifyBefore :: (a -> IO ()) -> IO ()
                verifyBefore action = case Verifier.verify mempty reformulated of
                   [] -> let unbounds = Binder.unboundNames reformulated
                         in if unbounds == mempty then action reformulated
                            else print unbounds
                   errors -> mapM_ (putStrLn . show) errors
                t :: Verifier.Verification l Int Text
                t = Verifier.Verification
                resolved, reformulated :: Bound (g l l Bound Bound)
                reformulated = foldr reformulate resolved (reverse optsReformulate)
                resolved = Haskell.resolvePositions defaultExtensions importableModules preludeBindings contents parsed
                bound = Binder.withBindings defaultExtensions importableModules preludeBindings parsed
                rewrap :: forall a. Reserializer.Wrapped (Down Int) Input a -> Reserializer.Wrapped Int Text a
                rewrap = Reserializer.mapWrapping (offset contents) content
                reformulate :: Extensions.Extension -> Bound (g l l Bound Bound) -> Bound (g l l Bound Bound)
                reformulate Extensions.NPlusKPatterns = Reformulator.dropNPlusKPatterns
                reformulate Extensions.RecordWildCards = Reformulator.dropRecordWildCards
                reformulate Extensions.TupleSections = Reformulator.dropTupleSections
                reformulate Extensions.ViewPatterns = Reformulator.orToViewPatterns
                reformulate ext = error ("Can't reformulate " <> show ext <> " yet")
                printTree :: forall w. (Data (g l l [] []), Foldable w, Functor w, Full.Functor (Rank2.Map w []) (g l l))
                          => w (g l l w w) -> IO ()
                printTree = putStrLn . reprTreeString . unwrap
                   where unwrap :: w (g l l w w) -> [g l l [] []]
                         unwrap = (Rank2.Map toList Full.<$>)
       report contents (Right l) =
          putStrLn ("Ambiguous: " ++ show optsIndex ++ "/" ++ show (length l) ++ " parses")
          >> report contents (Right [l !! optsIndex])
       report contents (Left err) = Text.putStrLn (failureDescription contents (content <$> err) 4)
   case optsFile of
      Just file -> (if file == "-" then getContents else readFile file)
                   >>= go parseModule file
      Nothing ->
         forever $
         getLine >>=
         case optsMode of
            ModuleMode     -> go parseModule "<stdin>"
            ExpressionMode -> go parseExpression "<stdin>"
   where
      parseModule :: (Abstract.ExtendedHaskell l,
                      Abstract.DeeplyFoldable (Reserializer.Serialization (Down Int) Input) l,
                      w ~ Grammar.NodeWrap Input)
                  => Input -> ParseResults Input [w (Abstract.Module l l w w)]
      parseModule = Grammar.parseModule defaultExtensions
      parseExpression :: forall l w. (Abstract.ExtendedHaskell l,
                                      Abstract.DeeplyFoldable (Reserializer.Serialization (Down Int) Input) l,
                                      w ~ Grammar.NodeWrap Input)
                      => Input -> ParseResults Input [w (Abstract.Expression l l w w)]
      parseExpression t =
        ((parseComplete (Grammar.extendedGrammar @l Extensions.allExtensions) t).report.expression.getCompose
         <&> snd).getCompose
      defaultExtensions = Map.fromSet (const True) Extensions.includedByDefault

type NodeWrap = ((,) Int)

instance (Rank2.Functor (g p), Deep.Functor (Rank2.Map p []) g, Functor p) => Full.Functor (Rank2.Map p []) g where
  (<$>) = Full.mapUpDefault
