{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings,
             ScopedTypeVariables, StandaloneDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Language.Haskell.Resolver where

import Control.Applicative ((<|>))
import Data.Either (partitionEithers)
import Data.Either.Validation (Validation(..), validationToEither)
import Data.Functor.Compose (Compose(..))
import Data.List.NonEmpty (NonEmpty(..), fromList, toList, nonEmpty)
import qualified Data.Map.Lazy as Map
import Data.Semigroup (sconcat)
import Data.Semigroup.Union (UnionWith(..))
import Data.String (IsString)

import qualified Transformation
import qualified Transformation.Deep as Deep
import qualified Transformation.Full as Full
import qualified Transformation.AG.Monomorphic as AG.Mono
import Text.Grampa (Ambiguous(..))

import qualified Language.Haskell.Abstract as Abstract
import qualified Language.Haskell.AST as AST
import qualified Language.Haskell.Extensions.AST as ExtAST
import qualified Language.Haskell.Binder as Binder
import qualified Language.Haskell.Disambiguator as Disambiguator
import qualified Language.Haskell.Reserializer as Reserializer
import Language.Haskell.Reserializer (Lexeme (Token, lexemeText, lexemeType), TokenType (Delimiter))

import Prelude hiding (mod, span)

data Resolution l pos s = Resolution

type Resolved l f = Validation (NonEmpty (Error l f))

prefixMinusPrecedence :: Int
prefixMinusPrecedence = 6

instance Transformation.Transformation (Resolution l pos s) where
    type Domain (Resolution l pos s) = Binder.WithEnvironment l (Disambiguator.Wrapped pos s)
    type Codomain (Resolution l pos s) = Compose (Resolved l (Reserializer.Wrapped pos s)) (Reserializer.Wrapped pos s)

instance {-# overlappable #-} Resolution l pos s
         `Transformation.At` g (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s) where
  Resolution $ Compose (_, Compose ((start, end), Compose (Ambiguous ((ws, x) :| [])))) =
    Compose (Success ((start, ws, end), x))
  Resolution $ Compose (_, Compose ((start, end), _)) = Compose (Failure $ pure AmbiguousParses)

instance {-# overlaps #-} forall l pos s.
         (Eq s, Eq pos, Eq (AST.Expression l l (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s)), IsString s,
          Abstract.Expression l ~ AST.Expression l,
          Abstract.ModuleName l ~ AST.ModuleName l,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.Name l ~ AST.Name l) =>
         Resolution l pos s
         `Transformation.At` AST.Expression l l (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s) where
   _res $ Compose (AG.Mono.Atts{AG.Mono.inh= UnionWith bindings}, expressions) =
      let resolveExpression :: f ~ Reserializer.Wrapped pos s
                            => AST.Expression l l f f
                            -> Validation (NonEmpty (Error l f)) (AST.Expression l l f f)
          resolveExpression e@(AST.InfixExpression left op right)
             | (_, AST.ReferenceExpression name) <- op =
                maybe (const $ Failure $ pure $ UnknownOperator name)
                      (verifyInfixApplication verifyArg left right) (Map.lookup name bindings) (pure e)
          resolveExpression e@(AST.ApplyExpression left right)
             | (_, AST.Negate{}) <- left = verifyArg (Just AST.LeftAssociative) prefixMinusPrecedence right (pure e)
          resolveExpression e = pure e
          verifyArg :: f ~ Reserializer.Wrapped pos s
                    => Maybe (AST.Associativity l) -> Int
                    -> f (AST.Expression l l f f)
                    -> Validation (NonEmpty (Error l f)) (AST.Expression l l f f)
                    -> Validation (NonEmpty (Error l f)) (AST.Expression l l f f)
          verifyArg associativity precedence arg result
             | ((_, lexemes, _), AST.InfixExpression _ op' _) <- arg,
               (_, AST.ReferenceExpression name) <- op',
               Just (Binder.InfixDeclaration _ associativity' precedence') <- Map.lookup name bindings =
               if parenthesized lexemes
                  || precedence < precedence'
                  || precedence == precedence' && elem associativity' associativity then result
               else Failure (pure ContradictoryAssociativity)
             | ((_, lexemes, _), AST.ApplyExpression (_, AST.Negate{}) _) <- arg =
               if parenthesized lexemes
                  || precedence < prefixMinusPrecedence
                  || precedence == prefixMinusPrecedence && elem AST.LeftAssociative associativity then result
               else Failure (pure ContradictoryAssociativity)
             | otherwise = result
      in Compose (Disambiguator.unique id (pure . AmbiguousExpression) (resolveExpression <$> expressions))

instance {-# overlaps #-} forall l pos s f.
         (Eq s, IsString s, Eq pos, f ~ Reserializer.Wrapped pos s,
          Show pos, Show s, Show (ExtAST.Expression l l f f),
          Eq (ExtAST.Expression l l f f),
          Abstract.Expression l ~ ExtAST.Expression l,
          Abstract.ModuleName l ~ ExtAST.ModuleName l,
          Abstract.QualifiedName l ~ ExtAST.QualifiedName l,
          Abstract.Name l ~ ExtAST.Name l) =>
         Resolution l pos s
         `Transformation.At` ExtAST.Expression l l (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s) where
  _res $ Compose (AG.Mono.Atts{AG.Mono.inh= UnionWith bindings}, expressions) =
      let resolveExpression :: ExtAST.Expression l l f f
                            -> Validation (NonEmpty (Error l f)) (ExtAST.Expression l l f f)
          resolveExpression e@(ExtAST.InfixExpression left op right)
             | ((_, lexemes, _), ExtAST.ReferenceExpression name) <- op =
               maybe (const $ Failure $ pure $ UnknownOperator name)
                     (verifyInfixApplication verifyArg left right)
                     (Map.lookup name bindings <|> defaultInfixDeclaration lexemes)
                     (pure e)
          resolveExpression (ExtAST.TupleSectionExpression items)
             | Just items' <- sequence items = Failure (TupleSectionWithNoOmission items' :| [])
          resolveExpression e@(ExtAST.ApplyExpression left right)
             | (_, ExtAST.Negate{}) <- left = verifyArg (Just AST.LeftAssociative) prefixMinusPrecedence right (pure e)
          resolveExpression e = pure e
          verifyArg :: f ~ Reserializer.Wrapped pos s
                    => Maybe (ExtAST.Associativity l) -> Int
                    -> f (ExtAST.Expression l l f f)
                    -> Validation (NonEmpty (Error l f)) (ExtAST.Expression l l f f)
                    -> Validation (NonEmpty (Error l f)) (ExtAST.Expression l l f f)
          verifyArg associativity precedence arg result
             | ((_, lexemes, _), ExtAST.InfixExpression _ op' _) <- arg,
               (_, ExtAST.ReferenceExpression name) <- op',
               Just (Binder.InfixDeclaration _ associativity' precedence') <- Map.lookup name bindings =
               if parenthesized lexemes
                  || precedence < precedence'
                  || precedence == precedence' && elem associativity' associativity then result
               else Failure (pure ContradictoryAssociativity)
             | (_, ExtAST.ApplyExpression ((_, lexemes, _), ExtAST.Negate{}) _) <- arg =
               if modifying lexemes
                  || precedence < prefixMinusPrecedence
                  || precedence == prefixMinusPrecedence && elem AST.LeftAssociative associativity then result
               else Failure (pure ContradictoryAssociativity)
             | otherwise = result
      in Compose (Disambiguator.unique id (pure . AmbiguousExtExpression) (resolveExpression <$> expressions))

--defaultInfixDeclaration :: ExtAST.QualifiedName l -> Maybe (Binder.Binding l)
defaultInfixDeclaration (Reserializer.Trailing lexemes)
   | any (== Token{lexemeType= Delimiter, lexemeText= "`"}) lexemes =
     Just (Binder.InfixDeclaration False AST.LeftAssociative 9)
   | otherwise = Nothing

verifyInfixApplication :: (Maybe (AST.Associativity λ) -> Int -> e -> a -> a) -> e -> e -> Binder.Binding l -> a -> a
verifyInfixApplication verifyArg left right (Binder.InfixDeclaration _ AST.LeftAssociative precedence) =
   verifyArg (Just AST.LeftAssociative) precedence left . verifyArg Nothing precedence right
verifyInfixApplication verifyArg left right (Binder.InfixDeclaration _ AST.RightAssociative precedence) =
   verifyArg (Just AST.RightAssociative) precedence right . verifyArg Nothing precedence left
verifyInfixApplication verifyArg left right (Binder.InfixDeclaration _ AST.NonAssociative precedence) =
   verifyArg Nothing precedence left . verifyArg Nothing precedence right

modifying, parenthesized :: (Eq s, IsString s) => Reserializer.ParsedLexemes s -> Bool
modifying (Reserializer.Trailing [Reserializer.Token{Reserializer.lexemeType= Reserializer.Modifier}]) = True
modifying _ = False
parenthesized (Reserializer.Trailing (paren:_)) = Reserializer.lexemeText paren == "("
parenthesized (Reserializer.Trailing []) = False

data Error l f = AmbiguousParses
               | AmbiguousExpression [AST.Expression l l f f]
               | AmbiguousExtExpression [ExtAST.Expression l l f f]
               | ContradictoryAssociativity
               | ClashingImports
               | ClashingNames
               | TupleSectionWithNoOmission (NonEmpty (f (ExtAST.Expression l l f f)))
               | UnknownOperator (ExtAST.QualifiedName l)

deriving instance (Show (AST.Expression l l f f), Show (ExtAST.Expression l l f f),
                   Show (f (ExtAST.Expression l l f f)), Show (ExtAST.QualifiedName l)) => Show (Error l f)

instance Monad (Validation (NonEmpty (Error l f))) where
   Success s >>= f = f s
   Failure errors >>= _ = Failure errors

-- | Resolve ambiguities in the given collection of modules, a 'Map' keyed by module name. Note that all class
-- constraints in the function's type signature are satisfied by the Haskell 'AST.Language'.
resolveModules :: forall l pos s f. (f ~ Reserializer.Wrapped pos s,
                                Abstract.Haskell l,
                                Abstract.Module l l ~ AST.Module l l,
                                Abstract.ModuleName l ~ AST.ModuleName l,
                                Abstract.Export l l ~ AST.Export l l,
                                Abstract.Import l l ~ AST.Import l l,
                                Abstract.ImportSpecification l l ~ AST.ImportSpecification l l,
                                Abstract.ImportItem l l ~ AST.ImportItem l l,
                                Abstract.Members l ~ AST.Members l,
                                Abstract.Declaration l ~ ExtAST.Declaration l,
                                Abstract.QualifiedName l ~ AST.QualifiedName l,
                                Abstract.Name l ~ AST.Name l,
                                Deep.Traversable (Resolution l pos s) (Abstract.Declaration l l),
                                Full.Traversable (Resolution l pos s) (Abstract.Module l l),
                                Full.Traversable (Resolution l pos s) (Abstract.Declaration l l)) =>
                  Map.Map (Abstract.ModuleName l)
                          (Binder.WithEnvironment l (Disambiguator.Wrapped pos s)
                                                  (AST.Module l l
                                                              (Binder.WithEnvironment l (Disambiguator.Wrapped pos s))
                                                              (Binder.WithEnvironment l (Disambiguator.Wrapped pos s))))
               -> Validation (NonEmpty (Abstract.ModuleName l, NonEmpty (Error l f)))
                             (Map.Map (Abstract.ModuleName l) (f (AST.Module l l f f)))
resolveModules modules = Map.traverseWithKey extractErrors resolvedModules
   where resolvedModules = Full.traverse Resolution <$> modules
         extractErrors moduleKey (Failure e)   = Failure ((moduleKey, e) :| [])
         extractErrors _         (Success mod) = Success mod

instance (Deep.Traversable (Resolution l pos s) g,
          Transformation.At (Resolution l pos s) (g (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s))) =>
         Full.Traversable (Resolution l pos s) g where
   traverse t x = sequenceValidations (Deep.traverse t <$> x) >>= getCompose . (t Transformation.$)

sequenceValidations :: Binder.WithEnvironment l (Disambiguator.Wrapped pos s) (Validation (NonEmpty e) x)
                    -> Validation (NonEmpty e) (Binder.WithEnvironment l (Disambiguator.Wrapped pos s) x)
sequenceValidations (Compose (env, Compose (span, Compose (Ambiguous xs)))) =
   case nonEmpty <$> partitionEithers (traverse validationToEither <$> toList xs)
   of (errors, Nothing) -> Failure (sconcat $ fromList errors)
      (_, Just successes) -> Success (Compose (env, Compose (span, Compose $ Ambiguous successes)))


