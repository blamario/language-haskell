{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings,
             ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Language.Haskell.Resolver where

import Data.Either.Validation (Validation(..), validationToEither)
import Data.Functor.Compose (Compose(..))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Lazy as Map
import Data.Map.Monoidal (MonoidalMap(..))
import Data.String (IsString)
import Language.Haskell.TH (appT, conT, varT, newName)

import qualified Rank2.TH
import qualified Transformation
import qualified Transformation.Deep as Deep
import qualified Transformation.Deep.TH
import qualified Transformation.Full as Full
import qualified Transformation.Full.TH
import qualified Transformation.Rank2 as Rank2
import qualified Transformation.AG.Monomorphic as AG.Mono
import Text.Grampa (Ambiguous(..))

import qualified Language.Haskell.Abstract as Abstract
import qualified Language.Haskell.AST as AST
import qualified Language.Haskell.Binder as Binder
import qualified Language.Haskell.Disambiguator as Disambiguator
import qualified Language.Haskell.Reserializer as Reserializer

data Resolution l pos s = Resolution

type Resolved = Validation (NonEmpty Error)

instance Transformation.Transformation (Resolution l pos s) where
    type Domain (Resolution l pos s) = Binder.WithEnvironment l (Disambiguator.Wrapped pos s)
    type Codomain (Resolution l pos s) = Compose Resolved (Reserializer.Wrapped pos s)

instance {-# overlappable #-} Resolution l pos s
         `Transformation.At` g (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s) where
   Resolution $ Compose (_, (Compose ((start, end), Compose (Ambiguous ((ws, x) :| []))))) =
     Compose (Success ((start, ws, end), x))
   Resolution{} $ _ = Compose (Failure $ pure AmbiguousParses)

instance {-# overlaps #-} forall l pos s.
         (Eq s, Eq pos, Eq (AST.Expression l l (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s)), IsString s,
          Abstract.Expression l ~ AST.Expression l,
          Abstract.ModuleName l ~ AST.ModuleName l,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.Name l ~ AST.Name l) =>
         Resolution l pos s
         `Transformation.At` AST.Expression l l (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s) where
   res $ Compose (AG.Mono.Atts{AG.Mono.inh= MonoidalMap bindings}, expressions) =
      let resolveExpression :: f ~ Reserializer.Wrapped pos s
                            => AST.Expression l l f f
                            -> Validation (NonEmpty Error) (AST.Expression l l f f)
          resolveExpression e@(AST.InfixExpression left op right)
             | (_, AST.ReferenceExpression name) <- op =
                maybe (const $ Failure $ pure UnknownOperator)
                      (verifyInfixApplication verifyArg left right) (Map.lookup name bindings) (pure e)
          resolveExpression e = pure e
          verifyArg :: f ~ Reserializer.Wrapped pos s
                    => Maybe (AST.Associativity l) -> Int
                    -> f (AST.Expression l l f f)
                    -> Validation (NonEmpty Error) (AST.Expression l l f f)
                    -> Validation (NonEmpty Error) (AST.Expression l l f f)
          verifyArg associativity precedence arg result
             | ((_, lexemes, _), AST.InfixExpression _ op' _) <- arg,
               (_, AST.ReferenceExpression name) <- op',
               Just (Binder.InfixDeclaration _ associativity' precedence') <- Map.lookup name bindings =
               if parenthesized lexemes
                  || precedence < precedence'
                  || precedence == precedence' && any (associativity' ==) associativity then result
               else Failure (pure ContradictoryAssociativity)
             | otherwise = result
      in Compose (Disambiguator.unique id (pure . const AmbiguousExpression) (resolveExpression <$> expressions))

verifyInfixApplication :: (Maybe (AST.Associativity λ) -> Int -> e -> a -> a) -> e -> e -> Binder.Binding l -> a -> a
verifyInfixApplication verifyArg left right (Binder.InfixDeclaration _ AST.LeftAssociative precedence) =
   verifyArg (Just AST.LeftAssociative) precedence left . verifyArg Nothing precedence right
verifyInfixApplication verifyArg left right (Binder.InfixDeclaration _ AST.RightAssociative precedence) =
   verifyArg (Just AST.RightAssociative) precedence right . verifyArg Nothing precedence left
verifyInfixApplication verifyArg left right (Binder.InfixDeclaration _ AST.NonAssociative precedence) =
   verifyArg Nothing precedence left . verifyArg Nothing precedence right

parenthesized :: (Eq s, IsString s) => Reserializer.ParsedLexemes s -> Bool
parenthesized (Reserializer.Trailing (paren:_)) = Reserializer.lexemeText paren == "("
parenthesized (Reserializer.Trailing []) = False

data Error = AmbiguousParses
           | AmbiguousExpression
           | ContradictoryAssociativity
           | ClashingImports
           | ClashingNames
           | UnknownOperator
           deriving Show

instance Monad (Validation (NonEmpty Error)) where
   Success s >>= f = f s
   Failure errors >>= _ = Failure errors

-- | Resolve ambiguities in the given collection of modules, a 'Map' keyed by module name. Note that all class
-- constraints in the function's type signature are satisfied by the Haskell 'AST.Language'.
resolveModules :: forall l pos s. (Abstract.Haskell l,
                              Abstract.Module l l ~ AST.Module l l,
                              Abstract.ModuleName l ~ AST.ModuleName l,
                              Abstract.Export l l ~ AST.Export l l,
                              Abstract.Import l l ~ AST.Import l l,
                              Abstract.ImportSpecification l l ~ AST.ImportSpecification l l,
                              Abstract.ImportItem l l ~ AST.ImportItem l l,
                              Abstract.Members l ~ AST.Members l,
                              Abstract.Declaration l ~ AST.Declaration l,
                              Abstract.QualifiedName l ~ AST.QualifiedName l,
                              Abstract.Name l ~ AST.Name l,
                              Deep.Traversable (Resolution l pos s) (Abstract.Declaration l l),
                              Full.Traversable (Resolution l pos s) (Abstract.Module l l),
                              Full.Traversable (Resolution l pos s) (Abstract.Declaration l l)) =>
                  Map.Map (Abstract.ModuleName l) (Binder.WithEnvironment l (Disambiguator.Wrapped pos s) (AST.Module l l (Binder.WithEnvironment l (Disambiguator.Wrapped pos s)) (Binder.WithEnvironment l (Disambiguator.Wrapped pos s))))
               -> Validation (NonEmpty (Abstract.ModuleName l, NonEmpty Error)) (Map.Map (Abstract.ModuleName l) ((Reserializer.Wrapped pos s) (AST.Module l l (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s))))
resolveModules modules = Map.traverseWithKey extractErrors resolvedModules
   where resolvedModules = Full.traverse Resolution <$> modules
         extractErrors moduleKey (Failure e)   = Failure ((moduleKey, e) :| [])
         extractErrors _         (Success mod) = Success mod

instance (Deep.Traversable (Resolution l pos s) g,
          Transformation.At (Resolution l pos s) (g (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s))) =>
         Full.Traversable (Resolution l pos s) g where
   traverse = Full.traverseUpDefault
