{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings,
             ScopedTypeVariables, TemplateHaskell, TypeFamilies, TypeOperators,
             UndecidableInstances #-}
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
      let resolveExpression :: AST.Expression l l (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s)
                            -> Validation (NonEmpty Error) (AST.Expression l l (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s))
          resolveExpression e@(AST.InfixExpression left op right)
             | (_, AST.ReferenceExpression name) <- op = case Map.lookup name bindings of
             Just (Binder.InfixDeclaration AST.LeftAssociative precedence) ->
               verifyArg (Just AST.LeftAssociative) precedence left $
               verifyArg Nothing (succ precedence) right $
               pure e
             Just (Binder.InfixDeclaration AST.RightAssociative precedence) ->
               verifyArg (Just AST.RightAssociative) precedence right $
               verifyArg Nothing (succ precedence) left $
               pure e
             Just (Binder.InfixDeclaration AST.NonAssociative precedence) ->
               verifyArg Nothing (succ precedence) left $
               verifyArg Nothing (succ precedence) right $
               pure e
          resolveExpression e = pure e
          verifyArg :: Maybe (AST.Associativity l) -> Int
                    -> Reserializer.Wrapped pos s (AST.Expression l l (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s))
                    -> Validation (NonEmpty Error) (AST.Expression l l (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s))
                    -> Validation (NonEmpty Error) (AST.Expression l l (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s))
          verifyArg associativity precedence arg result
             | ((_, lexemes, _), AST.InfixExpression _ op' _) <- arg,
               (_, AST.ReferenceExpression name) <- op',
               Just (Binder.InfixDeclaration associativity' precedence') <- Map.lookup name bindings =
               if parenthesized lexemes
                  || precedence < precedence'
                  || precedence == precedence' && any (associativity' ==) associativity then result
               else Failure (pure ContradictoryAssociativity)
             | otherwise = result
          parenthesized (Reserializer.Trailing (paren:_)) = Reserializer.lexemeText paren == "("
      in Compose (Disambiguator.unique id (pure . const AmbiguousExpression) (resolveExpression <$> expressions))

data Error = AmbiguousParses
           | AmbiguousExpression
           | ContradictoryAssociativity
           | ClashingImports
           | ClashingNames
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
   where resolvedModules = resolveModule <$> modules
         extractErrors moduleKey (Failure e)   = Failure ((moduleKey, e) :| [])
         extractErrors _         (Success mod) = Success mod
     
-- | Resolve ambiguities in a single module. The imports are resolved using the given map of already resolved
-- modules. Note that all class constraints in the function's type signature are satisfied by the Haskell
-- 'AST.Language'.
resolveModule :: forall l pos s. (Abstract.Haskell l,
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
                              Full.Traversable (Resolution l pos s) (Abstract.Module l l),
                              Full.Traversable (Resolution l pos s) (Abstract.Declaration l l),
                              Deep.Traversable (Resolution l pos s) (Abstract.Declaration l l)) =>
                 Binder.WithEnvironment l (Disambiguator.Wrapped pos s) (AST.Module l l (Binder.WithEnvironment l (Disambiguator.Wrapped pos s)) (Binder.WithEnvironment l (Disambiguator.Wrapped pos s)))
              -> Validation (NonEmpty Error) (Reserializer.Wrapped pos s (AST.Module l l (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s)))
resolveModule m@(Compose (_, Compose (pos, Compose (Ambiguous ((ls, AST.NamedModule moduleName exports modImports body) :| []))))) = Full.traverse Resolution m

instance (Deep.Traversable (Resolution l pos s) g,
          Transformation.At (Resolution l pos s) (g (Reserializer.Wrapped pos s) (Reserializer.Wrapped pos s))) =>
         Full.Traversable (Resolution l pos s) g where
   traverse = Full.traverseUpDefault
