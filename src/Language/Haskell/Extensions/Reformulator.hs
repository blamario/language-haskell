{-# Language ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes,
             TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}

module Language.Haskell.Extensions.Reformulator where

import Data.Functor.Compose (Compose (Compose, getCompose))
import qualified Data.Map.Lazy as Map
import Data.Semigroup.Union (UnionWith(..))

import qualified Transformation
import Transformation (Transformation)
import qualified Transformation.AG.Dimorphic as Di
import qualified Transformation.Deep as Deep
import qualified Transformation.Full as Full

import Language.Haskell.Extensions.Abstract (DeeplyFunctor, ExtendedWithAllOf)
import Language.Haskell.Extensions (Extension)
import qualified Language.Haskell.Extensions as Extensions
import qualified Language.Haskell.Extensions.Abstract as Abstract
import qualified Language.Haskell.Extensions.AST as AST
import qualified Language.Haskell.Binder as Binder
import qualified Language.Haskell.Reserializer as Reserializer

type family Without (e :: Extension) (es :: [Extension]) where
   Without _ '[] = '[]
   Without e (e ': es) = Without e es
   Without e1 (e2 ': es) = e2 ': Without e1 es

type family Difference (xs :: [Extension]) (ys :: [Extension]) where
   Difference xs '[] = xs
   Difference xs (y ': ys) = Difference (Without y xs) ys

type Transpiler λ1 λ2 f = Abstract.Module λ1 λ1 f f -> Abstract.Module λ2 λ2 f f

type Reformulator xs ys λ c f = ExtendedWithAllOf xs λ =>
   forall l. (Abstract.Haskell l, ExtendedWithAllOf (Difference ys xs) l, c λ l) => Transpiler λ l f


type Wrap l pos s = Binder.WithEnvironment l (Reserializer.Wrapped pos s)

data ReformulationOf (e :: Extension) (es :: [Extension]) λ l pos s = Reformulation

instance Transformation (ReformulationOf e es λ l pos s) where
   type Domain (ReformulationOf e es λ l pos s) = Wrap λ pos s
   type Codomain (ReformulationOf e es λ l pos s) = Wrap l pos s

type SameWrap e es pos s l1 l2 = (Binder.WithEnvironment l1 ~ Binder.WithEnvironment l2,
                                  DeeplyFunctor (ReformulationOf e es l1 l2 pos s) l2)
  

dropRecordWildCards :: Reformulator '[Extensions.RecordWildCards] '[ 'Extensions.NamedFieldPuns ] λ (SameWrap 'Extensions.RecordWildCards '[ 'Extensions.NamedFieldPuns ] pos s) (Wrap λ pos s)
dropRecordWildCards = Deep.fmap (Reformulation @'Extensions.RecordWildCards @'[ 'Extensions.NamedFieldPuns ])


instance {-# overlappable #-} SameWrap e es pos s λ l =>
   ReformulationOf e es λ l pos s `Transformation.At` g (Wrap λ pos s) (Wrap λ pos s) where
   Reformulation $ Compose (env, (s, node)) = Compose (env, (s, node))


instance (SameWrap 'Extensions.RecordWildCards '[ 'Extensions.NamedFieldPuns ] pos s λ l,
          Abstract.FieldPattern l ~ AST.FieldPattern l,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.ModuleName l ~ AST.ModuleName l,
          Abstract.Name l ~ AST.Name l) =>
   ReformulationOf 'Extensions.RecordWildCards '[ 'Extensions.NamedFieldPuns ] λ l pos s
   `Transformation.At` AST.Pattern λ l (Wrap λ pos s) (Wrap λ pos s)
  where
   Reformulation $ Compose (env@(Di.Atts inherited _),
                            (s, AST.WildcardRecordPattern con@(AST.QualifiedName modName _) fields)) =
      Compose (env, (s, AST.RecordPattern con $ fields ++ implicitFields))
      where implicitFields = case Binder.lookupValue con inherited of
               Just (Binder.RecordConstructor (UnionWith declaredFields)) ->
                  [ Compose (Di.Atts inherited mempty, (s, AST.PunnedFieldPattern $ qualified fieldName))
                  | fieldName <- Map.keys declaredFields, fieldName `notElem` explicitFieldNames]
               Just _ -> error ("Environment misaattributes record constructor " ++ show con)
               Nothing -> error ("Environment lacks record constructor " ++ show con)
            explicitFieldNames = map (explicitFieldName . snd . snd . getCompose) fields
            explicitFieldName (AST.FieldPattern name _) = Binder.baseName name
            explicitFieldName (AST.PunnedFieldPattern name) = Binder.baseName name
            qualified name = AST.QualifiedName modName name
   Reformulation $ Compose (env, (s, p)) = Compose (env, (s, p))


instance (SameWrap 'Extensions.RecordWildCards '[ 'Extensions.NamedFieldPuns ] pos s λ l,
          Abstract.Haskell l,
          Abstract.FieldBinding l ~ AST.FieldBinding l,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.ModuleName l ~ AST.ModuleName l,
          Abstract.Name l ~ AST.Name l) =>
   ReformulationOf 'Extensions.RecordWildCards '[ 'Extensions.NamedFieldPuns ] λ l pos s
   `Transformation.At` AST.Expression λ l (Wrap λ pos s) (Wrap λ pos s)
  where
   Reformulation $ Compose (env@(Di.Atts inherited _),
                            (s, AST.WildcardRecordExpression con@(AST.QualifiedName modName _) fields)) =
      Compose (env, (s, AST.RecordExpression conExp $ fields ++ implicitFields))
      where implicitFields = case Binder.lookupValue con inherited of
               Just (Binder.RecordConstructor (UnionWith declaredFields)) ->
                  [ Compose (Di.Atts inherited mempty, (s, AST.PunnedFieldBinding $ qualified fieldName))
                  | fieldName <- Map.keys declaredFields, fieldName `notElem` explicitFieldNames]
               Just _ -> error ("Environment misaattributes record constructor " ++ show con)
               Nothing -> error ("Environment lacks record constructor " ++ show con)
            explicitFieldNames = map (explicitFieldName . snd . snd . getCompose) fields
            explicitFieldName (AST.FieldBinding name _) = Binder.baseName name
            explicitFieldName (AST.PunnedFieldBinding name) = Binder.baseName name
            qualified name = AST.QualifiedName modName name
            conExp | let (start, _, _) = s = Compose (env, ((start, mempty, start), Abstract.referenceExpression con))
   Reformulation $ Compose (env, (s, p)) = Compose (env, (s, p))


instance (Deep.Functor (ReformulationOf e es λ l pos s) g,
          Transformation.At (ReformulationOf e es λ l pos s) (g (Compose (Wrap l pos s) (AbstractIn l)) (Compose (Wrap l pos s) (AbstractIn l)))) =>
         Full.Functor (ReformulationOf e es λ l pos s) g where
   (<$>) = Full.mapUpDefault


mapImport :: (Abstract.ExtendedHaskell λ2,
              Abstract.ModuleName λ1 ~ AST.ModuleName λ1,
              Abstract.Name λ1 ~ AST.Name λ1) => AST.Import λ1 l d s -> Abstract.Import λ2 l d s
mapImport (AST.Import False qualified Nothing modName alias detail) =
   Abstract.importDeclaration qualified (mapModuleName modName) (mapModuleName <$> alias) detail
mapImport (AST.Import True qualified Nothing modName alias detail) =
   Abstract.safeImportDeclaration qualified (mapModuleName modName) (mapModuleName <$> alias) detail
mapImport (AST.Import False qualified (Just package) modName alias detail) =
   Abstract.packageQualifiedImportDeclaration qualified package (mapModuleName modName) (mapModuleName <$> alias) detail
mapImport (AST.Import True qualified (Just package) modName alias detail) =
   Abstract.safePackageQualifiedImportDeclaration qualified package (mapModuleName modName) (mapModuleName <$> alias) detail

mapModuleName :: (Abstract.Haskell λ2, Abstract.Name λ1 ~ AST.Name λ1) => AST.ModuleName λ1 -> Abstract.ModuleName λ2
mapModuleName (AST.ModuleName parts) = Abstract.moduleName (mapName <$> parts)

mapName :: Abstract.Haskell λ2 => AST.Name λ1 -> Abstract.Name λ2
mapName (AST.Name name) = Abstract.name name
