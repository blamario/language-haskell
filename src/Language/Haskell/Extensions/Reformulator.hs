{-# Language ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
             OverloadedStrings, RankNTypes, ScopedTypeVariables,
             TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}

module Language.Haskell.Extensions.Reformulator where

import qualified Data.Foldable1 as Foldable1
import Data.Foldable1 (Foldable1)
import Data.Functor.Compose (Compose (Compose, getCompose))
import qualified Data.List as List
import qualified Data.Map.Lazy as Map
import Data.Monoid.Textual (TextualMonoid)
import Data.Semigroup.Union (UnionWith(..))

import qualified Rank2
import qualified Transformation
import Transformation (Transformation)
import qualified Transformation.AG.Dimorphic as Di
import qualified Transformation.Full as Full

import Language.Haskell.Extensions.Abstract (DeeplyFunctor, ExtendedWithAllOf)
import Language.Haskell.Extensions (Extension, ExtensionSwitch (ExtensionSwitch), On, Off)
import qualified Language.Haskell.Extensions as Extensions
import qualified Language.Haskell.Extensions.Abstract as Abstract
import qualified Language.Haskell.Extensions.AST as AST
import Language.Haskell.Extensions.Translation (
   NameTranslation, Translation, WrapTranslation, WrappedTranslation, FullyTranslatable)
import qualified Language.Haskell.Extensions.Translation as Translation
import Language.Haskell.Extensions.Verifier (Accounting (Accounting))
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

data ReformulationOf (e :: ExtensionSwitch) (es :: [ExtensionSwitch]) λ l pos s
   = Reformulation ExtensionSwitch [ExtensionSwitch] -- e es

data ArrowTranslation λ l pos s =
  ArrowTranslation (ReformulationOf (On 'Extensions.Arrows) '[] λ l pos s)
                   (forall x. x -> Wrap λ pos s x)
                   (Wrap λ pos s (AST.Pattern λ λ (Wrap λ pos s) (Wrap λ pos s)))

instance (Abstract.QualifiedName λ ~ AST.QualifiedName λ,
          Abstract.ModuleName λ ~ AST.ModuleName λ,
          Abstract.Name λ ~ AST.Name λ,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.ModuleName l ~ AST.ModuleName l,
          Abstract.Name l ~ AST.Name l) =>
  NameTranslation (ReformulationOf e es λ l pos s) where
   type Origin (ReformulationOf e es λ l pos s) = λ
   type Target (ReformulationOf e es λ l pos s) = l

instance (Abstract.QualifiedName λ ~ AST.QualifiedName λ,
          Abstract.ModuleName λ ~ AST.ModuleName λ,
          Abstract.Name λ ~ AST.Name λ,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.ModuleName l ~ AST.ModuleName l,
          Abstract.Name l ~ AST.Name l) =>
  WrapTranslation (ReformulationOf e es λ l pos s) where
   type Wrap (ReformulationOf e es λ l pos s) = Wrap λ pos s

instance (Abstract.QualifiedName λ ~ AST.QualifiedName λ,
          Abstract.ModuleName λ ~ AST.ModuleName λ,
          Abstract.Name λ ~ AST.Name λ,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.ModuleName l ~ AST.ModuleName l,
          Abstract.Name l ~ AST.Name l) => NameTranslation (ArrowTranslation λ l pos s) where
   type Origin (ArrowTranslation λ l pos s) = λ
   type Target (ArrowTranslation λ l pos s) = l

instance (Abstract.QualifiedName λ ~ AST.QualifiedName λ,
          Abstract.ModuleName λ ~ AST.ModuleName λ,
          Abstract.Name λ ~ AST.Name λ,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.ModuleName l ~ AST.ModuleName l,
          Abstract.Name l ~ AST.Name l) =>
  WrapTranslation (ArrowTranslation λ l pos s) where
   type Wrap (ArrowTranslation λ l pos s) = Wrap λ pos s

type SameWrap (e :: Extension) (es :: [Extension]) pos s l1 l2 = (
   Abstract.QualifiedName l1 ~ AST.QualifiedName l1,
   Abstract.ModuleName l1 ~ AST.ModuleName l1,
   Abstract.Name l1 ~ AST.Name l1,
   Abstract.QualifiedName l2 ~ AST.QualifiedName l2,
   Abstract.ModuleName l2 ~ AST.ModuleName l2,
   Abstract.Name l2 ~ AST.Name l2,
   Abstract.Module l1 ~ AST.Module l1,
   Abstract.Export l1 ~ AST.Export l1,
   Abstract.Import l1 ~ AST.Import l1,
   Abstract.ImportSpecification l1 ~ AST.ImportSpecification l1,
   Abstract.ImportItem l1 ~ AST.ImportItem l1,
   Abstract.Declaration l1 ~ AST.Declaration l1,
   Abstract.EquationLHS l1 ~ AST.EquationLHS l1,
   Abstract.EquationRHS l1 ~ AST.EquationRHS l1,
   Abstract.ClassInstanceLHS l1 ~ AST.ClassInstanceLHS l1,
   Abstract.Context l1 ~ AST.Context l1,
   Abstract.TypeLHS l1 ~ AST.TypeLHS l1,
   Abstract.Type l1 ~ AST.Type l1,
   Abstract.Kind l1 ~ AST.Type l1,
   Abstract.TypeVarBinding l1 ~ AST.TypeVarBinding l1,
   Abstract.Constructor l1 ~ AST.Constructor l1,
   Abstract.DataConstructor l1 ~ AST.DataConstructor l1,
   Abstract.GADTConstructor l1 ~ AST.GADTConstructor l1,
   Abstract.DerivingClause l1 ~ AST.DerivingClause l1,
   Abstract.FieldDeclaration l1 ~ AST.FieldDeclaration l1,
   Abstract.FieldBinding l1 ~ AST.FieldBinding l1,
   Abstract.FieldPattern l1 ~ AST.FieldPattern l1,
   Abstract.Pattern l1 ~ AST.Pattern l1,
   Abstract.GuardedExpression l1 ~ AST.GuardedExpression l1,
   Abstract.Expression l1 ~ AST.Expression l1,
   Abstract.Statement l1 ~ AST.Statement l1,
   Abstract.CaseAlternative l1 ~ AST.CaseAlternative l1,
   Abstract.Value l1 ~ AST.Value l1,
   Binder.WithEnvironment l1 ~ Binder.WithEnvironment l2)

dropArrows :: forall l1 l2 node pos s.
              (Abstract.Haskell l2, FullyTranslatable (ReformulationOf (On 'Extensions.Arrows) '[ ] l1 l2 pos s) node)
           => Wrap l1 pos s (node l1 l1 (Wrap l1 pos s) (Wrap l1 pos s))
           -> Wrap l1 pos s (node l2 l2 (Wrap l1 pos s) (Wrap l1 pos s))
dropArrows =
   Translation.translateFully
      (Reformulation (Extensions.on Extensions.Arrows) [] :: ReformulationOf (On 'Extensions.Arrows) '[ ] l1 l2 pos s)

dropNoListTuplePuns :: forall l1 l2 node pos s.
                       (Abstract.Haskell l2, Abstract.ExtendedWith '[ 'Extensions.NamedFieldPuns ] l2,
                        FullyTranslatable
                           (ReformulationOf (Off 'Extensions.ListTuplePuns) '[ ] l1 l2 pos s)
                           node)
                    => Wrap l1 pos s (node l1 l1 (Wrap l1 pos s) (Wrap l1 pos s))
                    -> Wrap l1 pos s (node l2 l2 (Wrap l1 pos s) (Wrap l1 pos s))
dropNoListTuplePuns =
   Translation.translateFully
      (Reformulation (Extensions.off Extensions.ListTuplePuns) []
       :: ReformulationOf (Off 'Extensions.ListTuplePuns) '[ ] l1 l2 pos s)

dropRecordWildCards :: forall l1 l2 node pos s.
                       (Abstract.Haskell l2, Abstract.ExtendedWith '[ 'Extensions.NamedFieldPuns ] l2,
                        SameWrap 'Extensions.RecordWildCards '[ 'Extensions.NamedFieldPuns ] pos s l1 l2,
                        FullyTranslatable
                           (ReformulationOf (On 'Extensions.RecordWildCards) '[ On 'Extensions.NamedFieldPuns ] l1 l2 pos s)
                           node)
                    => Wrap l1 pos s (node l1 l1 (Wrap l1 pos s) (Wrap l1 pos s))
                    -> Wrap l1 pos s (node l2 l2 (Wrap l1 pos s) (Wrap l1 pos s))
dropRecordWildCards =
   Translation.translateFully
      (Reformulation (Extensions.on Extensions.RecordWildCards) [Extensions.on Extensions.NamedFieldPuns]
       :: ReformulationOf (On 'Extensions.RecordWildCards) '[ On 'Extensions.NamedFieldPuns ] l1 l2 pos s)

dropNPlusKPatterns :: forall l1 l2 node pos s.
                      (Abstract.Haskell l2, Abstract.ExtendedWith '[ 'Extensions.ViewPatterns ] l2,
                       SameWrap 'Extensions.NPlusKPatterns '[ 'Extensions.ViewPatterns ] pos s l1 l2,
                       FullyTranslatable
                          (ReformulationOf (On 'Extensions.NPlusKPatterns) '[ On 'Extensions.ViewPatterns ] l1 l2 pos s)
                          node)
                    => Wrap l1 pos s (node l1 l1 (Wrap l1 pos s) (Wrap l1 pos s))
                    -> Wrap l1 pos s (node l2 l2 (Wrap l1 pos s) (Wrap l1 pos s))
dropNPlusKPatterns =
   Translation.translateFully
      (Reformulation (Extensions.on Extensions.NPlusKPatterns) [Extensions.on Extensions.ViewPatterns]
       :: ReformulationOf (On 'Extensions.NPlusKPatterns) '[ On 'Extensions.ViewPatterns ] l1 l2 pos s)

-- Generic instance to adjust the LANGUAGE pragma

instance (TextualMonoid s,
          Abstract.DeeplyFoldable (Accounting l1 pos s) l1,
          Abstract.QualifiedName l1 ~ AST.QualifiedName l1,
          Abstract.QualifiedName l2 ~ AST.QualifiedName l2,
          Abstract.ModuleName l1 ~ AST.ModuleName l1,
          Abstract.ModuleName l2 ~ AST.ModuleName l2,
          Abstract.Name l1 ~ AST.Name l1,
          Abstract.Name l2 ~ AST.Name l2,
          Abstract.Module l1 ~ AST.Module l1,
          Abstract.Module l2 ~ AST.Module l2) => Translation (ReformulationOf e es l1 l2 pos s) AST.Module
  where
   translate t@(Reformulation e@(ExtensionSwitch (e', _)) es) (AST.ExtendedModule oldExts m)
      | Map.notMember e' (getUnionWith $ Full.foldMap (Accounting :: Accounting l1 pos s) m)
      = withExtensions (List.delete e oldExts)
      | otherwise = withExtensions (List.union es $ List.delete e oldExts)
      where withExtensions [] = Translation.translate t (Foldable1.head m)
            withExtensions exts = AST.ExtendedModule exts m
   translate t (AST.NamedModule name exports imports declarations) =
      AST.NamedModule (Translation.translateModuleName t name) exports imports declarations
   translate t (AST.AnonymousModule imports declarations) = AST.AnonymousModule imports declarations

-- Arrows instances

instance (SameWrap 'Extensions.Arrows '[ ] pos s λ l,
          Abstract.Supports 'Extensions.Arrows λ,
          Abstract.Haskell l,
          Abstract.FieldBinding l ~ AST.FieldBinding l,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.ModuleName l ~ AST.ModuleName l,
          Abstract.Name l ~ AST.Name l) =>
   ReformulationOf (On 'Extensions.Arrows) '[] λ l pos s
   `WrappedTranslation` AST.Expression
  where
    t `translateWrapped` Compose (env, (s, AST.ProcExpression () p e)) = e'
      where e' = ArrowTranslation t (Compose . (,) env . (,) s) p `Translation.translateWrapped` e
    _ `translateWrapped` Compose (env, (s, e)) = Compose (env, (s, e))

instance (SameWrap 'Extensions.Arrows '[ ] pos s l1 l2,
          Abstract.Supports 'Extensions.Arrows l1,
          Abstract.Expression l1 ~ AST.Expression l1,
          Abstract.QualifiedName l1 ~ AST.QualifiedName l1,
          Abstract.QualifiedName l2 ~ AST.QualifiedName l2,
          Abstract.ModuleName l1 ~ AST.ModuleName l1,
          Abstract.ModuleName l2 ~ AST.ModuleName l2,
          Abstract.Name l1 ~ AST.Name l1,
          Abstract.Name l2 ~ AST.Name l2) => Translation (ArrowTranslation l1 l2 pos s) AST.Expression
  where
    translate t@(ArrowTranslation t' _ p) (AST.ArrowBrackets () e []) = Translation.translate t' (unwrap e)
    translate t@(ArrowTranslation _ wrap p) (AST.ArrowBrackets () e args)
      | Just (xs, x) <- List.unsnoc args = AST.ApplyExpression (wrap $ AST.ArrowBrackets () e xs) x

-- RecordWildCards instances

instance (SameWrap 'Extensions.RecordWildCards '[ 'Extensions.NamedFieldPuns ] pos s λ l2,
          Abstract.Supports 'Extensions.RecordWildCards λ,
          Abstract.FieldPattern l2 ~ AST.FieldPattern l2,
          Abstract.QualifiedName l2 ~ AST.QualifiedName l2,
          Abstract.ModuleName l2 ~ AST.ModuleName l2,
          Abstract.Name l2 ~ AST.Name l2) =>
   WrappedTranslation
      (ReformulationOf (On 'Extensions.RecordWildCards) '[On 'Extensions.NamedFieldPuns] λ l2 pos s)
      AST.Pattern
  where
   translateWrapped
      Reformulation{}
      (Compose (env@(Di.Atts inherited _),
               (s, AST.WildcardRecordPattern () con@(AST.QualifiedName modName _) fields))) =
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
   translateWrapped Reformulation{} (Compose (env, (s, p))) = Compose (env, (s, p))

instance (SameWrap 'Extensions.RecordWildCards '[ 'Extensions.NamedFieldPuns ] pos s λ l,
          Abstract.Supports 'Extensions.RecordWildCards λ,
          Abstract.Haskell l,
          Abstract.FieldBinding l ~ AST.FieldBinding l,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.ModuleName l ~ AST.ModuleName l,
          Abstract.Name l ~ AST.Name l) =>
   ReformulationOf (On 'Extensions.RecordWildCards) '[On 'Extensions.NamedFieldPuns] λ l pos s
   `WrappedTranslation` AST.Expression
  where
   _ `translateWrapped` Compose (env@(Di.Atts inherited _),
                                 (s, AST.WildcardRecordExpression () con@(AST.QualifiedName modName _) fields)) =
      Compose (env, (s, AST.RecordExpression conExp $ fields ++ implicitFields))
      where implicitFields = case Binder.lookupValue con inherited of
               Just (Binder.RecordConstructor (UnionWith declaredFields)) ->
                  [ Compose (Di.Atts inherited mempty, (s, AST.PunnedFieldBinding $ qualified fieldName))
                  | fieldName <- Map.keys declaredFields, fieldName `notElem` explicitFieldNames]
               Just _ -> error ("Environment misaattributes record constructor " ++ show con)
               Nothing -> error ("Environment lacks record constructor " ++ show con)
            explicitFieldNames :: [AST.Name l]
            explicitFieldNames = map (explicitFieldName . snd . snd . getCompose) fields
            explicitFieldName (AST.FieldBinding name _) = Binder.baseName name
            explicitFieldName (AST.PunnedFieldBinding name) = Binder.baseName name
            qualified name = AST.QualifiedName modName name
            conExp | let (start, _, _) = s = Compose (env, ((start, mempty, start), Abstract.referenceExpression con))
   _ `translateWrapped` Compose (env, (s, e)) = Compose (env, (s, e))

-- NPlusKPattern instances

instance (SameWrap 'Extensions.NPlusKPatterns '[ 'Extensions.ViewPatterns ] pos s λ l2,
          Abstract.Haskell l2,
          Abstract.Supports 'Extensions.NPlusKPatterns λ,
          Abstract.Supports 'Extensions.ViewPatterns l2,
          Abstract.FieldPattern l2 ~ AST.FieldPattern l2,
          Abstract.QualifiedName l2 ~ AST.QualifiedName l2,
          Abstract.ModuleName l2 ~ AST.ModuleName l2,
          Abstract.Name l2 ~ AST.Name l2) =>
   WrappedTranslation
      (ReformulationOf (On 'Extensions.NPlusKPatterns) '[On 'Extensions.ViewPatterns] λ l2 pos s)
      AST.Pattern
  where
   translateWrapped
      Reformulation{}
      (Compose (env, (s@(start, lexemes, end), AST.NPlusKPattern () n k))) =
      Compose (env,
               (s, AST.ViewPattern () (justGreaterOrEqual k) (rewrap $ AST.ConstructorPattern just [] [rewrap $ AST.VariablePattern n])))
      where justGreaterOrEqual k =
               rewrap
               $ AST.LambdaExpression [rewrap $ AST.VariablePattern n]
               $ rewrap $ AST.ConditionalExpression
                    (nOpK ">=")
                    (rewrap $ rewrap (AST.ConstructorExpression just) `AST.ApplyExpression` nOpK "-")
                    (rewrap $ AST.ConstructorExpression $
                     rewrap $ AST.ConstructorReference $ qualifiedWithPrelude $ AST.Name "Nothing")
            nOpK op =
               rewrap $
               AST.InfixExpression nExp (rewrap $ AST.ReferenceExpression $ qualifiedWithPrelude $ AST.Name op) kExp
            nExp = rewrap $ AST.ReferenceExpression $ Binder.unqualifiedName n
            kExp = rewrap $ AST.LiteralExpression $ rewrap $ AST.IntegerLiteral k
            just = rewrap $ AST.ConstructorReference $ qualifiedWithPrelude $ AST.Name "Just"
            qualifiedWithPrelude = AST.QualifiedName (Just Binder.preludeName)
            rewrap :: node -> Wrap l2 pos s node
            rewrap node = Compose (env, ((start, mempty, end), node))
   translateWrapped Reformulation{} (Compose (env, (s, p))) = Compose (env, (s, p))

-- auxiliary functions

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

mapFieldBinding :: (Abstract.Haskell λ2,
                    Abstract.ExtendedWith '[ 'Extensions.NamedFieldPuns ] λ2,
                    Abstract.QualifiedName λ1 ~ AST.QualifiedName λ1,
                    Abstract.ModuleName λ1 ~ AST.ModuleName λ1,
                    Abstract.Name λ1 ~ AST.Name λ1) => AST.FieldBinding λ1 l d s -> Abstract.FieldBinding λ2 l d s
mapFieldBinding (AST.FieldBinding name value) = Abstract.fieldBinding (mapQualifiedName name) value
mapFieldBinding (AST.PunnedFieldBinding name) = Abstract.punnedFieldBinding' Abstract.build (mapQualifiedName name)

mapQualifiedName :: (Abstract.Haskell λ2,
                     Abstract.ModuleName λ1 ~ AST.ModuleName λ1,
                     Abstract.Name λ1 ~ AST.Name λ1)
                 => AST.QualifiedName λ1 -> Abstract.QualifiedName λ2
mapQualifiedName (AST.QualifiedName modName localName) =
   Abstract.qualifiedName (mapModuleName <$> modName) (mapName localName)

mapModuleName :: (Abstract.Haskell λ2, Abstract.Name λ1 ~ AST.Name λ1) => AST.ModuleName λ1 -> Abstract.ModuleName λ2
mapModuleName (AST.ModuleName parts) = Abstract.moduleName (mapName <$> parts)

mapName :: Abstract.Haskell λ2 => AST.Name λ1 -> Abstract.Name λ2
mapName (AST.Name name) = Abstract.name name

applyNRewrap :: (Wrap l pos s node -> node') -> Wrap l pos s node -> Wrap l pos s node'
applyNRewrap f node@(Compose (env, (pos@(start, _, end), _))) = Compose (env, ((start, mempty, end), f node))

unwrap :: Wrap l pos s node -> node
unwrap (Compose (_, (_, node))) = node
