{-# Language DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,  OverloadedStrings,
             RankNTypes, ScopedTypeVariables, StandaloneDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}

module Language.Haskell.Binder where

import Data.Data (Data, Typeable)
import Data.Foldable (fold, toList)
import Data.Functor.Compose (Compose(..))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Semigroup.Union (UnionWith(..))
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

import qualified Rank2
import Transformation (Transformation)
import qualified Transformation
import qualified Transformation.AG.Monomorphic as AG.Mono
import qualified Transformation.Deep as Deep
import qualified Transformation.Full as Full
import qualified Transformation.Rank2

import qualified Language.Haskell.Abstract as Abstract
import qualified Language.Haskell.AST as AST

type Environment l = UnionWith (Map (AST.QualifiedName l)) (Binding l)

type WithEnvironment l = Compose ((,) (AG.Mono.Atts (Environment l)))

type FromEnvironment l f = Compose ((->) (Environment l)) (WithEnvironment l f)

data Binding l = ErroneousBinding String
               | ModuleBinding (UnionWith (Map (AST.Name l)) (Binding l))
               | InfixDeclaration Bool (AST.Associativity l) Int
               deriving (Data, Typeable, Eq, Show)

deriving instance (Ord k, Data k, Data v) => Data (UnionWith (Map k) v)
deriving instance (Ord k, Eq v) => Eq (UnionWith (Map k) v)
deriving instance (Show k, Show v) => Show (UnionWith (Map k) v)

instance Semigroup (Binding l) where
   a@InfixDeclaration{} <> InfixDeclaration False _ _ = a
   InfixDeclaration False _ _ <> a@InfixDeclaration{} = a
   a <> b
      | a == b = a
      | otherwise = ErroneousBinding ("Clashing: " ++ show (a, b))

instance Monoid (Binding l) where
   mempty = ModuleBinding (UnionWith mempty)

withBindings :: (Full.Traversable (AG.Mono.Keep (Binder l p)) g, q ~ Compose ((,) (AG.Mono.Atts (Environment l))) p)
             => Environment l -> p (g p p) -> q (g q q)
withBindings = flip (Full.traverse (AG.Mono.Keep Binder))

onMap :: (Map.Map j a -> Map.Map k b) -> UnionWith (Map j) a -> UnionWith (Map k) b
onMap f (UnionWith x) = UnionWith (f x)

data Binder l (f :: Type -> Type) = Binder

instance Transformation (AG.Mono.Keep (Binder l f)) where
   type Domain (AG.Mono.Keep (Binder l f)) = f
   type Codomain (AG.Mono.Keep (Binder l f)) = FromEnvironment l f

instance {-# OVERLAPS #-}
         (Abstract.Haskell l, Abstract.EquationLHS l ~ AST.EquationLHS l,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Ord (Abstract.QualifiedName l), Foldable f) =>
         AG.Mono.Attribution (AG.Mono.Keep (Binder l f)) (Environment l) (AST.Declaration l l) (FromEnvironment l f) f
         where
   attribution _ node atts = atts{AG.Mono.syn= synthesis, AG.Mono.inh= bequest}
      where bequeath, export :: AST.Declaration l l (FromEnvironment l f) (FromEnvironment l f) -> Environment l
            export (AST.FixityDeclaration associativity precedence names) =
               UnionWith (Map.fromList [(Abstract.qualifiedName Nothing name,
                                         InfixDeclaration True associativity $ fromMaybe 9 precedence)
                                        | name <- toList names])
            export AST.ClassDeclaration{} = AG.Mono.syn atts
            export (AST.EquationDeclaration lhs _ _)
               | [name] <- foldMap getOperatorName (getCompose lhs mempty)
               = UnionWith (Map.singleton (Abstract.qualifiedName Nothing name)
                            $ InfixDeclaration False AST.LeftAssociative 9)
            export _ = mempty
            getOperatorName (AST.InfixLHS _ name _) = [name]
            getOperatorName (AST.PrefixLHS lhs _) = foldMap getOperatorName (getCompose lhs mempty)
            getOperatorName (AST.VariableLHS name) = [name]
            getOperatorName _ = []
            bequeath AST.EquationDeclaration{} = AG.Mono.syn atts <> AG.Mono.inh atts
            bequeath _ = AG.Mono.inh atts
            synthesis = foldMap export node
            bequest = foldMap bequeath node

-- | Resolve ambiguities in a single module. The imports are resolved using the given map of already resolved
-- modules. Note that all class constraints in the function's type signature are satisfied by the Haskell
-- 'AST.Language'.
instance {-# OVERLAPS #-}
         (Abstract.Haskell l, Abstract.QualifiedName l ~ AST.QualifiedName l, Abstract.Name l ~ AST.Name l,
          Abstract.Module l l ~ AST.Module l l, Abstract.ModuleName l ~ AST.ModuleName l,
          Abstract.Export l l ~ AST.Export l l, Abstract.Import l l ~ AST.Import l l,
          Abstract.ImportSpecification l l ~ AST.ImportSpecification l l, Abstract.ImportItem l l ~ AST.ImportItem l l,
          Abstract.Members l ~ AST.Members l,
          Ord (Abstract.QualifiedName l), Foldable f) =>
         AG.Mono.Attribution (AG.Mono.Keep (Binder l f)) (Environment l) (AST.Module l l) (FromEnvironment l f) f where
   attribution _ node atts = foldMap moduleAttribution node
      where moduleAttribution :: AST.Module l l (FromEnvironment l f) (FromEnvironment l f)
                              -> AG.Mono.Atts (Environment l)
            moduleAttribution (AST.AnonymousModule modImports body) =
               AG.Mono.Atts{AG.Mono.inh= moduleGlobalScope,
                            AG.Mono.syn= onMap (Map.filterWithKey (const . (== mainName))) moduleGlobalScope}
               where moduleGlobalScope = importedScope modImports <> AG.Mono.syn atts
                     mainName = Abstract.qualifiedName Nothing (Abstract.name "main")
            moduleAttribution (AST.NamedModule moduleName exports modImports body) =
               atts{AG.Mono.syn= exportedScope, AG.Mono.inh= moduleGlobalScope}
               where exportedScope, moduleGlobalScope :: Environment l
                     exported :: AST.QualifiedName l -> Bool
                     exportedScope = UnionWith $ Map.singleton (qualifiedModuleName moduleName) $ ModuleBinding
                                     $ UnionWith $ Map.mapKeys baseName $ Map.filterWithKey (const . exported)
                                     $ getUnionWith moduleGlobalScope
                     exported qn@(AST.QualifiedName modName name) =
                        maybe True (any $ any exportedBy . ($ mempty) . getCompose) exports
                        where exportedBy (AST.ReExportModule modName') = modName == Just modName'
                                                                         || modName == Nothing && modName' == moduleName
                              exportedBy (AST.ExportVar qn') = qn == qn'
                              exportedBy (AST.ExportClassOrType parent members) = qn == parent || any exportedByMember members
                              exportedByMember AST.AllMembers = error "What does this refer to !?"
                              exportedByMember (AST.MemberList names) = elem name names
                     moduleGlobalScope = importedScope modImports
                                         <> requalifiedWith moduleName (AG.Mono.syn atts)
                                         <> AG.Mono.syn atts
            importedScope :: [FromEnvironment l f (AST.Import l l (FromEnvironment l f) (FromEnvironment l f))]
                          -> Environment l
            importedScope modImports = fold (Map.mapWithKey importsFrom $ getUnionWith $ AG.Mono.inh atts)
               where importsFromModule :: UnionWith (Map (AST.Name l)) (Binding l)
                                       -> AST.Import l l (FromEnvironment l f) (FromEnvironment l f) -> Environment l
                     importsFrom (AST.QualifiedName (Just moduleName) _) (ModuleBinding moduleExports)
                        | null matchingImports && moduleName == preludeName = unqualified moduleExports
                        | otherwise = foldMap (importsFromModule moduleExports) matchingImports
                        where matchingImports = foldMap (foldMap matchingImport . ($ mempty) . getCompose) modImports
                              matchingImport i@(AST.Import _ name _ _)
                                 | name == moduleName = [i]
                                 | otherwise = []
                     importsFrom _ _ = mempty
                     importsFromModule moduleExports (AST.Import qualified name alias spec)
                        | qualified = qualifiedWith (fromMaybe name alias) (imports spec)
                        | otherwise = unqualified (imports spec)
                                      <> maybe mempty (`qualifiedWith` imports spec) alias
                        where imports (Just spec) = foldMap specImports (getCompose spec mempty)
                              imports Nothing = allImports
                              specImports (AST.ImportSpecification False items) = itemsImports items
                              specImports (AST.ImportSpecification True items) =
                                 UnionWith (getUnionWith allImports `Map.difference` getUnionWith (itemsImports items))
                              allImports = moduleExports
                              itemsImports = foldMap (foldMap itemImports . ($ mempty) . getCompose)
                              itemImports (AST.ImportClassOrType name members) =
                                 nameImport name <> foldMap (memberImports name) members
                              itemImports (AST.ImportVar name) = nameImport name
                              memberImports name AST.AllMembers = allMemberImports name
                              memberImports name (AST.MemberList members) = foldMap (memberImport name) members
                              allMemberImports name = mempty
                              memberImport name member = mempty
                              nameImport name = mempty
                              getImportName (_, AST.Import _ moduleName _ _) = moduleName
                     getImportName (_, AST.Import _ moduleName _ _) = moduleName
            qualifiedWith moduleName = onMap (Map.mapKeysMonotonic $ AST.QualifiedName $ Just moduleName)
            requalifiedWith moduleName = onMap (Map.mapKeysMonotonic requalify)
               where requalify (AST.QualifiedName Nothing name) = AST.QualifiedName (Just moduleName) name
            unqualified :: UnionWith (Map (AST.Name l)) a -> UnionWith (Map (AST.QualifiedName l)) a
            unqualified = onMap (Map.mapKeysMonotonic $ AST.QualifiedName Nothing)

qualifiedModuleName :: Abstract.Haskell l => Abstract.ModuleName l -> Abstract.QualifiedName l
qualifiedModuleName moduleName = Abstract.qualifiedName (Just moduleName) (Abstract.name "[module]")
baseName (AST.QualifiedName _ name) = name

preludeName :: Abstract.Haskell l => Abstract.ModuleName l
preludeName = Abstract.moduleName (Abstract.name "Prelude" :| [])

predefinedModuleBindings :: (Abstract.Haskell l, Ord (Abstract.QualifiedName l),
                             Abstract.QualifiedName l ~ AST.QualifiedName l,
                             Abstract.Name l ~ AST.Name l,
                             Abstract.Associativity l ~ AST.Associativity l) => Environment l
predefinedModuleBindings = UnionWith (Map.fromList [(qualifiedModuleName preludeName,
                                                     ModuleBinding $ UnionWith unqualifiedPreludeBindings)])

preludeBindings :: (Abstract.Haskell l, Ord (Abstract.Name l),
                    Abstract.QualifiedName l ~ AST.QualifiedName l,
                    Abstract.Associativity l ~ AST.Associativity l) => Environment l
preludeBindings = UnionWith (Map.mapKeysMonotonic (Abstract.qualifiedName Nothing) unqualifiedPreludeBindings)

unqualifiedPreludeBindings :: (Abstract.Haskell l, Ord (Abstract.Name l),
                               Abstract.Associativity l ~ AST.Associativity l) => Map.Map (Abstract.Name l) (Binding l)
unqualifiedPreludeBindings = Map.fromList $
   [(Abstract.name "!!", InfixDeclaration True Abstract.leftAssociative 9),
    (Abstract.name ".", InfixDeclaration True Abstract.rightAssociative 9)]
   ++
   [(Abstract.name op, InfixDeclaration True Abstract.rightAssociative 8)
    | op <- ["^", "^^", "**"]]
   ++
   [(Abstract.name op, InfixDeclaration True Abstract.leftAssociative 7)
    | op <- ["*", "/", "`div`", "`mod`", "`rem`", "`quot`"]]
   ++
   [(Abstract.name "+", InfixDeclaration True Abstract.leftAssociative 6),
    (Abstract.name "-", InfixDeclaration True Abstract.leftAssociative 6)]
   ++
   [(Abstract.name ":", InfixDeclaration True Abstract.rightAssociative 5),
    (Abstract.name "++", InfixDeclaration True Abstract.rightAssociative 5)]
   ++
   [(Abstract.name op, InfixDeclaration True Abstract.nonAssociative 4)
    | op <- ["==", "/=", "<", "<=", ">", ">=", "`elem`", "`notElem`"]]
   ++
   [(Abstract.name "&&", InfixDeclaration True Abstract.rightAssociative 3),
    (Abstract.name "||", InfixDeclaration True Abstract.rightAssociative 2),
    (Abstract.name ">>", InfixDeclaration True Abstract.leftAssociative 1),
    (Abstract.name ">>=", InfixDeclaration True Abstract.leftAssociative 1)]
   ++
   [(Abstract.name op, InfixDeclaration True Abstract.rightAssociative 0)
    | op <- ["$", "$!", "`seq`"]]
