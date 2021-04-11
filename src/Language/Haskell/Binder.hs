{-# Language DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,  OverloadedStrings,
             RankNTypes, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}

module Language.Haskell.Binder where

import Data.Data (Data, Typeable)
import Data.Foldable (fold, toList)
import Data.Functor.Compose (Compose(..))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Map.Monoidal (MonoidalMap(..))
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

type Environment l = MonoidalMap (AST.QualifiedName l) (Binding l)

type WithEnvironment l = Compose ((,) (AG.Mono.Atts (Environment l)))

type FromEnvironment l f = Compose ((->) (Environment l)) (WithEnvironment l f)

data Binding l = ErroneousBinding String
               | ModuleBinding (MonoidalMap (AST.Name l) (Binding l))
               | InfixDeclaration Bool (AST.Associativity l) Int
               deriving (Data, Typeable, Eq, Show)

instance Semigroup (Binding l) where
   a@InfixDeclaration{} <> InfixDeclaration False _ _ = a
   InfixDeclaration False _ _ <> a@InfixDeclaration{} = a
   a <> b
      | a == b = a
      | otherwise = ErroneousBinding ("Clashing: " ++ show (a, b))

withBindings :: (Full.Traversable (Binder l p) g, q ~ Compose ((,) (AG.Mono.Atts (Environment l))) p)
             => Environment l -> p (g p p) -> q (g q q)
withBindings = flip (Full.traverse Binder)

onMap :: (Map.Map j a -> Map.Map k b) -> MonoidalMap j a -> MonoidalMap k b
onMap f (MonoidalMap x) = MonoidalMap (f x)

data Binder l (f :: Type -> Type) = Binder

instance Transformation (Binder l f) where
   type Domain (Binder l f) = f
   type Codomain (Binder l f) = FromEnvironment l f

instance {-# OVERLAPS #-}
         (Abstract.Haskell l, Abstract.EquationLHS l ~ AST.EquationLHS l,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Ord (Abstract.QualifiedName l), Foldable f) =>
         AG.Mono.Attribution (Binder l f) (Environment l) (AST.Declaration l l) (FromEnvironment l f) f where
   attribution _ node atts = atts{AG.Mono.syn= synthesis, AG.Mono.inh= bequest}
      where bequeath, export :: AST.Declaration l l (FromEnvironment l f) (FromEnvironment l f) -> Environment l
            export (AST.FixityDeclaration associativity precedence names) =
               MonoidalMap (Map.fromList [(Abstract.qualifiedName Nothing name,
                                         InfixDeclaration True associativity $ fromMaybe 9 precedence)
                                        | name <- toList names])
            export AST.ClassDeclaration{} = AG.Mono.syn atts
            export (AST.EquationDeclaration lhs _ _)
               | [name] <- foldMap getOperatorName (getCompose lhs mempty)
               = MonoidalMap (Map.singleton (Abstract.qualifiedName Nothing name)
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
         AG.Mono.Attribution (Binder l f) (Environment l) (AST.Module l l) (FromEnvironment l f) f where
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
                     exportedScope = MonoidalMap $ Map.singleton (qualifiedModuleName moduleName) $ ModuleBinding
                                     $ MonoidalMap $ Map.mapKeys baseName $ Map.filterWithKey (const . exported)
                                     $ getMonoidalMap moduleGlobalScope
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
            importedScope modImports = fold (Map.mapWithKey importsFrom $ getMonoidalMap $ AG.Mono.inh atts)
               where importsFromModule :: MonoidalMap (AST.Name l) (Binding l)
                                       -> AST.Import l l (FromEnvironment l f) (FromEnvironment l f) -> Environment l
                     importsFrom (AST.QualifiedName (Just moduleName) _) (ModuleBinding moduleExports)
                        | null matchingImports && moduleName == preludeName = unqualified moduleExports
                        | otherwise = foldMap (importsFromModule moduleExports) matchingImports
                        where matchingImports = foldMap (foldMap matchingImport . ($ mempty) . getCompose) modImports
                              matchingImport i@(AST.Import _ name _ _)
                                 | name == moduleName = [i]
                                 | otherwise = []
                     importsFromModule moduleExports (AST.Import qualified name alias spec)
                        | qualified = qualifiedWith (fromMaybe name alias) (imports spec)
                        | otherwise = unqualified (imports spec)
                                      <> maybe mempty (`qualifiedWith` imports spec) alias
                        where imports (Just spec) = foldMap specImports (getCompose spec mempty)
                              imports Nothing = allImports
                              specImports (AST.ImportSpecification False items) = itemsImports items
                              specImports (AST.ImportSpecification True items) =
                                 MonoidalMap (getMonoidalMap allImports `Map.difference` getMonoidalMap (itemsImports items))
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
            unqualified :: MonoidalMap (AST.Name l) a -> MonoidalMap (AST.QualifiedName l) a
            unqualified = onMap (Map.mapKeysMonotonic $ AST.QualifiedName Nothing)

instance (Traversable f, Rank2.Traversable (g f), Full.Functor (Binder l f) g,
          Transformation.At (Binder l f) (g (FromEnvironment l f) (FromEnvironment l f)),
          Deep.Traversable (AG.Mono.Feeder (Environment l) f) g) =>
         Full.Traversable (Binder l f) g where
   traverse = AG.Mono.traverseDefaultWithAttributes

instance (Transformation.At (Binder l f) (g (FromEnvironment l f) (FromEnvironment l f)),
          Deep.Functor (Binder l f) g, Functor f) =>
         Full.Functor (Binder l f) g where
   (<$>) = Full.mapUpDefault

instance {-# OVERLAPPABLE #-} (Ord (Abstract.ModuleName l), Ord (Abstract.QualifiedName l), Ord (Abstract.Name l),
                               AG.Mono.Attribution (Binder l f) (Environment l) g (FromEnvironment l f) f,
                               Foldable f, Functor f, Rank2.Foldable (g (FromEnvironment l f))) =>
         Transformation.At (Binder l f) (g (FromEnvironment l f) (FromEnvironment l f)) where
   ($) = AG.Mono.applyDefaultWithAttributes

qualifiedModuleName :: Abstract.Haskell l => Abstract.ModuleName l -> Abstract.QualifiedName l
qualifiedModuleName moduleName = Abstract.qualifiedName (Just moduleName) (Abstract.name "[module]")
baseName (AST.QualifiedName _ name) = name

preludeName :: Abstract.Haskell l => Abstract.ModuleName l
preludeName = Abstract.moduleName (Abstract.name "Prelude" :| [])

predefinedModuleBindings :: (Abstract.Haskell l, Ord (Abstract.QualifiedName l),
                             Abstract.QualifiedName l ~ AST.QualifiedName l,
                             Abstract.Name l ~ AST.Name l,
                             Abstract.Associativity l ~ AST.Associativity l) => Environment l
predefinedModuleBindings = MonoidalMap (Map.fromList [(qualifiedModuleName preludeName,
                                                       ModuleBinding $ MonoidalMap unqualifiedPreludeBindings)])

preludeBindings :: (Abstract.Haskell l, Ord (Abstract.Name l),
                    Abstract.QualifiedName l ~ AST.QualifiedName l,
                    Abstract.Associativity l ~ AST.Associativity l) => Environment l
preludeBindings = MonoidalMap (Map.mapKeysMonotonic (Abstract.qualifiedName Nothing) unqualifiedPreludeBindings)

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
