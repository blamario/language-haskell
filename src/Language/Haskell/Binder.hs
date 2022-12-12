{-# Language DeriveDataTypeable, FlexibleContexts, FlexibleInstances, LambdaCase, MultiParamTypeClasses,
             OverloadedStrings, RankNTypes, ScopedTypeVariables, StandaloneDeriving,
             TypeFamilies, TypeOperators, UndecidableInstances #-}

-- | Monomorphic attribute grammar for establishing the static identifier bindings

module Language.Haskell.Binder (
   Binder,
   Binding(ErroneousBinding, TypeBinding, ValueBinding, TypeAndValueBinding),
   TypeBinding(TypeClass), ValueBinding(InfixDeclaration),
   Environment, LocalEnvironment, ModuleEnvironment, WithEnvironment,
   lookupType, lookupValue,
   predefinedModuleBindings, preludeBindings, withBindings) where

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
import qualified Transformation.AG.Dimorphic as Di
import qualified Transformation.Deep as Deep
import qualified Transformation.Full as Full
import qualified Transformation.Rank2

import qualified Language.Haskell.Abstract as Abstract
import qualified Language.Haskell.AST as AST hiding (Declaration(..))
import qualified Language.Haskell.Extensions.AST as ExtAST
import qualified Language.Haskell.Extensions.AST as AST (Declaration(..))

type Environment l = UnionWith (Map (AST.QualifiedName l)) (Binding l)

type LocalEnvironment l = UnionWith (Map (AST.Name l)) (Binding l)

type ModuleEnvironment l = UnionWith (Map (AST.ModuleName l)) (LocalEnvironment l)

type WithEnvironment l = Compose ((,) (Di.Atts (Environment l) (LocalEnvironment l)))

type FromEnvironment l f = Compose ((->) (Environment l)) (WithEnvironment l f)

data Binding l = ErroneousBinding String
               | TypeBinding (TypeBinding l)
               | ValueBinding (ValueBinding l)
               | TypeAndValueBinding (TypeBinding l) (ValueBinding l)
               deriving Typeable

data TypeBinding l = TypeClass (LocalEnvironment l)
                   | DataType

data ValueBinding l = InfixDeclaration Bool (AST.Associativity l) Int
                    | DataConstructor
                    | RecordConstructor
                    | RecordField
                    deriving (Typeable, Data, Eq, Show)

lookupType :: (Ord (Abstract.ModuleName l), Ord (Abstract.Name l))
           => AST.QualifiedName l -> Environment l -> Maybe (TypeBinding l)
lookupType name (UnionWith env) = Map.lookup name env >>= \case
  TypeBinding t -> Just t
  TypeAndValueBinding t _ -> Just t
  _ -> Nothing

lookupValue :: (Ord (Abstract.ModuleName l), Ord (Abstract.Name l))
            => AST.QualifiedName l -> Environment l -> Maybe (ValueBinding l)
lookupValue name (UnionWith env) = Map.lookup name env >>= \case
  ValueBinding v -> Just v
  TypeAndValueBinding _ v -> Just v
  _ -> Nothing

deriving instance (Data l, Typeable l, Data (Abstract.ModuleName l), Data (Abstract.Name l),
                   Ord (Abstract.ModuleName l), Ord (Abstract.Name l)) => Data (Binding l)
deriving instance (Ord (Abstract.ModuleName l), Ord (Abstract.Name l)) => Eq (Binding l)
deriving instance (Show (Abstract.ModuleName l), Show (Abstract.Name l)) => Show (Binding l)

deriving instance (Data l, Typeable l, Data (Abstract.ModuleName l), Data (Abstract.Name l),
                   Ord (Abstract.ModuleName l), Ord (Abstract.Name l)) => Data (TypeBinding l)
deriving instance (Ord (Abstract.ModuleName l), Ord (Abstract.Name l)) => Eq (TypeBinding l)
deriving instance (Show (Abstract.ModuleName l), Show (Abstract.Name l)) => Show (TypeBinding l)

deriving instance (Ord k, Data k, Data v) => Data (UnionWith (Map k) v)
deriving instance (Ord k, Eq v) => Eq (UnionWith (Map k) v)
deriving instance (Show k, Show v) => Show (UnionWith (Map k) v)

instance (Ord (Abstract.ModuleName l), Ord (Abstract.Name l),
          Show (Abstract.ModuleName l), Show (Abstract.Name l)) => Semigroup (Binding l) where
   a@(ValueBinding InfixDeclaration{}) <> ValueBinding (InfixDeclaration False _ _) = a
   ValueBinding (InfixDeclaration False _ _) <> a@(ValueBinding InfixDeclaration{}) = a
   ValueBinding v <> TypeBinding t = TypeAndValueBinding t v
   TypeBinding t <> ValueBinding v = TypeAndValueBinding t v
   a <> b
      | a == b = a
      | otherwise = ErroneousBinding ("Clashing: " ++ show (a, b))

instance (Ord (Abstract.ModuleName l), Ord (Abstract.Name l),
          Show (Abstract.ModuleName l), Show (Abstract.Name l)) => Monoid (Binding l) where
   mempty = ErroneousBinding "nothing here"

withBindings :: (Full.Traversable (Di.Keep (Binder l p)) g, q ~ Compose ((,) (Di.Atts (Environment l) (LocalEnvironment l))) p)
             => ModuleEnvironment l -> Environment l -> p (g p p) -> q (g q q)
withBindings modEnv = flip (Full.traverse (Di.Keep $ Binder modEnv))

onMap :: (Map.Map j a -> Map.Map k b) -> UnionWith (Map j) a -> UnionWith (Map k) b
onMap f (UnionWith x) = UnionWith (f x)

data Binder l (f :: Type -> Type) = Binder (ModuleEnvironment l)

instance Transformation (Di.Keep (Binder l f)) where
   type Domain (Di.Keep (Binder l f)) = f
   type Codomain (Di.Keep (Binder l f)) = FromEnvironment l f

instance {-# OVERLAPS #-}
         (Abstract.Haskell l, Abstract.TypeLHS l ~ ExtAST.TypeLHS l, Abstract.EquationLHS l ~ AST.EquationLHS l,
          Abstract.QualifiedName l ~ AST.QualifiedName l, Abstract.Name l ~ AST.Name l,
          Ord (Abstract.ModuleName l), Ord (Abstract.Name l),
          Show (Abstract.ModuleName l), Show (Abstract.Name l),
          Foldable f) =>
         Di.Attribution
            (Di.Keep (Binder l f))
            (Environment l)
            (LocalEnvironment l)
            (AST.Declaration l l)
            (FromEnvironment l f)
            f
         where
   attribution _ node atts = atts{Di.syn= synthesis, Di.inh= bequest}
      where bequeath :: AST.Declaration l l (FromEnvironment l f) (FromEnvironment l f) -> Environment l
            export :: AST.Declaration l l (FromEnvironment l f) (FromEnvironment l f) -> LocalEnvironment l
            export (AST.FixityDeclaration associativity precedence names) =
               UnionWith (Map.fromList [(name,
                                         ValueBinding $ InfixDeclaration True associativity $ fromMaybe 9 precedence)
                                        | name <- toList names])
            export (ExtAST.ClassDeclaration _ lhs decls)
               | [name] <- foldMap getTypeName (getCompose lhs mempty)
               = Di.syn atts <> UnionWith (Map.singleton name $ TypeBinding $ TypeClass $ Di.syn atts)
            export (AST.EquationDeclaration lhs _ _)
               | [name] <- foldMap getOperatorName (getCompose lhs mempty)
               = UnionWith (Map.singleton name $ ValueBinding $ InfixDeclaration False AST.LeftAssociative 9)
            export (AST.DataDeclaration _context lhs _kind _constructors _derivings)
               | [name] <- foldMap getTypeName (getCompose lhs mempty)
               = Di.syn atts <> UnionWith (Map.singleton name $ TypeBinding DataType)
            export _ = mempty
            getOperatorName (AST.InfixLHS _ name _) = [name]
            getOperatorName (AST.PrefixLHS lhs _) = foldMap getOperatorName (getCompose lhs mempty)
            getOperatorName (AST.VariableLHS name) = [name]
            getOperatorName _ = []
            getTypeName (ExtAST.SimpleTypeLHS name _) = [name]
            bequeath AST.EquationDeclaration{} = unqualified (Di.syn atts) <> Di.inh atts
            bequeath _ = Di.inh atts
            synthesis = foldMap export node
            bequest = foldMap bequeath node

-- | Resolve ambiguities in a single module. The imports are resolved using the given map of already resolved
-- modules. Note that all class constraints in the function's type signature are satisfied by the Haskell
-- 'AST.Language'.
instance {-# OVERLAPS #-}
         (Abstract.Haskell l, Abstract.QualifiedName l ~ AST.QualifiedName l, Abstract.Name l ~ AST.Name l,
          Abstract.Module l l ~ AST.Module l l, Abstract.ModuleName l ~ AST.ModuleName l,
          Abstract.Export l l ~ AST.Export l l, Abstract.Import l l ~ ExtAST.Import l l,
          Abstract.ImportSpecification l l ~ AST.ImportSpecification l l, Abstract.ImportItem l l ~ AST.ImportItem l l,
          BindingMembers l,
          Ord (Abstract.QualifiedName l), Foldable f) =>
         Di.Attribution
            (Di.Keep (Binder l f))
            (Environment l)
            (LocalEnvironment l)
            (AST.Module l l)
            (FromEnvironment l f) f
         where
   attribution (Di.Keep (Binder modEnv)) node atts = foldMap moduleAttribution node
      where moduleAttribution :: AST.Module l l (FromEnvironment l f) (FromEnvironment l f)
                              -> Di.Atts (Environment l) (LocalEnvironment l)
            moduleAttribution (AST.ExtendedModule extensions body) = atts
            moduleAttribution (AST.AnonymousModule modImports body) =
               Di.Atts{
                  Di.inh= moduleGlobalScope,
                  Di.syn= onMap
                                  (Map.mapKeysMonotonic baseName . Map.filterWithKey (const . (== mainName)))
                                  moduleGlobalScope}
               where moduleGlobalScope = importedScope modImports <> unqualified (Di.syn atts)
                     mainName = Abstract.qualifiedName Nothing (Abstract.name "main")
            moduleAttribution (AST.NamedModule moduleName exports modImports body) =
               atts{Di.syn= exportedScope, Di.inh= moduleGlobalScope}
               where exportedScope :: LocalEnvironment l
                     moduleGlobalScope :: Environment l
                     exported :: AST.QualifiedName l -> Bool
                     exportedScope = UnionWith $ Map.mapKeysMonotonic baseName $ Map.filterWithKey (const . exported)
                                     $ getUnionWith moduleGlobalScope
                     exported qn@(AST.QualifiedName modName name) =
                        maybe True (any $ any exportedBy . ($ mempty) . getCompose) exports
                        where exportedBy (AST.ReExportModule modName') = modName == Just modName'
                                                                         || modName == Nothing && modName' == moduleName
                              exportedBy (AST.ExportVar qn') = qn == qn'
                              exportedBy (AST.ExportClassOrType parent members) =
                                 qn == parent || any (hasMember name) members
                     moduleGlobalScope = importedScope modImports
                                         <> qualifiedWith moduleName (Di.syn atts)
                                         <> unqualified (Di.syn atts)
            importedScope :: [FromEnvironment l f (ExtAST.Import l l (FromEnvironment l f) (FromEnvironment l f))]
                          -> Environment l
            importedScope modImports = fold (Map.mapWithKey importsFrom $ getUnionWith modEnv)
               where importsFromModule :: UnionWith (Map (AST.Name l)) (Binding l)
                                       -> ExtAST.Import l l (FromEnvironment l f) (FromEnvironment l f) -> Environment l
                     importsFrom moduleName moduleExports
                        | null matchingImports && moduleName == preludeName = unqualified moduleExports
                        | otherwise = foldMap (importsFromModule moduleExports) matchingImports
                        where matchingImports = foldMap (foldMap matchingImport . ($ mempty) . getCompose) modImports
                              matchingImport i@(ExtAST.Import _ _ _ name _ _)
                                 | name == moduleName = [i]
                                 | otherwise = []
                     importsFromModule moduleExports (ExtAST.Import _ qualified _ name alias spec)
                        | qualified = qualifiedWith (fromMaybe name alias) (imports spec)
                        | otherwise = unqualified (imports spec)
                                      <> maybe mempty (`qualifiedWith` imports spec) alias
                        where imports (Just spec) = foldMap specImports (getCompose spec mempty)
                              imports Nothing = allImports
                              specImports (AST.ImportSpecification True items) = itemsImports items
                              specImports (AST.ImportSpecification False items) =
                                 UnionWith (getUnionWith allImports `Map.difference` getUnionWith (itemsImports items))
                              allImports = moduleExports
                              itemsImports = foldMap (foldMap itemImports . ($ mempty) . getCompose)
                              itemImports (AST.ImportClassOrType name members) =
                                 nameImport name allImports <> foldMap (memberImports name) members
                              itemImports (AST.ImportVar name) = nameImport name allImports

instance {-# OVERLAPS #-}
         (Abstract.Haskell l,
          Abstract.QualifiedName l ~ AST.QualifiedName l, Abstract.Name l ~ AST.Name l,
          Ord (Abstract.ModuleName l), Ord (Abstract.Name l),
          Show (Abstract.ModuleName l), Show (Abstract.Name l),
          Foldable f) =>
         Di.Attribution
            (Di.Keep (Binder l f))
            (Environment l)
            (LocalEnvironment l)
            (AST.DataConstructor l l)
            (FromEnvironment l f)
            f
         where
   attribution _ node atts = atts{Di.syn= synthesis, Di.inh= Di.inh atts}
      where export :: AST.DataConstructor l l (FromEnvironment l f) (FromEnvironment l f) -> LocalEnvironment l
            export (AST.Constructor name _types) = UnionWith (Map.singleton name $ ValueBinding DataConstructor)
            export (AST.RecordConstructor name _flds) = UnionWith (Map.singleton name $ ValueBinding RecordConstructor)
            synthesis = foldMap export node <>  Di.syn atts

instance {-# OVERLAPS #-}
         (Abstract.Haskell l,
          Abstract.QualifiedName l ~ AST.QualifiedName l, Abstract.Name l ~ AST.Name l,
          Ord (Abstract.ModuleName l), Ord (Abstract.Name l),
          Show (Abstract.ModuleName l), Show (Abstract.Name l),
          Foldable f) =>
         Di.Attribution
            (Di.Keep (Binder l f))
            (Environment l)
            (LocalEnvironment l)
            (ExtAST.DataConstructor l l)
            (FromEnvironment l f)
            f
         where
   attribution _ node atts = atts{Di.syn= synthesis, Di.inh= Di.inh atts}
      where export :: ExtAST.DataConstructor l l (FromEnvironment l f) (FromEnvironment l f) -> LocalEnvironment l
            export (ExtAST.Constructor name _types) = UnionWith (Map.singleton name $ ValueBinding DataConstructor)
            export (ExtAST.RecordConstructor name _fields) =
               UnionWith (Map.singleton name $ ValueBinding RecordConstructor)
            export ExtAST.ExistentialConstructor{} = mempty
            synthesis = foldMap export node <>  Di.syn atts

instance {-# OVERLAPS #-}
         (Abstract.Haskell l,
          Abstract.QualifiedName l ~ AST.QualifiedName l, Abstract.Name l ~ AST.Name l,
          Ord (Abstract.ModuleName l), Ord (Abstract.Name l),
          Show (Abstract.ModuleName l), Show (Abstract.Name l),
          Foldable f) =>
         Di.Attribution
            (Di.Keep (Binder l f))
            (Environment l)
            (LocalEnvironment l)
            (AST.FieldDeclaration l l)
            (FromEnvironment l f)
            f
         where
   attribution _ node atts = atts{Di.syn= synthesis, Di.inh= Di.inh atts}
      where export :: AST.FieldDeclaration l l (FromEnvironment l f) (FromEnvironment l f) -> LocalEnvironment l
            export (AST.ConstructorFields names t) =
               UnionWith $ Map.fromList [(name, ValueBinding RecordField) | name <- toList names]
            synthesis = foldMap export node <>  Di.syn atts

class Abstract.Haskell l => BindingMembers l where
   memberImports :: Abstract.Name l -> Abstract.Members l -> UnionWith (Map (Abstract.Name l)) (Binding l)
   allMemberImports :: Abstract.Name l -> UnionWith (Map (Abstract.Name l)) (Binding l)
   hasMember :: Abstract.Name l -> Abstract.Members l -> Bool

instance BindingMembers AST.Language where
  memberImports name AST.AllMembers = allMemberImports name
  memberImports name (AST.MemberList members) = foldMap (`nameImport` allMemberImports name) members
  allMemberImports name = mempty
  hasMember _ AST.AllMembers = error "What does this refer to !?"
  hasMember name (AST.MemberList names) = elem name names

instance BindingMembers ExtAST.Language where
  memberImports name ExtAST.AllMembers = allMemberImports name
  memberImports name (ExtAST.MemberList members) = foldMap (`nameImport` allMemberImports name) members
  memberImports name (ExtAST.ExplicitlyNamespacedMemberList members) =
     foldMap (`memberImport` allMemberImports name) members
     where memberImport (ExtAST.DefaultMember name) imports = nameImport name imports
           memberImport (ExtAST.PatternMember name) imports = nameImport name imports
           memberImport (ExtAST.TypeMember name) imports = nameImport name imports
  allMemberImports name = mempty
  hasMember _ ExtAST.AllMembers = error "What does this refer to !?"
  hasMember name (ExtAST.MemberList names) = elem name names

nameImport name imports = foldMap (UnionWith . Map.singleton name) (Map.lookup name $ getUnionWith imports)

qualifiedWith :: Abstract.Haskell l
              => Abstract.ModuleName l -> UnionWith (Map (Abstract.Name l)) a -> UnionWith (Map (AST.QualifiedName l)) a
qualifiedWith moduleName = onMap (Map.mapKeysMonotonic $ AST.QualifiedName $ Just moduleName)

unqualified :: Abstract.Haskell l => UnionWith (Map (Abstract.Name l)) a -> UnionWith (Map (Abstract.QualifiedName l)) a
unqualified = onMap (Map.mapKeysMonotonic unqualifiedName)

baseName :: AST.QualifiedName l -> Abstract.Name l
baseName (AST.QualifiedName _ name) = name

unqualifiedName :: Abstract.Haskell l => Abstract.Name l -> Abstract.QualifiedName l
unqualifiedName = Abstract.qualifiedName Nothing

preludeName :: Abstract.Haskell l => Abstract.ModuleName l
preludeName = Abstract.moduleName (Abstract.name "Prelude" :| [])

predefinedModuleBindings :: (Abstract.Haskell l, Ord (Abstract.QualifiedName l),
                             Abstract.ModuleName l ~ AST.ModuleName l,
                             Abstract.Name l ~ AST.Name l,
                             Abstract.Associativity l ~ AST.Associativity l) => ModuleEnvironment l
predefinedModuleBindings = UnionWith (Map.fromList [(preludeName, UnionWith unqualifiedPreludeBindings)])

preludeBindings :: (Abstract.Haskell l, Ord (Abstract.Name l),
                    Abstract.QualifiedName l ~ AST.QualifiedName l,
                    Abstract.Associativity l ~ AST.Associativity l) => Environment l
preludeBindings = UnionWith (Map.mapKeysMonotonic (Abstract.qualifiedName Nothing) unqualifiedPreludeBindings)

unqualifiedPreludeBindings :: (Abstract.Haskell l, Ord (Abstract.Name l),
                               Abstract.Associativity l ~ AST.Associativity l) => Map.Map (Abstract.Name l) (Binding l)
unqualifiedPreludeBindings = Map.fromList $ map (ValueBinding <$>) $
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
