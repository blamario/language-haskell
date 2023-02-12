{-# Language DeriveDataTypeable, FlexibleContexts, FlexibleInstances, LambdaCase, MultiParamTypeClasses,
             OverloadedStrings, RankNTypes, ScopedTypeVariables, StandaloneDeriving,
             TypeFamilies, TypeOperators, UndecidableInstances #-}

-- | Dimorphic attribute grammar for establishing the static identifier bindings

module Language.Haskell.Binder (
   Binder, BindingVerifier,
   Binding(ErroneousBinding, TypeBinding, ValueBinding, TypeAndValueBinding),
   BindingError(ClashingBindings, DuplicateInfixDeclaration, DuplicateRecordField),
   TypeBinding(TypeClass), ValueBinding(InfixDeclaration),
   Environment, LocalEnvironment, ModuleEnvironment, WithEnvironment,
   lookupType, lookupValue, unboundNames,
   onMap, preludeName, withBindings) where

import Control.Applicative ((<|>))
import Data.Data (Data, Typeable)
import Data.Foldable (fold, toList)
import Data.Functor (($>))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Const (Const(Const))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Semigroup.Union (UnionWith(..))
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set

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

data Binding l = ErroneousBinding (BindingError l)
               | TypeBinding (TypeBinding l)
               | ValueBinding (ValueBinding l)
               | TypeAndValueBinding (TypeBinding l) (ValueBinding l)
               | PatternBinding
               deriving Typeable

data BindingError l = ClashingBindings (Binding l) (Binding l)
                    | DuplicateInfixDeclaration (ValueBinding l) (ValueBinding l)
                    | DuplicateRecordField
                    | NoBindings

data TypeBinding l = TypeClass (LocalEnvironment l) -- methods and associated types
                   | DataType (LocalEnvironment l)  -- constructors

data ValueBinding l = InfixDeclaration (AST.Associativity l) Int (Maybe (ValueBinding l))
                    | DataConstructor
                    | RecordConstructor (LocalEnvironment l) -- fields
                    | RecordField
                    | DefinedValue
                    | RecordFieldAndValue
                    deriving Typeable

data Unbound l = Unbound {types :: [AST.QualifiedName l],
                          values :: [AST.QualifiedName l],
                          constructors :: [AST.QualifiedName l]}

deriving instance (Eq (Abstract.ModuleName l), Eq (Abstract.Name l)) => Eq (Unbound l)
deriving instance (Show (Abstract.ModuleName l), Show (Abstract.Name l)) => Show (Unbound l)

instance Semigroup (Unbound l) where
  a <> b = Unbound{types= types a <> types b,
                   values= values a <> values b,
                   constructors= constructors a <> constructors b}

instance Monoid (Unbound l) where
  mempty = Unbound mempty mempty mempty

-- | Find all the names that have not been bound
unboundNames :: Full.Foldable (BindingVerifier l p) g
             => WithEnvironment l p (g (WithEnvironment l p) (WithEnvironment l p)) -> Unbound l
unboundNames = Full.foldMap BindingVerifier

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
                   Ord (Abstract.ModuleName l), Ord (Abstract.Name l)) => Data (BindingError l)
deriving instance (Ord (Abstract.ModuleName l), Ord (Abstract.Name l)) => Eq (BindingError l)
deriving instance (Show (Abstract.ModuleName l), Show (Abstract.Name l)) => Show (BindingError l)

deriving instance (Data l, Typeable l, Data (Abstract.ModuleName l), Data (Abstract.Name l),
                   Ord (Abstract.ModuleName l), Ord (Abstract.Name l)) => Data (TypeBinding l)
deriving instance (Ord (Abstract.ModuleName l), Ord (Abstract.Name l)) => Eq (TypeBinding l)
deriving instance (Show (Abstract.ModuleName l), Show (Abstract.Name l)) => Show (TypeBinding l)

deriving instance (Data l, Typeable l, Data (Abstract.ModuleName l), Data (Abstract.Name l),
                   Ord (Abstract.ModuleName l), Ord (Abstract.Name l)) => Data (ValueBinding l)
deriving instance (Ord (Abstract.ModuleName l), Ord (Abstract.Name l)) => Eq (ValueBinding l)
deriving instance (Show (Abstract.ModuleName l), Show (Abstract.Name l)) => Show (ValueBinding l)

deriving instance (Ord k, Data k, Data v) => Data (UnionWith (Map k) v)
deriving instance (Ord k, Eq v) => Eq (UnionWith (Map k) v)
deriving instance (Show k, Show v) => Show (UnionWith (Map k) v)

instance (Ord (Abstract.ModuleName l), Ord (Abstract.Name l)) => Semigroup (Binding l) where
   ValueBinding v <> TypeBinding t = TypeAndValueBinding t v
   TypeBinding t <> ValueBinding v = TypeAndValueBinding t v
   b@ErroneousBinding{} <> _ = b
   _ <> b@ErroneousBinding{} = b
   ValueBinding a@InfixDeclaration{} <> ValueBinding b@InfixDeclaration{} =
      ErroneousBinding (DuplicateInfixDeclaration a b)
   ValueBinding (InfixDeclaration assoc fixity Nothing) <> ValueBinding b =
      ValueBinding (InfixDeclaration assoc fixity $ Just b)
   ValueBinding (InfixDeclaration assoc fixity (Just b1)) <> ValueBinding b2 =
      ValueBinding (InfixDeclaration assoc fixity Nothing) <> (ValueBinding b1 <> ValueBinding b2)
   ValueBinding b <> ValueBinding (InfixDeclaration assoc fixity Nothing) =
      ValueBinding (InfixDeclaration assoc fixity $ Just b)
   ValueBinding b1 <> ValueBinding (InfixDeclaration assoc fixity (Just b2)) =
      (ValueBinding b1 <> ValueBinding b2) <> ValueBinding (InfixDeclaration assoc fixity Nothing)
   ValueBinding RecordField <> ValueBinding RecordField = ErroneousBinding DuplicateRecordField
   ValueBinding RecordField <> ValueBinding DefinedValue = ValueBinding RecordFieldAndValue
   ValueBinding DefinedValue <> ValueBinding RecordField = ValueBinding RecordFieldAndValue
   a <> b
      | a == b = a
      | otherwise = ErroneousBinding (ClashingBindings a b)

instance (Ord (Abstract.ModuleName l), Ord (Abstract.Name l)) => Monoid (Binding l) where
   mempty = ErroneousBinding NoBindings

-- | Add the inherited and synthesized bindings to every node in the argument AST.
withBindings :: (Full.Traversable (Di.Keep (Binder l p)) g, q ~ WithEnvironment l p)
             => ModuleEnvironment l -> Environment l -> p (g p p) -> q (g q q)
withBindings modEnv = flip (Full.traverse (Di.Keep $ Binder modEnv))

onMap :: (Map.Map j a -> Map.Map k b) -> UnionWith (Map j) a -> UnionWith (Map k) b
onMap f (UnionWith x) = UnionWith (f x)

data Binder l (f :: Type -> Type) = Binder (ModuleEnvironment l)

data BindingVerifier l (f :: Type -> Type) = BindingVerifier

instance Transformation (Di.Keep (Binder l f)) where
   type Domain (Di.Keep (Binder l f)) = f
   type Codomain (Di.Keep (Binder l f)) = FromEnvironment l f

instance Transformation (BindingVerifier l f) where
   type Domain (BindingVerifier l f) = WithEnvironment l f
   type Codomain (BindingVerifier l f) = Const (Unbound l)

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
   attribution _ node atts = atts{Di.syn= foldMap export node, Di.inh= bequest}
      where bequeath :: AST.Declaration l l (FromEnvironment l f) (FromEnvironment l f) -> Environment l
            export :: AST.Declaration l l (FromEnvironment l f) (FromEnvironment l f) -> LocalEnvironment l
            export (AST.FixityDeclaration associativity precedence names) =
               UnionWith (Map.fromList [(name,
                                         ValueBinding $ InfixDeclaration associativity (fromMaybe 9 precedence) Nothing)
                                        | name <- toList names])
            export (ExtAST.ClassDeclaration _ lhs decls)
               | [name] <- foldMap getTypeName (getCompose lhs mempty)
               = Di.syn atts <> UnionWith (Map.singleton name $ TypeBinding $ TypeClass $ Di.syn atts)
            export (AST.EquationDeclaration lhs _ _)
               | [name] <- foldMap getOperatorName (getCompose lhs mempty)
               = UnionWith (Map.singleton name $ ValueBinding DefinedValue)
            export (AST.DataDeclaration _context lhs _kind _constructors _derivings)
               | [name] <- foldMap getTypeName (getCompose lhs mempty)
               = Di.syn atts <> UnionWith (Map.singleton name $ TypeBinding $ DataType $ Di.syn atts)
            export (AST.NewtypeDeclaration _context lhs _kind _constructor _derivings)
               | [name] <- foldMap getTypeName (getCompose lhs mempty)
               = Di.syn atts <> UnionWith (Map.singleton name $ TypeBinding $ DataType $ Di.syn atts)
            export (AST.GADTDeclaration lhs _kind _constructors _derivings)
               | [name] <- foldMap getTypeName (getCompose lhs mempty)
               = Di.syn atts <> UnionWith (Map.singleton name $ TypeBinding $ DataType $ Di.syn atts)
            export (AST.GADTNewtypeDeclaration lhs _kind _constructor _derivings)
               | [name] <- foldMap getTypeName (getCompose lhs mempty)
               = Di.syn atts <> UnionWith (Map.singleton name $ TypeBinding $ DataType $ Di.syn atts)
            export _ = mempty
            getOperatorName (AST.InfixLHS _ name _) = [name]
            getOperatorName (AST.PrefixLHS lhs _) = foldMap getOperatorName (getCompose lhs mempty)
            getOperatorName (AST.VariableLHS name) = [name]
            getOperatorName _ = []
            getTypeName (ExtAST.SimpleTypeLHS name _) = [name]
            getTypeName (ExtAST.SimpleTypeLHSApplication lhs _) = foldMap getTypeName (getCompose lhs mempty)
            bequeath AST.EquationDeclaration{} = unqualified (Di.syn atts) <> Di.inh atts
            bequeath _ = Di.inh atts
            bequest = foldMap bequeath node

-- | Resolve ambiguities in a single module. The imports are resolved using the given map of already resolved
-- modules. Note that all class constraints in the function's type signature are satisfied by the Haskell
-- 'AST.Language'.
instance {-# OVERLAPS #-}
         (Abstract.Haskell l, Abstract.QualifiedName l ~ AST.QualifiedName l, Abstract.Name l ~ AST.Name l,
          Abstract.Module l l ~ AST.Module l l, Abstract.ModuleName l ~ AST.ModuleName l,
          Abstract.Export l l ~ AST.Export l l, Abstract.Import l l ~ ExtAST.Import l l,
          Abstract.ImportSpecification l l ~ AST.ImportSpecification l l,
          Abstract.ImportItem l l ~ AST.ImportItem l l,
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
                  Di.syn= filterEnv (== mainName) moduleGlobalScope}
               where moduleGlobalScope = importedScope modImports <> unqualified (Di.syn atts)
                     mainName = Abstract.qualifiedName Nothing (Abstract.name "main")
            moduleAttribution (AST.NamedModule moduleName exports modImports body) =
               atts{Di.syn= exportedScope, Di.inh= moduleGlobalScope}
               where exportedScope :: LocalEnvironment l
                     moduleGlobalScope :: Environment l
                     exportedScope = maybe (reexportModule moduleName) (foldMapWrapped itemExports) exports
                     reexportModule modName = filterEnv (fromModule modName) moduleGlobalScope
                     fromModule modName (AST.QualifiedName modName' _) = modName' == Just modName
                     itemExports (AST.ReExportModule modName) = reexportModule modName
                     itemExports (AST.ExportVar qn) = filterEnv (== qn) moduleGlobalScope
                     itemExports (AST.ExportClassOrType qn Nothing) = filterEnv (== qn) moduleGlobalScope
                     itemExports (AST.ExportClassOrType parent (Just members)) =
                        case Map.lookup parent (getUnionWith moduleGlobalScope)
                        of Just b@(TypeBinding (TypeClass env)) ->
                              onMap (Map.insert (baseName parent) b) (filterMembers members env)
                           Just (TypeAndValueBinding b@(TypeClass env) _) ->
                              onMap (Map.insert (baseName parent) (TypeBinding b)) (filterMembers members env)
                           Just b@(TypeBinding (DataType env)) ->
                              onMap (Map.insert (baseName parent) b) (filterMembers members env)
                           Just (TypeAndValueBinding b@(DataType env) _) ->
                              onMap (Map.insert (baseName parent) (TypeBinding b)) (filterMembers members env)
                           _ -> filterEnv (== parent) moduleGlobalScope
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
                        where matchingImports = foldMapWrapped matchingImport modImports
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
                              itemsImports = foldMapWrapped itemImports
                              itemImports (AST.ImportClassOrType name Nothing) = nameImport name allImports
                              itemImports (AST.ImportClassOrType parent (Just members)) =
                                 case Map.lookup parent (getUnionWith allImports)
                                 of Just b@(TypeBinding (TypeClass env)) ->
                                       onMap (Map.insert parent b) (filterMembers members env)
                                    Just (TypeAndValueBinding b@(TypeClass env) _) ->
                                       onMap (Map.insert parent $ TypeBinding b) (filterMembers members env)
                                    Just b@(TypeBinding (DataType env)) ->
                                       onMap (Map.insert parent b) (filterMembers members env)
                                    Just (TypeAndValueBinding b@(DataType env) _) ->
                                       onMap (Map.insert parent $ TypeBinding b) (filterMembers members env)
                                    _ -> nameImport parent allImports
                              itemImports (AST.ImportVar name) = nameImport name allImports
            filterEnv :: (AST.QualifiedName l -> Bool) -> Environment l -> LocalEnvironment l
            filterEnv f env = onMap (Map.mapKeysMonotonic baseName . Map.filterWithKey (const . f)) env
            foldMapWrapped :: forall a g m. (Foldable g, Monoid m)
                           => (a -> m)
                           -> g (Compose ((->) (Environment l)) (WithEnvironment l f) a)
                           -> m
            foldMapWrapped f = foldMap (foldMap f . ($ mempty) . getCompose)


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
   attribution _ node atts = atts{Di.syn= foldMap export node <>  Di.syn atts, Di.inh= Di.inh atts}
      where export :: AST.DataConstructor l l (FromEnvironment l f) (FromEnvironment l f) -> LocalEnvironment l
            export (AST.Constructor name _types) = UnionWith (Map.singleton name $ ValueBinding DataConstructor)
            export (AST.RecordConstructor name _flds) =
               UnionWith (Map.singleton name $ ValueBinding $ RecordConstructor $ Di.syn atts)

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
   attribution _ node atts = atts{Di.syn= foldMap export node <>  Di.syn atts, Di.inh= Di.inh atts}
      where export :: ExtAST.DataConstructor l l (FromEnvironment l f) (FromEnvironment l f) -> LocalEnvironment l
            export (ExtAST.Constructor name _types) = UnionWith (Map.singleton name $ ValueBinding DataConstructor)
            export (ExtAST.RecordConstructor name _fields) =
               UnionWith (Map.singleton name $ ValueBinding $ RecordConstructor $ Di.syn atts)
            export ExtAST.ExistentialConstructor{} = mempty

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
            (ExtAST.GADTConstructor l l)
            (FromEnvironment l f)
            f
         where
   attribution _ node atts = atts{Di.syn= foldMap export node <>  Di.syn atts, Di.inh= Di.inh atts}
      where export :: ExtAST.GADTConstructor l l (FromEnvironment l f) (FromEnvironment l f) -> LocalEnvironment l
            export (ExtAST.GADTConstructors names vars _ctx t)
              | Map.null (getUnionWith $ Di.syn atts)
              = UnionWith $ Map.fromList [(name, ValueBinding DataConstructor) | name <- toList names]
              | otherwise
              = UnionWith $ Map.fromList [(name, ValueBinding $ RecordConstructor $ Di.syn atts) | name <- toList names]

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
   attribution _ node atts = atts{Di.syn= foldMap export node <>  Di.syn atts, Di.inh= Di.inh atts}
      where export :: AST.FieldDeclaration l l (FromEnvironment l f) (FromEnvironment l f) -> LocalEnvironment l
            export (AST.ConstructorFields names t) =
               UnionWith $ Map.fromList [(name, ValueBinding RecordField) | name <- toList names]

instance {-# OVERLAPPABLE #-} BindingVerifier l f `Transformation.At` g where
   _ $ _ = mempty

instance (Foldable f, Abstract.QualifiedName l ~ AST.QualifiedName l,
          Ord (Abstract.ModuleName l), Ord (Abstract.Name l)) =>
   BindingVerifier l f `Transformation.At` AST.Export l l (WithEnvironment l f) (WithEnvironment l f)  where
   _ $ Compose (Di.Atts{Di.inh= env}, node) = foldMap verify node
      where verify (AST.ExportClassOrType q _) = verifyTypeName q env
            verify (AST.ExportVar q) = verifyValueName q env
            verify _ = mempty

instance (Foldable f, Abstract.QualifiedName l ~ AST.QualifiedName l,
          Ord (Abstract.ModuleName l), Ord (Abstract.Name l)) =>
   BindingVerifier l f `Transformation.At` AST.Declaration l l (WithEnvironment l f) (WithEnvironment l f)  where
   _ $ Compose (Di.Atts{Di.inh= env}, node) = foldMap verify node
      where verify (AST.TypeRoleDeclaration q _) = verifyTypeName q env
            verify _ = mempty

instance (Foldable f, Abstract.QualifiedName l ~ AST.QualifiedName l,
          Ord (Abstract.ModuleName l), Ord (Abstract.Name l)) =>
   BindingVerifier l f `Transformation.At` AST.ClassInstanceLHS l l (WithEnvironment l f) (WithEnvironment l f)  where
   _ $ Compose (Di.Atts{Di.inh= env}, node) = foldMap verify node
      where verify (AST.TypeClassInstanceLHS q _) = verifyTypeName q env

instance (Foldable f, Abstract.QualifiedName l ~ AST.QualifiedName l,
          Ord (Abstract.ModuleName l), Ord (Abstract.Name l)) =>
   BindingVerifier l f `Transformation.At` ExtAST.ClassInstanceLHS l l (WithEnvironment l f) (WithEnvironment l f)  where
   _ $ Compose (Di.Atts{Di.inh= env}, node) = foldMap verify node
      where verify (ExtAST.TypeClassInstanceLHS q _) = verifyTypeName q env
            verify (ExtAST.ClassReferenceInstanceLHS q) = verifyTypeName q env
            verify (ExtAST.InfixTypeClassInstanceLHS _ q _) = verifyTypeName q env

instance (Foldable f, Abstract.QualifiedName l ~ AST.QualifiedName l,
          Ord (Abstract.ModuleName l), Ord (Abstract.Name l)) =>
   BindingVerifier l f `Transformation.At` AST.Context l l (WithEnvironment l f) (WithEnvironment l f)  where
   _ $ Compose (Di.Atts{Di.inh= env}, node) = foldMap verify node
      where verify (AST.SimpleConstraint q _) = verifyTypeName q env
            verify (AST.ClassConstraint q _) = verifyTypeName q env
            verify _ = mempty

instance (Foldable f, Abstract.QualifiedName l ~ AST.QualifiedName l,
          Ord (Abstract.ModuleName l), Ord (Abstract.Name l)) =>
   BindingVerifier l f `Transformation.At` ExtAST.Context l l (WithEnvironment l f) (WithEnvironment l f)  where
   _ $ Compose (Di.Atts{Di.inh= env}, node) = foldMap verify node
      where verify (ExtAST.SimpleConstraint q _) = verifyTypeName q env
            verify (ExtAST.ClassConstraint q _) = verifyTypeName q env
            verify (ExtAST.InfixConstraint _ q _) = verifyTypeName q env
            verify _ = mempty

instance (Foldable f, Abstract.QualifiedName l ~ AST.QualifiedName l,
          Ord (Abstract.ModuleName l), Ord (Abstract.Name l)) =>
   BindingVerifier l f `Transformation.At` ExtAST.Type l l (WithEnvironment l f) (WithEnvironment l f)  where
   _ $ Compose (Di.Atts{Di.inh= env}, node) = foldMap verify node
      where verify (ExtAST.InfixTypeApplication _ q _) = verifyTypeName q env
            verify (ExtAST.InfixKindApplication _ q _) = verifyTypeName q env
            verify (ExtAST.PromotedInfixTypeApplication _ q _) = verifyTypeName q env
            verify _ = mempty

instance (Foldable f, Abstract.QualifiedName l ~ AST.QualifiedName l,
          Ord (Abstract.ModuleName l), Ord (Abstract.Name l)) =>
   BindingVerifier l f `Transformation.At` AST.DerivingClause l l (WithEnvironment l f) (WithEnvironment l f)  where
   _ $ Compose (Di.Atts{Di.inh= env}, node) = foldMap verify node
      where verify (AST.SimpleDerive q) = verifyTypeName q env

instance (Foldable f, Abstract.QualifiedName l ~ AST.QualifiedName l,
          Ord (Abstract.ModuleName l), Ord (Abstract.Name l)) =>
   BindingVerifier l f `Transformation.At` AST.FieldBinding l l (WithEnvironment l f) (WithEnvironment l f)  where
   _ $ Compose (Di.Atts{Di.inh= env}, node) = foldMap verify node
      where verify (AST.FieldBinding q _) = verifyValueName q env

instance (Foldable f, Abstract.QualifiedName l ~ AST.QualifiedName l,
          Ord (Abstract.ModuleName l), Ord (Abstract.Name l)) =>
   BindingVerifier l f `Transformation.At` ExtAST.FieldBinding l l (WithEnvironment l f) (WithEnvironment l f)  where
   _ $ Compose (Di.Atts{Di.inh= env}, node) = foldMap verify node
      where verify (ExtAST.FieldBinding q _) = verifyValueName q env
            verify (ExtAST.PunnedFieldBinding q) = verifyValueName q env

instance (Foldable f, Abstract.QualifiedName l ~ AST.QualifiedName l,
          Ord (Abstract.ModuleName l), Ord (Abstract.Name l)) =>
   BindingVerifier l f `Transformation.At` AST.FieldPattern l l (WithEnvironment l f) (WithEnvironment l f)  where
   _ $ Compose (Di.Atts{Di.inh= env}, node) = foldMap verify node
      where verify (AST.FieldPattern q _) = verifyValueName q env

instance (Foldable f, Abstract.QualifiedName l ~ AST.QualifiedName l,
          Ord (Abstract.ModuleName l), Ord (Abstract.Name l)) =>
   BindingVerifier l f `Transformation.At` ExtAST.FieldPattern l l (WithEnvironment l f) (WithEnvironment l f)  where
   _ $ Compose (Di.Atts{Di.inh= env}, node) = foldMap verify node
      where verify (ExtAST.FieldPattern q _) = verifyValueName q env
            verify (ExtAST.PunnedFieldPattern q) = verifyValueName q env

instance (Foldable f, Abstract.QualifiedName l ~ AST.QualifiedName l,
          Ord (Abstract.ModuleName l), Ord (Abstract.Name l)) =>
   BindingVerifier l f `Transformation.At` AST.Expression l l (WithEnvironment l f) (WithEnvironment l f)  where
   _ $ Compose (Di.Atts{Di.inh= env}, node) = foldMap verify node
      where verify (AST.ReferenceExpression q) = verifyValueName q env
            verify (AST.LeftSectionExpression _ q) = verifyValueName q env
            verify (AST.RightSectionExpression q _) = verifyValueName q env
            verify _ = mempty

instance (Foldable f, Abstract.QualifiedName l ~ AST.QualifiedName l,
          Ord (Abstract.ModuleName l), Ord (Abstract.Name l)) =>
   BindingVerifier l f `Transformation.At` AST.Pattern l l (WithEnvironment l f) (WithEnvironment l f)  where
   _ $ Compose (Di.Atts{Di.inh= env}, node) = foldMap verify node
      where verify (AST.InfixPattern _ q _) = verifyValueName q env
            verify (AST.RecordPattern q _) = verifyConstructorName q env
            verify _ = mempty

instance (Foldable f, Abstract.QualifiedName l ~ AST.QualifiedName l,
          Ord (Abstract.ModuleName l), Ord (Abstract.Name l)) =>
   BindingVerifier l f `Transformation.At` AST.Constructor l l (WithEnvironment l f) (WithEnvironment l f)  where
   _ $ Compose (Di.Atts{Di.inh= env}, node) = foldMap verify node
      where verify (AST.ConstructorReference q) = verifyConstructorName q env
            verify _ = mempty

instance (Foldable f, Deep.Foldable (BindingVerifier l f) g,
          Transformation.At (BindingVerifier l f) (g (WithEnvironment l f) (WithEnvironment l f))) =>
         Full.Foldable (BindingVerifier l f) g where
   foldMap = Full.foldMapDownDefault

verifyConstructorName q env = case lookupType q env $> () <|> lookupValue q env $> () of
   Nothing -> Const Unbound{types= [], constructors= [q], values= []}
   _ -> mempty

verifyTypeName q env = case lookupType q env of
   Nothing -> Const Unbound{types= [q], constructors= [], values= []}
   _ -> mempty

verifyValueName q env = case lookupValue q env of
   Nothing -> Const Unbound{types= [], constructors= [], values= [q]}
   _ -> mempty

class Abstract.Haskell l => BindingMembers l where
   filterMembers :: Abstract.Members l -> LocalEnvironment l -> LocalEnvironment l

instance BindingMembers AST.Language where
  filterMembers AST.AllMembers env = env
  filterMembers (AST.MemberList names) env = onMap (`Map.restrictKeys` Set.fromList names) env

instance BindingMembers ExtAST.Language where
  filterMembers ExtAST.AllMembers env = env
  filterMembers (ExtAST.MemberList names) env = onMap (`Map.restrictKeys` Set.fromList names) env
  filterMembers (ExtAST.ExplicitlyNamespacedMemberList members) env = foldMap memberImport members
     where memberImport (ExtAST.DefaultMember name) = onMap (`Map.restrictKeys` Set.singleton name) env
           memberImport (ExtAST.PatternMember name) = onMap (Map.filterWithKey namedPattern) env
              where namedPattern name' PatternBinding{} = name == name'
                    namedPattern _ _ = False
           memberImport (ExtAST.TypeMember name) = onMap (Map.filterWithKey namedType) env
              where namedType name' TypeBinding{} = name == name'
                    namedType _ _ = False

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
