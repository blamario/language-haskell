{-# Language DeriveDataTypeable, FlexibleContexts, FlexibleInstances, LambdaCase, MultiParamTypeClasses,
             NamedFieldPuns, OverloadedStrings, RankNTypes, ScopedTypeVariables, StandaloneDeriving,
             TypeFamilies, TypeOperators, UndecidableInstances #-}

-- | Dimorphic attribute grammar for establishing the static identifier bindings

module Language.Haskell.Binder (
   -- * Main functions
   withBindings, unboundNames,
   -- * Transformations
   Binder, BindingVerifier,
   -- * Node wrappers
   Attributes, Environment, LocalEnvironment, ModuleEnvironment, WithEnvironment, WithEnvironment',
   -- * Binding types
   Binding(ErroneousBinding, TypeBinding, ValueBinding, TypeAndValueBinding),
   BindingError(ClashingBindings, DuplicateInfixDeclaration, DuplicateRecordField),
   TypeBinding(TypeClass), ValueBinding(InfixDeclaration, RecordConstructor, RecordField),
   -- * Prelude
   preludeName, builtinPreludeBindings,
   -- * Utility functions
   lookupType, lookupValue, onMap, baseName, unqualifiedName) where

import Control.Applicative ((<|>))
import Control.Exception (assert)
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
import Data.Set (Set)

import qualified Rank2
import Transformation (Transformation)
import qualified Transformation
import qualified Transformation.AG.Dimorphic as Di
import qualified Transformation.Deep as Deep
import qualified Transformation.Full as Full
import qualified Transformation.Rank2

import qualified Language.Haskell.Abstract as Abstract
import qualified Language.Haskell.AST as AST hiding (Declaration(..))
import qualified Language.Haskell.Extensions as Extensions
import Language.Haskell.Extensions (Extension)
import qualified Language.Haskell.Extensions.AST as ExtAST
import qualified Language.Haskell.Extensions.AST as AST (Declaration(..))

-- | Bindings of all names in scope
type Environment l = UnionWith (Map (AST.QualifiedName l)) (Binding l)

-- | Bindings of local names
type LocalEnvironment l = UnionWith (Map (AST.Name l)) (Binding l)

-- | Bindings of all names exported by the available modules
type ModuleEnvironment l = UnionWith (Map (AST.ModuleName l)) (LocalEnvironment l)

-- | The inherited attributes are an 'Environment', the synthesized attributes a 'LocalEnvironment'
type Attributes l = Di.Atts (Environment l) (LocalEnvironment l)

-- | Tree node wrapper carrying the 'Attributes'
type WithEnvironment l = Compose ((,) (Attributes l))

-- | Tree node wrapper mapping an 'Environment' to 'Attributes'
type FromEnvironment l f = Compose ((->) (Map Extension Bool, Environment l)) (WithEnvironment l f)

type Attributes' l = Di.Atts (Map Extension Bool, Environment l) (LocalEnvironment l)
type WithEnvironment' l = Compose ((,) (Attributes' l))
type FromEnvironment' l f = Compose ((->) (Map Extension Bool, Environment l)) (WithEnvironment' l f)

-- | A binding for any single name
data Binding l = ErroneousBinding (BindingError l)
               | TypeBinding (TypeBinding l)
               | ValueBinding (ValueBinding l)
               | TypeAndValueBinding (TypeBinding l) (ValueBinding l)
               | PatternBinding
               deriving (Typeable, Data, Eq, Show)

-- | An erroneous binding
data BindingError l = ClashingBindings (Binding l) (Binding l)
                    | DuplicateInfixDeclaration (ValueBinding l) (ValueBinding l)
                    | DuplicateRecordField
                    | NoBindings
                    deriving (Typeable, Data, Eq, Show)

-- | A binding for a type name
data TypeBinding l = TypeClass (LocalEnvironment l) -- methods and associated types
                   | DataType (LocalEnvironment l)  -- constructors
                   | UnknownType
                   deriving (Typeable, Data, Eq, Show)

-- | A binding for a value name
data ValueBinding l = InfixDeclaration (AST.Associativity l) Int (Maybe (ValueBinding l))
                    | DataConstructor
                    | RecordConstructor (LocalEnvironment l) -- fields
                    | RecordField
                    | DefinedValue
                    | RecordFieldAndValue
                    deriving (Typeable, Data, Eq, Show)

-- | The list of erroneously unbound names
data Unbound l = Unbound {types :: Set (AST.QualifiedName l),
                          values :: Set (AST.QualifiedName l),
                          constructors :: Set (AST.QualifiedName l)}
                 deriving (Eq, Show)

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

-- | Look up a type name in the environment
lookupType :: AST.QualifiedName l -> Environment l -> Maybe (TypeBinding l)
lookupType name (UnionWith env) = Map.lookup name env >>= \case
  TypeBinding t -> Just t
  TypeAndValueBinding t _ -> Just t
  _ -> Nothing

-- | Look up a value name in the environment
lookupValue :: AST.QualifiedName l -> Environment l -> Maybe (ValueBinding l)
lookupValue name (UnionWith env) = Map.lookup name env >>= \case
  ValueBinding v -> Just v
  TypeAndValueBinding _ v -> Just v
  _ -> Nothing

deriving instance (Ord k, Data k, Data v) => Data (UnionWith (Map k) v)
deriving instance (Ord k, Eq v) => Eq (UnionWith (Map k) v)
deriving instance (Show k, Show v) => Show (UnionWith (Map k) v)

instance Semigroup (Binding l) where
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

instance Monoid (Binding l) where
   mempty = ErroneousBinding NoBindings

-- | Add the inherited and synthesized bindings to every node in the argument AST.
withBindings :: (Full.Traversable (Di.Keep (Binder l p)) g, q ~ WithEnvironment l p,
                 Deep.Functor (Transformation.Rank2.Map (WithEnvironment' l p) (WithEnvironment l p)) g,
                 Functor p)
             => Map Extension Bool -> ModuleEnvironment l -> Environment l -> p (g p p) -> q (g q q)
withBindings extensions modEnv env node =
   Full.mapUpDefault (Transformation.Rank2.Map trim) $ Full.traverse (Di.Keep $ Binder modEnv) node (extensions, env)
   where trim :: WithEnvironment' l f a -> WithEnvironment l f a
         trim (Compose (~Di.Atts{Di.inh= (exts, env), Di.syn}, x)) = Compose (Di.Atts{Di.inh= env, Di.syn}, x)

-- | Apply the function to the map inside 'UnionWith'
onMap :: (Map.Map j a -> Map.Map k b) -> UnionWith (Map j) a -> UnionWith (Map k) b
onMap f (UnionWith x) = UnionWith (f x)

-- | The transformation type used by 'withBindings'
data Binder l (f :: Type -> Type) = Binder {
   modules :: ModuleEnvironment l}

-- | The transformation type folds the tree wrapped 'WithEnvironment' to 'Unbound'
data BindingVerifier l (f :: Type -> Type) = BindingVerifier

instance Transformation (Di.Keep (Binder l f)) where
   type Domain (Di.Keep (Binder l f)) = f
   type Codomain (Di.Keep (Binder l f)) = FromEnvironment' l f

instance Transformation (BindingVerifier l f) where
   type Domain (BindingVerifier l f) = WithEnvironment l f
   type Codomain (BindingVerifier l f) = Const (Unbound l)

instance {-# OVERLAPS #-}
         (Abstract.Haskell l, Abstract.TypeLHS l ~ ExtAST.TypeLHS l, Abstract.EquationLHS l ~ AST.EquationLHS l,
          Abstract.Pattern l ~ ExtAST.Pattern l, Abstract.FieldPattern l ~ ExtAST.FieldPattern l,
          Abstract.QualifiedName l ~ AST.QualifiedName l, Abstract.Name l ~ AST.Name l,
          Foldable f) =>
         Di.Attribution
            (Di.Keep (Binder l f))
            (Map Extension Bool, Environment l)
            (LocalEnvironment l)
            (AST.Declaration l l)
            (FromEnvironment' l f)
            f
         where
   attribution _ node atts = atts{Di.syn= foldMap export node, Di.inh= bequest}
      where bequeath :: AST.Declaration l l (FromEnvironment' l f) (FromEnvironment' l f)
                     -> (Map Extension Bool, Environment l)
            export :: AST.Declaration l l (FromEnvironment' l f) (FromEnvironment' l f) -> LocalEnvironment l
            export (AST.FixityDeclaration associativity precedence names) =
               UnionWith (Map.fromList [(name,
                                         ValueBinding $ InfixDeclaration associativity (fromMaybe 9 precedence) Nothing)
                                        | name <- toList names])
            export (ExtAST.ClassDeclaration _ lhs decls)
               | [name] <- foldMap getTypeName (getCompose lhs mempty)
               = Di.syn atts <> UnionWith (Map.singleton name $ TypeBinding $ TypeClass $ Di.syn atts)
            export (ExtAST.InstanceDeclaration _vars _context lhs decls) =
               onMap (Map.mapMaybe constructorOrField) (Di.syn atts)
               where constructorOrField b@(ValueBinding DataConstructor{}) = Just b
                     constructorOrField b@(ValueBinding RecordConstructor{}) = Just b
                     constructorOrField b@(ValueBinding RecordField{}) = Just b
                     constructorOrField b@(ValueBinding RecordFieldAndValue{}) =
                        Just (ValueBinding RecordFieldAndValue)
                     constructorOrField (TypeAndValueBinding _ v) = constructorOrField (ValueBinding v)
                     constructorOrField _ = Nothing
            export (AST.EquationDeclaration lhs _ _) =
              UnionWith
              $ Map.fromList
              $ ((,) `flip` ValueBinding DefinedValue) <$> foldMap getBindingNames (getCompose lhs mempty)
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
            export (AST.TypeDataDeclaration _support lhs _kind _constructors)
               | [name] <- foldMap getTypeName (getCompose lhs mempty)
               = Di.syn atts <> UnionWith (Map.singleton name $ TypeBinding $ DataType $ Di.syn atts)
            export (AST.TypeGADTDeclaration _support1 _support2 lhs _kind _constructors)
               | [name] <- foldMap getTypeName (getCompose lhs mempty)
               = Di.syn atts <> UnionWith (Map.singleton name $ TypeBinding $ DataType $ Di.syn atts)
            export (AST.DataFamilyDeclaration _support lhs _kind)
               | [name] <- foldMap getTypeName (getCompose lhs mempty)
               = Di.syn atts <> UnionWith (Map.singleton name $ TypeBinding $ DataType $ Di.syn atts)
            export (AST.ClosedTypeFamilyDeclaration _support lhs _kind _decls)
               | [name] <- foldMap getTypeName (getCompose lhs mempty)
               = Di.syn atts <> UnionWith (Map.singleton name $ TypeBinding $ DataType $ Di.syn atts)
            export (AST.OpenTypeFamilyDeclaration _support lhs _kind)
               | [name] <- foldMap getTypeName (getCompose lhs mempty)
               = Di.syn atts <> UnionWith (Map.singleton name $ TypeBinding $ DataType $ Di.syn atts)
            export (AST.InjectiveClosedTypeFamilyDeclaration _support lhs _var _deps _decls)
               | [name] <- foldMap getTypeName (getCompose lhs mempty)
               = Di.syn atts <> UnionWith (Map.singleton name $ TypeBinding $ DataType $ Di.syn atts)
            export (AST.InjectiveOpenTypeFamilyDeclaration _support lhs _var _deps)
               | [name] <- foldMap getTypeName (getCompose lhs mempty)
               = Di.syn atts <> UnionWith (Map.singleton name $ TypeBinding $ DataType $ Di.syn atts)
            export (AST.DataFamilyInstance _support _vars context lhs _kind _constructors _derivings) = Di.syn atts
            export (AST.NewtypeFamilyInstance _support _vars context lhs _kind _constructors _derivings) = Di.syn atts
            export (AST.GADTDataFamilyInstance _support _vars lhs _kind _constructors _derivings) = Di.syn atts
            export (AST.GADTNewtypeFamilyInstance _support _vars lhs _kind _constructors _derivings) = Di.syn atts
            export (AST.TypeSignature names _context _type)
               = Di.syn atts <> UnionWith (Map.fromList $ flip (,) (ValueBinding DefinedValue) <$> toList names)
            export (AST.KindSignature name _type)
               = Di.syn atts <> UnionWith (Map.singleton name $ TypeBinding UnknownType)
            export ExtAST.ImplicitPatternSynonym{} = Di.syn atts
            export ExtAST.ExplicitPatternSynonym{} = Di.syn atts
            export ExtAST.UnidirectionalPatternSynonym{} = Di.syn atts
            export _ = mempty
            getBindingNames (AST.InfixLHS _ name _) = [name]
            getBindingNames (AST.PrefixLHS lhs _) = foldMap getBindingNames (getCompose lhs mempty)
            getBindingNames (AST.VariableLHS name) = [name]
            getBindingNames (AST.PatternLHS p) = foldMap getPatternVariables (getCompose p mempty)
            getPatternVariables (ExtAST.AsPattern name p) = name : foldMap getPatternVariables (getCompose p mempty)
            getPatternVariables (ExtAST.ConstructorPattern _ _ args) =
               foldMap (foldMap getPatternVariables . flip getCompose mempty) args
            getPatternVariables (ExtAST.InfixPattern left _ right) =
               foldMap getPatternVariables (getCompose left mempty)
               <> foldMap getPatternVariables (getCompose right mempty)
            getPatternVariables (ExtAST.IrrefutablePattern p) = foldMap getPatternVariables (getCompose p mempty)
            getPatternVariables (ExtAST.ListPattern items) =
               foldMap (foldMap getPatternVariables . flip getCompose mempty) items
            getPatternVariables ExtAST.LiteralPattern{} = []
            getPatternVariables (ExtAST.RecordPattern _ fields) =
               foldMap (foldMap getFieldPatternVariables . flip getCompose mempty) fields
            getPatternVariables (ExtAST.WildcardRecordPattern _ _ fields) =
               foldMap (foldMap getFieldPatternVariables . flip getCompose mempty) fields
            getPatternVariables (ExtAST.TypedPattern p _) = foldMap getPatternVariables (getCompose p mempty)
            getPatternVariables (ExtAST.InvisibleTypePattern _ _ty) = []
            getPatternVariables (ExtAST.ExplicitTypePattern _ _ty) = []
            getPatternVariables (ExtAST.BangPattern _ p) = foldMap getPatternVariables (getCompose p mempty)
            getPatternVariables (ExtAST.TuplePattern items) =
               foldMap (foldMap getPatternVariables . flip getCompose mempty) items
            getPatternVariables (ExtAST.UnboxedTuplePattern _ items) =
               foldMap (foldMap getPatternVariables . flip getCompose mempty) items
            getPatternVariables (ExtAST.VariablePattern name) = [name]
            getPatternVariables (ExtAST.ViewPattern _ view p) = foldMap getPatternVariables (getCompose p mempty)
            getPatternVariables (ExtAST.NPlusKPattern _ n _) = [n]
            getPatternVariables ExtAST.WildcardPattern = []
            getFieldPatternVariables (ExtAST.FieldPattern _ p) = foldMap getPatternVariables (getCompose p mempty)
            getFieldPatternVariables ExtAST.PunnedFieldPattern{} = []
            getTypeName (ExtAST.SimpleTypeLHS name _) = [name]
            getTypeName (ExtAST.TypeLHSApplication lhs _) = foldMap getTypeName (getCompose lhs mempty)
            getTypeName (ExtAST.TypeLHSTypeApplication _support lhs _) = foldMap getTypeName (getCompose lhs mempty)
            bequeath AST.EquationDeclaration{} = (unqualified (Di.syn atts) <>) <$> Di.inh atts
            bequeath _ = Di.inh atts
            bequest = foldMap bequeath node

-- | Resolve ambiguities in a single module. The imports are resolved using the given map of already resolved
-- modules. Note that all class constraints in the function's type signature are satisfied by the Haskell
-- 'AST.Language'.
instance {-# OVERLAPS #-}
         (Abstract.Haskell l, Abstract.QualifiedName l ~ AST.QualifiedName l, Abstract.Name l ~ AST.Name l,
          Abstract.Module l l ~ AST.Module l l, Abstract.ModuleName l ~ AST.ModuleName l,
          Abstract.Export l l ~ ExtAST.Export l l, Abstract.Import l l ~ ExtAST.Import l l,
          Abstract.ImportSpecification l l ~ AST.ImportSpecification l l,
          Abstract.ImportItem l l ~ ExtAST.ImportItem l l,
          BindingMembers l,
          Ord (Abstract.QualifiedName l), Foldable f) =>
         Di.Attribution
            (Di.Keep (Binder l f))
            (Map Extension Bool, Environment l)
            (LocalEnvironment l)
            (AST.Module l l)
            (FromEnvironment' l f) f
         where
   attribution (Di.Keep (Binder modEnv)) node ~atts@Di.Atts{Di.inh= (exts, inhEnv)} = foldMap moduleAttribution node
      where moduleAttribution :: AST.Module l l (FromEnvironment' l f) (FromEnvironment' l f) -> Attributes' l
            moduleAttribution (AST.ExtendedModule modExts body) = assert (Set.null contradictions) atts''
               where (contradictions, extensionMap) = Extensions.partitionContradictory (Set.fromList modExts)
                     exts' = Extensions.withImplications (extensionMap <> exts)
                     atts' = Di.attribution (Di.Keep $ Binder modEnv) body atts{Di.inh= (exts', inhEnv)}
                     atts'' = case Map.lookup Extensions.FieldSelectors exts' of
                        Just False -> atts'{Di.syn = onMap (Map.mapMaybe noFieldSelector) (Di.syn atts')}
                        _ -> atts'
                     noFieldSelector (ValueBinding RecordField) = Nothing
                     noFieldSelector (ValueBinding RecordFieldAndValue) = Just (ValueBinding DefinedValue)
                     noFieldSelector x = Just x
            moduleAttribution (AST.AnonymousModule modImports body) =
               Di.Atts{
                  Di.inh= (exts, moduleGlobalScope),
                  Di.syn= filterEnv (== mainName) moduleGlobalScope}
               where moduleGlobalScope = importedScope modImports <> unqualified (Di.syn atts)
                     mainName = Abstract.qualifiedName Nothing (Abstract.name "main")
            moduleAttribution (AST.NamedModule moduleName exports modImports body) =
               atts{Di.syn= exportedScope, Di.inh= (exts, moduleGlobalScope)}
               where exportedScope :: LocalEnvironment l
                     moduleGlobalScope :: Environment l
                     exportedScope = maybe (reexportModule moduleName) (foldMapWrapped itemExports) exports
                     reexportModule modName
                       | modName == moduleName = Di.syn atts
                       | otherwise = onMap (Map.mapKeys baseName) $ importsFrom modImports modName (fold $ Map.lookup modName $ getUnionWith modEnv)
                     fromModule modName (AST.QualifiedName modName' _) = modName' == Just modName
                     itemExports :: ExtAST.Export l l (FromEnvironment' l f) (FromEnvironment' l f)
                                 -> LocalEnvironment l
                     itemExports (ExtAST.ReExportModule modName) = reexportModule modName
                     itemExports (ExtAST.ExportVar qn) = filterEnv (== qn) moduleGlobalScope
                     itemExports (ExtAST.ExportPattern qn) = filterEnv (== qn) moduleGlobalScope
                     itemExports (ExtAST.ExportClassOrType qn Nothing) = filterEnv (== qn) moduleGlobalScope
                     itemExports (ExtAST.ExportClassOrType parent (Just members)) =
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
            importedScope :: [FromEnvironment' l f (ExtAST.Import l l (FromEnvironment' l f) (FromEnvironment' l f))]
                          -> Environment l
            importedScope modImports = fold (Map.mapWithKey (importsFrom modImports) $ getUnionWith modEnv)
            importsFrom :: [FromEnvironment' l f (ExtAST.Import l l (FromEnvironment' l f) (FromEnvironment' l f))]
                        -> Abstract.ModuleName l
                        -> UnionWith (Map (AST.Name l)) (Binding l)
                        -> Environment l
            importsFrom modImports moduleName moduleExports
               | null matchingImports && moduleName == preludeName
               = if withImplicitPrelude then unqualified moduleExports else mempty
               | otherwise = foldMap (importsFromModule moduleExports) matchingImports
               where matchingImports = foldMapWrapped matchingImport modImports
                     matchingImport i@(ExtAST.Import _ _ _ name _ _)
                        | name == moduleName = [i]
                        | otherwise = []
                     withImplicitPrelude = Map.findWithDefault True Extensions.ImplicitPrelude exts
            importsFromModule :: UnionWith (Map (AST.Name l)) (Binding l)
                              -> ExtAST.Import l l (FromEnvironment' l f) (FromEnvironment' l f) -> Environment l
            importsFromModule moduleExports (ExtAST.Import _ qualified _ name alias spec)
               | qualified = qualifiedWith (fromMaybe name alias) (imports spec)
               | otherwise = unqualified (imports spec)
                             <> maybe mempty (`qualifiedWith` imports spec) alias
               where imports (Just spec) = foldMap specImports (getCompose spec mempty)
                     imports Nothing = allImports
                     specImports (ExtAST.ImportSpecification True items) = itemsImports items
                     specImports (ExtAST.ImportSpecification False items) =
                        UnionWith (getUnionWith allImports `Map.difference` getUnionWith (itemsImports items))
                     allImports = moduleExports
                     itemsImports = foldMapWrapped itemImports
                     itemImports (ExtAST.ImportClassOrType name Nothing) = nameImport name allImports
                     itemImports (ExtAST.ImportClassOrType parent (Just members)) =
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
                     itemImports (ExtAST.ImportPattern name) = nameImport name allImports
                     itemImports (ExtAST.ImportVar name) = nameImport name allImports
            filterEnv :: (AST.QualifiedName l -> Bool) -> Environment l -> LocalEnvironment l
            filterEnv f env = onMap (Map.mapKeysMonotonic baseName . Map.filterWithKey (const . f)) env
            foldMapWrapped :: forall a g m. (Foldable g, Monoid m)
                           => (a -> m)
                           -> g (Compose ((->) (Map Extension Bool, Environment l)) (WithEnvironment' l f) a)
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
            (Map Extension Bool, Environment l)
            (LocalEnvironment l)
            (AST.DataConstructor l l)
            (FromEnvironment' l f)
            f
         where
   attribution _ node atts = atts{Di.syn= foldMap export node <>  Di.syn atts, Di.inh= Di.inh atts}
      where export :: AST.DataConstructor l l (FromEnvironment' l f) (FromEnvironment' l f) -> LocalEnvironment l
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
            (Map Extension Bool, Environment l)
            (LocalEnvironment l)
            (ExtAST.DataConstructor l l)
            (FromEnvironment' l f)
            f
         where
   attribution _ node atts = atts{Di.syn= foldMap export node <>  Di.syn atts, Di.inh= Di.inh atts}
      where export :: ExtAST.DataConstructor l l (FromEnvironment' l f) (FromEnvironment' l f) -> LocalEnvironment l
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
            (Map Extension Bool, Environment l)
            (LocalEnvironment l)
            (ExtAST.GADTConstructor l l)
            (FromEnvironment' l f)
            f
         where
   attribution _ node atts = atts{Di.syn= foldMap export node <>  Di.syn atts, Di.inh= Di.inh atts}
      where export :: ExtAST.GADTConstructor l l (FromEnvironment' l f) (FromEnvironment' l f) -> LocalEnvironment l
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
            (Map Extension Bool, Environment l)
            (LocalEnvironment l)
            (AST.FieldDeclaration l l)
            (FromEnvironment' l f)
            f
         where
   attribution _ node atts = atts{Di.syn= foldMap export node <>  Di.syn atts, Di.inh= Di.inh atts}
      where export :: AST.FieldDeclaration l l (FromEnvironment' l f) (FromEnvironment' l f) -> LocalEnvironment l
            export (AST.ConstructorFields names t) =
               UnionWith $ Map.fromList [(name, ValueBinding RecordField) | name <- toList names]

instance {-# OVERLAPS #-}
         (Abstract.Haskell l,
          Abstract.QualifiedName l ~ AST.QualifiedName l, Abstract.Name l ~ AST.Name l,
          Ord (Abstract.ModuleName l), Ord (Abstract.Name l),
          Show (Abstract.ModuleName l), Show (Abstract.Name l),
          Foldable f) =>
         Di.Attribution
            (Di.Keep (Binder l f))
            (Map Extension Bool, Environment l)
            (LocalEnvironment l)
            (ExtAST.PatternLHS l l)
            (FromEnvironment' l f)
            f
         where
   attribution _ node atts = atts{Di.syn= foldMap export node <>  Di.syn atts, Di.inh= Di.inh atts}
      where export :: ExtAST.PatternLHS l l (FromEnvironment' l f) (FromEnvironment' l f) -> LocalEnvironment l
            export (ExtAST.RecordPatternLHS con fields) =
               UnionWith (Map.singleton con $ ValueBinding $ RecordConstructor fieldEnv) <> fieldEnv
               where fieldEnv = UnionWith $ Map.fromList [(name, ValueBinding RecordField) | name <- fields]
            export _ = mempty

instance {-# OVERLAPS #-}
         (Abstract.Haskell l,
          Abstract.QualifiedName l ~ AST.QualifiedName l, Abstract.Name l ~ AST.Name l,
          Ord (Abstract.ModuleName l), Ord (Abstract.Name l),
          Show (Abstract.ModuleName l), Show (Abstract.Name l),
          Foldable f) =>
         Di.Attribution
            (Di.Keep (Binder l f))
            (Map Extension Bool, Environment l)
            (LocalEnvironment l)
            (AST.Pattern l l)
            (FromEnvironment' l f)
            f
         where
   attribution _ node atts = atts{Di.syn= foldMap export node <>  Di.syn atts, Di.inh= Di.inh atts}
      where export :: AST.Pattern l l (FromEnvironment' l f) (FromEnvironment' l f) -> LocalEnvironment l
            export (AST.VariablePattern name) = UnionWith $ Map.singleton name (ValueBinding DefinedValue)
            export _ = mempty

instance {-# OVERLAPS #-}
         (Abstract.Haskell l,
          Abstract.QualifiedName l ~ AST.QualifiedName l, Abstract.Name l ~ AST.Name l,
          Ord (Abstract.ModuleName l), Ord (Abstract.Name l),
          Show (Abstract.ModuleName l), Show (Abstract.Name l),
          Foldable f) =>
         Di.Attribution
            (Di.Keep (Binder l f))
            (Map Extension Bool, Environment l)
            (LocalEnvironment l)
            (ExtAST.Pattern l l)
            (FromEnvironment' l f)
            f
         where
   attribution _ node atts = atts{Di.syn= foldMap export node <>  Di.syn atts, Di.inh= Di.inh atts}
      where export :: ExtAST.Pattern l l (FromEnvironment' l f) (FromEnvironment' l f) -> LocalEnvironment l
            export (ExtAST.VariablePattern name) = UnionWith $ Map.singleton name (ValueBinding DefinedValue)
            export _ = mempty

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
      where verify (AST.TypeRoleDeclaration _ q _) = verifyTypeName q env
            verify (AST.NamedDefaultDeclaration _ q _) = verifyTypeName q env
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
            verify _ = mempty

instance (Foldable f, Abstract.QualifiedName l ~ AST.QualifiedName l,
          Ord (Abstract.ModuleName l), Ord (Abstract.Name l)) =>
   BindingVerifier l f `Transformation.At` AST.Context l l (WithEnvironment l f) (WithEnvironment l f)  where
   _ $ Compose (Di.Atts{Di.inh= env}, node) = foldMap verify node
      where verify (AST.ClassConstraint q _) = verifyTypeName q env
            verify _ = mempty

instance (Foldable f, Abstract.QualifiedName l ~ AST.QualifiedName l,
          Ord (Abstract.ModuleName l), Ord (Abstract.Name l)) =>
   BindingVerifier l f `Transformation.At` ExtAST.Context l l (WithEnvironment l f) (WithEnvironment l f)  where
   _ $ Compose (Di.Atts{Di.inh= env}, node) = foldMap verify node
      where verify (ExtAST.ClassConstraint q _) = verifyTypeName q env
            verify _ = mempty

instance (Foldable f, Abstract.QualifiedName l ~ AST.QualifiedName l,
          Ord (Abstract.ModuleName l), Ord (Abstract.Name l)) =>
   BindingVerifier l f `Transformation.At` ExtAST.Type l l (WithEnvironment l f) (WithEnvironment l f)  where
   _ $ Compose (Di.Atts{Di.inh= env}, node) = foldMap verify node
      where verify (ExtAST.InfixTypeApplication _ q _) = verifyTypeName q env
            verify (ExtAST.PromotedInfixTypeApplication _ _ q _) = verifyTypeName q env
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
            verify (ExtAST.PunnedFieldBinding _ q) = verifyValueName q env

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
            verify (ExtAST.PunnedFieldPattern _ q) = verifyValueName q env

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
   BindingVerifier l f `Transformation.At` ExtAST.Expression l l (WithEnvironment l f) (WithEnvironment l f)  where
   _ $ Compose (Di.Atts{Di.inh= env}, node) = foldMap verify node
      where verify (ExtAST.ReferenceExpression q) = verifyValueName q env
            verify (ExtAST.LeftSectionExpression _ q) = verifyValueName q env
            verify (ExtAST.RightSectionExpression q _) = verifyValueName q env
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
   BindingVerifier l f `Transformation.At` ExtAST.Pattern l l (WithEnvironment l f) (WithEnvironment l f)  where
   _ $ Compose (Di.Atts{Di.inh= env}, node) = foldMap verify node
      where verify (ExtAST.InfixPattern _ q _) = verifyValueName q env
            verify (ExtAST.RecordPattern q _) = verifyConstructorName q env
            verify (ExtAST.WildcardRecordPattern _ q _) = verifyConstructorName q env
            verify _ = mempty

instance (Foldable f, Abstract.QualifiedName l ~ AST.QualifiedName l,
          Ord (Abstract.ModuleName l), Ord (Abstract.Name l)) =>
   BindingVerifier l f `Transformation.At` AST.Constructor l l (WithEnvironment l f) (WithEnvironment l f)  where
   _ $ Compose (Di.Atts{Di.inh= env}, node) = foldMap verify node
      where verify (AST.ConstructorReference q) = verifyConstructorName q env
            verify _ = mempty

instance (Foldable f, Abstract.QualifiedName l ~ AST.QualifiedName l,
          Ord (Abstract.ModuleName l), Ord (Abstract.Name l)) =>
   BindingVerifier l f `Transformation.At` ExtAST.Constructor l l (WithEnvironment l f) (WithEnvironment l f)  where
   _ $ Compose (Di.Atts{Di.inh= env}, node) = foldMap verify node
      where verify (ExtAST.ConstructorReference q) = verifyConstructorName q env
            verify _ = mempty

instance (Foldable f, Rank2.Foldable (g (WithEnvironment l f)), Deep.Foldable (BindingVerifier l f) g,
          Transformation.At (BindingVerifier l f) (g (WithEnvironment l f) (WithEnvironment l f))) =>
         Full.Foldable (BindingVerifier l f) g where
   foldMap = Full.foldMapDownDefault

verifyConstructorName q env = case lookupType q env $> () <|> lookupValue q env $> () of
   Nothing -> Const Unbound{types= mempty, constructors= Set.singleton q, values= mempty}
   _ -> mempty

verifyTypeName q env = case lookupType q env of
   Nothing -> Const Unbound{types= Set.singleton q, constructors= mempty, values= mempty}
   _ -> mempty

verifyValueName q env = case lookupValue q env of
   Nothing -> Const Unbound{types= mempty, constructors= mempty, values= Set.singleton q}
   _ -> mempty

class Abstract.Haskell l => BindingMembers l where
   filterMembers :: Abstract.Members l -> LocalEnvironment l -> LocalEnvironment l

instance BindingMembers AST.Language where
  filterMembers AST.AllMembers env = env
  filterMembers (AST.MemberList names) env = onMap (`Map.restrictKeys` Set.fromList names) env

instance BindingMembers ExtAST.Language where
  filterMembers ExtAST.AllMembers env = env
  filterMembers ExtAST.AllMembersPlus{} env = env
  filterMembers (ExtAST.MemberList names) env = onMap (`Map.restrictKeys` Set.fromList names) env
  filterMembers (ExtAST.ExplicitlyNamespacedMemberList _support members) env = foldMap memberImport members
     where memberImport (ExtAST.DefaultMember name) = onMap (`Map.restrictKeys` Set.singleton name) env
           memberImport (ExtAST.PatternMember name) = onMap (Map.filterWithKey namedPattern) env
              where namedPattern name' PatternBinding{} = name == name'
                    namedPattern _ _ = False
           memberImport (ExtAST.TypeMember name) = onMap (Map.filterWithKey namedType) env
              where namedType name' TypeBinding{} = name == name'
                    namedType _ _ = False

nameImport name imports = foldMap (UnionWith . Map.singleton name) (Map.lookup name $ getUnionWith imports)

builtinPreludeBindings :: (Abstract.Haskell l, Abstract.Name l ~ AST.Name l,
                           Abstract.Associativity l ~ AST.Associativity l)
                       => LocalEnvironment l
builtinPreludeBindings =
   UnionWith $
   Map.fromList [(Abstract.name ":", ValueBinding $ InfixDeclaration Abstract.rightAssociative 5 $ Just DefinedValue)]

qualifiedWith :: AST.ModuleName l -> UnionWith (Map (AST.Name l)) a -> UnionWith (Map (AST.QualifiedName l)) a
qualifiedWith moduleName = onMap (Map.mapKeysMonotonic $ AST.QualifiedName $ Just moduleName)

unqualified :: Abstract.Haskell l => UnionWith (Map (Abstract.Name l)) a -> UnionWith (Map (Abstract.QualifiedName l)) a
unqualified = onMap (Map.mapKeysMonotonic unqualifiedName)

-- | The local name part of a qualified name
baseName :: AST.QualifiedName l -> AST.Name l
baseName (AST.QualifiedName _ name) = name

-- | Turns a local name into a 'QualifiedName'
unqualifiedName :: Abstract.Haskell l => Abstract.Name l -> Abstract.QualifiedName l
unqualifiedName = Abstract.qualifiedName Nothing

-- | The name of the @Prelude@ module
preludeName :: Abstract.Haskell l => Abstract.ModuleName l
preludeName = Abstract.moduleName (Abstract.name "Prelude" :| [])
