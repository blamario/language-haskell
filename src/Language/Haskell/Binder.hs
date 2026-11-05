{-# Language DeriveDataTypeable, FlexibleContexts, FlexibleInstances, InstanceSigs, LambdaCase, MultiParamTypeClasses,
             NamedFieldPuns, OverloadedStrings, QuantifiedConstraints, RankNTypes, ScopedTypeVariables, StandaloneDeriving,
             TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}

-- | Attribute grammar for statically binding the identifier uses to their definitions

module Language.Haskell.Binder (
   -- * Main functions
   withBindings, rebind, unbind, unboundNames,
   -- * Transformations
   Binder(Binder), BindingVerifier,
   -- * Node wrappers
   Attributes, Environment, LocalEnvironment, ModuleEnvironment, WithEnvironment,
   -- * Binding types
   InScope(InScope, bindingOf, moduleOf),
   Binding(ErroneousBinding, TypeBinding, ValueBinding, TypeAndValueBinding, PatternBinding),
   BindingError(ClashingBindings, DuplicateInfixDeclaration, DuplicateRecordField),
   TypeBinding(TypeClass, DataType), ValueBinding(InfixDeclaration, RecordConstructor, RecordField),
   SynAtts(SynAtts, localBindings, definedNames, exportedNames, importedNames, importsPrelude, memberFilter),
   Unbound(Unbound, types, values, constructors),
   -- * Utility functions
   builtinPreludeBindings, boundNames, inScopeFrom, lookupModule, lookupType, lookupValue, onMap,
   baseName) where

import Control.Applicative ((<|>), ZipList(ZipList, getZipList))
import Control.Exception (assert)
import Data.Data (Data, Typeable)
import Data.Foldable (fold, toList)
import Data.Foldable1 as Foldable1
import Data.Functor (($>))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Const (Const(Const))
import Data.Kind (Type)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (Any(Any, getAny), Endo(Endo, appEndo))
import Data.Semigroup.Union (UnionWith(..))
import qualified Data.Set as Set
import Data.Set (Set)

import qualified Rank2
import Transformation (Transformation)
import qualified Transformation
import qualified Transformation.AG as AG
import qualified Transformation.AG.Dimorphic as Di
import qualified Transformation.AG.Generics as AG (passDown)
import qualified Transformation.Deep as Deep
import qualified Transformation.Full as Full
import qualified Transformation.Rank2

import qualified Language.Haskell.Extensions.Abstract as Abstract
import qualified Language.Haskell.AST as AST hiding (Declaration(..))
import qualified Language.Haskell.Extensions as Extensions
import Language.Haskell.Extensions (Extension)
import qualified Language.Haskell.Extensions.AST as ExtAST
import qualified Language.Haskell.Extensions.AST as AST (Declaration(..))


-- | Bindings of all names in scope, each with the module it is in scope from
type Environment l = UnionWith (Map (AST.QualifiedName l)) (InScope l)

-- | Bindings of local names
type LocalEnvironment l = UnionWith (Map (AST.Name l)) (Binding l)

-- | Bindings of all names exported by the available modules
type ModuleEnvironment l = Map (AST.ModuleName l) (LocalEnvironment l)

-- | The inherited attributes are an 'Environment', the synthesized attributes a 'LocalEnvironment'
type Attributes l = Di.Atts (Environment l) (LocalEnvironment l)

-- | Tree node wrapper carrying the 'Attributes'
type WithEnvironment l = Compose ((,) (Attributes l))

-- | A binding for any single name
data Binding l = ErroneousBinding (BindingError l)
               | TypeBinding (TypeBinding l)
               | ValueBinding (ValueBinding l)
               | TypeAndValueBinding (TypeBinding l) (ValueBinding l)
               | TypeAndPatternBinding (TypeBinding l)
               | PatternBinding
               deriving (Typeable, Data, Eq, Show)

-- | An erroneous binding
data BindingError l = ClashingBindings (Binding l) (Binding l)
                    | DuplicateInfixDeclaration (ValueBinding l) (ValueBinding l)
                    | DuplicateRecordField
                    | MissingBinding (AST.QualifiedName l)
                    | NonExportableBinding (Binding l)
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

-- | A name in scope: what it binds, and which module it came from.
data InScope l = InScope {
   -- | what the name binds
   bindingOf :: Binding l,
   -- | the module the name is in scope from
   moduleOf :: Maybe (AST.ModuleName l)}
   deriving (Typeable, Data, Eq, Show)

instance Semigroup (InScope l) where
   a <> b = InScope{bindingOf= bindingOf a <> bindingOf b,
                    moduleOf= if moduleOf a == moduleOf b then moduleOf a else Nothing}

instance Monoid (InScope l) where
   mempty = InScope{bindingOf= mempty, moduleOf= Nothing}

-- | The sets of erroneously unbound names
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
unboundNames :: Full.Foldable (Full.Outward (BindingVerifier l p)) g
             => WithEnvironment l p (g (WithEnvironment l p) (WithEnvironment l p)) -> Unbound l
unboundNames = Full.foldMap (Full.Outward BindingVerifier)

-- | Look up a type name in the environment
lookupType :: AST.QualifiedName l -> Environment l -> Maybe (TypeBinding l)
lookupType name (UnionWith env) = bindingOf <$> Map.lookup name env >>= \case
  TypeBinding t -> Just t
  TypeAndValueBinding t _ -> Just t
  TypeAndPatternBinding t -> Just t
  _ -> Nothing

-- | Look up a value name in the environment
lookupValue :: AST.QualifiedName l -> Environment l -> Maybe (ValueBinding l)
lookupValue name (UnionWith env) = bindingOf <$> Map.lookup name env >>= \case
  ValueBinding v -> Just v
  TypeAndValueBinding _ v -> Just v
  _ -> Nothing

-- | Look up the module a name in scope came from; 'Nothing' means the name is either not in scope or bound where it
-- stands.
lookupModule :: AST.QualifiedName l -> Environment l -> Maybe (AST.ModuleName l)
lookupModule name (UnionWith env) = Map.lookup name env >>= moduleOf

-- | Assign the environment bindings to the given module
inScopeFrom :: Maybe (AST.ModuleName l) -> UnionWith (Map k) (Binding l) -> UnionWith (Map k) (InScope l)
inScopeFrom moduleName = onMap ((`InScope` moduleName) <$>)

-- | Drop the module provenance leaving only the bindings
boundNames :: UnionWith (Map k) (InScope l) -> UnionWith (Map k) (Binding l)
boundNames = onMap (bindingOf <$>)

deriving instance (Ord k, Data k, Data v) => Data (UnionWith (Map k) v)
deriving instance (Ord k, Eq v) => Eq (UnionWith (Map k) v)
deriving instance (Show k, Show v) => Show (UnionWith (Map k) v)

instance Semigroup (Binding l) where
   ValueBinding v <> TypeBinding t = TypeAndValueBinding t v
   TypeBinding t <> ValueBinding v = TypeAndValueBinding t v
   TypeBinding t <> PatternBinding = TypeAndPatternBinding t
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
withBindings :: forall l g p w q. (q ~ WithEnvironment l p, w ~ Di.Auto (Binder l p),
                                     AG.At w g, Foldable1 p, Traversable p,
                                     Rank2.Functor (g p),
                                     Rank2.Apply (g (AG.Semantics (AG.Keep w))),
                                     Rank2.Traversable (g (AG.Semantics (AG.Keep w))),
                                     Deep.Functor (AG.Knit (AG.Keep w)) g,
                                     Deep.Functor (Full.Outward (Transformation.Rank2.Map (AG.Kept w) q)) g)
             => Map Extension Bool -> ModuleEnvironment l -> Environment l -> p (g p p) -> q (g q q)
withBindings extensions modEnv env node =
   (Full.Outward (Transformation.Rank2.Map trim) Full.<$>)
   $ AG.syn
   $ ((AG.Knit $ AG.Keep $ Di.Auto $ Binder modEnv) Full.<$> node) Rank2.$ AG.Inherited (extensions, env)
   where trim :: AG.Kept w a -> WithEnvironment l p a
         trim AG.Kept{AG.inherited= (_exts, env'), AG.synthesized, AG.original}
            = Compose (Di.Atts{Di.inh= env', Di.syn= localBindings synthesized}, original)

-- | Recalculate the bindings within the given subtree. The 'ModuleEnvironment' only matters if the function is
-- applied to the module itself or its imports, otherwise it's easiest to supply 'mempty'.
rebind :: forall l g p w q. (q ~ WithEnvironment l p, w ~ Di.Auto (Binder l p),
                               AG.At w g, Foldable1 p, Traversable p,
                               Rank2.Functor (g p),
                               Rank2.Apply (g (AG.Semantics (AG.Keep w))),
                               Rank2.Traversable (g (AG.Semantics (AG.Keep w))),
                               Deep.Functor (AG.Knit (AG.Keep w)) g,
                               Deep.Functor (Full.Outward (Transformation.Rank2.Map q p)) g,
                               Deep.Functor (Full.Outward (Transformation.Rank2.Map (AG.Kept w) q)) g)
       => Map Extension Bool -> ModuleEnvironment l -> q (g q q) -> q (g q q)
rebind extensions modEnv root@(Compose (Di.Atts{Di.inh= env}, _)) = withBindings extensions modEnv env $ unbind root

-- | Remove the bindings within the given subtree.
unbind :: forall l g p w q x. (q ~ WithEnvironment l p, Rank2.Functor (g p), Functor p,
                               Deep.Functor (Full.Outward (Transformation.Rank2.Map q p)) g)
       => q (g q q) -> p (g p p)
unbind = (Full.Outward (Transformation.Rank2.Map $ snd . getCompose) Full.<$>)

-- | Apply the function to the map inside 'UnionWith'
onMap :: (Map.Map j a -> Map.Map k b) -> UnionWith (Map j) a -> UnionWith (Map k) b
onMap f (UnionWith x) = UnionWith (f x)

-- | Apply the function to two maps inside 'UnionWith'
onMaps :: (Map.Map i a -> Map.Map j b -> Map.Map k c)
       -> UnionWith (Map i) a -> UnionWith (Map j) b -> UnionWith (Map k) c
onMaps f (UnionWith x) (UnionWith y) = UnionWith (f x y)

-- | The transformation type used by 'withBindings'
newtype Binder l (f :: Type -> Type) = Binder (ModuleEnvironment l)

-- | Every node inherits the map of active language extensions and the 'Environment' of all names in scope.
type InhAtts l = (Map Extension Bool, Environment l)

instance Di.Attribution (Binder l f) where
   type Origin (Binder l f) = f
   type Inherited (Binder l f) = InhAtts l
   type Synthesized (Binder l f) = SynAtts l

-- | The synthesized attributes of every tree node. Only 'localBindings' is meaningful for every node type; each of
-- the other fields is synthesized by the node types named in its documentation and left 'mempty' by all the rest.
--
-- The attributes are uniform across all node types on purpose. That way the attribution rules can be given in the
-- simpler t'Di.Atts' form, where every rule sees the 'mconcat' of all its children's attributes instead of each
-- child's attributes separately, and bequeaths the same attributes to all its children. A node type that needs to
-- tell its children apart gets its own field above instead.
data SynAtts l = SynAtts{
   -- | the names bound by the node
   localBindings :: LocalEnvironment l,
   -- | the names defined by a declaration's left-hand side, excluding the variables bound by its patterns
   definedNames :: LocalEnvironment l,
   -- | the names exported by an 'Abstract.Export' node, qualified the way they are exported
   exportedNames :: Environment l,
   -- | the names an 'Abstract.Import' node brings into scope
   importedNames :: Environment l,
   -- | whether an 'Abstract.Import' node imports the @Prelude@
   importsPrelude :: Any,
   -- | how an 'Abstract.Members' node filters the members of its parent import or export item
   memberFilter :: Endo (LocalEnvironment l)}

instance Semigroup (SynAtts l) where
   a <> b = SynAtts{localBindings= localBindings a <> localBindings b,
                    definedNames= definedNames a <> definedNames b,
                    exportedNames= exportedNames a <> exportedNames b,
                    importedNames= importedNames a <> importedNames b,
                    importsPrelude= importsPrelude a <> importsPrelude b,
                    memberFilter= memberFilter a <> memberFilter b}

instance Monoid (SynAtts l) where
   mempty = SynAtts{localBindings= mempty,
                    definedNames= mempty,
                    exportedNames= mempty,
                    importedNames= mempty,
                    importsPrelude= mempty,
                    memberFilter= mempty}

-- | The attributes of a node that binds the given names and synthesizes nothing else
bindingsOnly :: LocalEnvironment l -> SynAtts l
bindingsOnly env = mempty{localBindings= env}

-- | The transformation type folds the tree wrapped 'WithEnvironment' to 'Unbound'
data BindingVerifier l (f :: Type -> Type) = BindingVerifier

instance Transformation (BindingVerifier l f) where
   type Domain (BindingVerifier l f) = WithEnvironment l f
   type Codomain (BindingVerifier l f) = Const (Unbound l)

instance (Abstract.Haskell l, Abstract.QualifiedName l ~ AST.QualifiedName l, Abstract.Name l ~ AST.Name l,
          Foldable f) =>
         Di.At (Binder l f) (AST.Declaration l l) where
   attribution _ node Di.Atts{Di.inh, Di.syn} = Di.Atts{Di.syn= foldMap export node, Di.inh= foldMap bequeath node}
      where export :: forall d. AST.Declaration l l d d -> SynAtts l
            export (AST.FixityDeclaration associativity precedence names) =
               bindingsOnly
               (UnionWith (Map.fromList [(name,
                                          ValueBinding
                                          $ InfixDeclaration associativity (fromMaybe 9 precedence) Nothing)
                                        | name <- toList names]))
            export (AST.TypeSignature names _context _type)
               = bindingsOnly (UnionWith (Map.fromList $ flip (,) (ValueBinding DefinedValue) <$> toList names))
            export (AST.KindSignature name _type)
               = bindingsOnly (UnionWith (Map.singleton name $ TypeBinding UnknownType))
            export ExtAST.ClassDeclaration{}
               = bindingsOnly (onMap (knowType (TypeClass members) <$>) (definedNames syn) <> members)
            export AST.DataDeclaration{}
               = bindingsOnly (onMap (knowType (DataType members) <$>) (definedNames syn) <> members)
            export ExtAST.InstanceDeclaration{} = bindingsOnly (onMap (Map.mapMaybe constructorOrField) members)
            export AST.NewtypeDeclaration{} = headWithMembers
            export AST.GADTDeclaration{} = headWithMembers
            export AST.GADTNewtypeDeclaration{} = headWithMembers
            export AST.TypeDataDeclaration{} = headWithMembers
            export AST.TypeGADTDeclaration{} = headWithMembers
            export AST.EquationDeclaration{} = declaredHead
            export AST.TypeSynonymDeclaration{} = declaredHead
            export AST.DataFamilyDeclaration{} = declaredHead
            export AST.OpenTypeFamilyDeclaration{} = declaredHead
            export AST.ClosedTypeFamilyDeclaration{} = declaredHead
            export AST.InjectiveOpenTypeFamilyDeclaration{} = declaredHead
            export AST.InjectiveClosedTypeFamilyDeclaration{} = declaredHead
            export ExtAST.ImplicitPatternSynonym{} = declaredHead
            export ExtAST.ExplicitPatternSynonym{} = declaredHead
            export ExtAST.UnidirectionalPatternSynonym{} = declaredHead
            export AST.DataFamilyInstance{} = bindingsOnly members
            export AST.NewtypeFamilyInstance{} = bindingsOnly members
            export AST.GADTDataFamilyInstance{} = bindingsOnly members
            export AST.GADTNewtypeFamilyInstance{} = bindingsOnly members
            export _ = mempty
            -- the names bound by the declaration's left-hand side, and by its constructors or methods
            declaredHead, headWithMembers :: SynAtts l
            declaredHead = bindingsOnly (definedNames syn)
            headWithMembers = bindingsOnly (definedNames syn <> members)
            members :: LocalEnvironment l
            members = localBindings syn
            bequeath :: forall d. AST.Declaration l l d d -> InhAtts l
            bequeath AST.EquationDeclaration{} = (fromLocalEnv members <>) <$> inh
            bequeath _ = inh
            constructorOrField b@(ValueBinding DataConstructor{}) = Just b
            constructorOrField b@(ValueBinding RecordConstructor{}) = Just b
            constructorOrField b@(ValueBinding RecordField{}) = Just b
            constructorOrField (ValueBinding RecordFieldAndValue{}) = Just (ValueBinding RecordFieldAndValue)
            constructorOrField (TypeAndValueBinding _ v) = constructorOrField (ValueBinding v)
            constructorOrField _ = Nothing
            knowType :: TypeBinding l -> Binding l -> Binding l
            knowType tb (TypeBinding UnknownType) = TypeBinding tb
            knowType _ b = b

-- | Resolve ambiguities in a single module. The imports are resolved using the given map of already resolved
-- modules. Note that all class constraints in the function's type signature are satisfied by the Haskell
-- 'AST.Language'.
instance (Abstract.Haskell l, Abstract.QualifiedName l ~ AST.QualifiedName l, Abstract.Name l ~ AST.Name l,
          Abstract.ModuleName l ~ AST.ModuleName l, Foldable1 f) =>
         Di.At (Binder l f) (AST.Module l l) where
   attribution (Binder modEnv) node Di.Atts{Di.inh= (exts, inhEnv), Di.syn} =
      moduleAttribution (Foldable1.head node)
      where moduleAttribution :: forall d. AST.Module l l d d -> Di.Atts (InhAtts l) (SynAtts l)
            moduleAttribution (AST.ExtendedModule modExts _body) =
               assert (Set.null contradictions)
               Di.Atts{Di.inh= (exts', inhEnv),
                       Di.syn= case Map.lookup Extensions.FieldSelectors exts'
                               of Just False -> bindingsOnly (onMap (Map.mapMaybe noFieldSelector) $ localBindings syn)
                                  _ -> syn}
               where (contradictions, extensionMap) = Extensions.partitionContradictory (Set.fromList modExts)
                     exts' = Extensions.withImplications (extensionMap <> exts)
                     noFieldSelector (ValueBinding RecordField) = Nothing
                     noFieldSelector (ValueBinding RecordFieldAndValue) = Just (ValueBinding DefinedValue)
                     noFieldSelector x = Just x
            moduleAttribution AST.AnonymousModule{} =
               Di.Atts{Di.inh= (exts, moduleGlobalScope),
                       Di.syn= bindingsOnly $ onMap (Map.mapKeysMonotonic baseName) $ boundNames
                               $ filterEnv (== mainName) moduleGlobalScope}
               where moduleGlobalScope = importedScope <> unqualified declared
                     declared = inScopeFrom (Just $ Abstract.moduleName $ Abstract.name "Main" :| []) declaredSyn
                     mainName = Abstract.qualifiedName Nothing (Abstract.name "main")
            moduleAttribution (AST.NamedModule moduleName exports _modImports _body) =
               Di.Atts{Di.inh= (exts, moduleGlobalScope),
                       Di.syn= bindingsOnly $ maybe declaredSyn (const exportedSyn) exports}
               where moduleGlobalScope = importedScope
                                         <> qualifiedWith moduleName declared
                                         <> unqualified declared
                     declared = inScopeFrom (Just moduleName) declaredSyn
                     -- 'Map.mapKeysMonotonic' would not do: 'baseName' is monotonic only within a single
                     -- export item, whose names all share the same qualifier
                     exportedSyn = boundNames $ onMap (Map.mapKeysWith (<>) baseName) (exportedNames syn)
            -- the names declared by the module body; the imports and exports synthesize their own attributes
            declaredSyn :: LocalEnvironment l
            declaredSyn = localBindings syn
            importedScope :: Environment l
            importedScope =
               importedNames syn
               <> if not (getAny $ importsPrelude syn) && Map.findWithDefault True Extensions.ImplicitPrelude exts
                  then qualifiedWith Abstract.preludeName prelude <> unqualified prelude
                  else mempty
            prelude = inScopeFrom (Just Abstract.preludeName) preludeEnv
            preludeEnv = fold $ Map.lookup Abstract.preludeName modEnv

instance (Abstract.Haskell l, Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.ModuleName l ~ AST.ModuleName l, Abstract.Name l ~ AST.Name l, Foldable f) =>
         Di.At (Binder l f) (ExtAST.Export l l) where
   attribution _ node atts@Di.Atts{Di.inh= (_, moduleGlobalScope), Di.syn} =
      atts{Di.syn= mempty{exportedNames= foldMap itemExports node}}
      where itemExports :: forall d. ExtAST.Export l l d d -> Environment l
            itemExports (ExtAST.ReExportModule modName) = reexportModule modName
            itemExports (ExtAST.ExportVar qn) = filterEnv (== qn) moduleGlobalScope
            itemExports (ExtAST.ExportPattern qn) = filterEnv (== qn) moduleGlobalScope
            itemExports (ExtAST.ExportClassOrType qn Nothing) = filterEnv (== qn) moduleGlobalScope
            itemExports (ExtAST.ExportClassOrType parent (Just _members)) =
               (\(b, e)-> onMap (Map.insert parent (InScope b from) . Map.mapKeysMonotonic (qualifyAs parent))
                          $ inScopeFrom from $ appEndo (memberFilter syn) e)
               $ case bindingOf <$> lookupEnv parent moduleGlobalScope
                 of Just b@(TypeBinding (TypeClass env)) -> (b, env)
                    Just (TypeAndValueBinding b@(TypeClass env) _) -> (TypeBinding b, env)
                    Just (TypeAndPatternBinding b@(TypeClass env)) -> (TypeBinding b, env)
                    Just b@(TypeBinding (DataType env)) -> (b, env)
                    Just (TypeAndValueBinding t@(DataType envT) v@(RecordConstructor envV))
                      -> (TypeAndValueBinding t v, envT <> envV)
                    Just (TypeAndValueBinding t@(DataType envT) v) -> (TypeAndValueBinding t v, envT)
                    Just (TypeAndPatternBinding b@(DataType env)) -> (TypeBinding b, env)
                    Just b -> (ErroneousBinding (NonExportableBinding b), mempty)
                    Nothing -> (ErroneousBinding (MissingBinding parent), mempty)
               where from = moduleOf =<< lookupEnv parent moduleGlobalScope
            qualifyAs :: AST.QualifiedName l -> AST.Name l -> AST.QualifiedName l
            qualifyAs (AST.QualifiedName prefix _) name = AST.QualifiedName prefix name
            reexportModule modName = filterEnv (unqualifiedAndQualifiedWith modName) moduleGlobalScope
            unqualifiedAndQualifiedWith modName qn@(AST.QualifiedName modName' localName) =
              modName' == Just modName
              && lookupEnv qn moduleGlobalScope == lookupEnv (Abstract.unqualifiedName localName) moduleGlobalScope

instance (Abstract.Haskell l, Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.ModuleName l ~ AST.ModuleName l, Abstract.Name l ~ AST.Name l, Foldable1 f) =>
         Di.At (Binder l f) (ExtAST.Import l l) where
   attribution (Binder modEnv) node Di.Atts{Di.inh= (exts, _), Di.syn} =
      Di.Atts{Di.syn= mempty{importsPrelude= Any (name == Abstract.preludeName), importedNames= scope},
              Di.inh= (exts, unqualified (inScopeFrom (Just name) available))}
      where ExtAST.Import _ qualified _ name alias spec = Foldable1.head node
            available = fold $ Map.lookup name modEnv
            used = inScopeFrom (Just name) (maybe available (const $ localBindings syn) spec)
            scope
               | qualified = qualifiedWith (fromMaybe name alias) used
               | otherwise = qualifiedWith (fromMaybe name alias) used <> unqualified used

instance (Abstract.QualifiedName l ~ AST.QualifiedName l, Abstract.Name l ~ AST.Name l, Foldable f) =>
         Di.At (Binder l f) (AST.ImportSpecification l l) where
   attribution _ node atts@Di.Atts{Di.inh= (_, importedEnv), Di.syn} = atts{Di.syn= bindingsOnly (foldMap imports node)}
      where imports :: forall d. AST.ImportSpecification l l d d -> LocalEnvironment l
            imports (AST.ImportSpecification True _) = listed
            imports (AST.ImportSpecification False _) = onMaps Map.difference available listed
            listed = localBindings syn
            available = boundNames (onMap (Map.mapKeysMonotonic baseName) importedEnv)

instance (Abstract.Haskell l, Abstract.QualifiedName l ~ AST.QualifiedName l, Abstract.Name l ~ AST.Name l,
          Foldable f) =>
         Di.At (Binder l f) (ExtAST.ImportItem l l) where
   attribution _ node atts@Di.Atts{Di.inh= (_, importedEnv), Di.syn} =
      atts{Di.syn= bindingsOnly (foldMap itemImports node)}
      where itemImports :: forall d. ExtAST.ImportItem l l d d -> LocalEnvironment l
            itemImports (ExtAST.ImportClassOrType name Nothing) = nameImport name available
            itemImports (ExtAST.ImportClassOrType parent (Just _members)) =
               (\(b, e)-> onMap (Map.insert parent b) (appEndo (memberFilter syn) e))
               $ case lookupEnv parent available
                 of Just b@(TypeBinding (TypeClass env)) -> (b, env)
                    Just (TypeAndValueBinding b@(TypeClass env) _) -> (TypeBinding b, env)
                    Just (TypeAndPatternBinding b@(TypeClass env)) -> (TypeBinding b, env)
                    Just b@(TypeBinding (DataType env)) -> (b, env)
                    Just (TypeAndValueBinding t@(DataType envT) v@(RecordConstructor envV))
                      -> (TypeAndValueBinding t v, envT <> envV)
                    Just (TypeAndValueBinding t@(DataType env) v) -> (TypeAndValueBinding t v, env)
                    Just (TypeAndPatternBinding b@(DataType env)) -> (TypeBinding b, env)
                    _ -> (ErroneousBinding (MissingBinding $ Abstract.unqualifiedName parent), mempty)
            itemImports (ExtAST.ImportPattern name) = nameImport name available
            itemImports (ExtAST.ImportVar name) = nameImport name available
            available = boundNames (onMap (Map.mapKeysMonotonic baseName) importedEnv)
            nameImport :: AST.Name l -> LocalEnvironment l -> LocalEnvironment l
            nameImport name imports = foldMap (UnionWith . Map.singleton name) (lookupEnv name imports)

instance (Abstract.Name l ~ AST.Name l, Foldable f) => Di.At (Binder l f) (Deep.Const2 (AST.Members l)) where
  attribution _ node atts = atts{Di.syn= mempty{memberFilter= foldMap (Endo . filterMembers . Deep.getConst2) node}}
     where filterMembers AST.AllMembers = id
           filterMembers (AST.MemberList names) = onMap (`Map.restrictKeys` Set.fromList names)

instance (Abstract.Name l ~ AST.Name l, Foldable f) => Di.At (Binder l f) (Deep.Const2 (ExtAST.Members l)) where
  attribution _ node atts = atts{Di.syn= mempty{memberFilter= foldMap (Endo . filterMembers . Deep.getConst2) node}}
     where filterMembers ExtAST.AllMembers = id
           filterMembers ExtAST.AllMembersPlus{} = id
           filterMembers (ExtAST.MemberList names) = onMap (`Map.restrictKeys` Set.fromList names)
           filterMembers (ExtAST.ExplicitlyNamespacedMemberList _support names) = \env-> foldMap (memberImport env) names
           memberImport env (ExtAST.DefaultMember name) = onMap (`Map.restrictKeys` Set.singleton name) env
           memberImport env (ExtAST.PatternMember name) = onMap (Map.filterWithKey namedPattern) env
              where namedPattern name' PatternBinding{} = name == name'
                    namedPattern _ _ = False
           memberImport env (ExtAST.TypeMember name) = onMap (Map.filterWithKey namedType) env
              where namedType name' TypeBinding{} = name == name'
                    namedType _ _ = False

instance (Abstract.Name l ~ AST.Name l, Foldable f) => Di.At (Binder l f) (AST.DataConstructor l l) where
   attribution _ node atts@Di.Atts{Di.syn} = atts{Di.syn= bindingsOnly (foldMap export node <> fieldEnv)}
      where export :: forall d. AST.DataConstructor l l d d -> LocalEnvironment l
            export (AST.Constructor name _types) = UnionWith (Map.singleton name $ ValueBinding DataConstructor)
            export (AST.RecordConstructor name _flds) =
               UnionWith (Map.singleton name $ ValueBinding $ RecordConstructor fieldEnv)
            fieldEnv = localBindings syn

instance (Abstract.Name l ~ AST.Name l, Foldable f) => Di.At (Binder l f) (ExtAST.DataConstructor l l) where
   attribution _ node atts@Di.Atts{Di.syn} = atts{Di.syn= bindingsOnly (foldMap export node <> fieldEnv)}
      where export :: forall d. ExtAST.DataConstructor l l d d -> LocalEnvironment l
            export (ExtAST.Constructor name _types) = UnionWith (Map.singleton name $ ValueBinding DataConstructor)
            export (ExtAST.RecordConstructor name _fields) =
               UnionWith (Map.singleton name $ ValueBinding $ RecordConstructor fieldEnv)
            export ExtAST.ExistentialConstructor{} = mempty
            fieldEnv = localBindings syn

instance (Abstract.Name l ~ AST.Name l, Foldable f) => Di.At (Binder l f) (ExtAST.GADTConstructor l l) where
   attribution _ node atts@Di.Atts{Di.syn} = atts{Di.syn= bindingsOnly (foldMap export node <> fieldEnv)}
      where export :: forall d. ExtAST.GADTConstructor l l d d -> LocalEnvironment l
            export (ExtAST.GADTConstructors names _vars _ctx _type)
              | Map.null (getUnionWith fieldEnv)
              = UnionWith $ Map.fromList [(name, ValueBinding DataConstructor) | name <- toList names]
              | otherwise
              = UnionWith $ Map.fromList [(name, ValueBinding $ RecordConstructor fieldEnv) | name <- toList names]
            fieldEnv = localBindings syn

instance (Abstract.Name l ~ AST.Name l, Foldable f) => Di.At (Binder l f) (AST.FieldDeclaration l l) where
   attribution _ node atts = atts{Di.syn= bindingsOnly (foldMap export node)}
      where export :: forall d. AST.FieldDeclaration l l d d -> LocalEnvironment l
            export (AST.ConstructorFields names _type) =
               UnionWith $ Map.fromList [(name, ValueBinding RecordField) | name <- toList names]

instance (Abstract.Name l ~ AST.Name l, Foldable f) => Di.At (Binder l f) (ExtAST.TypeLHS l l) where
   attribution _ node atts@Di.Atts{Di.syn} =
      atts{Di.syn= mempty{definedNames= foldMap export node <> definedNames syn}}
      where export :: forall d. ExtAST.TypeLHS l l d d -> LocalEnvironment l
            export (ExtAST.SimpleTypeLHS name _vars) = UnionWith (Map.singleton name $ TypeBinding UnknownType)
            export (ExtAST.SimpleKindedTypeLHS name _vars) = UnionWith (Map.singleton name $ TypeBinding UnknownType)
            export (ExtAST.InfixTypeLHSApplication _ name _) = UnionWith (Map.singleton name $ TypeBinding UnknownType)
            export _ = mempty

instance (Abstract.Name l ~ AST.Name l, Foldable f) => Di.At (Binder l f) (AST.EquationLHS l l) where
   attribution _ node atts@Di.Atts{Di.syn} =
      atts{Di.syn= mempty{definedNames= defined, localBindings= defined <> localBindings syn}}
      where defined :: LocalEnvironment l
            defined = foldMap export node
            export :: forall d. ExtAST.EquationLHS l l d d -> LocalEnvironment l
            export (ExtAST.InfixLHS _ name _) = UnionWith (Map.singleton name $ ValueBinding DefinedValue)
            export (ExtAST.VariableLHS name) = UnionWith (Map.singleton name $ ValueBinding DefinedValue)
            -- the variables bound by the pattern of a pattern binding, or the name defined by a nested prefix LHS
            export ExtAST.PatternLHS{} = localBindings syn
            export ExtAST.PrefixLHS{} = definedNames syn

instance (Abstract.Name l ~ AST.Name l, Foldable f) => Di.At (Binder l f) (ExtAST.PatternLHS l l) where
   attribution _ node atts = atts{Di.syn= mempty{definedNames= foldMap export node}}
      where export :: forall d. ExtAST.PatternLHS l l d d -> LocalEnvironment l
            export (ExtAST.PrefixPatternLHS name _) = UnionWith (Map.singleton name PatternBinding)
            export (ExtAST.InfixPatternLHS _ name _) = UnionWith (Map.singleton name PatternBinding)
            export (ExtAST.RecordPatternLHS con fields) =
               UnionWith (Map.singleton con $ ValueBinding $ RecordConstructor fieldEnv) <> fieldEnv
               where fieldEnv = UnionWith $ Map.fromList [(name, ValueBinding RecordField) | name <- fields]

instance (Abstract.Name l ~ AST.Name l, Foldable f) => Di.At (Binder l f) (AST.Pattern l l) where
   attribution _ node atts@Di.Atts{Di.syn} =
      atts{Di.syn= bindingsOnly (foldMap export node <> localBindings syn)}
      where export :: forall d. AST.Pattern l l d d -> LocalEnvironment l
            export (AST.VariablePattern name) = UnionWith $ Map.singleton name (ValueBinding DefinedValue)
            export _ = mempty

instance (Abstract.Name l ~ AST.Name l, Foldable f) => Di.At (Binder l f) (ExtAST.Pattern l l) where
   attribution _ node atts@Di.Atts{Di.syn} =
      atts{Di.syn= bindingsOnly (foldMap export node <> localBindings syn)}
      where export :: forall d. ExtAST.Pattern l l d d -> LocalEnvironment l
            export (ExtAST.VariablePattern name) = UnionWith $ Map.singleton name (ValueBinding DefinedValue)
            export _ = mempty

-- The remaining node types bequeath different attributes to different children, which the t'Di.Atts' form cannot
-- express, so they are given in the general 'AG.At' form instead.

instance {-# OVERLAPPING #-}
         (Abstract.Haskell l, Abstract.QualifiedName l ~ AST.QualifiedName l, Abstract.Name l ~ AST.Name l,
          Foldable1 f) =>
         AG.At (Di.Auto (Binder l f)) (AST.GuardedExpression l l)
         where
   attribution _ node (AG.Inherited i, ~(AST.GuardedExpression guardsSyn _)) =
      (AG.Synthesized mempty, AST.GuardedExpression guardsInh resultInh)
      where guardsInh = ZipList $ AG.Inherited <$> init allInh
            resultInh = AG.Inherited (List.last allInh)
            allInh = scanl acc i $ sameShape (getZipList guards) (getZipList guardsSyn)
            acc old (AG.Synthesized SynAtts{localBindings= new}) = (fromLocalEnv new <>) <$> old
            AST.GuardedExpression guards _result = Foldable1.head node

instance {-# OVERLAPPING #-}
         (Abstract.Haskell l, Abstract.QualifiedName l ~ AST.QualifiedName l, Abstract.Name l ~ AST.Name l,
          Foldable1 f) =>
         AG.At (Di.Auto (Binder l f)) (AST.Expression l l)
         where
   attribution _ node (AG.Inherited i, chSyn) = (AG.Synthesized mempty, bequeath $ Foldable1.head node)
      where bequeath :: forall d. AST.Expression l l d d
                     -> AST.Expression l l d (AG.Inherited (Di.Auto (Binder l f)))
            bequest :: forall d. AST.Expression l l d d -> LocalEnvironment l
            bequeath (AST.LambdaExpression lhs _rhs)
              | AST.LambdaExpression vars _ <- chSyn =
                  AST.LambdaExpression (AG.Inherited i <$ lhs)
                  $ AG.Inherited $ (fromLocalEnv (foldMap (localBindings . AG.syn) vars) <>) <$> i
            bequeath e = AG.passDown ((fromLocalEnv (foldMap bequest node) <>) <$> i) e
            bequest AST.LambdaExpression{} = Rank2.foldMap (localBindings . AG.syn) chSyn
            bequest AST.LetExpression{} = Rank2.foldMap (localBindings . AG.syn) chSyn
            bequest AST.ListComprehension{} = Rank2.foldMap (localBindings . AG.syn) chSyn
            bequest _ = mempty

instance {-# OVERLAPPING #-}
         (Abstract.Haskell l, Abstract.QualifiedName l ~ AST.QualifiedName l, Abstract.Name l ~ AST.Name l,
          Foldable1 f) =>
         AG.At (Di.Auto (Binder l f)) (ExtAST.Expression l l)
         where
   attribution _ node (AG.Inherited i, chSyn) = (AG.Synthesized mempty, bequeath $ Foldable1.head node)
      where bequeath :: forall d. ExtAST.Expression l l d d
                     -> ExtAST.Expression l l d (AG.Inherited (Di.Auto (Binder l f)))
            bequest :: forall d. ExtAST.Expression l l d d -> LocalEnvironment l
            bequeath (ExtAST.LambdaExpression lhs _rhs)
              | ExtAST.LambdaExpression vars _ <- chSyn =
                  ExtAST.LambdaExpression (AG.Inherited i <$ lhs)
                  $ AG.Inherited $ (fromLocalEnv (foldMap (localBindings . AG.syn) vars) <>) <$> i
            bequeath e = AG.passDown ((fromLocalEnv (foldMap bequest node) <>) <$> i) e
            bequest ExtAST.LetExpression{} = Rank2.foldMap (localBindings . AG.syn) chSyn
            bequest ExtAST.ListComprehension{} = Rank2.foldMap (localBindings . AG.syn) chSyn
            bequest ExtAST.ParallelListComprehension{} = Rank2.foldMap (localBindings . AG.syn) chSyn
            bequest _ = mempty

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

verifyConstructorName :: AST.QualifiedName l -> Environment l -> Const (Unbound l) b
verifyConstructorName q env = case lookupType q env $> () <|> lookupValue q env $> () of
   Nothing -> Const Unbound{types= mempty, constructors= Set.singleton q, values= mempty}
   _ -> mempty

verifyTypeName :: AST.QualifiedName l -> Environment l -> Const (Unbound l) b
verifyTypeName q env = case lookupType q env of
   Nothing -> Const Unbound{types= Set.singleton q, constructors= mempty, values= mempty}
   _ -> mempty

verifyValueName :: AST.QualifiedName l -> Environment l -> Const (Unbound l) b
verifyValueName q env = case lookupValue q env of
   Nothing -> Const Unbound{types= mempty, constructors= mempty, values= Set.singleton q}
   _ -> mempty

sameShape :: [a] -> [b] -> [b]
sameShape [] _ = []
sameShape (_:xs) ~(y:ys) = y : sameShape xs ys

-- | The @Prelude@ bindings that must be defined outside the language
builtinPreludeBindings :: (Abstract.Haskell l, Abstract.Name l ~ AST.Name l,
                           Abstract.Associativity l ~ AST.Associativity l)
                       => LocalEnvironment l
builtinPreludeBindings =
   UnionWith $
   Map.fromList [(Abstract.name ":", ValueBinding $ InfixDeclaration Abstract.rightAssociative 5 $ Just DefinedValue)]

qualifiedWith :: AST.ModuleName l -> UnionWith (Map (AST.Name l)) a -> UnionWith (Map (AST.QualifiedName l)) a
qualifiedWith moduleName = onMap (Map.mapKeysMonotonic $ AST.QualifiedName $ Just moduleName)

unqualified :: Abstract.Haskell l => UnionWith (Map (Abstract.Name l)) a -> UnionWith (Map (Abstract.QualifiedName l)) a
unqualified = onMap (Map.mapKeysMonotonic Abstract.unqualifiedName)

fromLocalEnv :: (Abstract.Haskell l, Abstract.Name l ~ AST.Name l, Abstract.QualifiedName l ~ AST.QualifiedName l)
             => LocalEnvironment l -> Environment l
fromLocalEnv = unqualified . inScopeFrom Nothing

filterEnv :: (AST.QualifiedName l -> Bool) -> Environment l -> Environment l
filterEnv f env = onMap (Map.filterWithKey (const . f)) env

lookupEnv :: Ord k => k -> UnionWith (Map k) a -> Maybe a
lookupEnv k = Map.lookup k . getUnionWith

-- | The local name part of a qualified name
baseName :: AST.QualifiedName l -> AST.Name l
baseName (AST.QualifiedName _ name) = name
