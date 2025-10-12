{-# Language DeriveDataTypeable, FlexibleContexts, FlexibleInstances, InstanceSigs, LambdaCase, MultiParamTypeClasses,
             NamedFieldPuns, OverloadedStrings, QuantifiedConstraints, RankNTypes, ScopedTypeVariables, StandaloneDeriving,
             TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}

-- | Dimorphic attribute grammar for establishing the static identifier bindings

module Language.Haskell.Binder (
   -- * Main functions
   withBindings, unboundNames,
   -- * Transformations
   Binder, BindingVerifier,
   -- * Node wrappers
   Attributes, Environment, LocalEnvironment, ModuleEnvironment, WithEnvironment,
   -- * Binding types
   Binding(ErroneousBinding, TypeBinding, ValueBinding, TypeAndValueBinding),
   BindingError(ClashingBindings, DuplicateInfixDeclaration, DuplicateRecordField),
   TypeBinding(TypeClass), ValueBinding(InfixDeclaration, RecordConstructor, RecordField),
   -- * Prelude
   preludeName, builtinPreludeBindings,
   -- * Utility functions
   lookupType, lookupValue, onMap, baseName, unqualifiedName) where

import Control.Applicative ((<|>), ZipList(ZipList))
import Control.Exception (assert)
import Data.Coerce (coerce)
import Data.Bifunctor (first)
import Data.Data (Data, Typeable)
import Data.Foldable (fold, toList)
import Data.Functor (($>))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Const (Const(Const))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (Any(Any))
import Data.Semigroup.Union (UnionWith(..))
import qualified Data.Set as Set
import Data.Set (Set)
import Unsafe.Coerce (unsafeCoerce)

import qualified Rank2
import Transformation (Transformation)
import qualified Transformation
import qualified Transformation.AG as AG
import qualified Transformation.AG.Dimorphic as Di
import qualified Transformation.AG.Generics as AG (Auto(Auto), Revelation(reveal), Synthesizer(synthesis), passDown)
import qualified Transformation.Deep as Deep
import qualified Transformation.Full as Full
import qualified Transformation.Rank2

import qualified Language.Haskell.Extensions.Abstract as Abstract
import qualified Language.Haskell.AST as AST hiding (Declaration(..))
import qualified Language.Haskell.Extensions as Extensions
import Language.Haskell.Extensions (Extension)
import qualified Language.Haskell.Extensions.AST as ExtAST
import qualified Language.Haskell.Extensions.AST as AST (Declaration(..))

import Transformation.Deep (Const2)

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
  TypeAndPatternBinding t -> Just t
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
withBindings :: forall l g p w q x. (q ~ WithEnvironment l p, w ~ AG.Auto (Binder l p),
                                     AG.Attribution w g, Traversable p,
                                     AG.Atts (AG.Synthesized w) g ~ (x, LocalEnvironment l),
                                     Rank2.Apply (g (AG.Semantics (AG.Keep w))),
                                     Rank2.Traversable (g (AG.Semantics (AG.Keep w))),
                                     Deep.Functor (AG.Keep w) g,
                                     Deep.Functor (Transformation.Rank2.Map (AG.Kept w) q) g)
             => Map Extension Bool -> ModuleEnvironment l -> Environment l -> p (g p p) -> q (g q q)
withBindings extensions modEnv env node =
   Full.mapDownDefault (Transformation.Rank2.Map trim)
   $ AG.syn
   $ (AG.Keep (AG.Auto $ Binder modEnv) Full.<$> node) Rank2.$ AG.Inherited (extensions, env)
   where trim :: AG.Kept w a -> WithEnvironment l p a
         trim AG.Kept{AG.inherited= (exts, env), AG.synthesized, AG.original}
            = Compose (Di.Atts{Di.inh= env, Di.syn= snd synthesized}, original)

-- | Apply the function to the map inside 'UnionWith'
onMap :: (Map.Map j a -> Map.Map k b) -> UnionWith (Map j) a -> UnionWith (Map k) b
onMap f (UnionWith x) = UnionWith (f x)

-- | Apply the function to two maps inside 'UnionWith'
onMaps :: (Map.Map i a -> Map.Map j b -> Map.Map k c)
       -> UnionWith (Map i) a -> UnionWith (Map j) b -> UnionWith (Map k) c
onMaps f (UnionWith x) (UnionWith y) = UnionWith (f x y)

-- | The transformation type used by 'withBindings'
newtype Binder l (f :: Type -> Type) = Binder (ModuleEnvironment l)

instance Foldable f => AG.Revelation (AG.Auto (Binder l f)) where
  reveal _ = foldr1 const

instance Transformation (AG.Auto (Binder l f)) where
   type Domain (AG.Auto (Binder l f)) = f
   type Codomain (AG.Auto (Binder l f)) = AG.Semantics (AG.Auto (Binder l f))

type instance AG.Atts (AG.Inherited (Binder l f)) g = (Map Extension Bool, Environment l)
type instance AG.Atts (AG.Synthesized (Binder l f)) g = (OtherSynAtts l g, LocalEnvironment l)

type family OtherSynAtts l g where
  OtherSynAtts l (ExtAST.Import l l) = (Any, Environment l)
  OtherSynAtts l _ = ()

instance {-# OVERLAPPABLE #-} (OtherSynAtts l g ~ (),
                               forall sem. Rank2.Foldable (g sem),
                               Foldable f) =>
         AG.Synthesizer (AG.Auto (Binder l f)) g where
   synthesis (AG.Auto (Binder t)) node _ chSyn = ((), Rank2.foldMap (snd . AG.syn) chSyn)

-- | The transformation type folds the tree wrapped 'WithEnvironment' to 'Unbound'
data BindingVerifier l (f :: Type -> Type) = BindingVerifier

instance Transformation (BindingVerifier l f) where
   type Domain (BindingVerifier l f) = WithEnvironment l f
   type Codomain (BindingVerifier l f) = Const (Unbound l)

instance (Traversable f, Rank2.Functor (g f), Rank2.Apply (g (AG.Semantics (AG.Keep (AG.Auto (Binder l f))))),
          Rank2.Traversable (g (AG.Semantics (AG.Keep (AG.Auto (Binder l f))))),
          Deep.Functor (AG.Keep (AG.Auto (Binder l f))) g, AG.Attribution (AG.Keep (AG.Auto (Binder l f))) g) =>
         Full.Functor (AG.Keep (AG.Auto (Binder l f))) g where
   t <$> x = AG.fullMapDefault (foldr1 const) t x

instance (Rank2.Functor (g p), Rank2.Functor (g (WithEnvironment l f)), Functor f,
          Deep.Functor (Transformation.Rank2.Map p (WithEnvironment l f)) g) =>
         Full.Functor (Transformation.Rank2.Map p (WithEnvironment l f)) g where
  (<$>) = Full.mapDownDefault

instance {-# OVERLAPS #-}
         (Abstract.Haskell l, Abstract.Declaration l ~ ExtAST.Declaration l,
          Abstract.TypeLHS l ~ ExtAST.TypeLHS l, Abstract.EquationLHS l ~ AST.EquationLHS l,
          Abstract.Pattern l ~ ExtAST.Pattern l, Abstract.FieldPattern l ~ ExtAST.FieldPattern l,
          Abstract.PatternLHS l ~ ExtAST.PatternLHS l,
          Abstract.DataConstructor l ~ ExtAST.DataConstructor l,
          Abstract.GADTConstructor l ~ ExtAST.GADTConstructor l,
          Abstract.QualifiedName l ~ AST.QualifiedName l, Abstract.Name l ~ AST.Name l,
          Foldable f) =>
         AG.Attribution (AG.Auto (Binder l f)) (AST.Declaration l l)
         where
   attribution :: forall f' sem. (Rank2.Functor (AST.Declaration l l f), Rank2.Traversable (AST.Declaration l l sem))
               => AG.Auto (Binder l f) -> f (AST.Declaration l l f' f')
               -> (AG.Inherited   (AG.Auto (Binder l f)) (AST.Declaration l l sem sem), AST.Declaration l l sem (AG.Synthesized (AG.Auto (Binder l f))))
               -> (AG.Synthesized (AG.Auto (Binder l f)) (AST.Declaration l l sem sem), AST.Declaration l l sem (AG.Inherited (AG.Auto (Binder l f))))
   attribution _ node (inh, chSyn) = (AG.Synthesized $ foldMap (`export` chSyn) node,
                                      unsafeCoerce $ const bequest Rank2.<$> foldr1 const node)
      where bequeath :: forall d.
                        AST.Declaration l l d d
                     -> AST.Declaration l l sem (AG.Synthesized (AG.Auto (Binder l f)))
                     -> (Map Extension Bool, Environment l)
            export :: forall d.
                      AST.Declaration l l d d
                   -> AST.Declaration l l sem (AG.Synthesized (AG.Auto (Binder l f)))
                   -> ((), LocalEnvironment l)
            export (AST.FixityDeclaration associativity precedence names) chSyn =
               ((),
                UnionWith (Map.fromList [(name,
                                          ValueBinding
                                          $ InfixDeclaration associativity (fromMaybe 9 precedence) Nothing)
                                        | name <- toList names]))
            export ExtAST.ClassDeclaration{} ~(ExtAST.ClassDeclaration _ lhs decls)
               = (onMap (knowType (TypeClass $ snd methodEnv) <$>) <$> AG.syn lhs) <> methodEnv
               where methodEnv = foldMap AG.syn decls
            export ExtAST.InstanceDeclaration{} ~(ExtAST.InstanceDeclaration _vars _context _lhs decls) =
               onMap (Map.mapMaybe constructorOrField) <$> foldMap AG.syn decls
               where constructorOrField b@(ValueBinding DataConstructor{}) = Just b
                     constructorOrField b@(ValueBinding RecordConstructor{}) = Just b
                     constructorOrField b@(ValueBinding RecordField{}) = Just b
                     constructorOrField b@(ValueBinding RecordFieldAndValue{}) =
                        Just (ValueBinding RecordFieldAndValue)
                     constructorOrField (TypeAndValueBinding _ v) = constructorOrField (ValueBinding v)
                     constructorOrField _ = Nothing
            export AST.EquationDeclaration{} ~(AST.EquationDeclaration lhs _ _) = AG.syn lhs
            export AST.DataDeclaration{} ~(AST.DataDeclaration _context lhs _kind constructors _derivings)
               = (onMap (knowType (DataType $ snd conEnv) <$>) <$> AG.syn lhs) <> conEnv
               where conEnv = foldMap AG.syn constructors
            export AST.NewtypeDeclaration{} ~(AST.NewtypeDeclaration _context lhs _kind constructor _derivings)
               = AG.syn lhs <> AG.syn constructor
            export AST.GADTDeclaration{} ~(AST.GADTDeclaration lhs _kind constructors _derivings)
               = AG.syn lhs <> foldMap AG.syn constructors
            export AST.GADTNewtypeDeclaration{} ~(AST.GADTNewtypeDeclaration lhs _kind constructor _derivings)
               = AG.syn lhs <> AG.syn constructor
            export AST.TypeDataDeclaration{} ~(AST.TypeDataDeclaration _support lhs _kind constructors)
               = AG.syn lhs <> foldMap AG.syn constructors
            export AST.TypeGADTDeclaration{} ~(AST.TypeGADTDeclaration _support1 _support2 lhs _kind constructors)
               = AG.syn lhs <> foldMap AG.syn constructors
            export AST.DataFamilyDeclaration{} ~(AST.DataFamilyDeclaration _support lhs _kind) = AG.syn lhs
            export AST.ClosedTypeFamilyDeclaration{} ~(AST.ClosedTypeFamilyDeclaration _support lhs _kind decls)
               = AG.syn lhs
            export AST.OpenTypeFamilyDeclaration{} ~(AST.OpenTypeFamilyDeclaration _support lhs _kind) = AG.syn lhs
            export AST.InjectiveClosedTypeFamilyDeclaration{}
                   ~(AST.InjectiveClosedTypeFamilyDeclaration _support lhs _var _deps decls) = AG.syn lhs
            export AST.InjectiveOpenTypeFamilyDeclaration{}
                   ~(AST.InjectiveOpenTypeFamilyDeclaration _support lhs _var _deps) = AG.syn lhs
            export AST.DataFamilyInstance{}
                   (AST.DataFamilyInstance _support _vars context _lhs _kind constructors _derivings)
               = foldMap AG.syn constructors
            export AST.NewtypeFamilyInstance{}
                   (AST.NewtypeFamilyInstance _support _vars _context _lhs _kind constructor _derivings)
               = AG.syn constructor
            export AST.GADTDataFamilyInstance{}
                   (AST.GADTDataFamilyInstance _support _vars _lhs _kind constructors _derivings)
               = foldMap AG.syn constructors
            export AST.GADTNewtypeFamilyInstance{}
                   (AST.GADTNewtypeFamilyInstance _support _vars _lhs _kind constructor _derivings)
               = AG.syn constructor
            export AST.TypeSynonymDeclaration{} ~(AST.TypeSynonymDeclaration lhs _type) = AG.syn lhs
            export (AST.TypeSignature names _context _type) _chSyn
               = ((), UnionWith (Map.fromList $ flip (,) (ValueBinding DefinedValue) <$> toList names))
            export (AST.KindSignature name _type) _chSyn
               = ((), UnionWith (Map.singleton name $ TypeBinding UnknownType))
            export ExtAST.ImplicitPatternSynonym{} ~(ExtAST.ImplicitPatternSynonym _ lhs _) = AG.syn lhs
            export ExtAST.ExplicitPatternSynonym{} ~(ExtAST.ExplicitPatternSynonym _ lhs _ _) = AG.syn lhs
            export ExtAST.UnidirectionalPatternSynonym{} ~(ExtAST.UnidirectionalPatternSynonym _ lhs _) = AG.syn lhs
            export _ _ = mempty
            bequeath AST.EquationDeclaration{} (AST.EquationDeclaration _ _ wheres) =
               (unqualified (foldMap (snd . AG.syn) wheres) <>) <$> AG.inh inh
            bequeath _ _ = AG.inh inh
            bequest :: forall a. AG.Inherited (AG.Auto (Binder l f)) a
            bequest = AG.Inherited (foldMap (`bequeath` chSyn) node)
            knowType :: TypeBinding l -> Binding l -> Binding l
            knowType tb (TypeBinding UnknownType) = TypeBinding tb

-- | Resolve ambiguities in a single module. The imports are resolved using the given map of already resolved
-- modules. Note that all class constraints in the function's type signature are satisfied by the Haskell
-- 'AST.Language'.
instance {-# OVERLAPS #-}
         (Abstract.Haskell l, Abstract.QualifiedName l ~ AST.QualifiedName l, Abstract.Name l ~ AST.Name l,
          Abstract.Module l l ~ AST.Module l l, Abstract.ModuleName l ~ AST.ModuleName l,
          Abstract.Export l l ~ ExtAST.Export l l, Abstract.Import l l ~ ExtAST.Import l l,
          Abstract.ImportSpecification l l ~ AST.ImportSpecification l l,
          Abstract.ImportItem l l ~ ExtAST.ImportItem l l,
          Abstract.Declaration l l ~ ExtAST.Declaration l l,
          BindingMembers l,
          Ord (Abstract.QualifiedName l), Foldable f) =>
         AG.Attribution (AG.Auto (Binder l f)) (AST.Module l l)
         where
   attribution :: forall f' sem. (Rank2.Functor (AST.Module l l f), Rank2.Traversable (AST.Module l l sem))
               => AG.Auto (Binder l f) -> f (AST.Module l l f' f')
               -> (AG.Inherited   (AG.Auto (Binder l f)) (AST.Module l l sem sem), AST.Module l l sem (AG.Synthesized (AG.Auto (Binder l f))))
               -> (AG.Synthesized (AG.Auto (Binder l f)) (AST.Module l l sem sem), AST.Module l l sem (AG.Inherited (AG.Auto (Binder l f))))
   attribution (AG.Auto (Binder modEnv)) node (AG.Inherited inh@(exts, inhEnv), childSyn) =
      moduleAttribution $ foldr1 const node
      where moduleAttribution :: forall t d. (t ~ AG.Auto (Binder l f))
                              => AST.Module l l d d -> (AG.Synthesized t (AST.Module l l sem sem),
                                                        AST.Module l l sem (AG.Inherited t))
            moduleAttribution (AST.ExtendedModule modExts body) = assert (Set.null contradictions) atts'
               where (contradictions, extensionMap) = Extensions.partitionContradictory (Set.fromList modExts)
                     exts' = Extensions.withImplications (extensionMap <> exts)
                     AST.ExtendedModule _ bodySyn = childSyn
                     atts' = case Map.lookup Extensions.FieldSelectors exts' of
                        Just False ->
                           (AG.mapSynthesized (onMap (Map.mapMaybe noFieldSelector) <$>) bodySyn, inheritance)
                        _ -> (bodySyn, inheritance)
                     inheritance = AST.ExtendedModule modExts $ AG.Inherited (exts', inhEnv)
                     noFieldSelector (ValueBinding RecordField) = Nothing
                     noFieldSelector (ValueBinding RecordFieldAndValue) = Just (ValueBinding DefinedValue)
                     noFieldSelector x = Just x
            moduleAttribution (AST.AnonymousModule modImports body) =
               (AG.Synthesized ((), filterEnv (== mainName) moduleGlobalScope),
                AST.AnonymousModule (AG.Inherited childInh <$ modImports) (AG.Inherited childInh <$ body))
               where moduleGlobalScope = importedScope importSyn <> unqualified bodySyn
                     mainName = Abstract.qualifiedName Nothing (Abstract.name "main")
                     AST.AnonymousModule importSyn declsSyn = childSyn
                     bodySyn = foldMap (snd . AG.syn) declsSyn
                     childInh = (exts, moduleGlobalScope)
            moduleAttribution (AST.NamedModule moduleName exports modImports body) =
               (AG.Synthesized $ maybe ((), bodySyn) (const $ foldMap AG.syn (Compose exportSyn)) exports,
                AST.NamedModule moduleName ((AG.Inherited childInh <$) <$> exports)
                                (AG.Inherited childInh <$ modImports)
                                (AG.Inherited childInh <$ body))
               where bodySyn :: LocalEnvironment l
                     moduleGlobalScope :: Environment l
                     childInh = (exts, moduleGlobalScope)
                     AST.NamedModule _ exportSyn importSyn declsSyn = childSyn
                     (_, bodySyn) = foldMap AG.syn declsSyn
                     moduleGlobalScope = importedScope importSyn
                                         <> qualifiedWith moduleName bodySyn
                                         <> unqualified bodySyn
            importedScope :: forall d sem. ZipList (AG.Synthesized (AG.Auto (Binder l f)) (ExtAST.Import l l sem sem))
                          -> Environment l
            importedScope importSyns =
               allImports
               <> if not importsPrelude && Map.findWithDefault True Extensions.ImplicitPrelude exts
                  then qualifiedWith preludeName preludeEnv <> unqualified preludeEnv
                  else mempty
              where ((Any importsPrelude, allImports), _) = foldMap AG.syn importSyns
            preludeEnv = fold $ lookupEnv preludeName modEnv

instance {-# OVERLAPS #-}
         (Abstract.Haskell l, BindingMembers l,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.ModuleName l ~ AST.ModuleName l, Abstract.Name l ~ AST.Name l,
          Ord (Abstract.ModuleName l), Ord (Abstract.Name l),
          Show (Abstract.ModuleName l), Show (Abstract.Name l),
          Foldable f) =>
         AG.Attribution (AG.Auto (Binder l f)) (ExtAST.Export l l) where
   attribution _ node (AG.Inherited i@(_, moduleGlobalScope), _) = (AG.Synthesized ((), foldMap itemExports node),
                                                                    coerce $ foldr1 const node) where
      itemExports :: forall d. ExtAST.Export l l d d -> LocalEnvironment l
      itemExports (ExtAST.ReExportModule modName) = reexportModule modName
      itemExports (ExtAST.ExportVar qn) = filterEnv (== qn) moduleGlobalScope
      itemExports (ExtAST.ExportPattern qn) = filterEnv (== qn) moduleGlobalScope
      itemExports (ExtAST.ExportClassOrType qn Nothing) = filterEnv (== qn) moduleGlobalScope
      itemExports (ExtAST.ExportClassOrType parent (Just members)) =
         case lookupEnv parent moduleGlobalScope
         of Just b@(TypeBinding (TypeClass env)) ->
               onMap (Map.insert (baseName parent) b) (filterMembers members env)
            Just (TypeAndValueBinding b@(TypeClass env) _) ->
               onMap (Map.insert (baseName parent) (TypeBinding b)) (filterMembers members env)
            Just (TypeAndPatternBinding b@(TypeClass env)) ->
               onMap (Map.insert (baseName parent) (TypeBinding b)) (filterMembers members env)
            Just b@(TypeBinding (DataType env)) ->
               onMap (Map.insert (baseName parent) b) (filterMembers members env)
            Just (TypeAndValueBinding b@(DataType env) _) ->
               onMap (Map.insert (baseName parent) (TypeBinding b)) (filterMembers members env)
            Just (TypeAndPatternBinding b@(DataType env)) ->
               onMap (Map.insert (baseName parent) (TypeBinding b)) (filterMembers members env)
            Just b -> error (show (parent, b))
            Nothing -> error (show (parent, moduleGlobalScope))
      reexportModule modName = filterEnv (unqualifiedAndQualifiedWith modName) moduleGlobalScope
      unqualifiedAndQualifiedWith modName qn@(AST.QualifiedName modName' localName) =
        modName' == Just modName
        && lookupEnv qn moduleGlobalScope == lookupEnv (unqualifiedName localName) moduleGlobalScope

instance {-# OVERLAPS #-}
         (Abstract.Haskell l,
          Abstract.QualifiedName l ~ AST.QualifiedName l,
          Abstract.ModuleName l ~ AST.ModuleName l, Abstract.Name l ~ AST.Name l,
          AG.Atts (AG.Synthesized (Binder l f)) (Abstract.ImportSpecification l l) ~ ((), LocalEnvironment l),
          Ord (Abstract.ModuleName l), Ord (Abstract.Name l),
          Show (Abstract.ModuleName l), Show (Abstract.Name l),
          Foldable f) =>
         AG.Attribution (AG.Auto (Binder l f)) (ExtAST.Import l l) where
   attribution (AG.Auto (Binder modEnv)) node (AG.Inherited (exts, _), ExtAST.Import _ _ _ _ _ specSyn) =
      (AG.Synthesized ((Any (name == preludeName), scope), mempty),
       ExtAST.Import False qualified Nothing name alias (Just $ AG.Inherited (exts, unqualified available)))
      where ExtAST.Import _ qualified _ name alias _ = foldr1 const node
            available = fold $ lookupEnv name modEnv
            used = maybe available (snd . AG.syn) specSyn
            scope
               | qualified = qualifiedWith (fromMaybe name alias) used
               | otherwise = qualifiedWith (fromMaybe name alias) used <> unqualified used

instance {-# OVERLAPS #-}
         (Abstract.Haskell l,
          Abstract.QualifiedName l ~ AST.QualifiedName l, Abstract.Name l ~ AST.Name l,
          Ord (Abstract.ModuleName l), Ord (Abstract.Name l),
          Show (Abstract.ModuleName l), Show (Abstract.Name l),
          Foldable f) =>
         AG.Attribution (AG.Auto (Binder l f)) (AST.ImportSpecification l l) where
   attribution t node (AG.Inherited i, chSyn) = (AG.Synthesized ((), foldMap imports node),
                                                 AG.passDown i $ foldr1 const node)
      where imports (AST.ImportSpecification True _) = listed
            imports (AST.ImportSpecification False _) = onMaps Map.difference available listed
            listed = Rank2.foldMap (snd . AG.syn) chSyn
            available = onMap (Map.mapKeysMonotonic baseName) (snd i)

instance {-# OVERLAPS #-}
         (Abstract.Haskell l, BindingMembers l,
          Abstract.QualifiedName l ~ AST.QualifiedName l, Abstract.Name l ~ AST.Name l,
          Ord (Abstract.ModuleName l), Ord (Abstract.Name l),
          Show (Abstract.ModuleName l), Show (Abstract.Name l),
          Foldable f) =>
         AG.Attribution (AG.Auto (Binder l f)) (ExtAST.ImportItem l l)
         where
   attribution _ node (AG.Inherited i, chSyn) = (AG.Synthesized ((), foldMap itemImports node),
                                                 AG.passDown i $ foldr1 const node)
      where itemImports :: forall d. ExtAST.ImportItem l l d d -> LocalEnvironment l
            itemImports (ExtAST.ImportClassOrType name Nothing) = nameImport name available
            itemImports (ExtAST.ImportClassOrType parent (Just members)) =
               case lookupEnv parent available
               of Just b@(TypeBinding (TypeClass env)) ->
                     onMap (Map.insert parent b) (filterMembers members env)
                  Just (TypeAndValueBinding b@(TypeClass env) _) ->
                     onMap (Map.insert parent $ TypeBinding b) (filterMembers members env)
                  Just (TypeAndPatternBinding b@(TypeClass env)) ->
                     onMap (Map.insert parent $ TypeBinding b) (filterMembers members env)
                  Just b@(TypeBinding (DataType env)) ->
                     onMap (Map.insert parent b) (filterMembers members env)
                  Just (TypeAndValueBinding b@(DataType env) _) ->
                     onMap (Map.insert parent $ TypeBinding b) (filterMembers members env)
                  Just (TypeAndPatternBinding b@(DataType env)) ->
                     onMap (Map.insert parent $ TypeBinding b) (filterMembers members env)
                  _ -> nameImport parent available
            itemImports (ExtAST.ImportPattern name) = nameImport name available
            itemImports (ExtAST.ImportVar name) = nameImport name available
            available = onMap (Map.mapKeysMonotonic baseName) (snd i)

instance {-# OVERLAPS #-}
         (Abstract.Haskell l,
          Abstract.QualifiedName l ~ AST.QualifiedName l, Abstract.Name l ~ AST.Name l,
          Ord (Abstract.ModuleName l), Ord (Abstract.Name l),
          Show (Abstract.ModuleName l), Show (Abstract.Name l),
          Foldable f) =>
         AG.Attribution (AG.Auto (Binder l f)) (AST.DataConstructor l l)
         where
   attribution _ node (AG.Inherited i, chSyn) = (AG.Synthesized ((), foldMap export node <> childEnv),
                                                 AG.passDown i $ foldr1 const node)
      where export :: forall d. AST.DataConstructor l l d d -> LocalEnvironment l
            export (AST.Constructor name _types) = UnionWith (Map.singleton name $ ValueBinding DataConstructor)
            export (AST.RecordConstructor name _flds) =
               UnionWith (Map.singleton name $ ValueBinding $ RecordConstructor childEnv)
            childEnv = Rank2.foldMap (snd . AG.syn) chSyn

instance {-# OVERLAPS #-}
         (Abstract.Haskell l,
          Abstract.QualifiedName l ~ AST.QualifiedName l, Abstract.Name l ~ AST.Name l,
          Ord (Abstract.ModuleName l), Ord (Abstract.Name l),
          Show (Abstract.ModuleName l), Show (Abstract.Name l),
          Foldable f) =>
         AG.Attribution (AG.Auto (Binder l f)) (ExtAST.DataConstructor l l)
         where
   attribution _ node (AG.Inherited i, chSyn) = (AG.Synthesized ((), foldMap export node <> childEnv),
                                                 AG.passDown i $ foldr1 const node)
      where export :: forall d. ExtAST.DataConstructor l l d d -> LocalEnvironment l
            export (ExtAST.Constructor name _types) = UnionWith (Map.singleton name $ ValueBinding DataConstructor)
            export (ExtAST.RecordConstructor name _fields) =
               UnionWith (Map.singleton name $ ValueBinding $ RecordConstructor childEnv)
            export ExtAST.ExistentialConstructor{} = mempty
            childEnv = Rank2.foldMap (snd . AG.syn) chSyn

instance {-# OVERLAPS #-}
         (Abstract.Haskell l,
          Abstract.QualifiedName l ~ AST.QualifiedName l, Abstract.Name l ~ AST.Name l,
          Ord (Abstract.ModuleName l), Ord (Abstract.Name l),
          Show (Abstract.ModuleName l), Show (Abstract.Name l),
          Foldable f) =>
         AG.Attribution (AG.Auto (Binder l f)) (ExtAST.GADTConstructor l l)
         where
   attribution _ node (AG.Inherited i, chSyn) = (AG.Synthesized ((), foldMap export node <> childEnv),
                                                 AG.passDown i $ foldr1 const node)
      where export :: forall d. ExtAST.GADTConstructor l l d d -> LocalEnvironment l
            export (ExtAST.GADTConstructors names vars _ctx t)
              | Map.null (getUnionWith childEnv)
              = UnionWith $ Map.fromList [(name, ValueBinding DataConstructor) | name <- toList names]
              | otherwise
              = UnionWith $ Map.fromList [(name, ValueBinding $ RecordConstructor childEnv) | name <- toList names]
            childEnv = Rank2.foldMap (snd . AG.syn) chSyn

instance {-# OVERLAPS #-}
         (Abstract.Haskell l,
          Abstract.QualifiedName l ~ AST.QualifiedName l, Abstract.Name l ~ AST.Name l,
          Ord (Abstract.ModuleName l), Ord (Abstract.Name l),
          Show (Abstract.ModuleName l), Show (Abstract.Name l),
          Foldable f) =>
         AG.Attribution (AG.Auto (Binder l f)) (AST.FieldDeclaration l l)
         where
   attribution _ node (AG.Inherited i, chSyn) =
      (AG.Synthesized ((), foldMap export node <> Rank2.foldMap (snd . AG.syn) chSyn),
       AG.passDown i $ foldr1 const node)
      where export :: forall d. AST.FieldDeclaration l l d d -> LocalEnvironment l
            export (AST.ConstructorFields names t) =
               UnionWith $ Map.fromList [(name, ValueBinding RecordField) | name <- toList names]

instance {-# OVERLAPS #-}
         (Abstract.Haskell l,
          Abstract.QualifiedName l ~ AST.QualifiedName l, Abstract.Name l ~ AST.Name l,
          Ord (Abstract.ModuleName l), Ord (Abstract.Name l),
          Show (Abstract.ModuleName l), Show (Abstract.Name l),
          Foldable f) =>
         AG.Attribution (AG.Auto (Binder l f)) (ExtAST.TypeLHS l l)
         where
   attribution _ node (AG.Inherited i, chSyn) =
      (AG.Synthesized ((), foldMap export node <> Rank2.foldMap (snd . AG.syn) chSyn),
       AG.passDown i $ foldr1 const node)
      where export :: forall d. ExtAST.TypeLHS l l d d -> LocalEnvironment l
            export (ExtAST.SimpleTypeLHS name _vars) = UnionWith (Map.singleton name $ TypeBinding UnknownType)
            export (ExtAST.SimpleKindedTypeLHS name _vars) = UnionWith (Map.singleton name $ TypeBinding UnknownType)
            export (ExtAST.InfixTypeLHSApplication _ name _) = UnionWith (Map.singleton name $ TypeBinding UnknownType)
            export _ = mempty

instance {-# OVERLAPS #-}
         (Abstract.Haskell l,
          Abstract.QualifiedName l ~ AST.QualifiedName l, Abstract.Name l ~ AST.Name l,
          Ord (Abstract.ModuleName l), Ord (Abstract.Name l),
          Show (Abstract.ModuleName l), Show (Abstract.Name l),
          Foldable f) =>
         AG.Attribution (AG.Auto (Binder l f)) (AST.EquationLHS l l)
         where
   attribution _ node (AG.Inherited i, chSyn) =
      (AG.Synthesized ((), foldMap export node <> Rank2.foldMap (snd . AG.syn) chSyn),
       AG.passDown i $ foldr1 const node)
      where export :: forall d. ExtAST.EquationLHS l l d d -> LocalEnvironment l
            export (ExtAST.InfixLHS _ name _) = UnionWith (Map.singleton name $ ValueBinding DefinedValue)
            export (ExtAST.VariableLHS name) = UnionWith (Map.singleton name $ ValueBinding DefinedValue)
            export _ = mempty

instance {-# OVERLAPS #-}
         (Abstract.Haskell l,
          Abstract.QualifiedName l ~ AST.QualifiedName l, Abstract.Name l ~ AST.Name l,
          Ord (Abstract.ModuleName l), Ord (Abstract.Name l),
          Show (Abstract.ModuleName l), Show (Abstract.Name l),
          Foldable f) =>
         AG.Attribution (AG.Auto (Binder l f)) (ExtAST.PatternLHS l l)
         where
   attribution _ node (AG.Inherited i, chSyn) =
      (AG.Synthesized ((), foldMap export node <> Rank2.foldMap (snd . AG.syn) chSyn),
       AG.passDown i $ foldr1 const node)
      where export :: forall d. ExtAST.PatternLHS l l d d -> LocalEnvironment l
            export (ExtAST.PrefixPatternLHS name _) = UnionWith (Map.singleton name PatternBinding)
            export (ExtAST.InfixPatternLHS _ name _) = UnionWith (Map.singleton name PatternBinding)
            export (ExtAST.RecordPatternLHS con fields) =
               UnionWith (Map.singleton con $ ValueBinding $ RecordConstructor fieldEnv) <> fieldEnv
               where fieldEnv = UnionWith $ Map.fromList [(name, ValueBinding RecordField) | name <- fields]

instance {-# OVERLAPS #-}
         (Abstract.Haskell l,
          Abstract.QualifiedName l ~ AST.QualifiedName l, Abstract.Name l ~ AST.Name l,
          Ord (Abstract.ModuleName l), Ord (Abstract.Name l),
          Show (Abstract.ModuleName l), Show (Abstract.Name l),
          Foldable f) =>
         AG.Attribution (AG.Auto (Binder l f)) (AST.Pattern l l)
         where
   attribution _ node (AG.Inherited i, chSyn) =
      (AG.Synthesized ((), foldMap export node <> Rank2.foldMap (snd . AG.syn) chSyn),
       AG.passDown i $ foldr1 const node)
      where export :: forall d. AST.Pattern l l d d -> LocalEnvironment l
            export (AST.VariablePattern name) = UnionWith $ Map.singleton name (ValueBinding DefinedValue)
            export _ = mempty

instance {-# OVERLAPS #-}
         (Abstract.Haskell l,
          Abstract.QualifiedName l ~ AST.QualifiedName l, Abstract.Name l ~ AST.Name l,
          Ord (Abstract.ModuleName l), Ord (Abstract.Name l),
          Show (Abstract.ModuleName l), Show (Abstract.Name l),
          Foldable f) =>
         AG.Attribution (AG.Auto (Binder l f)) (ExtAST.Pattern l l)
         where
   attribution _ node (AG.Inherited i, chSyn) =
      (AG.Synthesized ((), foldMap export node <> Rank2.foldMap (snd . AG.syn) chSyn),
       AG.passDown i $ foldr1 const node)
      where export :: forall d. ExtAST.Pattern l l d d -> LocalEnvironment l
            export (ExtAST.VariablePattern name) = UnionWith $ Map.singleton name (ValueBinding DefinedValue)
            export _ = mempty

instance {-# OVERLAPS #-}
         (Abstract.Haskell l,
          Abstract.QualifiedName l ~ AST.QualifiedName l, Abstract.Name l ~ AST.Name l,
          Ord (Abstract.ModuleName l), Ord (Abstract.Name l),
          Show (Abstract.ModuleName l), Show (Abstract.Name l),
          Foldable f) =>
         AG.Attribution (AG.Auto (Binder l f)) (AST.Expression l l)
         where
   attribution _ node (AG.Inherited i, chSyn) =
      (AG.Synthesized mempty, AG.passDown ((unqualified (foldMap bequest node) <>) <$> i) $ foldr1 const node)
      where bequest :: forall d. AST.Expression l l d d -> LocalEnvironment l
            bequest AST.LetExpression{} = Rank2.foldMap (snd . AG.syn) chSyn
            bequest AST.ListComprehension{} = Rank2.foldMap (snd . AG.syn) chSyn
            bequest _ = mempty

instance {-# OVERLAPS #-}
         (Abstract.Haskell l,
          Abstract.QualifiedName l ~ AST.QualifiedName l, Abstract.Name l ~ AST.Name l,
          Ord (Abstract.ModuleName l), Ord (Abstract.Name l),
          Show (Abstract.ModuleName l), Show (Abstract.Name l),
          Foldable f) =>
         AG.Attribution (AG.Auto (Binder l f)) (ExtAST.Expression l l)
         where
   attribution _ node (AG.Inherited i, chSyn) =
      (AG.Synthesized mempty, AG.passDown ((unqualified (foldMap bequest node) <>) <$> i) $ foldr1 const node)
      where bequest :: forall d. ExtAST.Expression l l d d -> LocalEnvironment l
            bequest ExtAST.LetExpression{} = Rank2.foldMap (snd . AG.syn) chSyn
            bequest ExtAST.ListComprehension{} = Rank2.foldMap (snd . AG.syn) chSyn
            bequest ExtAST.ParallelListComprehension{} = Rank2.foldMap (snd . AG.syn) chSyn
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

nameImport name imports = foldMap (UnionWith . Map.singleton name) (lookupEnv name imports)

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

filterEnv :: (AST.QualifiedName l -> Bool) -> Environment l -> LocalEnvironment l
filterEnv f env = onMap (Map.mapKeysMonotonic baseName . Map.filterWithKey (const . f)) env

lookupEnv :: Ord k => k -> UnionWith (Map k) a -> Maybe a
lookupEnv k = Map.lookup k . getUnionWith

-- | The local name part of a qualified name
baseName :: AST.QualifiedName l -> AST.Name l
baseName (AST.QualifiedName _ name) = name

-- | Turns a local name into a 'QualifiedName'
unqualifiedName :: Abstract.Haskell l => Abstract.Name l -> Abstract.QualifiedName l
unqualifiedName = Abstract.qualifiedName Nothing

-- | The name of the @Prelude@ module
preludeName :: Abstract.Haskell l => Abstract.ModuleName l
preludeName = Abstract.moduleName (Abstract.name "Prelude" :| [])
