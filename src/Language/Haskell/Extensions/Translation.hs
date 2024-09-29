{-# Language ConstraintKinds, DataKinds, DefaultSignatures,
             FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
             ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
{-# Options_GHC -Wall -Werror=incomplete-patterns #-}

module Language.Haskell.Extensions.Translation where

import Data.Coerce (Coercible, coerce)
import qualified Language.Haskell.Extensions.Abstract as Abstract
import qualified Language.Haskell.Extensions.AST as AST
import qualified Language.Haskell.Extensions as Extensions

class NameTranslation t where
   type Origin t :: Abstract.Language
   type Target t :: Abstract.Language
   translateName :: t -> Abstract.Name (Origin t) -> Abstract.Name (Target t)
   translateModuleName :: t -> Abstract.ModuleName (Origin t) -> Abstract.ModuleName (Target t)
   translateQualifiedName :: t -> Abstract.QualifiedName (Origin t) -> Abstract.QualifiedName (Target t)
   default translateName :: (Abstract.Name (Origin t) ~ AST.Name (Origin t),
                             Abstract.Name (Target t) ~ AST.Name (Target t))
                         => t -> Abstract.Name (Origin t) -> Abstract.Name (Target t)
   default translateModuleName :: (Abstract.Name (Target t) ~ AST.Name (Target t),
                                   Abstract.ModuleName (Origin t) ~ AST.ModuleName (Origin t),
                                   Abstract.ModuleName (Target t) ~ AST.ModuleName (Target t))
                               => t -> Abstract.ModuleName (Origin t) -> Abstract.ModuleName (Target t)
   default translateQualifiedName :: (Abstract.Name (Target t) ~ AST.Name (Target t),
                                      Abstract.ModuleName (Target t) ~ AST.ModuleName (Target t),
                                      Abstract.QualifiedName (Origin t) ~ AST.QualifiedName (Origin t),
                                      Abstract.QualifiedName (Target t) ~ AST.QualifiedName (Target t))
                                  => t -> Abstract.QualifiedName (Origin t) -> Abstract.QualifiedName (Target t)
   translateName = const coerce
   translateModuleName = const coerce
   translateQualifiedName = const coerce


class WrapTranslation t where
   type Wrap t :: Abstract.NodeWrap

class (NameTranslation t, WrapTranslation t) => WrappedTranslation t (node :: Abstract.TreeNodeKind) where
   translateWrapped :: t -> Wrap t (node (Origin t) (Origin t) (Wrap t) (Wrap t)) -> Wrap t (node (Target t) (Origin t) (Wrap t) (Wrap t))
   default translateWrapped :: (Functor (Wrap t), Translation t node)
                            => t -> Wrap t (node (Origin t) (Origin t) (Wrap t) (Wrap t))
                            -> Wrap t (node (Target t) (Origin t) (Wrap t) (Wrap t))
   translateWrapped = fmap . translate

class (NameTranslation t, WrapTranslation t) => Translation t (node :: Abstract.TreeNodeKind) where
   translate :: t -> node (Origin t) (Origin t) (Wrap t) (Wrap t) -> node (Target t) (Origin t) (Wrap t) (Wrap t)

class WrapTranslation t => DeeplyTranslatable t (node :: Abstract.TreeNodeKind) where
   translateDeeply :: Functor (Wrap t)
                   => t -> node l (Origin t) (Wrap t) (Wrap t) -> node l (Target t) (Wrap t) (Wrap t)

translateFully :: (FullyTranslatable t node, Functor (Wrap t)) =>
                  t -> Wrap t (node (Origin t) (Origin t) (Wrap t) (Wrap t)) -> Wrap t (node (Target t) (Target t) (Wrap t) (Wrap t))
translateFully t = (translateDeeply t <$>) . translateWrapped t

type FullyTranslatable t node = (WrappedTranslation t node, DeeplyTranslatable t node)

-- DeeplyTranslatable instances

instance (WrapTranslation t, WrappedTranslation t AST.Module,
          FullyTranslatable t AST.Export, FullyTranslatable t AST.Import, FullyTranslatable t AST.Declaration,
          Abstract.Module (Origin t) ~ AST.Module (Origin t), Abstract.Module (Target t) ~ AST.Module (Target t),
          Abstract.Export (Origin t) ~ AST.Export (Origin t), Abstract.Export (Target t) ~ AST.Export (Target t),
          Abstract.Import (Origin t) ~ AST.Import (Origin t), Abstract.Import (Target t) ~ AST.Import (Target t),
          Abstract.Declaration (Origin t) ~ AST.Declaration (Origin t),
          Abstract.Declaration (Target t) ~ AST.Declaration (Target t)) => DeeplyTranslatable t AST.Module where
   translateDeeply t (AST.NamedModule modName exports imports declarations) =
      AST.NamedModule modName
         ((translateFully t <$>) <$> exports)
         (translateFully t <$> imports)
         (translateFully t <$> declarations)
   translateDeeply t (AST.AnonymousModule imports declarations) =
      AST.AnonymousModule (translateFully t <$> imports) (translateFully t <$> declarations)
   translateDeeply t (AST.ExtendedModule extensions m) = AST.ExtendedModule extensions (translateFully t m)

instance (WrapTranslation t, FullyTranslatable t AST.ImportSpecification,
          Abstract.ModuleName (Origin t) ~ AST.ModuleName (Origin t),
          Abstract.QualifiedName (Origin t) ~ AST.QualifiedName (Origin t)) =>
         DeeplyTranslatable t AST.Export where
   translateDeeply _ (AST.ExportClassOrType name members) = AST.ExportClassOrType name members
   translateDeeply _ (AST.ExportVar name) = AST.ExportVar name
   translateDeeply _ (AST.ExportPattern name) = AST.ExportPattern name
   translateDeeply _ (AST.ReExportModule name) = AST.ReExportModule name

instance (WrapTranslation t, FullyTranslatable t AST.ImportSpecification,
          Abstract.ModuleName (Origin t) ~ AST.ModuleName (Origin t),
          Abstract.ImportSpecification (Origin t) ~ AST.ImportSpecification (Origin t),
          Abstract.ImportSpecification (Target t) ~ AST.ImportSpecification (Target t)) =>
         DeeplyTranslatable t AST.Import where
   translateDeeply t (AST.Import safe qualified package name alias detail) =
      AST.Import safe qualified package name alias (translateFully t <$> detail)

instance (WrapTranslation t, FullyTranslatable t AST.ImportItem,
          Abstract.ImportItem (Origin t) ~ AST.ImportItem (Origin t),
          Abstract.ImportItem (Target t) ~ AST.ImportItem (Target t)) =>
         DeeplyTranslatable t AST.ImportSpecification where
   translateDeeply t (AST.ImportSpecification hiding items) =
      AST.ImportSpecification hiding (translateFully t <$> items)

instance (WrapTranslation t, FullyTranslatable t AST.ImportSpecification,
          Abstract.QualifiedName (Origin t) ~ AST.QualifiedName (Origin t)) =>
         DeeplyTranslatable t AST.ImportItem where
   translateDeeply _ (AST.ImportClassOrType name members) = AST.ImportClassOrType name members
   translateDeeply _ (AST.ImportVar name) = AST.ImportVar name
   translateDeeply _ (AST.ImportPattern name) = AST.ImportPattern name

instance (WrapTranslation t, WrappedTranslation t AST.Declaration,
          FullyTranslatable t AST.Context, FullyTranslatable t AST.ClassInstanceLHS,
          FullyTranslatable t AST.FunctionalDependency,
          FullyTranslatable t AST.TypeLHS, FullyTranslatable t AST.Type, DeeplyTranslatable t AST.TypeVarBinding,
          FullyTranslatable t AST.DataConstructor, FullyTranslatable t AST.GADTConstructor,
          FullyTranslatable t AST.DerivingClause, FullyTranslatable t AST.DerivingStrategy,
          FullyTranslatable t AST.EquationLHS, FullyTranslatable t AST.EquationRHS, FullyTranslatable t AST.Expression,
          FullyTranslatable t AST.PatternEquationClause,
          FullyTranslatable t AST.PatternLHS, FullyTranslatable t AST.Pattern,
          Abstract.Declaration (Origin t) ~ AST.Declaration (Origin t),
          Abstract.Declaration (Target t) ~ AST.Declaration (Target t),
          Abstract.Context (Origin t) ~ AST.Context (Origin t),
          Abstract.Context (Target t) ~ AST.Context (Target t),
          Abstract.ClassInstanceLHS (Origin t) ~ AST.ClassInstanceLHS (Origin t),
          Abstract.ClassInstanceLHS (Target t) ~ AST.ClassInstanceLHS (Target t),
          Abstract.FunctionalDependency (Origin t) ~ AST.FunctionalDependency (Origin t),
          Abstract.FunctionalDependency (Target t) ~ AST.FunctionalDependency (Target t),
          Abstract.TypeLHS (Origin t) ~ AST.TypeLHS (Origin t),
          Abstract.TypeLHS (Target t) ~ AST.TypeLHS (Target t),
          Abstract.Type (Origin t) ~ AST.Type (Origin t),
          Abstract.Type (Target t) ~ AST.Type (Target t),
          Abstract.Kind (Origin t) ~ AST.Type (Origin t),
          Abstract.Kind (Target t) ~ AST.Type (Target t),
          Abstract.TypeVarBinding (Origin t) ~ AST.TypeVarBinding (Origin t),
          Abstract.TypeVarBinding (Target t) ~ AST.TypeVarBinding (Target t),
          Abstract.DataConstructor (Origin t) ~ AST.DataConstructor (Origin t),
          Abstract.DataConstructor (Target t) ~ AST.DataConstructor (Target t),
          Abstract.GADTConstructor (Origin t) ~ AST.GADTConstructor (Origin t),
          Abstract.GADTConstructor (Target t) ~ AST.GADTConstructor (Target t),
          Abstract.DerivingClause (Origin t) ~ AST.DerivingClause (Origin t),
          Abstract.DerivingClause (Target t) ~ AST.DerivingClause (Target t),
          Abstract.DerivingStrategy (Origin t) ~ AST.DerivingStrategy (Origin t),
          Abstract.DerivingStrategy (Target t) ~ AST.DerivingStrategy (Target t),
          Abstract.EquationLHS (Origin t) ~ AST.EquationLHS (Origin t),
          Abstract.EquationLHS (Target t) ~ AST.EquationLHS (Target t),
          Abstract.EquationRHS (Origin t) ~ AST.EquationRHS (Origin t),
          Abstract.EquationRHS (Target t) ~ AST.EquationRHS (Target t),
          Abstract.Expression (Origin t) ~ AST.Expression (Origin t),
          Abstract.Expression (Target t) ~ AST.Expression (Target t),
          Abstract.PatternLHS (Origin t) ~ AST.PatternLHS (Origin t),
          Abstract.PatternLHS (Target t) ~ AST.PatternLHS (Target t),
          Abstract.PatternEquationClause (Origin t) ~ AST.PatternEquationClause (Origin t),
          Abstract.PatternEquationClause (Target t) ~ AST.PatternEquationClause (Target t),
          Abstract.Pattern (Origin t) ~ AST.Pattern (Origin t),
          Abstract.Pattern (Target t) ~ AST.Pattern (Target t)) =>
         DeeplyTranslatable t AST.Declaration where
   translateDeeply t (AST.ClassDeclaration context lhs methods) =
      AST.ClassDeclaration (translateFully t context) (translateFully t lhs) (translateFully t <$> methods)
   translateDeeply t (AST.FunDepClassDeclaration support context lhs fundeps methods) =
      AST.FunDepClassDeclaration support (translateFully t context) (translateFully t lhs)
                                 (translateFully t <$> fundeps) (translateFully t <$> methods)
   translateDeeply t (AST.DataDeclaration context lhs kind constructors derivings) =
      AST.DataDeclaration (translateFully t context) (translateFully t lhs) (translateFully t <$> kind)
                          (translateFully t <$> constructors) (translateFully t <$> derivings)
   translateDeeply t (AST.GADTDeclaration lhs kind constructors derivings) =
      AST.GADTDeclaration (translateFully t lhs) (translateFully t <$> kind)
                          (translateFully t <$> constructors) (translateFully t <$> derivings)
   translateDeeply t (AST.TypeDataDeclaration sup lhs kind constructors) =
      AST.TypeDataDeclaration sup (translateFully t lhs) (translateFully t <$> kind)
                              (translateFully t <$> constructors)
   translateDeeply t (AST.TypeGADTDeclaration sup1 sup2 lhs kind constructors) =
      AST.TypeGADTDeclaration sup1 sup2 (translateFully t lhs) (translateFully t <$> kind)
                              (translateFully t <$> constructors)
   translateDeeply t (AST.DefaultDeclaration types) = AST.DefaultDeclaration (translateFully t <$> types)
   translateDeeply t (AST.EquationDeclaration lhs rhs wheres) =
      AST.EquationDeclaration (translateFully t lhs) (translateFully t rhs) (translateFully t <$> wheres)
   translateDeeply _ (AST.FixityDeclaration assoc prec names) = AST.FixityDeclaration assoc prec names
   translateDeeply _ (AST.ExplicitTypeFixityDeclaration sup assoc prec names) =
      AST.ExplicitTypeFixityDeclaration sup assoc prec names
   translateDeeply _ (AST.ExplicitDataFixityDeclaration sup assoc prec names) =
      AST.ExplicitDataFixityDeclaration sup assoc prec names
   translateDeeply t (AST.ForeignExport convention extName varName ty) =
      AST.ForeignExport convention extName varName (translateFully t ty)
   translateDeeply t (AST.ForeignImport convention safety extName varName ty) =
      AST.ForeignImport convention safety extName varName (translateFully t ty)
   translateDeeply t (AST.InstanceDeclaration vars context lhs methods) =
      AST.InstanceDeclaration (translateDeeply t <$> vars) (translateFully t context)
                              (translateFully t lhs) (translateFully t <$> methods)
   translateDeeply t (AST.NewtypeDeclaration context lhs kind constructor derivings) =
      AST.NewtypeDeclaration (translateFully t context) (translateFully t lhs) (translateFully t <$> kind)
                             (translateFully t constructor) (translateFully t <$> derivings)
   translateDeeply t (AST.GADTNewtypeDeclaration lhs kind constructor derivings) =
      AST.GADTNewtypeDeclaration (translateFully t lhs) (translateFully t <$> kind)
                                 (translateFully t constructor) (translateFully t <$> derivings)
   translateDeeply t (AST.TypeSynonymDeclaration lhs ty) =
      AST.TypeSynonymDeclaration (translateFully t lhs) (translateFully t ty)
   translateDeeply t (AST.TypeSignature names context ty) =
      AST.TypeSignature names (translateFully t context) (translateFully t ty)
   translateDeeply t (AST.DataFamilyDeclaration lhs kind) =
      AST.DataFamilyDeclaration (translateFully t lhs) (translateFully t <$> kind)
   translateDeeply t (AST.OpenTypeFamilyDeclaration lhs kind) =
      AST.OpenTypeFamilyDeclaration (translateFully t lhs) (translateFully t <$> kind)
   translateDeeply t (AST.ClosedTypeFamilyDeclaration lhs kind eqs) =
      AST.ClosedTypeFamilyDeclaration (translateFully t lhs) (translateFully t <$> kind) (translateFully t <$> eqs)
   translateDeeply t (AST.InjectiveOpenTypeFamilyDeclaration lhs var deps) =
      AST.InjectiveOpenTypeFamilyDeclaration (translateFully t lhs) (translateDeeply t var) deps
   translateDeeply t (AST.InjectiveClosedTypeFamilyDeclaration lhs var deps eqs) =
      AST.InjectiveClosedTypeFamilyDeclaration (translateFully t lhs) (translateDeeply t var)
                                               deps (translateFully t <$> eqs)
   translateDeeply t (AST.DataFamilyInstance vars context lhs kind constructors derivings) =
      AST.DataFamilyInstance (translateDeeply t <$> vars) (translateFully t context) (translateFully t lhs)
                             (translateFully t <$> kind) (translateFully t <$> constructors)
                             (translateFully t <$> derivings)
   translateDeeply t (AST.NewtypeFamilyInstance vars context lhs kind constructor derivings) =
      AST.NewtypeFamilyInstance (translateDeeply t <$> vars) (translateFully t context) (translateFully t lhs)
                                (translateFully t <$> kind) (translateFully t constructor)
                                (translateFully t <$> derivings)
   translateDeeply t (AST.GADTDataFamilyInstance vars lhs kind constructors derivings) =
      AST.GADTDataFamilyInstance (translateDeeply t <$> vars) (translateFully t lhs) (translateFully t <$> kind)
                                 (translateFully t <$> constructors) (translateFully t <$> derivings)
   translateDeeply t (AST.GADTNewtypeFamilyInstance vars lhs kind constructor derivings) =
      AST.GADTNewtypeFamilyInstance (translateDeeply t <$> vars) (translateFully t lhs) (translateFully t <$> kind)
                                    (translateFully t constructor) (translateFully t <$> derivings)
   translateDeeply t (AST.TypeFamilyInstance vars lhs rhs) =
      AST.TypeFamilyInstance (translateDeeply t <$> vars) (translateFully t lhs) (translateFully t rhs)
   translateDeeply t (AST.KindSignature name kind) = AST.KindSignature name (translateFully t kind)
   translateDeeply t (AST.DefaultMethodSignature support name context ty) =
      AST.DefaultMethodSignature support name (translateFully t context) (translateFully t ty)
   translateDeeply _ (AST.TypeRoleDeclaration name role) = AST.TypeRoleDeclaration name role
   translateDeeply t (AST.StandaloneDerivingDeclaration support vars context lhs) =
      AST.StandaloneDerivingDeclaration support (translateDeeply t <$> vars)
                                        (translateFully t context) (translateFully t lhs)
   translateDeeply t (AST.StandaloneStrategicDerivingDeclaration support1 support2 strategy vars context lhs) =
      AST.StandaloneStrategicDerivingDeclaration support1 support2 (translateFully t strategy)
                                                 (translateDeeply t <$> vars)
                                                 (translateFully t context)
                                                 (translateFully t lhs)
   translateDeeply t (AST.ImplicitParameterDeclaration support name value) =
      AST.ImplicitParameterDeclaration support name (translateFully t value)
   translateDeeply t (AST.ImplicitPatternSynonym support lhs rhs) =
      AST.ImplicitPatternSynonym support (translateFully t lhs) (translateFully t rhs)
   translateDeeply t (AST.UnidirectionalPatternSynonym support lhs rhs) =
      AST.UnidirectionalPatternSynonym support (translateFully t lhs) (translateFully t rhs)
   translateDeeply t (AST.ExplicitPatternSynonym support lhs rhs clauses) =
      AST.ExplicitPatternSynonym support (translateFully t lhs) (translateFully t rhs) (translateFully t <$> clauses)
   translateDeeply t (AST.PatternSynonymSignature support names vars1 context1 vars2 context2 args result) =
      AST.PatternSynonymSignature support names
                                  (translateDeeply t <$> vars1) (translateFully t context1)
                                  (translateDeeply t <$> vars2) (translateFully t context2)
                                  (translateFully t <$> args) (translateFully t result)

instance (WrapTranslation t,
          FullyTranslatable t AST.PatternEquationLHS, FullyTranslatable t AST.EquationRHS,
          FullyTranslatable t AST.Declaration,
          Abstract.PatternEquationLHS (Origin t) ~ AST.PatternEquationLHS (Origin t),
          Abstract.PatternEquationLHS (Target t) ~ AST.PatternEquationLHS (Target t),
          Abstract.EquationRHS (Origin t) ~ AST.EquationRHS (Origin t),
          Abstract.EquationRHS (Target t) ~ AST.EquationRHS (Target t),
          Abstract.Declaration (Origin t) ~ AST.Declaration (Origin t),
          Abstract.Declaration (Target t) ~ AST.Declaration (Target t)) =>
         DeeplyTranslatable t AST.PatternEquationClause where
   translateDeeply t (AST.PatternEquationClause support lhs rhs wheres) =
      AST.PatternEquationClause support (translateFully t lhs) (translateFully t rhs) (translateFully t <$> wheres)

instance (WrapTranslation t,
          FullyTranslatable t AST.Pattern,
          Abstract.Pattern (Origin t) ~ AST.Pattern (Origin t),
          Abstract.Pattern (Target t) ~ AST.Pattern (Target t)) =>
         DeeplyTranslatable t AST.PatternEquationLHS where
   translateDeeply t (AST.PrefixPatternEquationLHS name patterns) =
      AST.PrefixPatternEquationLHS name (translateFully t <$> patterns)
   translateDeeply t (AST.InfixPatternEquationLHS l name r) =
      AST.InfixPatternEquationLHS (translateFully t l) name (translateFully t r)

instance (WrapTranslation t,
          FullyTranslatable t AST.Context, FullyTranslatable t AST.Type,
          Abstract.Context (Origin t) ~ AST.Context (Origin t),
          Abstract.Context (Target t) ~ AST.Context (Target t),
          Abstract.Type (Origin t) ~ AST.Type (Origin t),
          Abstract.Type (Target t) ~ AST.Type (Target t)) =>
         DeeplyTranslatable t AST.Context where
   translateDeeply t (AST.ClassConstraint className ty) =
      AST.ClassConstraint className (translateFully t ty)
   translateDeeply t (AST.Constraints constraints) = AST.Constraints (translateFully t <$> constraints)
   translateDeeply t (AST.TypeConstraint ty) = AST.TypeConstraint (translateFully t ty)
   translateDeeply t (AST.TypeEquality left right) = AST.TypeEquality (translateFully t left) (translateFully t right)
   translateDeeply t (AST.ImplicitParameterConstraint sup name ty) =
      AST.ImplicitParameterConstraint sup name (translateFully t ty)
   translateDeeply _ AST.NoContext = AST.NoContext

instance (WrapTranslation t,
          Abstract.Constructor (Origin t) ~ AST.Constructor (Origin t),
          Abstract.Constructor (Target t) ~ AST.Constructor (Target t)) => DeeplyTranslatable t AST.Constructor where
   translateDeeply _ (AST.ConstructorReference name) = AST.ConstructorReference name
   translateDeeply _ AST.EmptyListConstructor = AST.EmptyListConstructor
   translateDeeply _ (AST.TupleConstructor size) = AST.TupleConstructor size
   translateDeeply _ (AST.UnboxedTupleConstructor support size) = AST.UnboxedTupleConstructor support size
   translateDeeply _ (AST.UnboxedSumConstructor support size) = AST.UnboxedSumConstructor support size
   translateDeeply _ AST.UnitConstructor = AST.UnitConstructor

instance WrapTranslation t => DeeplyTranslatable t AST.FunctionalDependency where
   translateDeeply _ (AST.FunctionalDependency from to) = AST.FunctionalDependency from to

instance WrapTranslation t => DeeplyTranslatable t AST.PatternLHS where
   translateDeeply _ (AST.PrefixPatternLHS con args) = AST.PrefixPatternLHS con args
   translateDeeply _ (AST.InfixPatternLHS l con r) = AST.InfixPatternLHS l con r
   translateDeeply _ (AST.RecordPatternLHS con fields) = AST.RecordPatternLHS con fields

instance (WrapTranslation t, WrappedTranslation t AST.DataConstructor,
          FullyTranslatable t AST.Context, FullyTranslatable t AST.Type, FullyTranslatable t AST.FieldDeclaration,
          DeeplyTranslatable t AST.TypeVarBinding,
          Abstract.DataConstructor (Origin t) ~ AST.DataConstructor (Origin t),
          Abstract.DataConstructor (Target t) ~ AST.DataConstructor (Target t),
          Abstract.Context (Origin t) ~ AST.Context (Origin t),
          Abstract.Context (Target t) ~ AST.Context (Target t),
          Abstract.Type (Origin t) ~ AST.Type (Origin t),
          Abstract.Type (Target t) ~ AST.Type (Target t),
          Abstract.TypeVarBinding (Origin t) ~ AST.TypeVarBinding (Origin t),
          Abstract.TypeVarBinding (Target t) ~ AST.TypeVarBinding (Target t),
          Abstract.FieldDeclaration (Origin t) ~ AST.FieldDeclaration (Origin t),
          Abstract.FieldDeclaration (Target t) ~ AST.FieldDeclaration (Target t)) =>
         DeeplyTranslatable t AST.DataConstructor where
   translateDeeply t (AST.Constructor name params) = AST.Constructor name (translateFully t <$> params)
   translateDeeply t (AST.RecordConstructor name fields) = AST.RecordConstructor name (translateFully t <$> fields)
   translateDeeply t (AST.ExistentialConstructor vars context body) =
      AST.ExistentialConstructor (translateDeeply t <$> vars) (translateFully t context) (translateFully t body)

instance (WrapTranslation t,
          FullyTranslatable t AST.Context, FullyTranslatable t AST.Type, DeeplyTranslatable t AST.TypeVarBinding,
          Abstract.Context (Origin t) ~ AST.Context (Origin t),
          Abstract.Context (Target t) ~ AST.Context (Target t),
          Abstract.Type (Origin t) ~ AST.Type (Origin t),
          Abstract.Type (Target t) ~ AST.Type (Target t),
          Abstract.TypeVarBinding (Origin t) ~ AST.TypeVarBinding (Origin t),
          Abstract.TypeVarBinding (Target t) ~ AST.TypeVarBinding (Target t)) =>
         DeeplyTranslatable t AST.GADTConstructor where
   translateDeeply t (AST.GADTConstructors names vars context ty) =
      AST.GADTConstructors names (translateDeeply t <$> vars) (translateFully t context) (translateFully t ty)

instance (WrapTranslation t, FullyTranslatable t AST.Type, FullyTranslatable t AST.DerivingStrategy,
          Abstract.DerivingClause (Origin t) ~ AST.DerivingClause (Origin t),
          Abstract.DerivingClause (Target t) ~ AST.DerivingClause (Target t),
          Abstract.Type (Origin t) ~ AST.Type (Origin t),
          Abstract.Type (Target t) ~ AST.Type (Target t),
          Abstract.DerivingStrategy (Origin t) ~ AST.DerivingStrategy (Origin t),
          Abstract.DerivingStrategy (Target t) ~ AST.DerivingStrategy (Target t),
          Abstract.Type (Origin t) ~ AST.Type (Origin t),
          Abstract.Type (Target t) ~ AST.Type (Target t)) =>
         DeeplyTranslatable t AST.DerivingClause where
   translateDeeply _ (AST.SimpleDerive name) = AST.SimpleDerive name
   translateDeeply t (AST.StrategicDerive support strategy types) =
      AST.StrategicDerive support (translateFully t strategy) (translateFully t <$> types)
   translateDeeply t (AST.DeriveVia support types via) =
      AST.DeriveVia support (translateFully t <$> types) (translateFully t via)

instance (WrapTranslation t, FullyTranslatable t AST.Type,
          Abstract.DerivingStrategy (Origin t) ~ AST.DerivingStrategy (Origin t),
          Abstract.DerivingStrategy (Target t) ~ AST.DerivingStrategy (Target t),
          Abstract.Type (Origin t) ~ AST.Type (Origin t),
          Abstract.Type (Target t) ~ AST.Type (Target t)) =>
         DeeplyTranslatable t AST.DerivingStrategy where
   translateDeeply _ AST.Default = AST.Default
   translateDeeply _ AST.Stock = AST.Stock
   translateDeeply _ AST.Newtype = AST.Newtype
   translateDeeply _ AST.AnyClass = AST.AnyClass
   translateDeeply t (AST.Via support ty) = AST.Via support (translateFully t ty)

instance (WrapTranslation t, FullyTranslatable t AST.Type,
          Abstract.FieldDeclaration (Origin t) ~ AST.FieldDeclaration (Origin t),
          Abstract.FieldDeclaration (Target t) ~ AST.FieldDeclaration (Target t),
          Abstract.Type (Origin t) ~ AST.Type (Origin t),
          Abstract.Type (Target t) ~ AST.Type (Target t)) =>
         DeeplyTranslatable t AST.FieldDeclaration where
   translateDeeply t (AST.ConstructorFields name ty) = AST.ConstructorFields name (translateFully t ty)

instance (WrapTranslation t, FullyTranslatable t AST.Expression,
          Abstract.FieldBinding (Origin t) ~ AST.FieldBinding (Origin t),
          Abstract.FieldBinding (Target t) ~ AST.FieldBinding (Target t),
          Abstract.Expression (Origin t) ~ AST.Expression (Origin t),
          Abstract.Expression (Target t) ~ AST.Expression (Target t)) =>
         DeeplyTranslatable t AST.FieldBinding where
   translateDeeply t (AST.FieldBinding name value) = AST.FieldBinding name (translateFully t value)
   translateDeeply _ (AST.PunnedFieldBinding name) = AST.PunnedFieldBinding name

instance (WrapTranslation t, FullyTranslatable t AST.Pattern,
          Abstract.FieldPattern (Origin t) ~ AST.FieldPattern (Origin t),
          Abstract.FieldPattern (Target t) ~ AST.FieldPattern (Target t),
          Abstract.Pattern (Origin t) ~ AST.Pattern (Origin t),
          Abstract.Pattern (Target t) ~ AST.Pattern (Target t)) =>
         DeeplyTranslatable t AST.FieldPattern where
   translateDeeply t (AST.FieldPattern name pat) = AST.FieldPattern name (translateFully t pat)
   translateDeeply _ (AST.PunnedFieldPattern name) = AST.PunnedFieldPattern name

instance (WrapTranslation t, WrappedTranslation t AST.TypeLHS, DeeplyTranslatable t AST.TypeVarBinding,
          Abstract.TypeLHS (Origin t) ~ AST.TypeLHS (Origin t),
          Abstract.TypeLHS (Target t) ~ AST.TypeLHS (Target t),
          Abstract.Type (Origin t) ~ AST.Type (Origin t),
          Abstract.Type (Target t) ~ AST.Type (Target t),
          Abstract.TypeVarBinding (Origin t) ~ AST.TypeVarBinding (Origin t),
          Abstract.TypeVarBinding (Target t) ~ AST.TypeVarBinding (Target t)) =>
         DeeplyTranslatable t AST.TypeLHS where
   translateDeeply t (AST.SimpleTypeLHS name vars) = AST.SimpleTypeLHS name (translateDeeply t <$> vars)
   translateDeeply t (AST.TypeLHSApplication left var) =
      AST.TypeLHSApplication (translateFully t left) (translateDeeply t var)
   translateDeeply t (AST.TypeLHSTypeApplication support left var) =
      AST.TypeLHSTypeApplication support (translateFully t left) (translateDeeply t var)

instance (WrapTranslation t, WrappedTranslation t AST.ClassInstanceLHS,
          FullyTranslatable t AST.Type, DeeplyTranslatable t AST.TypeVarBinding,
          Abstract.ClassInstanceLHS (Origin t) ~ AST.ClassInstanceLHS (Origin t),
          Abstract.ClassInstanceLHS (Target t) ~ AST.ClassInstanceLHS (Target t),
          Abstract.TypeLHS (Origin t) ~ AST.TypeLHS (Origin t),
          Abstract.TypeLHS (Target t) ~ AST.TypeLHS (Target t),
          Abstract.Type (Origin t) ~ AST.Type (Origin t),
          Abstract.Type (Target t) ~ AST.Type (Target t),
          Abstract.Kind (Origin t) ~ AST.Type (Origin t),
          Abstract.Kind (Target t) ~ AST.Type (Target t)) =>
         DeeplyTranslatable t AST.ClassInstanceLHS where
   translateDeeply t (AST.TypeClassInstanceLHS name ty) = AST.TypeClassInstanceLHS name (translateFully t ty)
   translateDeeply _ (AST.ClassReferenceInstanceLHS name) = AST.ClassReferenceInstanceLHS name
   translateDeeply t (AST.InfixTypeClassInstanceLHS left op right) =
      AST.InfixTypeClassInstanceLHS (translateFully t left) op (translateFully t right)
   translateDeeply t (AST.ClassInstanceLHSApplication left right) =
      AST.ClassInstanceLHSApplication (translateFully t left) (translateFully t right)
   translateDeeply t (AST.ClassInstanceLHSKindApplication left right) =
      AST.ClassInstanceLHSKindApplication (translateFully t left) (translateFully t right)

instance (WrapTranslation t, WrappedTranslation t AST.Type,
          FullyTranslatable t AST.Constructor, FullyTranslatable t AST.Context,
          FullyTranslatable t AST.FieldDeclaration, FullyTranslatable t AST.Type,
          DeeplyTranslatable t AST.TypeVarBinding,
          Abstract.Constructor (Origin t) ~ AST.Constructor (Origin t),
          Abstract.Constructor (Target t) ~ AST.Constructor (Target t),
          Abstract.Context (Origin t) ~ AST.Context (Origin t),
          Abstract.Context (Target t) ~ AST.Context (Target t),
          Abstract.Type (Origin t) ~ AST.Type (Origin t),
          Abstract.Type (Target t) ~ AST.Type (Target t),
          Abstract.Kind (Origin t) ~ AST.Type (Origin t),
          Abstract.Kind (Target t) ~ AST.Type (Target t),
          Abstract.FieldDeclaration (Origin t) ~ AST.FieldDeclaration (Origin t),
          Abstract.FieldDeclaration (Target t) ~ AST.FieldDeclaration (Target t),
          Abstract.TypeVarBinding (Origin t) ~ AST.TypeVarBinding (Origin t),
          Abstract.TypeVarBinding (Target t) ~ AST.TypeVarBinding (Target t)) =>
         DeeplyTranslatable t AST.Type where
   translateDeeply t (AST.ConstructorType con) = AST.ConstructorType (translateFully t con)
   translateDeeply _ AST.FunctionConstructorType = AST.FunctionConstructorType
   translateDeeply t (AST.FunctionType domain codomain) =
      AST.FunctionType (translateFully t domain) (translateFully t codomain)
   translateDeeply t (AST.LinearFunctionType domain codomain) =
      AST.LinearFunctionType (translateFully t domain) (translateFully t codomain)
   translateDeeply t (AST.MultiplicityFunctionType domain multiplicity codomain) =
      AST.MultiplicityFunctionType (translateFully t domain) (translateFully t multiplicity) (translateFully t codomain)
   translateDeeply t (AST.RecordFunctionType domain codomain) =
      AST.RecordFunctionType (translateFully t <$> domain) (translateFully t codomain)
   translateDeeply t (AST.ListType item) = AST.ListType (translateFully t item)
   translateDeeply t (AST.StrictType body) = AST.StrictType (translateFully t body)
   translateDeeply t (AST.TupleType items) = AST.TupleType (translateFully t <$> items)
   translateDeeply t (AST.UnboxedTupleType support items) = AST.UnboxedTupleType support (translateFully t <$> items)
   translateDeeply t (AST.UnboxedSumType support items) = AST.UnboxedSumType support (translateFully t <$> items)
   translateDeeply t (AST.TypeApplication left right) =
      AST.TypeApplication (translateFully t left) (translateFully t right)
   translateDeeply t (AST.InfixTypeApplication left op right) =
      AST.InfixTypeApplication (translateFully t left) op (translateFully t right)
   translateDeeply _ (AST.TypeVariable name) = AST.TypeVariable name
   translateDeeply t (AST.ForallType vars body) = AST.ForallType (translateDeeply t <$> vars) (translateFully t body)
   translateDeeply t (AST.ConstrainedType context body) =
      AST.ConstrainedType (translateFully t context) (translateFully t body)
   translateDeeply t (AST.ConstraintType context) = AST.ConstraintType (translateFully t context)
   translateDeeply t (AST.KindedType body kind) = AST.KindedType (translateFully t body) (translateFully t kind)
   translateDeeply _ AST.TypeWildcard = AST.TypeWildcard
   translateDeeply t (AST.TypeKind body) = AST.TypeKind (translateFully t body)
   translateDeeply t (AST.VisibleDependentType vars body) =
      AST.VisibleDependentType (translateDeeply t <$> vars) (translateFully t body)
   translateDeeply _ AST.GroundTypeKind = AST.GroundTypeKind
   translateDeeply t (AST.PromotedConstructorType con) = AST.PromotedConstructorType (translateFully t con)
   translateDeeply t (AST.PromotedTupleType items) = AST.PromotedTupleType (translateFully t <$> items)
   translateDeeply t (AST.PromotedListType items) = AST.PromotedListType (translateFully t <$> items)
   translateDeeply _ (AST.PromotedIntegerLiteral n) = AST.PromotedIntegerLiteral n
   translateDeeply _ (AST.PromotedCharLiteral c) = AST.PromotedCharLiteral c
   translateDeeply _ (AST.PromotedStringLiteral t) = AST.PromotedStringLiteral t
   translateDeeply t (AST.PromotedInfixTypeApplication left op right) =
      AST.PromotedInfixTypeApplication (translateFully t left) op (translateFully t right)
   translateDeeply t (AST.VisibleKindApplication ty kind) =
      AST.VisibleKindApplication (translateFully t ty) (translateFully t kind)

instance (WrapTranslation t, FullyTranslatable t AST.Type,
          Abstract.Kind (Origin t) ~ AST.Type (Origin t),
          Abstract.Kind (Target t) ~ AST.Type (Target t)) =>
         DeeplyTranslatable t AST.TypeVarBinding where
   translateDeeply t (AST.ExplicitlyKindedTypeVariable inferred name kind) =
      AST.ExplicitlyKindedTypeVariable inferred name (translateFully t kind)
   translateDeeply _ (AST.ImplicitlyKindedTypeVariable inferred name) = AST.ImplicitlyKindedTypeVariable inferred name

instance (WrapTranslation t, WrappedTranslation t AST.EquationLHS, FullyTranslatable t AST.Pattern,
          Abstract.EquationLHS (Origin t) ~ AST.EquationLHS (Origin t),
          Abstract.EquationLHS (Target t) ~ AST.EquationLHS (Target t),
          Abstract.Pattern (Origin t) ~ AST.Pattern (Origin t),
          Abstract.Pattern (Target t) ~ AST.Pattern (Target t)) =>
         DeeplyTranslatable t AST.EquationLHS where
   translateDeeply t (AST.PrefixLHS con args) = AST.PrefixLHS (translateFully t con) (translateFully t <$> args)
   translateDeeply t (AST.InfixLHS left op right) = AST.InfixLHS (translateFully t left) op (translateFully t right)
   translateDeeply t (AST.PatternLHS pat) = AST.PatternLHS (translateFully t pat)
   translateDeeply _ (AST.VariableLHS name) = AST.VariableLHS name

instance (WrapTranslation t, WrappedTranslation t AST.EquationRHS,
          FullyTranslatable t AST.Expression, FullyTranslatable t AST.GuardedExpression,
          Abstract.EquationRHS (Origin t) ~ AST.EquationRHS (Origin t),
          Abstract.EquationRHS (Target t) ~ AST.EquationRHS (Target t),
          Abstract.Expression (Origin t) ~ AST.Expression (Origin t),
          Abstract.Expression (Target t) ~ AST.Expression (Target t),
          Abstract.GuardedExpression (Origin t) ~ AST.GuardedExpression (Origin t),
          Abstract.GuardedExpression (Target t) ~ AST.GuardedExpression (Target t)) =>
         DeeplyTranslatable t AST.EquationRHS where
   translateDeeply t (AST.GuardedRHS choices) = AST.GuardedRHS (translateFully t <$> choices)
   translateDeeply t (AST.NormalRHS body) = AST.NormalRHS (translateFully t body)

instance (WrapTranslation t, WrappedTranslation t AST.Pattern,
          FullyTranslatable t AST.Constructor, FullyTranslatable t AST.FieldPattern,
          FullyTranslatable t AST.Type, FullyTranslatable t AST.Expression, FullyTranslatable t AST.Value,
          Abstract.Pattern (Origin t) ~ AST.Pattern (Origin t),
          Abstract.Pattern (Target t) ~ AST.Pattern (Target t),
          Abstract.Constructor (Origin t) ~ AST.Constructor (Origin t),
          Abstract.Constructor (Target t) ~ AST.Constructor (Target t),
          Abstract.Expression (Origin t) ~ AST.Expression (Origin t),
          Abstract.Expression (Target t) ~ AST.Expression (Target t),
          Abstract.FieldPattern (Origin t) ~ AST.FieldPattern (Origin t),
          Abstract.FieldPattern (Target t) ~ AST.FieldPattern (Target t),
          Abstract.Type (Origin t) ~ AST.Type (Origin t),
          Abstract.Kind (Origin t) ~ AST.Type (Origin t),
          Abstract.Type (Target t) ~ AST.Type (Target t),
          Abstract.Kind (Target t) ~ AST.Type (Target t),
          Abstract.Value (Origin t) ~ AST.Value (Origin t),
          Abstract.Value (Target t) ~ AST.Value (Target t)) =>
         DeeplyTranslatable t AST.Pattern where
   translateDeeply t (AST.AsPattern name body) = AST.AsPattern name (translateFully t body)
   translateDeeply t (AST.BangPattern support body) = AST.BangPattern support (translateFully t body)
   translateDeeply t (AST.ViewPattern support view pattern) =
      AST.ViewPattern support (translateFully t view) (translateFully t pattern)
   translateDeeply _ (AST.NPlusKPattern support n k) = AST.NPlusKPattern support n k
   translateDeeply t (AST.ConstructorPattern con types args) =
      AST.ConstructorPattern (translateFully t con) (translateFully t <$> types) (translateFully t <$> args)
   translateDeeply t (AST.InfixPattern left con right) =
      AST.InfixPattern (translateFully t left) con (translateFully t right)
   translateDeeply t (AST.IrrefutablePattern body) = AST.IrrefutablePattern (translateFully t body)
   translateDeeply t (AST.ListPattern items) = AST.ListPattern (translateFully t <$> items)
   translateDeeply t (AST.LiteralPattern value) = AST.LiteralPattern (translateFully t value)
   translateDeeply t (AST.RecordPattern con fields) = AST.RecordPattern con (translateFully t <$> fields)
   translateDeeply t (AST.WildcardRecordPattern support con fields) =
      AST.WildcardRecordPattern support con (translateFully t <$> fields)
   translateDeeply t (AST.TuplePattern items) = AST.TuplePattern (translateFully t <$> items)
   translateDeeply t (AST.UnboxedTuplePattern support items) =
      AST.UnboxedTuplePattern support (translateFully t <$> items)
   translateDeeply t (AST.UnboxedSumPattern support before branch after) =
      AST.UnboxedSumPattern support before (translateFully t branch) after
   translateDeeply t (AST.TypedPattern p ty) = AST.TypedPattern (translateFully t p) (translateFully t ty)
   translateDeeply t (AST.InvisibleTypePattern support ty) = AST.InvisibleTypePattern support (translateFully t ty)
   translateDeeply t (AST.ExplicitTypePattern support ty) = AST.ExplicitTypePattern support (translateFully t ty)
   translateDeeply _ (AST.VariablePattern name) = AST.VariablePattern name
   translateDeeply _ AST.WildcardPattern = AST.WildcardPattern

instance (WrapTranslation t, WrappedTranslation t AST.Expression,
          FullyTranslatable t AST.CaseAlternative, FullyTranslatable t AST.Constructor,
          FullyTranslatable t AST.Declaration, FullyTranslatable t AST.FieldBinding,
          FullyTranslatable t AST.GuardedExpression, FullyTranslatable t AST.Pattern,
          FullyTranslatable t AST.Statement, FullyTranslatable t AST.Type, FullyTranslatable t AST.Value,
          Abstract.Expression (Origin t) ~ AST.Expression (Origin t),
          Abstract.Expression (Target t) ~ AST.Expression (Target t),
          Abstract.CaseAlternative (Origin t) ~ AST.CaseAlternative (Origin t),
          Abstract.CaseAlternative (Target t) ~ AST.CaseAlternative (Target t),
          Abstract.Constructor (Origin t) ~ AST.Constructor (Origin t),
          Abstract.Constructor (Target t) ~ AST.Constructor (Target t),
          Abstract.Declaration (Origin t) ~ AST.Declaration (Origin t),
          Abstract.Declaration (Target t) ~ AST.Declaration (Target t),
          Abstract.FieldBinding (Origin t) ~ AST.FieldBinding (Origin t),
          Abstract.FieldBinding (Target t) ~ AST.FieldBinding (Target t),
          Abstract.GuardedExpression (Origin t) ~ AST.GuardedExpression (Origin t),
          Abstract.GuardedExpression (Target t) ~ AST.GuardedExpression (Target t),
          Abstract.Pattern (Origin t) ~ AST.Pattern (Origin t),
          Abstract.Pattern (Target t) ~ AST.Pattern (Target t),
          Abstract.Statement (Origin t) ~ AST.Statement (Origin t),
          Abstract.Statement (Target t) ~ AST.Statement (Target t),
          Abstract.Type (Origin t) ~ AST.Type (Origin t), Abstract.Type (Target t) ~ AST.Type (Target t),
          Abstract.Value (Origin t) ~ AST.Value (Origin t), Abstract.Value (Target t) ~ AST.Value (Target t)) =>
         DeeplyTranslatable t AST.Expression where
   translateDeeply t (AST.ApplyExpression f x) = AST.ApplyExpression (translateFully t f) (translateFully t x)
   translateDeeply t (AST.ConditionalExpression cond true false) =
      AST.ConditionalExpression (translateFully t cond) (translateFully t true) (translateFully t false)
   translateDeeply t (AST.ConstructorExpression con) = AST.ConstructorExpression (translateFully t con)
   translateDeeply t (AST.CaseExpression scrutinee alts) =
      AST.CaseExpression (translateFully t scrutinee) (translateFully t <$> alts)
   translateDeeply t (AST.LambdaCaseExpression alts) = AST.LambdaCaseExpression (translateFully t <$> alts)
   translateDeeply t (AST.MultiWayIfExpression alts) = AST.MultiWayIfExpression (translateFully t <$> alts)
   translateDeeply t (AST.DoExpression body) = AST.DoExpression (translateFully t body)
   translateDeeply t (AST.MDoExpression body) = AST.MDoExpression (translateFully t body)
   translateDeeply t (AST.QualifiedDoExpression sup m body) =
      AST.QualifiedDoExpression sup m (translateFully t body)
   translateDeeply t (AST.MDoQualifiedExpression sup1 sup2 m body) =
      AST.MDoQualifiedExpression sup1 sup2 m (translateFully t body)
   translateDeeply t (AST.InfixExpression left op right) =
      AST.InfixExpression (translateFully t left) (translateFully t op) (translateFully t right)
   translateDeeply t (AST.LeftSectionExpression left op) = AST.LeftSectionExpression (translateFully t left) op
   translateDeeply t (AST.LambdaExpression patterns body) =
      AST.LambdaExpression (translateFully t <$> patterns) (translateFully t body)
   translateDeeply t (AST.LetExpression bindings body) =
      AST.LetExpression (translateFully t <$> bindings) (translateFully t body)
   translateDeeply t (AST.ListComprehension element guards) =
      AST.ListComprehension (translateFully t element) (translateFully t <$> guards)
   translateDeeply t (AST.ParallelListComprehension element guards1 guards2 guardses) =
      AST.ParallelListComprehension
         (translateFully t element) (translateFully t <$> guards1)
         (translateFully t <$> guards2) ((translateFully t <$>) <$> guardses)
   translateDeeply t (AST.ListExpression items) = AST.ListExpression (translateFully t <$> items)
   translateDeeply t (AST.LiteralExpression value) = AST.LiteralExpression (translateFully t value)
   translateDeeply _ AST.Negate = AST.Negate
   translateDeeply t (AST.RecordExpression record fields) =
      AST.RecordExpression (translateFully t record) (translateFully t <$> fields)
   translateDeeply _ (AST.ReferenceExpression name) = AST.ReferenceExpression name
   translateDeeply t (AST.RightSectionExpression op right) = AST.RightSectionExpression op (translateFully t right)
   translateDeeply t (AST.SequenceExpression start next end) =
      AST.SequenceExpression (translateFully t start) (translateFully t <$> next) (translateFully t <$> end)
   translateDeeply t (AST.TupleExpression items) = AST.TupleExpression (translateFully t <$> items)
   translateDeeply t (AST.TupleSectionExpression items) =
      AST.TupleSectionExpression ((translateFully t <$>) <$> items)
   translateDeeply t (AST.UnboxedTupleExpression support items) =
      AST.UnboxedTupleExpression support (translateFully t <$> items)
   translateDeeply t (AST.UnboxedTupleSectionExpression support items) =
      AST.UnboxedTupleSectionExpression support ((translateFully t <$>) <$> items)
   translateDeeply t (AST.UnboxedSumExpression support before branch after) =
      AST.UnboxedSumExpression support before (translateFully t branch) after
   translateDeeply t (AST.TypedExpression x signature) =
      AST.TypedExpression (translateFully t x) (translateFully t signature)
   translateDeeply t (AST.VisibleTypeApplication x ty) =
      AST.VisibleTypeApplication (translateFully t x) (translateFully t ty)
   translateDeeply t (AST.ExplicitTypeExpression sup ty) =
      AST.ExplicitTypeExpression sup (translateFully t ty)
   translateDeeply _ (AST.OverloadedLabel l) = AST.OverloadedLabel l
   translateDeeply _ (AST.ImplicitParameterExpression sup name) = AST.ImplicitParameterExpression sup name
   translateDeeply t (AST.GetField record name) = AST.GetField (translateFully t record) name
   translateDeeply _ (AST.FieldProjection fields) = AST.FieldProjection fields
   translateDeeply t (AST.WildcardRecordExpression sup con fields) =
      AST.WildcardRecordExpression sup con (translateFully t <$> fields)

instance (WrapTranslation t,
          FullyTranslatable t AST.Expression, FullyTranslatable t AST.Statement,
          Abstract.GuardedExpression (Origin t) ~ AST.GuardedExpression (Origin t),
          Abstract.GuardedExpression (Target t) ~ AST.GuardedExpression (Target t),
          Abstract.Expression (Origin t) ~ AST.Expression (Origin t),
          Abstract.Expression (Target t) ~ AST.Expression (Target t),
          Abstract.Statement (Origin t) ~ AST.Statement (Origin t),
          Abstract.Statement (Target t) ~ AST.Statement (Target t)) =>
         DeeplyTranslatable t AST.GuardedExpression where
   translateDeeply t (AST.GuardedExpression guards result) =
      AST.GuardedExpression (translateFully t <$> guards) (translateFully t result)

instance (WrapTranslation t, WrappedTranslation t AST.Statement,
          FullyTranslatable t AST.Declaration, FullyTranslatable t AST.Expression, FullyTranslatable t AST.Pattern,
          Abstract.Expression (Origin t) ~ AST.Expression (Origin t),
          Abstract.Expression (Target t) ~ AST.Expression (Target t),
          Abstract.Declaration (Origin t) ~ AST.Declaration (Origin t),
          Abstract.Declaration (Target t) ~ AST.Declaration (Target t),
          Abstract.Pattern (Origin t) ~ AST.Pattern (Origin t),
          Abstract.Pattern (Target t) ~ AST.Pattern (Target t),
          Abstract.Statement (Origin t) ~ AST.Statement (Origin t),
          Abstract.Statement (Target t) ~ AST.Statement (Target t)) =>
         DeeplyTranslatable t AST.Statement where
   translateDeeply t (AST.BindStatement pat value) = AST.BindStatement (translateFully t pat) (translateFully t value)
   translateDeeply t (AST.ExpressionStatement body) = AST.ExpressionStatement (translateFully t body)
   translateDeeply t (AST.LetStatement bindings) = AST.LetStatement (translateFully t <$> bindings)
   translateDeeply t (AST.RecursiveStatement statements) = AST.RecursiveStatement (translateFully t <$> statements)

instance (WrapTranslation t,
          FullyTranslatable t AST.Declaration, FullyTranslatable t AST.EquationRHS, FullyTranslatable t AST.Pattern,
          Abstract.CaseAlternative (Origin t) ~ AST.CaseAlternative (Origin t),
          Abstract.CaseAlternative (Target t) ~ AST.CaseAlternative (Target t),
          Abstract.Declaration (Origin t) ~ AST.Declaration (Origin t),
          Abstract.Declaration (Target t) ~ AST.Declaration (Target t),
          Abstract.EquationRHS (Origin t) ~ AST.EquationRHS (Origin t),
          Abstract.EquationRHS (Target t) ~ AST.EquationRHS (Target t),
          Abstract.Pattern (Origin t) ~ AST.Pattern (Origin t),
          Abstract.Pattern (Target t) ~ AST.Pattern (Target t)) =>
         DeeplyTranslatable t AST.CaseAlternative where
   translateDeeply t (AST.CaseAlternative lhs rhs wheres) =
      AST.CaseAlternative (translateFully t lhs) (translateFully t rhs) (translateFully t <$> wheres)

instance WrapTranslation t => DeeplyTranslatable t AST.Value where
   translateDeeply _ (AST.CharLiteral l)     = AST.CharLiteral l
   translateDeeply _ (AST.FloatingLiteral l) = AST.FloatingLiteral l
   translateDeeply _ (AST.IntegerLiteral l)  = AST.IntegerLiteral l
   translateDeeply _ (AST.StringLiteral l)   = AST.StringLiteral l
   translateDeeply t (AST.HashLiteral sup l) = AST.HashLiteral sup (translateDeeply t l)
   translateDeeply _ (AST.ExtendedLiteral sup n ty) = AST.ExtendedLiteral sup n ty

-- Default overlappable Translation instances

instance {-# overlappable #-} (NameTranslation t, WrapTranslation t, Functor (Wrap t), Translation t node) =>
                              WrappedTranslation t node where
   translateWrapped = fmap . translate

instance {-# overlappable #-} (NameTranslation t, WrapTranslation t, Functor (Wrap t),
                               Coercible
                                  (node (Origin t) (Origin t) (Wrap t) (Wrap t))
                                  (node (Target t) (Origin t) (Wrap t) (Wrap t))) =>
                              Translation t node where
   translate = const coerce

instance {-# overlappable #-} (NameTranslation t, WrapTranslation t, Functor (Wrap t)) =>
                              Translation t AST.Module where
   translate t (AST.NamedModule modName exports imports declarations) =
      AST.NamedModule (translateModuleName t modName) exports imports declarations
   translate _ (AST.AnonymousModule imports declarations) = AST.AnonymousModule imports declarations
   translate _ (AST.ExtendedModule extensions m) = AST.ExtendedModule extensions m

instance {-# overlappable #-} (NameTranslation t, WrapTranslation t, Functor (Wrap t)) =>
                              Translation t AST.Import where
   translate t (AST.Import safe qualified package name alias detail) =
      AST.Import safe qualified package (translateModuleName t name) (translateModuleName t <$> alias) detail

instance {-# overlappable #-}
   (NameTranslation t, WrapTranslation t, Functor (Wrap t),
    Abstract.SupportFor 'Extensions.ExplicitNamespaces (Origin t)
    ~ Abstract.SupportFor 'Extensions.ExplicitNamespaces (Target t),
    Abstract.SupportFor 'Extensions.ImplicitParameters (Origin t)
    ~ Abstract.SupportFor 'Extensions.ImplicitParameters (Target t),
    Abstract.SupportFor 'Extensions.QualifiedDo (Origin t)
    ~ Abstract.SupportFor 'Extensions.QualifiedDo (Target t),
    Abstract.SupportFor 'Extensions.RecursiveDo (Origin t)
    ~ Abstract.SupportFor 'Extensions.RecursiveDo (Target t),
    Abstract.SupportFor 'Extensions.RecordWildCards (Origin t)
    ~ Abstract.SupportFor 'Extensions.RecordWildCards (Target t),
    Abstract.SupportFor 'Extensions.UnboxedSums (Origin t)
    ~ Abstract.SupportFor 'Extensions.UnboxedSums (Target t),
    Abstract.SupportFor 'Extensions.UnboxedTuples (Origin t)
    ~ Abstract.SupportFor 'Extensions.UnboxedTuples (Target t)) =>
   Translation t AST.Expression where
   translate _ (AST.ApplyExpression f x) = AST.ApplyExpression f x
   translate _ (AST.ConditionalExpression cond true false) = AST.ConditionalExpression cond true false
   translate _ (AST.ConstructorExpression con) = AST.ConstructorExpression con
   translate _ (AST.CaseExpression scrutinee alts) = AST.CaseExpression scrutinee alts
   translate _ (AST.LambdaCaseExpression alts) = AST.LambdaCaseExpression alts
   translate _ (AST.MultiWayIfExpression alts) = AST.MultiWayIfExpression alts
   translate _ (AST.DoExpression statements) = AST.DoExpression statements
   translate _ (AST.MDoExpression statements) = AST.MDoExpression statements
   translate t (AST.QualifiedDoExpression sup m statements) =
      AST.QualifiedDoExpression sup (translateModuleName t m) statements
   translate t (AST.MDoQualifiedExpression sup1 sup2 m statements) =
      AST.MDoQualifiedExpression sup1 sup2 (translateModuleName t m) statements
   translate _ (AST.InfixExpression left op right) = AST.InfixExpression left op right
   translate t (AST.LeftSectionExpression left op) = AST.LeftSectionExpression left (translateQualifiedName t op)
   translate _ (AST.LambdaExpression pat body) = AST.LambdaExpression pat body
   translate _ (AST.LetExpression bindings body) = AST.LetExpression bindings body
   translate _ (AST.ListComprehension element guards) = AST.ListComprehension element guards
   translate _ (AST.ParallelListComprehension element guards1 guards2 guardses) =
      AST.ParallelListComprehension element guards1 guards2 guardses
   translate _ (AST.ListExpression items) = AST.ListExpression items
   translate _ (AST.LiteralExpression value) = AST.LiteralExpression value
   translate _ AST.Negate = AST.Negate
   translate _ (AST.RecordExpression record fields) = AST.RecordExpression record fields
   translate t (AST.ReferenceExpression name) = AST.ReferenceExpression (translateQualifiedName t name)
   translate t (AST.RightSectionExpression op right) = AST.RightSectionExpression (translateQualifiedName t op) right
   translate _ (AST.SequenceExpression start next end) = AST.SequenceExpression start next end
   translate _ (AST.TupleExpression items) = AST.TupleExpression items
   translate _ (AST.TupleSectionExpression items) = AST.TupleSectionExpression items
   translate _ (AST.UnboxedTupleExpression support items) = AST.UnboxedTupleExpression support items
   translate _ (AST.UnboxedTupleSectionExpression support items) = AST.UnboxedTupleSectionExpression support items
   translate _ (AST.UnboxedSumExpression support before branch after) =
      AST.UnboxedSumExpression support before branch after
   translate _ (AST.TypedExpression x signature) = AST.TypedExpression x signature
   translate _ (AST.VisibleTypeApplication x ty) = AST.VisibleTypeApplication x ty
   translate _ (AST.ExplicitTypeExpression sup ty) = AST.ExplicitTypeExpression sup ty
   translate _ (AST.OverloadedLabel l) = AST.OverloadedLabel l
   translate t (AST.ImplicitParameterExpression support name) =
      AST.ImplicitParameterExpression support (translateName t name)
   translate t (AST.GetField record name) = AST.GetField record (translateName t name)
   translate t (AST.FieldProjection fields) = AST.FieldProjection (translateName t <$> fields)
   translate t (AST.WildcardRecordExpression sup con fields) =
      AST.WildcardRecordExpression sup (translateQualifiedName t con) fields

instance {-# overlappable #-}
   (NameTranslation t, WrapTranslation t, Functor (Wrap t),
    Abstract.Name (Origin t) ~ AST.Name (Origin t),
    Abstract.Name (Target t) ~ AST.Name (Target t),
    Abstract.SupportFor 'Extensions.ExtendedLiterals (Origin t)
    ~ Abstract.SupportFor 'Extensions.ExtendedLiterals (Target t),
    Abstract.SupportFor 'Extensions.MagicHash (Origin t) ~ Abstract.SupportFor 'Extensions.MagicHash (Target t)) =>
   Translation t AST.Value where
   translate _ (AST.CharLiteral l)     = AST.CharLiteral l
   translate _ (AST.FloatingLiteral l) = AST.FloatingLiteral l
   translate _ (AST.IntegerLiteral l)  = AST.IntegerLiteral l
   translate _ (AST.StringLiteral l)   = AST.StringLiteral l
   translate t (AST.HashLiteral sup l) = AST.HashLiteral sup (translate t l)
   translate t (AST.ExtendedLiteral sup n ty) = AST.ExtendedLiteral sup n (translateName t ty)
