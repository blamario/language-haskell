{-# Language DataKinds, DeriveDataTypeable, FlexibleContexts, FlexibleInstances,
             MultiParamTypeClasses, OverloadedStrings, StandaloneDeriving,
             TemplateHaskell, TypeFamilies, TypeOperators, UndecidableInstances #-}

-- | The types of nodes forming the Abstract Syntax Tree of the Haskell language with GHC extensions
module Language.Haskell.Extensions.AST (Language(Language),
                                        Export(..), Import(..), ImportItem(..), Members(..), ModuleMember(..),
                                        Declaration(..), DataConstructor(..), GADTConstructor(..),
                                        FunctionalDependency(..), PatternEquationClause(..),
                                        DerivingClause(..), DerivingStrategy(..),
                                        Expression(..), Pattern(..), PatternLHS(..), PatternEquationLHS(..),
                                        FieldBinding(..), FieldPattern(..),
                                        Statement(..), LambdaCasesAlternative(..),
                                        ClassInstanceLHS(..), Context(..),
                                        Type(..), TypeLHS(..), TypeVarBinding(..), TypeRole(..),
                                        Constructor(..), Value(..),
                                        CallSafety(..), CallingConvention(..),
                                        module Report) where

import Control.Monad (forM)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Data (Data, Typeable)
import qualified Data.Kind as Kind
import Data.Text (Text)

import qualified Language.Haskell.Extensions as Extensions
import qualified Language.Haskell.Extensions.Abstract as Abstract
import qualified Language.Haskell.AST as Report
import Language.Haskell.AST (Module(..), EquationLHS(..), EquationRHS(..), GuardedExpression(..),
                             FieldDeclaration(..), CaseAlternative(..),
                             Associativity(..),
                             Name(..), ModuleName(..), QualifiedName(..),
                             ImportSpecification(..))
import qualified Rank2.TH
import qualified Transformation.Deep as Deep
import qualified Transformation.Deep.TH
import qualified Transformation.Shallow.TH

-- | The extended Haskell language node parameter type
data Language = Language deriving (Data, Eq, Show)

type instance Abstract.ExtensionsSupportedBy Language = '[
   'Extensions.ExplicitNamespaces,
   'Extensions.ExtendedLiterals,
   'Extensions.MagicHash,
   'Extensions.NamedFieldPuns,
   'Extensions.ParallelListComprehensions,
   'Extensions.RecordWildCards,
   'Extensions.RecursiveDo,
   'Extensions.QualifiedDo,
   'Extensions.LambdaCase,
   'Extensions.ImplicitParameters,
   'Extensions.TupleSections,
   'Extensions.UnboxedSums,
   'Extensions.UnboxedTuples,
   'Extensions.InterruptibleFFI,
   'Extensions.CApiFFI,
   'Extensions.StrictData,
   'Extensions.Strict,
   'Extensions.BangPatterns,
   'Extensions.OrPatterns,
   'Extensions.ViewPatterns,
   'Extensions.NPlusKPatterns,
   'Extensions.PatternSynonyms,
   'Extensions.NamedDefaults,
   'Extensions.StandaloneDeriving,
   'Extensions.DerivingStrategies,
   'Extensions.DerivingVia,
   'Extensions.DefaultSignatures,
   'Extensions.GADTs,
   'Extensions.RoleAnnotations,
   'Extensions.TypeAbstractions,
   'Extensions.TypeData,
   'Extensions.DataKinds,
   'Extensions.FunctionalDependencies]

instance Abstract.ExtendedWith '[ 'Extensions.ExplicitNamespaces ] Language where
   build = Abstract.ExplicitNamespacesConstruction {
      Abstract.explicitlyNamespacedMemberList = ExplicitlyNamespacedMemberList (),
      Abstract.defaultMember = DefaultMember,
      Abstract.patternMember = PatternMember,
      Abstract.typeMember = TypeMember,
      Abstract.explicitTypeFixityDeclaration = ExplicitTypeFixityDeclaration (),
      Abstract.explicitDataFixityDeclaration = ExplicitDataFixityDeclaration (),
      Abstract.explicitTypeExpression = ExplicitTypeExpression (),
      Abstract.explicitTypePattern = ExplicitTypePattern ()}

instance Abstract.ExtendedWith '[ 'Extensions.MagicHash ] Language where
   build = Abstract.MagicHashConstruction {
      Abstract.hashLiteral' = HashLiteral ()}

instance Abstract.ExtendedWith '[ 'Extensions.NamedFieldPuns ] Language where
   build = Abstract.NamedFieldPunsConstruction {
      Abstract.punnedFieldBinding = PunnedFieldBinding (),
      Abstract.punnedFieldPattern = PunnedFieldPattern ()}

instance Abstract.ExtendedWith '[ 'Extensions.ParallelListComprehensions ] Language where
   build = Abstract.ParallelListComprehensionConstruction {
      Abstract.parallelListComprehension = ParallelListComprehension ()}

instance Abstract.ExtendedWith '[ 'Extensions.RecordWildCards ] Language where
   build = Abstract.RecordWildCardConstruction {
      Abstract.wildcardRecordExpression = WildcardRecordExpression (),
      Abstract.wildcardRecordPattern = WildcardRecordPattern ()}

instance Abstract.ExtendedWith '[ 'Extensions.RecursiveDo ] Language where
   build = Abstract.RecursiveDoConstruction {
      Abstract.mdoExpression = MDoExpression,
      Abstract.recursiveStatement = RecursiveStatement}

instance Abstract.ExtendedWith '[ 'Extensions.QualifiedDo ] Language where
   build = Abstract.QualifiedDoConstruction {
      Abstract.qualifiedDoExpression = QualifiedDoExpression ()}

instance Abstract.ExtendedWith '[ 'Extensions.QualifiedDo, 'Extensions.RecursiveDo ] Language where
   build = Abstract.QualifiedRecursiveDoConstruction {
      Abstract.mdoQualifiedExpression = MDoQualifiedExpression () ()}

instance Abstract.ExtendedWith '[ 'Extensions.LambdaCase ] Language where
   build = Abstract.LambdaCaseConstruction {
      Abstract.lambdaCaseExpression = LambdaCaseExpression (),
      Abstract.lambdaCasesExpression = LambdaCasesExpression (),
      Abstract.lambdaCasesAlternative = LambdaCasesAlternative ()}

instance Abstract.ExtendedWith '[ 'Extensions.TupleSections ] Language where
   build = Abstract.TupleSectionConstruction {
      Abstract.tupleSectionExpression = TupleSectionExpression ()}

instance Abstract.ExtendedWith '[ 'Extensions.UnboxedTuples ] Language where
   build = Abstract.UnboxedTuplesConstruction {
      Abstract.unboxedTupleType = UnboxedTupleType (),
      Abstract.unboxedTupleConstructor = UnboxedTupleConstructor (),
      Abstract.unboxedTupleExpression = UnboxedTupleExpression (),
      Abstract.unboxedTupleSectionExpression = UnboxedTupleSectionExpression (),
      Abstract.unboxedTuplePattern = UnboxedTuplePattern ()}

instance Abstract.ExtendedWith '[ 'Extensions.UnboxedSums ] Language where
   build = Abstract.UnboxedSumsConstruction {
      Abstract.unboxedSumType = UnboxedSumType (),
      Abstract.unboxedSumConstructor = UnboxedSumConstructor (),
      Abstract.unboxedSumExpression = UnboxedSumExpression (),
      Abstract.unboxedSumPattern = UnboxedSumPattern ()}

instance Abstract.ExtendedWith '[ 'Extensions.ExtendedLiterals ] Language where
   build = Abstract.ExtendedLiteralsConstruction {
      Abstract.extendedLiteral = ExtendedLiteral ()}

instance Abstract.ExtendedWith '[ 'Extensions.InterruptibleFFI ] Language where
   build = Abstract.InterruptibleFFIConstruction {
      Abstract.interruptibleCall = InterruptibleCall ()}

instance Abstract.ExtendedWith '[ 'Extensions.CApiFFI ] Language where
   build = Abstract.CApiFFIConstruction {
      Abstract.cApiCall = CApiCall ()}

instance Abstract.ExtendedWith '[ 'Extensions.ImplicitParameters ] Language where
   build = Abstract.ImplicitParametersConstruction {
      Abstract.implicitParameterConstraint = ImplicitParameterConstraint (),
      Abstract.implicitParameterDeclaration = ImplicitParameterDeclaration (),
      Abstract.implicitParameterExpression = ImplicitParameterExpression ()}

instance Abstract.ExtendedWith '[ 'Extensions.StrictData ] Language where
   build = Abstract.StrictDataConstruction {
      Abstract.lazyType = LazyType ()}

instance Abstract.ExtendedWith '[ 'Extensions.Strict ] Language where
   build = Abstract.StrictConstruction {
      Abstract.lazyPattern = LazyPattern ()}

instance Abstract.ExtendedWith '[ 'Extensions.BangPatterns ] Language where
   build = Abstract.BangPatternConstruction {
      Abstract.bangPattern = BangPattern ()}

instance Abstract.ExtendedWith '[ 'Extensions.OrPatterns ] Language where
   build = Abstract.OrPatternConstruction {
      Abstract.orPattern = OrPattern ()}

instance Abstract.ExtendedWith '[ 'Extensions.ViewPatterns ] Language where
   build = Abstract.ViewPatternConstruction {
      Abstract.viewPattern = ViewPattern ()}

instance Abstract.ExtendedWith '[ 'Extensions.NPlusKPatterns ] Language where
   build = Abstract.NPlusKPatternConstruction {
      Abstract.nPlusKPattern = NPlusKPattern ()}

instance Abstract.ExtendedWith '[ 'Extensions.PatternSynonyms ] Language where
   build = Abstract.PatternSynonymConstruction {
      Abstract.exportPattern = ExportPattern,
      Abstract.importPattern = ImportPattern,
      Abstract.allMembersPlus = AllMembersPlus,
      Abstract.prefixPatternLHS = PrefixPatternLHS,
      Abstract.infixPatternLHS = InfixPatternLHS,
      Abstract.recordPatternLHS = RecordPatternLHS,
      Abstract.prefixPatternEquationLHS = PrefixPatternEquationLHS,
      Abstract.infixPatternEquationLHS = InfixPatternEquationLHS,
      Abstract.patternEquationClause = PatternEquationClause (),
      Abstract.implicitPatternSynonym = ImplicitPatternSynonym (),
      Abstract.unidirectionalPatternSynonym = UnidirectionalPatternSynonym (),
      Abstract.explicitPatternSynonym = ExplicitPatternSynonym (),
      Abstract.patternSynonymSignature = PatternSynonymSignature ()}

instance Abstract.ExtendedWith '[ 'Extensions.NamedDefaults ] Language where
   build = Abstract.NamedDefaultsConstruction {
      Abstract.namedDefaultDeclaration = NamedDefaultDeclaration ()}

instance Abstract.ExtendedWith '[ 'Extensions.StandaloneDeriving ] Language where
   build = Abstract.StandaloneDerivingConstruction {
      Abstract.standaloneDerivingDeclaration = StandaloneDerivingDeclaration ()}

instance Abstract.ExtendedWith '[ 'Extensions.DerivingStrategies ] Language where
   build = Abstract.DerivingStrategiesConstruction {
      Abstract.defaultStrategy = Default,
      Abstract.stockStrategy = Stock,
      Abstract.newtypeStrategy = Newtype,
      Abstract.anyClassStrategy = AnyClass,
      Abstract.strategicDerive = StrategicDerive (),
      Abstract.standaloneStrategicDerivingDeclaration = StandaloneStrategicDerivingDeclaration () ()}

instance Abstract.ExtendedWith '[ 'Extensions.DerivingVia ] Language where
   build = Abstract.DerivingViaConstruction {
      Abstract.derivingViaStrategy = Via (),
      Abstract.deriveVia = DeriveVia ()}

instance Abstract.ExtendedWith '[ 'Extensions.DefaultSignatures ] Language where
   build = Abstract.DefaultSignatureConstruction {
      Abstract.defaultMethodSignature = DefaultMethodSignature ()}

instance Abstract.ExtendedWith '[ 'Extensions.DataKinds ] Language where
   build = Abstract.DataKindsConstruction {
      Abstract.promotedConstructorType = PromotedConstructorType (),
      Abstract.promotedTupleType = PromotedTupleType (),
      Abstract.promotedListType = PromotedListType (),
      Abstract.promotedIntegerLiteral = PromotedIntegerLiteral (),
      Abstract.promotedCharLiteral = PromotedCharLiteral (),
      Abstract.promotedStringLiteral = PromotedStringLiteral (),
      Abstract.promotedInfixTypeApplication = PromotedInfixTypeApplication ()}

instance Abstract.ExtendedWith '[ 'Extensions.TypeData ] Language where
   build = Abstract.TypeDataConstruction {
      Abstract.typeDataDeclaration = TypeDataDeclaration ()}

instance Abstract.ExtendedWith '[ 'Extensions.GADTs, 'Extensions.TypeData ] Language where
   build = Abstract.TypeGADTConstruction {
      Abstract.typeGADTDeclaration = TypeGADTDeclaration () ()}

instance Abstract.ExtendedWith '[ 'Extensions.TypeAbstractions ] Language where
   build = Abstract.TypeAbstractionConstruction {
      Abstract.typeLHSTypeApplication = TypeLHSTypeApplication (),
      Abstract.invisibleTypePattern = InvisibleTypePattern ()}

instance Abstract.ExtendedWith '[ 'Extensions.FunctionalDependencies ] Language where
   build = Abstract.FunctionalDependenciesConstruction {
      Abstract.functionalDependency = FunctionalDependency,
      Abstract.fundepClassDeclaration = FunDepClassDeclaration ()}

instance Abstract.ExtendedWith '[ 'Extensions.RoleAnnotations ] Language where
   build = Abstract.RoleAnnotationsConstruction {
      Abstract.typeRoleDeclaration = TypeRoleDeclaration (),
      Abstract.inferredRole = InferredRole (),
      Abstract.nominalRole = NominalRole (),
      Abstract.representationalRole = RepresentationalRole (),
      Abstract.phantomRole = PhantomRole ()}

type instance Abstract.FunctionalDependency Language = FunctionalDependency Language
type instance Abstract.LambdaCasesAlternative Language = LambdaCasesAlternative Language
type instance Abstract.DerivingStrategy Language = DerivingStrategy Language
type instance Abstract.PatternLHS Language = PatternLHS Language
type instance Abstract.PatternEquationLHS Language = PatternEquationLHS Language
type instance Abstract.PatternEquationClause Language = PatternEquationClause Language
type instance Abstract.TypeRole Language = TypeRole Language

instance Abstract.ExtendedHaskell Language where
   type GADTConstructor Language = GADTConstructor Language
   type Kind Language = Type Language
   type TypeVarBinding Language = TypeVarBinding Language
   type ModuleMember Language = ModuleMember Language
   multiWayIfExpression = MultiWayIfExpression
   safeImportDeclaration q = Import True q Nothing 
   packageQualifiedImportDeclaration q p = Import False q (Just p)
   safePackageQualifiedImportDeclaration q p = Import True q (Just p)
   infixTypeApplication = InfixTypeApplication
   simpleKindedTypeLHS = SimpleTypeLHS
   infixTypeLHSApplication left op right = SimpleTypeLHS op [left, right]
   typeLHSApplication = TypeLHSApplication
   visibleDependentType = VisibleDependentType
   existentialConstructor = ExistentialConstructor
   explicitlyScopedInstanceDeclaration = InstanceDeclaration
   forallType = ForallType
   kindedType = KindedType
   constrainedType = ConstrainedType
   constraintType = ConstraintType
   typeKind = TypeKind
   typeWildcard = TypeWildcard
   groundType = GroundTypeKind
   typeEquality = TypeEquality

   kindedDataDeclaration context lhs = DataDeclaration context lhs . Just
   kindedNewtypeDeclaration context lhs = NewtypeDeclaration context lhs . Just
   gadtDeclaration = GADTDeclaration
   gadtNewtypeDeclaration = GADTNewtypeDeclaration
   gadtConstructors = GADTConstructors
   recordFunctionType = RecordFunctionType
   linearFunctionType = LinearFunctionType
   multiplicityFunctionType = MultiplicityFunctionType

   explicitlyKindedTypeVariable = ExplicitlyKindedTypeVariable False
   implicitlyKindedTypeVariable = ImplicitlyKindedTypeVariable False
   inferredTypeVariable = ImplicitlyKindedTypeVariable True
   inferredExplicitlyKindedTypeVariable = ExplicitlyKindedTypeVariable True
   wildcardTypeBinding = WildcardTypeBinding
   explicitlyKindedWildcardTypeBinding = ExplicitlyKindedWildcardTypeBinding
   
   groundTypeKind = GroundTypeKind
   typeConstraint = TypeConstraint

   dataFamilyDeclaration = DataFamilyDeclaration
   openTypeFamilyDeclaration = OpenTypeFamilyDeclaration
   closedTypeFamilyDeclaration = ClosedTypeFamilyDeclaration
   injectiveOpenTypeFamilyDeclaration = InjectiveOpenTypeFamilyDeclaration
   injectiveClosedTypeFamilyDeclaration = InjectiveClosedTypeFamilyDeclaration
   dataFamilyInstance = DataFamilyInstance
   newtypeFamilyInstance = NewtypeFamilyInstance
   gadtDataFamilyInstance = GADTDataFamilyInstance
   gadtNewtypeFamilyInstance = GADTNewtypeFamilyInstance
   typeFamilyInstance = TypeFamilyInstance
   classReferenceInstanceLHS = ClassReferenceInstanceLHS
   infixTypeClassInstanceLHS = InfixTypeClassInstanceLHS
   classInstanceLHSApplication = ClassInstanceLHSApplication
   classInstanceLHSKindApplication = ClassInstanceLHSKindApplication
   kindSignature = KindSignature

   visibleTypeApplication = VisibleTypeApplication
   visibleKindApplication = VisibleKindApplication
   typedPattern = TypedPattern
   constructorPatternWithTypeApplications = ConstructorPattern

   overloadedLabel = OverloadedLabel
   getField = GetField
   fieldProjection = FieldProjection

instance Abstract.Haskell Language where
   type Module Language = Module Language
   type Declaration Language = Declaration Language
   type Expression Language = Expression Language
   type Type Language = Type Language

   type EquationLHS Language = EquationLHS Language
   type EquationRHS Language = EquationRHS Language
   type GuardedExpression Language = GuardedExpression Language
   type Pattern Language = Pattern Language
   type Statement Language = Statement Language
   type ClassInstanceLHS Language = ClassInstanceLHS Language
   type TypeLHS Language = TypeLHS Language

   type Import Language = Import Language
   type ImportSpecification Language = ImportSpecification Language
   type ImportItem Language = ImportItem Language
   type Export Language = Export Language

   type Context Language = Context Language
   type DataConstructor Language = DataConstructor Language
   type DerivingClause Language = DerivingClause Language
   type FieldDeclaration Language = FieldDeclaration Language
   type FieldBinding Language = FieldBinding Language
   type FieldPattern Language = FieldPattern Language
   type CaseAlternative Language = CaseAlternative Language

   type Constructor Language = Constructor Language
   type Value Language = Value Language

   type CallingConvention Language = CallingConvention Language
   type CallSafety Language = CallSafety Language
   type Associativity Language = Associativity Language
   type Members Language = Members Language
   type Name Language = Name Language
   type ModuleName Language = ModuleName Language
   type QualifiedName Language = QualifiedName Language

   anonymousModule = AnonymousModule
   namedModule = NamedModule
   withLanguagePragma = ExtendedModule

   exportClassOrType = ExportClassOrType
   exportVar = ExportVar
   reExportModule = ReExportModule

   importDeclaration q = Import False q Nothing
   excludedImports = ImportSpecification False
   includedImports = ImportSpecification True
   importClassOrType = ImportClassOrType
   importVar = ImportVar

   allMembers = AllMembers
   memberList = MemberList

   classDeclaration = ClassDeclaration
   dataDeclaration context lhs = DataDeclaration context lhs Nothing
   defaultDeclaration = DefaultDeclaration
   equationDeclaration = EquationDeclaration
   fixityDeclaration = FixityDeclaration
   foreignExport = ForeignExport
   foreignImport = ForeignImport
   instanceDeclaration = InstanceDeclaration []
   newtypeDeclaration context lhs = NewtypeDeclaration context lhs Nothing
   typeSynonymDeclaration = TypeSynonymDeclaration
   typeSignature = TypeSignature

   applyExpression = ApplyExpression
   conditionalExpression = ConditionalExpression
   constructorExpression = ConstructorExpression
   caseExpression = CaseExpression
   doExpression = DoExpression
   infixExpression = InfixExpression
   leftSectionExpression = LeftSectionExpression
   lambdaExpression = LambdaExpression
   letExpression = LetExpression
   listComprehension = ListComprehension
   listExpression = ListExpression
   literalExpression = LiteralExpression
   negate = Negate
   recordExpression = RecordExpression
   referenceExpression = ReferenceExpression
   rightSectionExpression = RightSectionExpression
   sequenceExpression = SequenceExpression
   tupleExpression = TupleExpression
   typedExpression = TypedExpression

   asPattern = AsPattern
   constructorPattern = flip ConstructorPattern []
   infixPattern = InfixPattern
   irrefutablePattern = IrrefutablePattern
   listPattern = ListPattern
   literalPattern = LiteralPattern
   recordPattern = RecordPattern
   tuplePattern = TuplePattern
   variablePattern = VariablePattern
   wildcardPattern = WildcardPattern

   constructorType = ConstructorType
   functionConstructorType = FunctionConstructorType
   functionType = FunctionType
   listType = ListType
   strictType = StrictType
   tupleType = TupleType
   typeApplication = TypeApplication
   typeVariable = TypeVariable

   constructorReference = ConstructorReference
   emptyListConstructor = EmptyListConstructor
   tupleConstructor = TupleConstructor
   unitConstructor = UnitConstructor

   constructor = Constructor
   recordConstructor = RecordConstructor
   constructorFields = ConstructorFields

   fieldBinding = FieldBinding
   fieldPattern = FieldPattern

   simpleDerive = SimpleDerive

   typeClassInstanceLHS = TypeClassInstanceLHS
   simpleTypeLHS con = SimpleTypeLHS con . map Abstract.implicitlyKindedTypeVariable

   prefixLHS = PrefixLHS
   infixLHS = InfixLHS
   patternLHS = PatternLHS
   variableLHS = VariableLHS

   caseAlternative = CaseAlternative

   guardedRHS = GuardedRHS
   normalRHS = NormalRHS

   guardedExpression = GuardedExpression

   classConstraint cls t = ClassConstraint cls t
   constraints = Constraints
   noContext = NoContext

   bindStatement = BindStatement
   expressionStatement = ExpressionStatement
   letStatement = LetStatement

   charLiteral = CharLiteral
   floatingLiteral = FloatingLiteral
   integerLiteral = IntegerLiteral
   stringLiteral = StringLiteral

   name = Name
   moduleName = ModuleName
   qualifiedName = QualifiedName

   nonAssociative = NonAssociative
   leftAssociative = LeftAssociative
   rightAssociative = RightAssociative

   cCall = CCall
   cppCall = CppCall
   dotNetCall = DotNetCall
   jvmCall = JvmCall
   stdCall = StdCall

   safeCall = SafeCall
   unsafeCall = UnsafeCall

data Export λ l (d :: Kind.Type -> Kind.Type) (s :: Kind.Type -> Kind.Type) =
   ExportClassOrType (Abstract.QualifiedName λ) (Maybe (Abstract.Members λ))
   | ExportVar (Abstract.QualifiedName λ)
   | ExportPattern (Abstract.QualifiedName λ)
   | ReExportModule (Abstract.ModuleName λ)

data Import λ l d s = Import Bool Bool (Maybe Text) (Abstract.ModuleName λ) (Maybe (Abstract.ModuleName λ))
                             (Maybe (s (Abstract.ImportSpecification l l d d)))

data ImportItem λ l (d :: Kind.Type -> Kind.Type) (s :: Kind.Type -> Kind.Type) =
   ImportClassOrType (Abstract.Name λ) (Maybe (Abstract.Members λ))
   | ImportVar (Abstract.Name λ)
   | ImportPattern (Abstract.Name λ)

data Members λ = AllMembers
               | AllMembersPlus [Name λ]
               | MemberList [Name λ]
               | ExplicitlyNamespacedMemberList !(Abstract.SupportFor 'Extensions.ExplicitNamespaces λ)
                                                [ModuleMember λ]

data ModuleMember λ = DefaultMember (Name λ)
                    | PatternMember (Name λ)
                    | TypeMember (Name λ)
                    deriving (Data, Eq, Show)

data TypeRole λ = InferredRole !(Abstract.SupportFor 'Extensions.RoleAnnotations λ)
                | NominalRole !(Abstract.SupportFor 'Extensions.RoleAnnotations λ)
                | RepresentationalRole !(Abstract.SupportFor 'Extensions.RoleAnnotations λ)
                | PhantomRole !(Abstract.SupportFor 'Extensions.RoleAnnotations λ)

data Declaration λ l d s =
   ClassDeclaration (s (Abstract.Context l l d d)) (s (Abstract.TypeLHS l l d d)) [s (Abstract.Declaration l l d d)]
   | FunDepClassDeclaration !(Abstract.SupportFor 'Extensions.FunctionalDependencies λ)
                            (s (Abstract.Context l l d d)) (s (Abstract.TypeLHS l l d d))
                            [s (Abstract.FunctionalDependency l l d d)] [s (Abstract.Declaration l l d d)]
   | DataDeclaration (s (Abstract.Context l l d d)) (s (Abstract.TypeLHS l l d d)) (Maybe (s (Abstract.Kind l l d d)))
                     [s (Abstract.DataConstructor l l d d)] [s (Abstract.DerivingClause l l d d)]
   | GADTDeclaration (s (Abstract.TypeLHS l l d d)) (Maybe (s (Abstract.Kind l l d d)))
                     [s (Abstract.GADTConstructor l l d d)] [s (Abstract.DerivingClause l l d d)]
   | TypeDataDeclaration !(Abstract.SupportFor 'Extensions.TypeData λ)
                         (s (Abstract.TypeLHS l l d d)) (Maybe (s (Abstract.Kind l l d d)))
                         [s (Abstract.DataConstructor l l d d)]
   | TypeGADTDeclaration !(Abstract.SupportFor 'Extensions.GADTs λ)
                         !(Abstract.SupportFor 'Extensions.TypeData λ)
                         (s (Abstract.TypeLHS l l d d)) (Maybe (s (Abstract.Kind l l d d)))
                         [s (Abstract.GADTConstructor l l d d)]
   | DefaultDeclaration [s (Abstract.Type l l d d)]
   | NamedDefaultDeclaration !(Abstract.SupportFor 'Extensions.NamedDefaults λ)
                             (Abstract.QualifiedName λ) [s (Abstract.Type l l d d)]
   | DefaultMethodSignature !(Abstract.SupportFor 'Extensions.DefaultSignatures λ)
                             (Name λ) (s (Abstract.Context l l d d)) (s (Abstract.Type l l d d))
   | EquationDeclaration (s (Abstract.EquationLHS l l d d)) (s (Abstract.EquationRHS l l d d))
                         [s (Abstract.Declaration l l d d)]
   | FixityDeclaration (Associativity λ) (Maybe Int) (NonEmpty (Abstract.Name λ))
   | ExplicitTypeFixityDeclaration !(Abstract.SupportFor 'Extensions.ExplicitNamespaces λ)
                                   (Associativity λ) (Maybe Int) (NonEmpty (Abstract.Name λ))
   | ExplicitDataFixityDeclaration !(Abstract.SupportFor 'Extensions.ExplicitNamespaces λ)
                                   (Associativity λ) (Maybe Int) (NonEmpty (Abstract.Name λ))
   | ForeignExport (CallingConvention λ) (Maybe Text) (Abstract.Name λ) (s (Abstract.Type l l d d))
   | ForeignImport (CallingConvention λ) (Maybe (CallSafety λ)) (Maybe Text) (Abstract.Name λ)
                   (s (Abstract.Type l l d d))
   | InstanceDeclaration [TypeVarBinding λ l d s] (s (Abstract.Context l l d d))
                         (s (Abstract.ClassInstanceLHS l l d d)) [s (Abstract.Declaration l l d d)]
   | StandaloneDerivingDeclaration !(Abstract.SupportFor 'Extensions.StandaloneDeriving λ)
                                   [TypeVarBinding λ l d s]
                                   (s (Abstract.Context l l d d))
                                   (s (Abstract.ClassInstanceLHS l l d d))
   | StandaloneStrategicDerivingDeclaration !(Abstract.SupportFor 'Extensions.StandaloneDeriving λ)
                                            !(Abstract.SupportFor 'Extensions.DerivingStrategies λ)
                                            (s (Abstract.DerivingStrategy l l d d))
                                            [TypeVarBinding λ l d s]
                                            (s (Abstract.Context l l d d))
                                            (s (Abstract.ClassInstanceLHS l l d d))
   | NewtypeDeclaration (s (Abstract.Context l l d d)) (s (Abstract.TypeLHS l l d d))
                        (Maybe (s (Abstract.Kind l l d d))) (s (Abstract.DataConstructor l l d d))
                        [s (Abstract.DerivingClause l l d d)]
   | GADTNewtypeDeclaration (s (Abstract.TypeLHS l l d d)) (Maybe (s (Abstract.Kind l l d d)))
                            (s (Abstract.GADTConstructor l l d d)) [s (Abstract.DerivingClause l l d d)]
   | TypeSynonymDeclaration (s (Abstract.TypeLHS l l d d)) (s (Abstract.Type l l d d))
   | TypeSignature (NonEmpty (Abstract.Name λ)) (s (Abstract.Context l l d d)) (s (Abstract.Type l l d d))
   | DataFamilyDeclaration (s (Abstract.TypeLHS l l d d)) (Maybe (s (Abstract.Kind l l d d)))
   | OpenTypeFamilyDeclaration (s (Abstract.TypeLHS l l d d)) (Maybe (s (Abstract.Kind l l d d)))
   | ClosedTypeFamilyDeclaration (s (Abstract.TypeLHS l l d d)) (Maybe (s (Abstract.Kind l l d d)))
                                 [s (Abstract.Declaration l l d d)]
   | InjectiveOpenTypeFamilyDeclaration (s (Abstract.TypeLHS l l d d)) (TypeVarBinding λ l d s)
                                        (Maybe (Abstract.Name λ, NonEmpty (Abstract.Name λ)))
   | InjectiveClosedTypeFamilyDeclaration (s (Abstract.TypeLHS l l d d)) (TypeVarBinding λ l d s)
                                          (Maybe (Abstract.Name λ, NonEmpty (Abstract.Name λ)))
                                          [s (Abstract.Declaration l l d d)]
   | DataFamilyInstance [TypeVarBinding λ l d s] (s (Abstract.Context l l d d))
                        (s (Abstract.ClassInstanceLHS l l d d)) (Maybe (s (Abstract.Kind l l d d)))
                        [s (Abstract.DataConstructor l l d d)]
                        [s (Abstract.DerivingClause l l d d)]
   | NewtypeFamilyInstance [TypeVarBinding λ l d s] (s (Abstract.Context l l d d))
                           (s (Abstract.ClassInstanceLHS l l d d)) (Maybe (s (Abstract.Kind l l d d)))
                           (s (Abstract.DataConstructor l l d d)) [s (Abstract.DerivingClause l l d d)]
   | GADTDataFamilyInstance [TypeVarBinding λ l d s] (s (Abstract.ClassInstanceLHS l l d d))
                            (Maybe (s (Abstract.Kind l l d d)))
                            [s (Abstract.GADTConstructor l l d d)] [s (Abstract.DerivingClause l l d d)]
   | GADTNewtypeFamilyInstance [TypeVarBinding λ l d s] (s (Abstract.ClassInstanceLHS l l d d))
                               (Maybe (s (Abstract.Kind l l d d)))
                               (s (Abstract.GADTConstructor l l d d)) [s (Abstract.DerivingClause l l d d)]
   | TypeFamilyInstance [TypeVarBinding λ l d s] (s (Abstract.ClassInstanceLHS l l d d))
                        (s (Abstract.Type l l d d))
   | KindSignature (Abstract.Name λ) (s (Abstract.Kind l l d d))
   | TypeRoleDeclaration !(Abstract.SupportFor 'Extensions.RoleAnnotations λ)
                         (Abstract.QualifiedName λ) [Abstract.TypeRole λ]
   | ImplicitPatternSynonym !(Abstract.SupportFor 'Extensions.PatternSynonyms λ)
                            (s (Abstract.PatternLHS l l d d)) (s (Abstract.Pattern l l d d))
   | UnidirectionalPatternSynonym !(Abstract.SupportFor 'Extensions.PatternSynonyms λ)
                                  (s (Abstract.PatternLHS l l d d)) (s (Abstract.Pattern l l d d))
   | ExplicitPatternSynonym !(Abstract.SupportFor 'Extensions.PatternSynonyms λ)
                            (s (Abstract.PatternLHS l l d d)) (s (Abstract.Pattern l l d d))
                            [s (Abstract.PatternEquationClause l l d d)]
   | PatternSynonymSignature !(Abstract.SupportFor 'Extensions.PatternSynonyms λ)
                             (NonEmpty (Abstract.Name λ))
                             [TypeVarBinding λ l d s] (s (Abstract.Context l l d d))
                             [TypeVarBinding λ l d s] (s (Abstract.Context l l d d))
                             [s (Abstract.Type l l d d)]
                             (s (Abstract.Type l l d d))
   | ImplicitParameterDeclaration !(Abstract.SupportFor 'Extensions.ImplicitParameters λ)
                                  (Abstract.Name λ)
                                  (s (Abstract.Expression l l d d))

data PatternEquationClause λ l d s =
   PatternEquationClause !(Abstract.SupportFor 'Extensions.PatternSynonyms λ)
                         (s (Abstract.PatternEquationLHS l l d d))
                         (s (Abstract.EquationRHS l l d d))
                         [s (Abstract.Declaration l l d d)]

data PatternEquationLHS λ l d s =
   PrefixPatternEquationLHS (Abstract.Name λ) [s (Abstract.Pattern l l d d)]
   | InfixPatternEquationLHS (s (Abstract.Pattern l l d d)) (Abstract.Name λ) (s (Abstract.Pattern l l d d))

data FunctionalDependency λ l (d :: Kind.Type -> Kind.Type) (s :: Kind.Type -> Kind.Type) =
   FunctionalDependency [Abstract.Name λ] [Abstract.Name λ]

data GADTConstructor λ l d s =
   GADTConstructors (NonEmpty (Abstract.Name λ)) [TypeVarBinding λ l d s]
                    (s (Abstract.Context l l d d)) (s (Abstract.Type l l d d))

data DataConstructor λ l d s =
   Constructor (Abstract.Name λ) [s (Abstract.Type l l d d)]
   | RecordConstructor (Abstract.Name λ) [s (Abstract.FieldDeclaration l l d d)]
   | ExistentialConstructor [TypeVarBinding λ l d s] (s (Abstract.Context l l d d)) (s (Abstract.DataConstructor l l d d))

data DerivingClause λ l (d :: Kind.Type -> Kind.Type) (s :: Kind.Type -> Kind.Type) =
   SimpleDerive (Abstract.QualifiedName λ)
   | StrategicDerive !(Abstract.SupportFor 'Extensions.DerivingStrategies λ)
                     (s (Abstract.DerivingStrategy l l d d)) [s (Abstract.Type l l d d)]
   | DeriveVia !(Abstract.SupportFor 'Extensions.DerivingVia λ)
               [s (Abstract.Type l l d d)] (s (Abstract.Type l l d d))

data DerivingStrategy λ l (d :: Kind.Type -> Kind.Type) (s :: Kind.Type -> Kind.Type) =
   Default | Stock | AnyClass | Newtype
   | Via !(Abstract.SupportFor 'Extensions.DerivingVia λ) (s (Abstract.Type l l d d))

data Context λ l d s =
   ClassConstraint (Abstract.QualifiedName λ) (s (Abstract.Type l l d d))
   | Constraints [s (Abstract.Context l l d d)]
   | TypeConstraint (s (Abstract.Type l l d d))
   | TypeEquality (s (Abstract.Type l l d d)) (s (Abstract.Type l l d d))
   | ImplicitParameterConstraint !(Abstract.SupportFor 'Extensions.ImplicitParameters λ)
                                 (Abstract.Name λ)
                                 (s (Abstract.Type l l d d))
   | NoContext

data Type λ l d s =
   ConstructorType (s (Abstract.Constructor l l d d))
   | FunctionConstructorType
   | FunctionType (s (Abstract.Type l l d d)) (s (Abstract.Type l l d d))
   | LinearFunctionType (s (Abstract.Type l l d d)) (s (Abstract.Type l l d d))
   | MultiplicityFunctionType (s (Abstract.Type l l d d)) (s (Abstract.Type l l d d)) (s (Abstract.Type l l d d))
   | RecordFunctionType [s (Abstract.FieldDeclaration l l d d)] (s (Abstract.Type l l d d))
   | ListType (s (Abstract.Type l l d d))
   | StrictType (s (Abstract.Type l l d d))
   | LazyType !(Abstract.SupportFor 'Extensions.StrictData λ) (s (Abstract.Type l l d d))
   | TupleType (NonEmpty (s (Abstract.Type l l d d)))
   | UnboxedTupleType !(Abstract.SupportFor 'Extensions.UnboxedTuples λ) (NonEmpty (s (Abstract.Type l l d d)))
   | UnboxedSumType !(Abstract.SupportFor 'Extensions.UnboxedSums λ) (NonEmpty (s (Abstract.Type l l d d)))
   | TypeApplication (s (Abstract.Type l l d d)) (s (Abstract.Type l l d d))
   | InfixTypeApplication (s (Abstract.Type l l d d)) (Abstract.QualifiedName λ) (s (Abstract.Type l l d d))
   | TypeVariable (Abstract.Name λ)
   | ForallType [TypeVarBinding λ l d s] (s (Abstract.Type l l d d))
   | ConstrainedType (s (Abstract.Context l l d d)) (s (Abstract.Type l l d d))
   | ConstraintType (s (Abstract.Context l l d d))
   | KindedType (s (Abstract.Type l l d d)) (s (Abstract.Kind l l d d))
   | TypeWildcard
   | TypeKind (s (Abstract.Type l l d d))
   | GroundTypeKind
   | VisibleDependentType [TypeVarBinding λ l d s] (s (Abstract.Type l l d d))
   | PromotedConstructorType !(Abstract.SupportFor 'Extensions.DataKinds λ) (s (Abstract.Constructor l l d d))
   | PromotedTupleType !(Abstract.SupportFor 'Extensions.DataKinds λ) [s (Abstract.Type l l d d)]
   | PromotedListType !(Abstract.SupportFor 'Extensions.DataKinds λ) [s (Abstract.Type l l d d)]
   | PromotedIntegerLiteral !(Abstract.SupportFor 'Extensions.DataKinds λ) Integer
   | PromotedCharLiteral !(Abstract.SupportFor 'Extensions.DataKinds λ) Char
   | PromotedStringLiteral !(Abstract.SupportFor 'Extensions.DataKinds λ) Text
   | PromotedInfixTypeApplication !(Abstract.SupportFor 'Extensions.DataKinds λ)
                                  (s (Abstract.Type l l d d)) (Abstract.QualifiedName λ) (s (Abstract.Type l l d d))
   | VisibleKindApplication (s (Abstract.Type l l d d)) (s (Abstract.Kind l l d d))

data TypeVarBinding λ l d s =
   ExplicitlyKindedTypeVariable Bool (Abstract.Name λ) (s (Abstract.Kind l l d d))
   | ImplicitlyKindedTypeVariable Bool (Abstract.Name λ)
   | WildcardTypeBinding
   | ExplicitlyKindedWildcardTypeBinding (s (Abstract.Kind l l d d))

data TypeLHS λ l d s =
   SimpleTypeLHS (Abstract.Name λ) [TypeVarBinding λ l d s]
   | TypeLHSApplication (s (Abstract.TypeLHS l l d d)) (TypeVarBinding λ l d s)
   | TypeLHSTypeApplication !(Abstract.SupportFor 'Extensions.TypeAbstractions λ)
                            (s (Abstract.TypeLHS l l d d))
                            (TypeVarBinding λ l d s)

data ClassInstanceLHS λ l d s =
   TypeClassInstanceLHS (Abstract.QualifiedName λ) (s (Abstract.Type l l d d))
   | ClassReferenceInstanceLHS (Abstract.QualifiedName λ)
   | InfixTypeClassInstanceLHS (s (Abstract.Type l l d d)) (Abstract.QualifiedName λ) (s (Abstract.Type l l d d))
   | ClassInstanceLHSApplication (s (Abstract.ClassInstanceLHS l l d d)) (s (Abstract.Type l l d d))
   | ClassInstanceLHSKindApplication (s (Abstract.ClassInstanceLHS l l d d)) (s (Abstract.Kind l l d d))

data Expression λ l d s =
   ApplyExpression (s (Abstract.Expression l l d d)) (s (Abstract.Expression l l d d))
   | ConditionalExpression (s (Abstract.Expression l l d d)) (s (Abstract.Expression l l d d))
                           (s (Abstract.Expression l l d d))
   | ConstructorExpression (s (Abstract.Constructor l l d d))
   | CaseExpression (s (Abstract.Expression l l d d)) [s (Abstract.CaseAlternative l l d d)]
   | LambdaCaseExpression !(Abstract.SupportFor 'Extensions.LambdaCase λ) [s (Abstract.CaseAlternative l l d d)]
   | LambdaCasesExpression !(Abstract.SupportFor 'Extensions.LambdaCase λ)
                           [s (Abstract.LambdaCasesAlternative l l d d)]
   | MultiWayIfExpression [s (Abstract.GuardedExpression l l d d)]
   | DoExpression (s (Abstract.GuardedExpression l l d d))
   | MDoExpression (s (Abstract.GuardedExpression l l d d))
   | QualifiedDoExpression !(Abstract.SupportFor 'Extensions.QualifiedDo λ)
                           (Abstract.ModuleName λ) (s (Abstract.GuardedExpression l l d d))
   | MDoQualifiedExpression !(Abstract.SupportFor 'Extensions.QualifiedDo λ)
                            !(Abstract.SupportFor 'Extensions.RecursiveDo λ)
                            (Abstract.ModuleName λ) (s (Abstract.GuardedExpression l l d d))
   | InfixExpression (s (Abstract.Expression l l d d)) (s (Abstract.Expression l l d d))
                     (s (Abstract.Expression l l d d))
   | LeftSectionExpression (s (Abstract.Expression l l d d)) (Abstract.QualifiedName λ)
   | LambdaExpression [s (Abstract.Pattern l l d d)] (s (Abstract.Expression l l d d))
   | LetExpression [s (Abstract.Declaration l l d d)] (s (Abstract.Expression l l d d))
   | ListComprehension (s (Abstract.Expression l l d d)) (NonEmpty (s (Abstract.Statement l l d d)))
   | ParallelListComprehension !(Abstract.SupportFor 'Extensions.ParallelListComprehensions λ)
                               (s (Abstract.Expression l l d d)) (NonEmpty (s (Abstract.Statement l l d d)))
                               (NonEmpty (s (Abstract.Statement l l d d))) [NonEmpty (s (Abstract.Statement l l d d))]
   | ListExpression [s (Abstract.Expression l l d d)]
   | LiteralExpression (s (Abstract.Value l l d d))
   | Negate
   | RecordExpression (s (Abstract.Expression l l d d)) [s (Abstract.FieldBinding l l d d)]
   | ReferenceExpression (Abstract.QualifiedName λ)
   | RightSectionExpression (Abstract.QualifiedName λ) (s (Abstract.Expression l l d d))
   | SequenceExpression (s (Abstract.Expression l l d d)) (Maybe (s (Abstract.Expression l l d d)))
                        (Maybe (s (Abstract.Expression l l d d)))
   | TupleExpression (NonEmpty (s (Abstract.Expression l l d d)))
   | TupleSectionExpression !(Abstract.SupportFor 'Extensions.TupleSections λ)
                            (NonEmpty (Maybe (s (Abstract.Expression l l d d))))
   | UnboxedSumExpression !(Abstract.SupportFor 'Extensions.UnboxedSums λ)
                          Int (s (Abstract.Expression l l d d)) Int
   | UnboxedTupleExpression !(Abstract.SupportFor 'Extensions.UnboxedTuples λ)
                            (NonEmpty (s (Abstract.Expression l l d d)))
   | UnboxedTupleSectionExpression !(Abstract.SupportFor 'Extensions.UnboxedTuples λ)
                                   (NonEmpty (Maybe (s (Abstract.Expression l l d d))))
   | TypedExpression (s (Abstract.Expression l l d d)) (s (Abstract.Type l l d d))
   | VisibleTypeApplication (s (Abstract.Expression l l d d)) (s (Abstract.Type l l d d))
   | ExplicitTypeExpression !(Abstract.SupportFor 'Extensions.ExplicitNamespaces λ) (s (Abstract.Type l l d d))
   | OverloadedLabel Text
   | ImplicitParameterExpression !(Abstract.SupportFor 'Extensions.ImplicitParameters λ) (Abstract.Name λ)
   | GetField (s (Abstract.Expression l l d d)) (Abstract.Name λ)
   | FieldProjection (NonEmpty (Abstract.Name λ))
   | WildcardRecordExpression !(Abstract.SupportFor 'Extensions.RecordWildCards λ)
                               (Abstract.QualifiedName λ)
                               [s (Abstract.FieldBinding l l d d)]

data LambdaCasesAlternative λ l d s =
   LambdaCasesAlternative !(Abstract.SupportFor 'Extensions.LambdaCase λ)
                          [s (Abstract.Pattern l l d d)] (s (Abstract.EquationRHS l l d d))

data FieldBinding λ l d s =
   FieldBinding (Abstract.QualifiedName λ) (s (Abstract.Expression l l d d))
   | PunnedFieldBinding !(Abstract.SupportFor 'Extensions.NamedFieldPuns λ) (Abstract.QualifiedName λ)

data Pattern λ l d s =
   AsPattern (Abstract.Name λ) (s (Abstract.Pattern l l d d))
   | ConstructorPattern (s (Abstract.Constructor l l d d)) [s (Abstract.Type l l d d)] [s (Abstract.Pattern l l d d)]
   | InfixPattern (s (Abstract.Pattern l l d d)) (Abstract.QualifiedName λ) (s (Abstract.Pattern l l d d))
   | IrrefutablePattern (s (Abstract.Pattern l l d d))
   | ListPattern [s (Abstract.Pattern l l d d)]
   | LiteralPattern (s (Abstract.Value l l d d))
   | RecordPattern (Abstract.QualifiedName λ) [s (Abstract.FieldPattern l l d d)]
   | WildcardRecordPattern !(Abstract.SupportFor 'Extensions.RecordWildCards λ)
                            (Abstract.QualifiedName λ)
                            [s (Abstract.FieldPattern l l d d)]
   | TypedPattern (s (Abstract.Pattern l l d d)) (s (Abstract.Type l l d d))
   | InvisibleTypePattern !(Abstract.SupportFor 'Extensions.TypeAbstractions λ) (s (Abstract.Type l l d d))
   | ExplicitTypePattern !(Abstract.SupportFor 'Extensions.ExplicitNamespaces λ) (s (Abstract.Type l l d d))
   | BangPattern !(Abstract.SupportFor 'Extensions.BangPatterns λ) (s (Abstract.Pattern l l d d))
   | LazyPattern !(Abstract.SupportFor 'Extensions.Strict λ) (s (Abstract.Pattern l l d d))
   | OrPattern !(Abstract.SupportFor 'Extensions.OrPatterns λ) (NonEmpty (s (Abstract.Pattern l l d d)))
   | ViewPattern !(Abstract.SupportFor 'Extensions.ViewPatterns λ)
                  (s (Abstract.Expression l l d d))
                  (s (Abstract.Pattern l l d d))
   | NPlusKPattern !(Abstract.SupportFor 'Extensions.NPlusKPatterns λ) (Abstract.Name λ) Integer
   | TuplePattern (NonEmpty (s (Abstract.Pattern l l d d)))
   | UnboxedTuplePattern !(Abstract.SupportFor 'Extensions.UnboxedTuples λ) (NonEmpty (s (Abstract.Pattern l l d d)))
   | UnboxedSumPattern !(Abstract.SupportFor 'Extensions.UnboxedSums λ)
                       Int (s (Abstract.Pattern l l d d)) Int
   | VariablePattern (Abstract.Name λ)
   | WildcardPattern

data PatternLHS λ l (d :: Kind.Type -> Kind.Type) (s :: Kind.Type -> Kind.Type) =
   PrefixPatternLHS (Name λ) [Name λ]
   | InfixPatternLHS (Name λ) (Name λ) (Name λ)
   | RecordPatternLHS (Name λ) [Name λ]

data FieldPattern λ l d s =
  FieldPattern (Abstract.QualifiedName λ) (s (Abstract.Pattern l l d d))
  | PunnedFieldPattern !(Abstract.SupportFor 'Extensions.NamedFieldPuns λ) (Abstract.QualifiedName λ)

data Statement λ l d s =
   BindStatement (s (Abstract.Pattern l l d d)) (s (Abstract.Expression l l d d))
   | ExpressionStatement (s (Abstract.Expression l l d d))
   | LetStatement [s (Abstract.Declaration l l d d)]
   | RecursiveStatement [s (Abstract.Statement l l d d)]

data Constructor λ l (d :: Kind.Type -> Kind.Type) (s :: Kind.Type -> Kind.Type) =
   ConstructorReference (Abstract.QualifiedName λ)
   | EmptyListConstructor
   | TupleConstructor Int
   | UnboxedTupleConstructor !(Abstract.SupportFor 'Extensions.UnboxedTuples λ) Int
   | UnboxedSumConstructor !(Abstract.SupportFor 'Extensions.UnboxedSums λ) Int
   | UnitConstructor

data Value λ l (d :: Kind.Type -> Kind.Type) (s :: Kind.Type -> Kind.Type) =
   CharLiteral Char
   | FloatingLiteral Rational
   | IntegerLiteral Integer
   | StringLiteral Text
   | HashLiteral !(Abstract.SupportFor 'Extensions.MagicHash λ) (Value λ l d s)
   | ExtendedLiteral !(Abstract.SupportFor 'Extensions.ExtendedLiterals λ) Integer (Name λ)

data CallSafety λ = SafeCall | UnsafeCall | InterruptibleCall !(Abstract.SupportFor 'Extensions.InterruptibleFFI λ)
data CallingConvention λ = CCall | CppCall | DotNetCall | JvmCall | StdCall
                         | CApiCall !(Abstract.SupportFor 'Extensions.CApiFFI λ)

deriving instance Typeable (Export λ l d s)
deriving instance (Data (Abstract.Members λ), Data (Abstract.ModuleName λ), Data (Abstract.QualifiedName λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (Export λ l d s)
deriving instance (Show (Abstract.Members λ),
                   Show (Abstract.ModuleName λ), Show (Abstract.QualifiedName λ)) => Show (Export λ l d s)
deriving instance (Eq (Abstract.Members λ),
                   Eq (Abstract.ModuleName λ), Eq (Abstract.QualifiedName λ)) => Eq (Export λ l d s)

deriving instance Typeable (Import λ l d s)
deriving instance (Data (s (Abstract.ImportSpecification l l d d)), Data (Abstract.ModuleName λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (Import λ l d s)
deriving instance (Show (s (Abstract.ImportSpecification l l d d)), Show (Abstract.ModuleName λ)) =>
                  Show (Import λ l d s)
deriving instance (Eq (s (Abstract.ImportSpecification l l d d)), Eq (Abstract.ModuleName λ)) =>
                  Eq (Import λ l d s)

deriving instance Typeable (ImportItem λ l d s)
deriving instance (Data (Abstract.Members λ), Data (Abstract.Name λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (ImportItem λ l d s)
deriving instance (Show (Abstract.Members λ), Show (Abstract.Name λ)) => Show (ImportItem λ l d s)
deriving instance (Eq (Abstract.Members λ), Eq (Abstract.Name λ)) => Eq (ImportItem λ l d s)

deriving instance (Data λ, Data (Abstract.SupportFor 'Extensions.ExplicitNamespaces λ)) => Data (Members λ)
deriving instance (Show (Abstract.SupportFor 'Extensions.ExplicitNamespaces λ)) => Show (Members λ)
deriving instance (Eq (Abstract.SupportFor 'Extensions.ExplicitNamespaces λ)) => Eq (Members λ)

deriving instance Typeable (TypeRole λ)
deriving instance (Data (Abstract.SupportFor 'Extensions.RoleAnnotations λ), Data λ) => Data (TypeRole λ)
deriving instance (Show (Abstract.SupportFor 'Extensions.RoleAnnotations λ), Show λ) => Show (TypeRole λ)
deriving instance (Eq (Abstract.SupportFor 'Extensions.RoleAnnotations λ), Eq λ) => Eq (TypeRole λ)

deriving instance Typeable (Declaration λ l d s)
deriving instance (Data (Abstract.SupportFor 'Extensions.ExplicitNamespaces λ),
                   Data (Abstract.SupportFor 'Extensions.StandaloneDeriving λ),
                   Data (Abstract.SupportFor 'Extensions.DerivingStrategies λ),
                   Data (Abstract.SupportFor 'Extensions.DefaultSignatures λ),
                   Data (Abstract.SupportFor 'Extensions.GADTs λ),
                   Data (Abstract.SupportFor 'Extensions.NamedDefaults λ),
                   Data (Abstract.SupportFor 'Extensions.RoleAnnotations λ),
                   Data (Abstract.SupportFor 'Extensions.TypeData λ),
                   Data (Abstract.SupportFor 'Extensions.FunctionalDependencies λ),
                   Data (Abstract.SupportFor 'Extensions.ImplicitParameters λ),
                   Data (Abstract.SupportFor 'Extensions.InterruptibleFFI λ),
                   Data (Abstract.SupportFor 'Extensions.CApiFFI λ),
                   Data (Abstract.SupportFor 'Extensions.PatternSynonyms λ),
                   Data (s (Abstract.Context l l d d)), Data (s (Abstract.Kind l l d d)),
                   Data (s (Abstract.FunctionalDependency l l d d)),
                   Data (s (Abstract.DataConstructor l l d d)), Data (s (Abstract.GADTConstructor l l d d)),
                   Data (s (Abstract.DerivingStrategy l l d d)),
                   Data (s (Abstract.Declaration l l d d)), Data (s (Abstract.DerivingClause l l d d)),
                   Data (s (Abstract.EquationLHS l l d d)), Data (s (Abstract.EquationRHS l l d d)),
                   Data (s (Abstract.Expression l l d d)),
                   Data (s (Abstract.Type l l d d)), Data (s (Abstract.TypeLHS l l d d)),
                   Data (s (Abstract.Kind l l d d)), Data (s (Abstract.ClassInstanceLHS l l d d)),
                   Data (s (Abstract.PatternLHS l l d d)), Data (s (Abstract.PatternEquationClause l l d d)),
                   Data (s (Abstract.Pattern l l d d)),
                   Data (Abstract.TypeVarBinding λ l d s),
                   Data (Abstract.Name λ), Data (Abstract.QualifiedName λ), Data (Abstract.TypeRole λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (Declaration λ l d s)
deriving instance (Show (Abstract.SupportFor 'Extensions.ExplicitNamespaces λ),
                   Show (Abstract.SupportFor 'Extensions.StandaloneDeriving λ),
                   Show (Abstract.SupportFor 'Extensions.DerivingStrategies λ),
                   Show (Abstract.SupportFor 'Extensions.DefaultSignatures λ),
                   Show (Abstract.SupportFor 'Extensions.GADTs λ),
                   Show (Abstract.SupportFor 'Extensions.NamedDefaults λ),
                   Show (Abstract.SupportFor 'Extensions.RoleAnnotations λ),
                   Show (Abstract.SupportFor 'Extensions.TypeData λ),
                   Show (Abstract.SupportFor 'Extensions.FunctionalDependencies λ),
                   Show (Abstract.SupportFor 'Extensions.ImplicitParameters λ),
                   Show (Abstract.SupportFor 'Extensions.InterruptibleFFI λ),
                   Show (Abstract.SupportFor 'Extensions.CApiFFI λ),
                   Show (Abstract.SupportFor 'Extensions.PatternSynonyms λ),
                   Show (s (Abstract.Context l l d d)), Show (s (Abstract.Kind l l d d)),
                   Show (s (Abstract.FunctionalDependency l l d d)),
                   Show (s (Abstract.DataConstructor l l d d)), Show (s (Abstract.GADTConstructor l l d d)),
                   Show (s (Abstract.DerivingStrategy l l d d)),
                   Show (s (Abstract.Declaration l l d d)), Show (s (Abstract.DerivingClause l l d d)),
                   Show (s (Abstract.EquationLHS l l d d)), Show (s (Abstract.EquationRHS l l d d)),
                   Show (s (Abstract.Expression l l d d)),
                   Show (s (Abstract.Type l l d d)), Show (s (Abstract.TypeLHS l l d d)),
                   Show (s (Abstract.Kind l l d d)), Show (s (Abstract.ClassInstanceLHS l l d d)),
                   Show (s (Abstract.PatternLHS l l d d)), Show (s (Abstract.PatternEquationClause l l d d)),
                   Show (s (Abstract.Pattern l l d d)),
                   Show (Abstract.TypeVarBinding λ l d s),
                   Show (Abstract.Name λ), Show (Abstract.QualifiedName λ),
                   Show (Abstract.TypeRole λ)) => Show (Declaration λ l d s)
deriving instance (Eq (Abstract.SupportFor 'Extensions.ExplicitNamespaces λ),
                   Eq (Abstract.SupportFor 'Extensions.StandaloneDeriving λ),
                   Eq (Abstract.SupportFor 'Extensions.DerivingStrategies λ),
                   Eq (Abstract.SupportFor 'Extensions.DefaultSignatures λ),
                   Eq (Abstract.SupportFor 'Extensions.GADTs λ),
                   Eq (Abstract.SupportFor 'Extensions.NamedDefaults λ),
                   Eq (Abstract.SupportFor 'Extensions.RoleAnnotations λ),
                   Eq (Abstract.SupportFor 'Extensions.TypeData λ),
                   Eq (Abstract.SupportFor 'Extensions.FunctionalDependencies λ),
                   Eq (Abstract.SupportFor 'Extensions.ImplicitParameters λ),
                   Eq (Abstract.SupportFor 'Extensions.InterruptibleFFI λ),
                   Eq (Abstract.SupportFor 'Extensions.CApiFFI λ),
                   Eq (Abstract.SupportFor 'Extensions.PatternSynonyms λ),
                   Eq (s (Abstract.Context l l d d)), Eq (s (Abstract.Kind l l d d)),
                   Eq (s (Abstract.DataConstructor l l d d)), Eq (s (Abstract.GADTConstructor l l d d)),
                   Eq (s (Abstract.DerivingStrategy l l d d)),
                   Eq (s (Abstract.FunctionalDependency l l d d)),
                   Eq (s (Abstract.Declaration l l d d)), Eq (s (Abstract.DerivingClause l l d d)),
                   Eq (s (Abstract.EquationLHS l l d d)), Eq (s (Abstract.EquationRHS l l d d)),
                   Eq (s (Abstract.Expression l l d d)),
                   Eq (s (Abstract.Type l l d d)), Eq (s (Abstract.TypeLHS l l d d)), Eq (s (Abstract.Kind l l d d)),
                   Eq (s (Abstract.ClassInstanceLHS l l d d)),
                   Eq (s (Abstract.PatternLHS l l d d)), Eq (s (Abstract.PatternEquationClause l l d d)),
                   Eq (s (Abstract.Pattern l l d d)),
                   Eq (Abstract.TypeVarBinding λ l d s),
                   Eq (Abstract.Name λ), Eq (Abstract.QualifiedName λ),
                   Eq (Abstract.TypeRole λ)) => Eq (Declaration λ l d s)

deriving instance Typeable (PatternEquationClause λ l d s)
deriving instance (Typeable λ, Typeable l, Typeable d, Typeable s,
                   Data (Abstract.SupportFor 'Extensions.PatternSynonyms λ),
                   Data (s (Abstract.PatternEquationLHS l l d d)), Data (s (Abstract.EquationRHS l l d d)),
                   Data (s (Abstract.Declaration l l d d))) => Data (PatternEquationClause λ l d s)
deriving instance (Eq (Abstract.SupportFor 'Extensions.PatternSynonyms λ),
                   Eq (s (Abstract.PatternEquationLHS l l d d)), Eq (s (Abstract.EquationRHS l l d d)),
                   Eq (s (Abstract.Declaration l l d d))) => Eq (PatternEquationClause λ l d s)
deriving instance (Show (Abstract.SupportFor 'Extensions.PatternSynonyms λ),
                   Show (s (Abstract.PatternEquationLHS l l d d)), Show (s (Abstract.EquationRHS l l d d)),
                   Show (s (Abstract.Declaration l l d d))) => Show (PatternEquationClause λ l d s)

deriving instance Typeable (PatternEquationLHS λ l d s)
deriving instance (Typeable λ, Typeable l, Typeable d, Typeable s,
                   Data (Abstract.Name λ), Data (s (Abstract.Pattern l l d d))) => Data (PatternEquationLHS λ l d s)
deriving instance (Eq (Abstract.Name λ), Eq (s (Abstract.Pattern l l d d))) => Eq (PatternEquationLHS λ l d s)
deriving instance (Show (Abstract.Name λ), Show (s (Abstract.Pattern l l d d))) => Show (PatternEquationLHS λ l d s)

deriving instance Typeable (FunctionalDependency λ l d s)
deriving instance (Data (Abstract.Name λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (FunctionalDependency λ l d s)
deriving instance (Show (Abstract.Name λ)) => Show (FunctionalDependency λ l d s)
deriving instance (Eq (Abstract.Name λ)) => Eq (FunctionalDependency λ l d s)

deriving instance Typeable (DataConstructor λ l d s)
deriving instance (Data (s (Abstract.Context l l d d)), Data (s (Abstract.DataConstructor l l d d)),
                   Data (s (Abstract.FieldDeclaration l l d d)), Data (s (Abstract.Type l l d d)),
                   Data (s (Abstract.Kind l l d d)), Data (Abstract.Name λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (DataConstructor λ l d s)
deriving instance (Show (s (Abstract.Context l l d d)), Show (s (Abstract.DataConstructor l l d d)),
                   Show (s (Abstract.FieldDeclaration l l d d)), Show (s (Abstract.Type l l d d)),
                   Show (s (Abstract.Kind l l d d)), Show (Abstract.Name λ)) => Show (DataConstructor λ l d s)
deriving instance (Eq (s (Abstract.Context l l d d)), Eq (s (Abstract.DataConstructor l l d d)),
                   Eq (s (Abstract.FieldDeclaration l l d d)), Eq (s (Abstract.Type l l d d)),
                   Eq (s (Abstract.Kind l l d d)), Eq (Abstract.Name λ)) => Eq (DataConstructor λ l d s)

deriving instance Typeable (GADTConstructor λ l d s)
deriving instance (Data (s (Abstract.Context l l d d)), Data (s (Abstract.Type l l d d)),
                   Data (s (Abstract.Kind l l d d)), Data (Abstract.Name λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (GADTConstructor λ l d s)
deriving instance (Show (s (Abstract.Context l l d d)), Show (s (Abstract.Type l l d d)),
                   Show (s (Abstract.Kind l l d d)),
                   Show (Abstract.Name λ), Show (Abstract.Name λ)) => Show (GADTConstructor λ l d s)
deriving instance (Eq (s (Abstract.Context l l d d)), Eq (s (Abstract.Type l l d d)),
                   Eq (s (Abstract.Kind l l d d)),
                   Eq (Abstract.Name λ), Eq (Abstract.Name λ)) => Eq (GADTConstructor λ l d s)

deriving instance Typeable (DerivingClause λ l d s)
deriving instance (Data (Abstract.SupportFor 'Extensions.DerivingStrategies λ),
                   Data (Abstract.SupportFor 'Extensions.DerivingVia λ),
                   Data (Abstract.QualifiedName λ), Data (s (Abstract.Type l l d d)),
                   Data (s (Abstract.DerivingStrategy l l d d)),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (DerivingClause λ l d s)
deriving instance (Show (Abstract.SupportFor 'Extensions.DerivingStrategies λ),
                   Show (Abstract.SupportFor 'Extensions.DerivingVia λ),
                   Show (Abstract.QualifiedName λ), Show (s (Abstract.Type l l d d)),
                   Show (s (Abstract.DerivingStrategy l l d d))) => Show (DerivingClause λ l d s)
deriving instance (Eq (Abstract.SupportFor 'Extensions.DerivingStrategies λ),
                   Eq (Abstract.SupportFor 'Extensions.DerivingVia λ),
                   Eq (Abstract.QualifiedName λ), Eq (s (Abstract.Type l l d d)),
                   Eq (s (Abstract.DerivingStrategy l l d d))) => Eq (DerivingClause λ l d s)

deriving instance Typeable (DerivingStrategy λ l d s)
deriving instance (Data (Abstract.SupportFor 'Extensions.DerivingVia λ),
                   Data λ, Typeable l, Typeable d, Typeable s, Data (s (Abstract.Type l l d d))) =>
                  Data (DerivingStrategy λ l d s)
deriving instance (Show (Abstract.SupportFor 'Extensions.DerivingVia λ),
                   Show (s (Abstract.Type l l d d))) => Show (DerivingStrategy λ l d s)
deriving instance (Eq (Abstract.SupportFor 'Extensions.DerivingVia λ),
                   Eq (s (Abstract.Type l l d d))) => Eq (DerivingStrategy λ l d s)

deriving instance Typeable (Type λ l d s)
deriving instance (Data (Abstract.SupportFor 'Extensions.UnboxedSums λ),
                   Data (Abstract.SupportFor 'Extensions.UnboxedTuples λ),
                   Data (Abstract.SupportFor 'Extensions.StrictData λ),
                   Data (Abstract.SupportFor 'Extensions.DataKinds λ),
                   Data (s (Abstract.Constructor l l d d)), Data (s (Abstract.Context l l d d)),
                   Data (s (Abstract.Kind l l d d)), Data (s (Abstract.Type l l d d)),
                   Data (s (Abstract.FieldDeclaration l l d d)),
                   Data (Abstract.Name λ), Data (Abstract.QualifiedName λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (Type λ l d s)
deriving instance (Show (Abstract.SupportFor 'Extensions.UnboxedSums λ),
                   Show (Abstract.SupportFor 'Extensions.UnboxedTuples λ),
                   Show (Abstract.SupportFor 'Extensions.StrictData λ),
                   Show (Abstract.SupportFor 'Extensions.DataKinds λ),
                   Show (s (Abstract.Constructor l l d d)), Show (s (Abstract.Context l l d d)),
                   Show (s (Abstract.Kind l l d d)), Show (s (Abstract.Type l l d d)),
                   Show (s (Abstract.FieldDeclaration l l d d)),
                   Show (Abstract.Name λ), Show (Abstract.QualifiedName λ)) => Show (Type λ l d s)
deriving instance (Eq (Abstract.SupportFor 'Extensions.UnboxedSums λ),
                   Eq (Abstract.SupportFor 'Extensions.UnboxedTuples λ),
                   Eq (Abstract.SupportFor 'Extensions.StrictData λ),
                   Eq (Abstract.SupportFor 'Extensions.DataKinds λ),
                   Eq (s (Abstract.Constructor l l d d)), Eq (s (Abstract.Context l l d d)),
                   Eq (s (Abstract.Kind l l d d)), Eq (s (Abstract.Type l l d d)),
                   Eq (s (Abstract.FieldDeclaration l l d d)),
                   Eq (Abstract.Name λ), Eq (Abstract.QualifiedName λ)) => Eq (Type λ l d s)

deriving instance Typeable (TypeVarBinding λ l d s)
deriving instance (Data (s (Abstract.Kind l l d d)), Data (Abstract.Name λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (TypeVarBinding λ l d s)
deriving instance (Show (s (Abstract.Kind l l d d)), Show (Abstract.Name λ)) => Show (TypeVarBinding λ l d s)
deriving instance (Eq (s (Abstract.Kind l l d d)), Eq (Abstract.Name λ)) => Eq (TypeVarBinding λ l d s)

deriving instance Typeable (TypeLHS λ l d s)
deriving instance (Data (Abstract.SupportFor 'Extensions.TypeAbstractions λ),
                   Data (s (Abstract.Type l l d d)), Data (s (Abstract.TypeLHS l l d d)),
                   Data (s (Abstract.Kind l l d d)),
                   Data (Abstract.QualifiedName λ), Data (Abstract.Name λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (TypeLHS λ l d s)
deriving instance (Show (Abstract.SupportFor 'Extensions.TypeAbstractions λ),
                   Show (s (Abstract.Type l l d d)), Show (s (Abstract.TypeLHS l l d d)),
                   Show (s (Abstract.Kind l l d d)),
                   Show (Abstract.QualifiedName λ), Show (Abstract.Name λ)) => Show (TypeLHS λ l d s)
deriving instance (Eq (Abstract.SupportFor 'Extensions.TypeAbstractions λ),
                   Eq (s (Abstract.Type l l d d)), Eq (s (Abstract.TypeLHS l l d d)), Eq (s (Abstract.Kind l l d d)),
                   Eq (Abstract.QualifiedName λ), Eq (Abstract.Name λ)) => Eq (TypeLHS λ l d s)

deriving instance Typeable (ClassInstanceLHS λ l d s)
deriving instance (Data (s (Abstract.ClassInstanceLHS l l d d)), Data (s (Abstract.Type l l d d)),
                   Data (s (Abstract.Kind l l d d)), Data (Abstract.QualifiedName λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (ClassInstanceLHS λ l d s)
deriving instance (Show (s (Abstract.ClassInstanceLHS l l d d)), Show (s (Abstract.Type l l d d)),
                   Show (s (Abstract.Kind l l d d)), Show (Abstract.QualifiedName λ)) => Show (ClassInstanceLHS λ l d s)
deriving instance (Eq (s (Abstract.ClassInstanceLHS l l d d)), Eq (s (Abstract.Type l l d d)),
                   Eq (s (Abstract.Kind l l d d)), Eq (Abstract.QualifiedName λ)) => Eq (ClassInstanceLHS λ l d s)

deriving instance Typeable (Context λ l d s)
deriving instance (Data (Abstract.SupportFor 'Extensions.ImplicitParameters λ),
                   Data (s (Abstract.Context l l d d)), Data (s (Abstract.Type l l d d)),
                   Data (Abstract.Name λ), Data (Abstract.QualifiedName λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (Context λ l d s)
deriving instance (Show (Abstract.SupportFor 'Extensions.ImplicitParameters λ),
                   Show (s (Abstract.Context l l d d)), Show (s (Abstract.Type l l d d)),
                   Show (Abstract.QualifiedName λ), Show (Abstract.Name λ)) => Show (Context λ l d s)
deriving instance (Eq (Abstract.SupportFor 'Extensions.ImplicitParameters λ),
                   Eq (s (Abstract.Context l l d d)), Eq (s (Abstract.Type l l d d)),
                   Eq (Abstract.QualifiedName λ), Eq (Abstract.Name λ)) => Eq (Context λ l d s)

deriving instance Typeable (Expression λ l d s)
deriving instance (Typeable (Abstract.Pattern l), Typeable (Abstract.EquationRHS l),
                   Data (Abstract.SupportFor 'Extensions.ExplicitNamespaces λ),
                   Data (Abstract.SupportFor 'Extensions.ImplicitParameters λ),
                   Data (Abstract.SupportFor 'Extensions.ParallelListComprehensions λ),
                   Data (Abstract.SupportFor 'Extensions.QualifiedDo λ),
                   Data (Abstract.SupportFor 'Extensions.RecursiveDo λ),
                   Data (Abstract.SupportFor 'Extensions.LambdaCase λ),
                   Data (Abstract.SupportFor 'Extensions.RecordWildCards λ),
                   Data (Abstract.SupportFor 'Extensions.TupleSections λ),
                   Data (Abstract.SupportFor 'Extensions.UnboxedSums λ),
                   Data (Abstract.SupportFor 'Extensions.UnboxedTuples λ),
                   Data (s (Abstract.CaseAlternative l l d d)), Data (s (Abstract.Constructor l l d d)),
                   Data (s (Abstract.Expression l l d d)), Data (s (Abstract.GuardedExpression l l d d)),
                   Data (s (Abstract.Declaration l l d d)), Data (s (Abstract.FieldBinding l l d d)),
                   Data (s (Abstract.Pattern l l d d)), Data (s (Abstract.Statement l l d d)),
                   Data (s (Abstract.LambdaCasesAlternative l l d d)),
                   Data (s (Abstract.Type l l d d)), Data (s (Abstract.Value l l d d)),
                   Data (Abstract.QualifiedName λ), Data (Abstract.ModuleName λ), Data (Abstract.Name λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (Expression λ l d s)
deriving instance (Show (Abstract.SupportFor 'Extensions.ExplicitNamespaces λ),
                   Show (Abstract.SupportFor 'Extensions.ImplicitParameters λ),
                   Show (Abstract.SupportFor 'Extensions.ParallelListComprehensions λ),
                   Show (Abstract.SupportFor 'Extensions.QualifiedDo λ),
                   Show (Abstract.SupportFor 'Extensions.RecursiveDo λ),
                   Show (Abstract.SupportFor 'Extensions.LambdaCase λ),
                   Show (Abstract.SupportFor 'Extensions.RecordWildCards λ),
                   Show (Abstract.SupportFor 'Extensions.TupleSections λ),
                   Show (Abstract.SupportFor 'Extensions.UnboxedSums λ),
                   Show (Abstract.SupportFor 'Extensions.UnboxedTuples λ),
                   Show (s (Abstract.CaseAlternative l l d d)), Show (s (Abstract.Constructor l l d d)),
                   Show (s (Abstract.Expression l l d d)), Show (s (Abstract.GuardedExpression l l d d)),
                   Show (s (Abstract.Declaration l l d d)), Show (s (Abstract.FieldBinding l l d d)),
                   Show (s (Abstract.Pattern l l d d)), Show (s (Abstract.Statement l l d d)),
                   Show (s (Abstract.LambdaCasesAlternative l l d d)),
                   Show (s (Abstract.EquationRHS l l d d)),
                   Show (s (Abstract.Type l l d d)), Show (s (Abstract.Value l l d d)),
                   Show (Abstract.QualifiedName λ), Show (Abstract.ModuleName λ),
                   Show (Abstract.Name λ)) => Show (Expression λ l d s)
deriving instance (Eq (Abstract.SupportFor 'Extensions.ExplicitNamespaces λ),
                   Eq (Abstract.SupportFor 'Extensions.ImplicitParameters λ),
                   Eq (Abstract.SupportFor 'Extensions.QualifiedDo λ),
                   Eq (Abstract.SupportFor 'Extensions.ParallelListComprehensions λ),
                   Eq (Abstract.SupportFor 'Extensions.RecursiveDo λ),
                   Eq (Abstract.SupportFor 'Extensions.LambdaCase λ),
                   Eq (Abstract.SupportFor 'Extensions.RecordWildCards λ),
                   Eq (Abstract.SupportFor 'Extensions.TupleSections λ),
                   Eq (Abstract.SupportFor 'Extensions.UnboxedSums λ),
                   Eq (Abstract.SupportFor 'Extensions.UnboxedTuples λ),
                   Eq (s (Abstract.CaseAlternative l l d d)), Eq (s (Abstract.Constructor l l d d)),
                   Eq (s (Abstract.Expression l l d d)), Eq (s (Abstract.GuardedExpression l l d d)),
                   Eq (s (Abstract.Declaration l l d d)), Eq (s (Abstract.FieldBinding l l d d)),
                   Eq (s (Abstract.Pattern l l d d)), Eq (s (Abstract.Statement l l d d)),
                   Eq (s (Abstract.LambdaCasesAlternative l l d d)),
                   Eq (s (Abstract.EquationRHS l l d d)),
                   Eq (s (Abstract.Type l l d d)), Eq (s (Abstract.Value l l d d)),
                   Eq (Abstract.QualifiedName λ), Eq (Abstract.ModuleName λ),
                   Eq (Abstract.Name λ)) => Eq (Expression λ l d s)

deriving instance Typeable (FieldBinding λ l d s)
deriving instance (Data (Abstract.SupportFor 'Extensions.NamedFieldPuns λ),
                   Data (s (Abstract.Expression l l d d)), Data (Abstract.QualifiedName λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (FieldBinding λ l d s)
deriving instance (Show (Abstract.SupportFor 'Extensions.NamedFieldPuns λ),
                   Show (s (Abstract.Expression l l d d)), Show (Abstract.QualifiedName λ)) =>
                  Show (FieldBinding λ l d s)
deriving instance (Eq (Abstract.SupportFor 'Extensions.NamedFieldPuns λ),
                   Eq (s (Abstract.Expression l l d d)), Eq (Abstract.QualifiedName λ)) =>
                  Eq (FieldBinding λ l d s)

deriving instance Typeable (LambdaCasesAlternative λ l d s)
deriving instance (Data (Abstract.SupportFor 'Extensions.LambdaCase λ),
                   Data (s (Abstract.Pattern l l d d)), Data (s (Abstract.EquationRHS l l d d)),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (LambdaCasesAlternative λ l d s)
deriving instance (Show (Abstract.SupportFor 'Extensions.LambdaCase λ),
                   Show (s (Abstract.Pattern l l d d)), Show (s (Abstract.EquationRHS l l d d))) =>
                  Show (LambdaCasesAlternative λ l d s)
deriving instance (Eq (Abstract.SupportFor 'Extensions.LambdaCase λ),
                   Eq (s (Abstract.Pattern l l d d)), Eq (s (Abstract.EquationRHS l l d d))) =>
                  Eq (LambdaCasesAlternative λ l d s)

deriving instance Typeable (Pattern λ l d s)
deriving instance (Data (Abstract.SupportFor 'Extensions.ExplicitNamespaces λ),
                   Data (Abstract.SupportFor 'Extensions.RecordWildCards λ),
                   Data (Abstract.SupportFor 'Extensions.UnboxedSums λ),
                   Data (Abstract.SupportFor 'Extensions.UnboxedTuples λ),
                   Data (Abstract.SupportFor 'Extensions.Strict λ),
                   Data (Abstract.SupportFor 'Extensions.BangPatterns λ),
                   Data (Abstract.SupportFor 'Extensions.OrPatterns λ),
                   Data (Abstract.SupportFor 'Extensions.ViewPatterns λ),
                   Data (Abstract.SupportFor 'Extensions.NPlusKPatterns λ),
                   Data (Abstract.SupportFor 'Extensions.TypeAbstractions λ),
                   Data (s (Abstract.Constructor l l d d)), Data (s (Abstract.Expression l l d d)),
                   Data (s (Abstract.FieldPattern l l d d)), Data (s (Abstract.Pattern l l d d)),
                   Data (s (Abstract.Value l l d d)), Data (s (Abstract.Type l l d d)),
                   Data (Abstract.Name λ), Data (Abstract.QualifiedName λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (Pattern λ l d s)
deriving instance (Show (Abstract.SupportFor 'Extensions.ExplicitNamespaces λ),
                   Show (Abstract.SupportFor 'Extensions.RecordWildCards λ),
                   Show (Abstract.SupportFor 'Extensions.UnboxedSums λ),
                   Show (Abstract.SupportFor 'Extensions.UnboxedTuples λ),
                   Show (Abstract.SupportFor 'Extensions.Strict λ),
                   Show (Abstract.SupportFor 'Extensions.BangPatterns λ),
                   Show (Abstract.SupportFor 'Extensions.OrPatterns λ),
                   Show (Abstract.SupportFor 'Extensions.ViewPatterns λ),
                   Show (Abstract.SupportFor 'Extensions.NPlusKPatterns λ),
                   Show (Abstract.SupportFor 'Extensions.TypeAbstractions λ),
                   Show (s (Abstract.Constructor l l d d)), Show (s (Abstract.Expression l l d d)),
                   Show (s (Abstract.FieldPattern l l d d)), Show (s (Abstract.Pattern l l d d)),
                   Show (s (Abstract.Value l l d d)), Show (s (Abstract.Type l l d d)),
                   Show (Abstract.QualifiedName λ), Show (Abstract.Name λ)) => Show (Pattern λ l d s)
deriving instance (Eq (Abstract.SupportFor 'Extensions.ExplicitNamespaces λ),
                   Eq (Abstract.SupportFor 'Extensions.RecordWildCards λ),
                   Eq (Abstract.SupportFor 'Extensions.UnboxedSums λ),
                   Eq (Abstract.SupportFor 'Extensions.UnboxedTuples λ),
                   Eq (Abstract.SupportFor 'Extensions.Strict λ),
                   Eq (Abstract.SupportFor 'Extensions.BangPatterns λ),
                   Eq (Abstract.SupportFor 'Extensions.OrPatterns λ),
                   Eq (Abstract.SupportFor 'Extensions.ViewPatterns λ),
                   Eq (Abstract.SupportFor 'Extensions.NPlusKPatterns λ),
                   Eq (Abstract.SupportFor 'Extensions.TypeAbstractions λ),
                   Eq (s (Abstract.Constructor l l d d)), Eq (s (Abstract.Expression l l d d)),
                   Eq (s (Abstract.FieldPattern l l d d)), Eq (s (Abstract.Pattern l l d d)),
                   Eq (s (Abstract.Value l l d d)), Eq (s (Abstract.Type l l d d)),
                   Eq (Abstract.QualifiedName λ), Eq (Abstract.Name λ)) => Eq (Pattern λ l d s)

deriving instance Typeable (FieldPattern λ l d s)
deriving instance (Data (Abstract.SupportFor 'Extensions.NamedFieldPuns λ),
                   Data (s (Abstract.Pattern l l d d)), Data (Abstract.QualifiedName λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (FieldPattern λ l d s)
deriving instance (Show (Abstract.SupportFor 'Extensions.NamedFieldPuns λ),
                   Show (s (Abstract.Pattern l l d d)), Show (Abstract.QualifiedName λ)) => Show (FieldPattern λ l d s)
deriving instance (Eq (Abstract.SupportFor 'Extensions.NamedFieldPuns λ),
                   Eq (s (Abstract.Pattern l l d d)), Eq (Abstract.QualifiedName λ)) => Eq (FieldPattern λ l d s)

deriving instance Typeable (PatternLHS λ l d s)
deriving instance (Data (Abstract.Name λ), Data λ, Typeable l, Typeable d, Typeable s) => Data (PatternLHS λ l d s)
deriving instance (Show (Abstract.Name λ)) => Show (PatternLHS λ l d s)
deriving instance (Eq (Abstract.Name λ)) => Eq (PatternLHS λ l d s)

deriving instance Typeable (Statement λ l d s)
deriving instance (Data (s (Abstract.Declaration l l d d)), Data (s (Abstract.Expression l l d d)),
                   Data (s (Abstract.Pattern l l d d)), Data (s (Abstract.Statement l l d d)),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (Statement λ l d s)
deriving instance (Show (s (Abstract.Declaration l l d d)), Show (s (Abstract.Expression l l d d)),
                   Show (s (Abstract.Pattern l l d d)), Show (s (Abstract.Statement l l d d)))
                   => Show (Statement λ l d s)
deriving instance (Eq (s (Abstract.Declaration l l d d)), Eq (s (Abstract.Expression l l d d)),
                   Eq (s (Abstract.Pattern l l d d)), Eq (s (Abstract.Statement l l d d))) => Eq (Statement λ l d s)

deriving instance Data (Abstract.QualifiedName λ) => Typeable (Constructor λ l d s)
deriving instance (Data (Abstract.SupportFor 'Extensions.UnboxedSums λ),
                   Data (Abstract.SupportFor 'Extensions.UnboxedTuples λ),
                   Data (Abstract.QualifiedName λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (Constructor λ l d s)
deriving instance (Show (Abstract.SupportFor 'Extensions.UnboxedSums λ),
                   Show (Abstract.SupportFor 'Extensions.UnboxedTuples λ),
                   Show (Abstract.QualifiedName λ)) => Show (Constructor λ l d s)
deriving instance (Eq (Abstract.SupportFor 'Extensions.UnboxedSums λ),
                   Eq (Abstract.SupportFor 'Extensions.UnboxedTuples λ),
                   Eq (Abstract.QualifiedName λ)) => Eq (Constructor λ l d s)

deriving instance Typeable (Value λ l d s)
deriving instance (Data (Abstract.SupportFor 'Extensions.ExtendedLiterals λ),
                   Data (Abstract.SupportFor 'Extensions.MagicHash λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (Value λ l d s)
deriving instance (Show (Abstract.SupportFor 'Extensions.ExtendedLiterals λ),
                   Show (Abstract.SupportFor 'Extensions.MagicHash λ)) => Show (Value λ l d s)
deriving instance (Eq (Abstract.SupportFor 'Extensions.ExtendedLiterals λ),
                   Eq (Abstract.SupportFor 'Extensions.MagicHash λ)) => Eq (Value λ l d s)

deriving instance Typeable (CallSafety λ)
deriving instance (Data (Abstract.SupportFor 'Extensions.InterruptibleFFI λ), Data λ) => Data (CallSafety λ)
deriving instance Show (Abstract.SupportFor 'Extensions.InterruptibleFFI λ) => Show (CallSafety λ)
deriving instance Eq (Abstract.SupportFor 'Extensions.InterruptibleFFI λ) => Eq (CallSafety λ)

deriving instance Typeable (CallingConvention λ)
deriving instance (Data (Abstract.SupportFor 'Extensions.CApiFFI λ), Data λ) => Data (CallingConvention λ)
deriving instance Show (Abstract.SupportFor 'Extensions.CApiFFI λ) => Show (CallingConvention λ)
deriving instance Eq (Abstract.SupportFor 'Extensions.CApiFFI λ) => Eq (CallingConvention λ)


$(concat <$>
  (forM [Rank2.TH.deriveFunctor, Rank2.TH.deriveFoldable, Rank2.TH.deriveTraversable, Rank2.TH.unsafeDeriveApply,
         Transformation.Shallow.TH.deriveAll, Transformation.Deep.TH.deriveAll] $
   \derive-> mconcat <$> mapM derive
             [''Export, ''Import, ''ImportItem,
              ''Declaration, ''FunctionalDependency, ''DataConstructor, ''GADTConstructor,
              ''PatternEquationClause, ''DerivingClause, ''DerivingStrategy,
              ''Type, ''TypeLHS, ''TypeVarBinding, ''ClassInstanceLHS, ''Context,
              ''Expression, ''FieldBinding, ''LambdaCasesAlternative,
              ''Pattern, ''PatternLHS, ''PatternEquationLHS, ''FieldPattern,
              ''Statement, ''Constructor, ''Value]))
