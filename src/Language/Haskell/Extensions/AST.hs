{-# Language DataKinds, DeriveDataTypeable, FlexibleContexts, FlexibleInstances,
             MultiParamTypeClasses, OverloadedStrings,
             StandaloneDeriving, TemplateHaskell, TypeFamilies, TypeOperators, UndecidableInstances #-}

module Language.Haskell.Extensions.AST (Language(Language), Import(..), Members(..), ModuleMember(..),
                                        Declaration(..), DataConstructor(..), GADTConstructor(..),
                                        FunctionalDependency(..), DerivingClause(..), DerivingStrategy(..),
                                        Expression(..), Pattern(..), FieldBinding(..), FieldPattern(..), Statement(..),
                                        ClassInstanceLHS(..), Context(..),
                                        Type(..), TypeLHS(..), TypeVarBinding(..), TypeRole(..), Value(..),
                                        module Report) where

import Control.Monad (forM)
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Data (Data, Typeable)
import qualified Data.Kind as Kind
import Data.Text (Text)

import qualified Language.Haskell.Extensions as Extensions
import qualified Language.Haskell.Extensions.Abstract as Abstract
import qualified Language.Haskell.AST as Report
import Language.Haskell.AST (Module(..), EquationLHS(..), EquationRHS(..),
                             GuardedExpression(..), Constructor(..),
                             FieldDeclaration(..), CaseAlternative(..),
                             CallingConvention(..), CallSafety(..), Associativity(..),
                             Name(..), ModuleName(..), QualifiedName(..),
                             ImportSpecification(..), ImportItem(..), Export(..))
import qualified Rank2.TH
import qualified Transformation.Deep.TH
import qualified Transformation.Shallow.TH

data Language = Language deriving (Data, Eq, Show)

type instance Abstract.ExtensionsSupportedBy Language = '[
   'Extensions.MagicHash,
   'Extensions.NamedFieldPuns,
   'Extensions.ParallelListComprehensions,
   'Extensions.RecordWildCards,
   'Extensions.RecursiveDo,
   'Extensions.TupleSections,
   'Extensions.BangPatterns,
   'Extensions.StandaloneDeriving,
   'Extensions.DerivingStrategies,
   'Extensions.DerivingVia,
   'Extensions.DefaultSignatures,
   'Extensions.FunctionalDependencies]

instance Abstract.ExtendedWith 'Extensions.MagicHash Language where
   build = Abstract.MagicHashConstruction {
      Abstract.hashLiteral' = HashLiteral ()}

instance Abstract.ExtendedWith 'Extensions.NamedFieldPuns Language where
   build = Abstract.NamedFieldPunsConstruction {
      Abstract.punnedFieldBinding' = PunnedFieldBinding,
      Abstract.punnedFieldPattern' = PunnedFieldPattern}

instance Abstract.ExtendedWith 'Extensions.ParallelListComprehensions Language where
   build = Abstract.ParallelListComprehensionConstruction {
      Abstract.parallelListComprehension' = ParallelListComprehension}

instance Abstract.ExtendedWith 'Extensions.RecordWildCards Language where
   build = Abstract.RecordWildCardConstruction {
      Abstract.wildcardRecordExpression' = WildcardRecordExpression (),
      Abstract.wildcardRecordPattern' = WildcardRecordPattern ()}

instance Abstract.ExtendedWith 'Extensions.RecursiveDo Language where
   build = Abstract.RecursiveDoConstruction {
      Abstract.mdoExpression' = MDoExpression,
      Abstract.recursiveStatement' = RecursiveStatement}

instance Abstract.ExtendedWith 'Extensions.TupleSections Language where
   build = Abstract.TupleSectionConstruction {
      Abstract.tupleSectionExpression' = TupleSectionExpression}

instance Abstract.ExtendedWith 'Extensions.BangPatterns Language where
   build = Abstract.BangPatternConstruction {
      Abstract.bangPattern = BangPattern ()}

instance Abstract.ExtendedWith 'Extensions.StandaloneDeriving Language where
   build = Abstract.StandaloneDerivingConstruction {
      Abstract.standaloneDerivingDeclaration = StandaloneDerivingDeclaration ()}

instance Abstract.ExtendedWith 'Extensions.DerivingStrategies Language where
   build = Abstract.DerivingStrategiesConstruction {
      Abstract.stockStrategy = Stock,
      Abstract.newtypeStrategy = Newtype,
      Abstract.anyClassStrategy = AnyClass,
      Abstract.strategicDerive = StrategicDerive (),
      Abstract.standaloneStrategicDerivingDeclaration = StandaloneStrategicDerivingDeclaration () ()}

instance Abstract.ExtendedWith 'Extensions.DerivingVia Language where
   build = Abstract.DerivingViaConstruction {
      Abstract.derivingViaStrategy = Via ()}

instance Abstract.ExtendedWith 'Extensions.DefaultSignatures Language where
   build = Abstract.DefaultSignatureConstruction {
      Abstract.defaultMethodSignature = DefaultMethodSignature ()}

instance Abstract.ExtendedWith 'Extensions.FunctionalDependencies Language where
   build = Abstract.FunctionalDependenciesConstruction {
      Abstract.functionalDependency = FunctionalDependency,
      Abstract.fundepClassDeclaration = FunDepClassDeclaration ()}

type instance Abstract.FunctionalDependency Language = FunctionalDependency Language
type instance Abstract.DerivingStrategy Language = DerivingStrategy Language

instance Abstract.ExtendedHaskell Language where
   type GADTConstructor Language = GADTConstructor Language
   type Kind Language = Type Language
   type TypeVarBinding Language = TypeVarBinding Language
   type ModuleMember Language = ModuleMember Language
   type TypeRole Language = TypeRole Language
   lambdaCaseExpression = LambdaCaseExpression
   multiWayIfExpression = MultiWayIfExpression
   safeImportDeclaration q = Import True q Nothing 
   packageQualifiedImportDeclaration q p = Import False q (Just p)
   safePackageQualifiedImportDeclaration q p = Import True q (Just p)
   infixTypeApplication = InfixTypeApplication
   simpleKindedTypeLHS = SimpleTypeLHS
   simpleInfixTypeLHSApplication left op right = SimpleTypeLHS op [left, right]
   simpleTypeLHSApplication = SimpleTypeLHSApplication
   visibleDependentType = VisibleDependentType
   existentialConstructor = ExistentialConstructor
   explicitlyScopedInstanceDeclaration = InstanceDeclaration
   forallType = ForallType
   kindedType = KindedType
   constrainedType = ConstrainedType
   typeKind = TypeKind
   typeWildcard = TypeWildcard
   groundType = GroundTypeKind

   kindedDataDeclaration context lhs = DataDeclaration context lhs . Just
   kindedNewtypeDeclaration context lhs = NewtypeDeclaration context lhs . Just
   gadtDeclaration = GADTDeclaration
   gadtNewtypeDeclaration = GADTNewtypeDeclaration
   gadtConstructors = GADTConstructors
   recordFunctionType = RecordFunctionType
   linearFunctionType = LinearFunctionType
   multiplicityFunctionType = MultiplicityFunctionType
   promotedConstructorType = PromotedConstructorType
   promotedTupleType = PromotedTupleType
   promotedListType = PromotedListType
   promotedIntegerLiteral = PromotedIntegerLiteral
   promotedCharLiteral = PromotedCharLiteral
   promotedStringLiteral = PromotedStringLiteral
   promotedInfixTypeApplication = PromotedInfixTypeApplication

   explicitlyNamespacedMemberList = ExplicitlyNamespacedMemberList
   defaultMember = DefaultMember
   patternMember = PatternMember
   typeMember = TypeMember

   typeRoleDeclaration = TypeRoleDeclaration
   inferredRole = InferredRole
   nominalRole = NominalRole
   representationalRole = RepresentationalRole
   phantomRole = PhantomRole

   explicitlyKindedTypeVariable = ExplicitlyKindedTypeVariable False
   implicitlyKindedTypeVariable = ImplicitlyKindedTypeVariable False
   inferredTypeVariable = ImplicitlyKindedTypeVariable True
   inferredExplicitlyKindedTypeVariable = ExplicitlyKindedTypeVariable True
   
   constructorKind = ConstructorType
   kindVariable = TypeVariable
   functionKind = FunctionKind
   forallKind = ForallKind
   kindApplication = KindApplication
   infixKindApplication = InfixKindApplication
   groundTypeKind = GroundTypeKind
   typeEqualityConstraint = TypeEqualityConstraint
   typeConstraint = TypeConstraint
   tupleKind = TupleKind
   listKind = ListKind
   typeRepresentationKind = TypeRepresentationKind
   constraintType = ConstraintType
   multiParameterClassConstraint = ClassConstraint
   infixConstraint = InfixConstraint

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
   visibleKindKindApplication = VisibleKindKindApplication
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

   classConstraint cls t = ClassConstraint cls [t]
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

data Import λ l d s = Import Bool Bool (Maybe Text) (Abstract.ModuleName λ) (Maybe (Abstract.ModuleName λ))
                             (Maybe (s (Abstract.ImportSpecification l l d d)))

data Members λ = AllMembers
               | MemberList [Name λ]
               | ExplicitlyNamespacedMemberList [ModuleMember λ]
               deriving (Data, Eq, Show)

data ModuleMember λ = DefaultMember (Name λ)
                    | PatternMember (Name λ)
                    | TypeMember (Name λ)
                    deriving (Data, Eq, Show)

data TypeRole λ = InferredRole
                | NominalRole
                | RepresentationalRole
                | PhantomRole
                deriving (Data, Eq, Show)

data Declaration λ l d s =
   ClassDeclaration (s (Abstract.Context l l d d)) (s (Abstract.TypeLHS l l d d)) [s (Abstract.Declaration l l d d)]
   | FunDepClassDeclaration !(Abstract.SupportFor 'Extensions.FunctionalDependencies λ)
                            (s (Abstract.Context l l d d)) (s (Abstract.TypeLHS l l d d))
                            [s (Abstract.FunctionalDependency l l d d)] [s (Abstract.Declaration l l d d)]
   | DataDeclaration (s (Abstract.Context l l d d)) (s (Abstract.TypeLHS l l d d)) (Maybe (s (Abstract.Kind l l d d)))
                     [s (Abstract.DataConstructor l l d d)] [s (Abstract.DerivingClause l l d d)]
   | GADTDeclaration (s (Abstract.TypeLHS l l d d)) (Maybe (s (Abstract.Kind l l d d)))
                     [s (Abstract.GADTConstructor l l d d)] [s (Abstract.DerivingClause l l d d)]
   | DefaultDeclaration [s (Abstract.Type l l d d)]
   | DefaultMethodSignature !(Abstract.SupportFor 'Extensions.DefaultSignatures λ)
                             (Name λ) (s (Abstract.Context l l d d)) (s (Abstract.Type l l d d))
   | EquationDeclaration (s (Abstract.EquationLHS l l d d)) (s (Abstract.EquationRHS l l d d))
                         [s (Abstract.Declaration l l d d)]
   | FixityDeclaration (Associativity λ) (Maybe Int) (NonEmpty (Abstract.Name λ))
   | ForeignExport (CallingConvention λ) (Maybe Text) (Abstract.Name λ) (s (Abstract.Type l l d d))
   | ForeignImport (CallingConvention λ) (Maybe (CallSafety λ)) (Maybe Text) (Abstract.Name λ)
                   (s (Abstract.Type l l d d))
   | InstanceDeclaration [TypeVarBinding λ l d s] (s (Abstract.Context l l d d))
                         (s (Abstract.ClassInstanceLHS l l d d)) [s (Abstract.Declaration l l d d)]
   | StandaloneDerivingDeclaration !(Abstract.SupportFor 'Extensions.StandaloneDeriving λ)
                                   (s (Abstract.Context l l d d)) (s (Abstract.ClassInstanceLHS l l d d))
   | StandaloneStrategicDerivingDeclaration !(Abstract.SupportFor 'Extensions.StandaloneDeriving λ)
                                            !(Abstract.SupportFor 'Extensions.DerivingStrategies λ)
                                            (s (Abstract.DerivingStrategy l l d d))
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
   | KindSignature (Abstract.Name λ) (s (Abstract.Context l l d d)) (s (Abstract.Kind l l d d))
   | TypeRoleDeclaration (Abstract.QualifiedName λ) [Abstract.TypeRole λ]

data FunctionalDependency λ l (d :: Kind.Type -> Kind.Type) (s :: Kind.Type -> Kind.Type) =
   FunctionalDependency (NonEmpty (Abstract.Name λ)) (NonEmpty (Abstract.Name λ))

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
                     (s (Abstract.DerivingStrategy l l d d)) [Abstract.QualifiedName λ]

data DerivingStrategy λ l (d :: Kind.Type -> Kind.Type) (s :: Kind.Type -> Kind.Type) =
   Stock | AnyClass | Newtype | Via !(Abstract.SupportFor 'Extensions.DerivingVia λ) (s (Abstract.Type l l d d))

data Context λ l d s =
   ClassConstraint (Abstract.QualifiedName λ) [s (Abstract.Type l l d d)]
   | Constraints [s (Abstract.Context l l d d)]
   | InfixConstraint (s (Abstract.Type l l d d)) (Abstract.QualifiedName λ) (s (Abstract.Type l l d d))
   | TypeEqualityConstraint (s (Abstract.Type l l d d)) (s (Abstract.Type l l d d))
   | TypeConstraint (s (Abstract.Type l l d d))
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
   | TupleType (NonEmpty (s (Abstract.Type l l d d)))
   | TypeApplication (s (Abstract.Type l l d d)) (s (Abstract.Type l l d d))
   | InfixTypeApplication (s (Abstract.Type l l d d)) (Abstract.QualifiedName λ) (s (Abstract.Type l l d d))
   | TypeVariable (Abstract.Name λ)
   | ForallType [TypeVarBinding λ l d s] (s (Abstract.Type l l d d))
   | ForallKind [TypeVarBinding λ l d s] (s (Abstract.Kind l l d d))
   | ConstrainedType (s (Abstract.Context l l d d)) (s (Abstract.Type l l d d))
   | KindedType (s (Abstract.Type l l d d)) (s (Abstract.Kind l l d d))
   | TypeWildcard
   | TypeKind (s (Abstract.Type l l d d))
   | VisibleDependentType [TypeVarBinding λ l d s] (s (Abstract.Type l l d d))
   | GroundTypeKind
   | FunctionKind (s (Abstract.Kind l l d d)) (s (Abstract.Kind l l d d))
   | KindApplication (s (Abstract.Kind l l d d)) (s (Abstract.Kind l l d d))
   | InfixKindApplication (s (Abstract.Kind l l d d)) (Abstract.QualifiedName λ) (s (Abstract.Kind l l d d))
   | PromotedConstructorType (s (Abstract.Constructor l l d d))
   | PromotedTupleType (NonEmpty (s (Abstract.Type l l d d)))
   | PromotedListType [s (Abstract.Type l l d d)]
   | PromotedIntegerLiteral Integer
   | PromotedCharLiteral Char
   | PromotedStringLiteral Text
   | PromotedInfixTypeApplication (s (Abstract.Type l l d d)) (Abstract.QualifiedName λ) (s (Abstract.Type l l d d))
   | TupleKind (NonEmpty (s (Abstract.Kind l l d d)))
   | ListKind (s (Abstract.Kind l l d d))
   | TypeRepresentationKind (s (Abstract.Type l l d d))
   | ConstraintType (s (Abstract.Context l l d d))
   | VisibleKindApplication (s (Abstract.Type l l d d)) (s (Abstract.Kind l l d d))
   | VisibleKindKindApplication (s (Abstract.Kind l l d d)) (s (Abstract.Kind l l d d))

data TypeVarBinding λ l d s =
   ExplicitlyKindedTypeVariable Bool (Abstract.Name λ) (s (Abstract.Kind l l d d))
   | ImplicitlyKindedTypeVariable Bool (Abstract.Name λ)

data TypeLHS λ l d s =
   SimpleTypeLHS (Abstract.Name λ) [TypeVarBinding λ l d s]
   | SimpleTypeLHSApplication (s (Abstract.TypeLHS l l d d)) (TypeVarBinding λ l d s)

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
   | LambdaCaseExpression [s (Abstract.CaseAlternative l l d d)]
   | MultiWayIfExpression [s (Abstract.GuardedExpression l l d d)]
   | DoExpression (s (Abstract.GuardedExpression l l d d))
   | MDoExpression (s (Abstract.GuardedExpression l l d d))
   | InfixExpression (s (Abstract.Expression l l d d)) (s (Abstract.Expression l l d d))
                     (s (Abstract.Expression l l d d))
   | LeftSectionExpression (s (Abstract.Expression l l d d)) (Abstract.QualifiedName λ)
   | LambdaExpression [s (Abstract.Pattern l l d d)] (s (Abstract.Expression l l d d))
   | LetExpression [s (Abstract.Declaration l l d d)] (s (Abstract.Expression l l d d))
   | ListComprehension (s (Abstract.Expression l l d d)) (NonEmpty (s (Abstract.Statement l l d d)))
   | ParallelListComprehension (s (Abstract.Expression l l d d)) (NonEmpty (s (Abstract.Statement l l d d)))
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
   | TupleSectionExpression (NonEmpty (Maybe (s (Abstract.Expression l l d d))))
   | TypedExpression (s (Abstract.Expression l l d d)) (s (Abstract.Type l l d d))
   | VisibleTypeApplication (s (Abstract.Expression l l d d)) (s (Abstract.Type l l d d))
   | OverloadedLabel Text
   | GetField (s (Abstract.Expression l l d d)) (Abstract.Name λ)
   | FieldProjection (NonEmpty (Abstract.Name λ))
   | WildcardRecordExpression !(Abstract.SupportFor 'Extensions.RecordWildCards λ)
                               (Abstract.QualifiedName λ)
                               [s (Abstract.FieldBinding l l d d)]

data FieldBinding λ l d s =
  FieldBinding (Abstract.QualifiedName λ) (s (Abstract.Expression l l d d))
  | PunnedFieldBinding (Abstract.QualifiedName λ)

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
   | BangPattern !(Abstract.SupportFor 'Extensions.BangPatterns λ) (s (Abstract.Pattern l l d d))
   | TuplePattern (NonEmpty (s (Abstract.Pattern l l d d)))
   | VariablePattern (Abstract.Name λ)
   | WildcardPattern

data FieldPattern λ l d s =
  FieldPattern (Abstract.QualifiedName λ) (s (Abstract.Pattern l l d d))
  | PunnedFieldPattern (Abstract.QualifiedName λ)

data Statement λ l d s =
   BindStatement (s (Abstract.Pattern l l d d)) (s (Abstract.Expression l l d d))
   | ExpressionStatement (s (Abstract.Expression l l d d))
   | LetStatement [s (Abstract.Declaration l l d d)]
   | RecursiveStatement [s (Abstract.Statement l l d d)]

data Value λ l (d :: Kind.Type -> Kind.Type) (s :: Kind.Type -> Kind.Type) =
   CharLiteral Char
   | FloatingLiteral Rational
   | IntegerLiteral Integer
   | StringLiteral Text
   | HashLiteral !(Abstract.SupportFor 'Extensions.MagicHash λ) (Value λ l d s)

deriving instance Typeable (Import λ l d s)
deriving instance (Data (s (Abstract.ImportSpecification l l d d)), Data (Abstract.ModuleName λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (Import λ l d s)
deriving instance (Show (s (Abstract.ImportSpecification l l d d)), Show (Abstract.ModuleName λ)) =>
                  Show (Import λ l d s)
deriving instance (Eq (s (Abstract.ImportSpecification l l d d)), Eq (Abstract.ModuleName λ)) =>
                  Eq (Import λ l d s)

deriving instance Typeable (Declaration λ l d s)
deriving instance (Data (Abstract.SupportFor 'Extensions.StandaloneDeriving λ),
                   Data (Abstract.SupportFor 'Extensions.DerivingStrategies λ),
                   Data (Abstract.SupportFor 'Extensions.DefaultSignatures λ),
                   Data (Abstract.SupportFor 'Extensions.FunctionalDependencies λ),
                   Data (s (Abstract.Context l l d d)), Data (s (Abstract.Kind l l d d)),
                   Data (s (Abstract.FunctionalDependency l l d d)),
                   Data (s (Abstract.DataConstructor l l d d)), Data (s (Abstract.GADTConstructor l l d d)),
                   Data (s (Abstract.DerivingStrategy l l d d)),
                   Data (s (Abstract.Declaration l l d d)), Data (s (Abstract.DerivingClause l l d d)),
                   Data (s (Abstract.EquationLHS l l d d)), Data (s (Abstract.EquationRHS l l d d)),
                   Data (s (Abstract.Type l l d d)), Data (s (Abstract.TypeLHS l l d d)),
                   Data (s (Abstract.Kind l l d d)), Data (s (Abstract.ClassInstanceLHS l l d d)),
                   Data (Abstract.Name λ), Data (Abstract.QualifiedName λ), Data (Abstract.TypeRole λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (Declaration λ l d s)
deriving instance (Show (Abstract.SupportFor 'Extensions.StandaloneDeriving λ),
                   Show (Abstract.SupportFor 'Extensions.DerivingStrategies λ),
                   Show (Abstract.SupportFor 'Extensions.DefaultSignatures λ),
                   Show (Abstract.SupportFor 'Extensions.FunctionalDependencies λ),
                   Show (s (Abstract.Context l l d d)), Show (s (Abstract.Kind l l d d)),
                   Show (s (Abstract.FunctionalDependency l l d d)),
                   Show (s (Abstract.DataConstructor l l d d)), Show (s (Abstract.GADTConstructor l l d d)),
                   Show (s (Abstract.DerivingStrategy l l d d)),
                   Show (s (Abstract.Declaration l l d d)), Show (s (Abstract.DerivingClause l l d d)),
                   Show (s (Abstract.EquationLHS l l d d)), Show (s (Abstract.EquationRHS l l d d)),
                   Show (s (Abstract.Type l l d d)), Show (s (Abstract.TypeLHS l l d d)),
                   Show (s (Abstract.Kind l l d d)), Show (s (Abstract.ClassInstanceLHS l l d d)),
                   Show (Abstract.Name λ), Show (Abstract.QualifiedName λ),
                   Show (Abstract.TypeRole λ)) => Show (Declaration λ l d s)
deriving instance (Eq (Abstract.SupportFor 'Extensions.StandaloneDeriving λ),
                   Eq (Abstract.SupportFor 'Extensions.DerivingStrategies λ),
                   Eq (Abstract.SupportFor 'Extensions.DefaultSignatures λ),
                   Eq (Abstract.SupportFor 'Extensions.FunctionalDependencies λ),
                   Eq (s (Abstract.Context l l d d)), Eq (s (Abstract.Kind l l d d)),
                   Eq (s (Abstract.DataConstructor l l d d)), Eq (s (Abstract.GADTConstructor l l d d)),
                   Eq (s (Abstract.DerivingStrategy l l d d)),
                   Eq (s (Abstract.FunctionalDependency l l d d)),
                   Eq (s (Abstract.Declaration l l d d)), Eq (s (Abstract.DerivingClause l l d d)),
                   Eq (s (Abstract.EquationLHS l l d d)), Eq (s (Abstract.EquationRHS l l d d)),
                   Eq (s (Abstract.Type l l d d)), Eq (s (Abstract.TypeLHS l l d d)), Eq (s (Abstract.Kind l l d d)),
                   Eq (s (Abstract.ClassInstanceLHS l l d d)),
                   Eq (Abstract.Name λ), Eq (Abstract.QualifiedName λ),
                   Eq (Abstract.TypeRole λ)) => Eq (Declaration λ l d s)

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
                   Data (Abstract.QualifiedName λ), Data (s (Abstract.DerivingStrategy l l d d)),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (DerivingClause λ l d s)
deriving instance (Show (Abstract.SupportFor 'Extensions.DerivingStrategies λ),
                   Show (Abstract.QualifiedName λ),
                   Show (s (Abstract.DerivingStrategy l l d d))) => Show (DerivingClause λ l d s)
deriving instance (Eq (Abstract.SupportFor 'Extensions.DerivingStrategies λ),
                   Eq (Abstract.QualifiedName λ),
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
deriving instance (Data (s (Abstract.Constructor l l d d)), Data (s (Abstract.Context l l d d)),
                   Data (s (Abstract.Kind l l d d)), Data (s (Abstract.Type l l d d)),
                   Data (s (Abstract.FieldDeclaration l l d d)),
                   Data (Abstract.Name λ), Data (Abstract.QualifiedName λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (Type λ l d s)
deriving instance (Show (s (Abstract.Constructor l l d d)), Show (s (Abstract.Context l l d d)),
                   Show (s (Abstract.Kind l l d d)), Show (s (Abstract.Type l l d d)),
                   Show (s (Abstract.FieldDeclaration l l d d)),
                   Show (Abstract.Name λ), Show (Abstract.QualifiedName λ)) => Show (Type λ l d s)
deriving instance (Eq (s (Abstract.Constructor l l d d)), Eq (s (Abstract.Context l l d d)),
                   Eq (s (Abstract.Kind l l d d)), Eq (s (Abstract.Type l l d d)),
                   Eq (s (Abstract.FieldDeclaration l l d d)),
                   Eq (Abstract.Name λ), Eq (Abstract.QualifiedName λ)) => Eq (Type λ l d s)

deriving instance Typeable (TypeVarBinding λ l d s)
deriving instance (Data (s (Abstract.Kind l l d d)), Data (Abstract.Name λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (TypeVarBinding λ l d s)
deriving instance (Show (s (Abstract.Kind l l d d)), Show (Abstract.Name λ)) => Show (TypeVarBinding λ l d s)
deriving instance (Eq (s (Abstract.Kind l l d d)), Eq (Abstract.Name λ)) => Eq (TypeVarBinding λ l d s)

deriving instance Typeable (TypeLHS λ l d s)
deriving instance (Data (s (Abstract.Type l l d d)), Data (s (Abstract.TypeLHS l l d d)),
                   Data (s (Abstract.Kind l l d d)),
                   Data (Abstract.QualifiedName λ), Data (Abstract.Name λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (TypeLHS λ l d s)
deriving instance (Show (s (Abstract.Type l l d d)), Show (s (Abstract.TypeLHS l l d d)),
                   Show (s (Abstract.Kind l l d d)),
                   Show (Abstract.QualifiedName λ), Show (Abstract.Name λ)) => Show (TypeLHS λ l d s)
deriving instance (Eq (s (Abstract.Type l l d d)), Eq (s (Abstract.TypeLHS l l d d)), Eq (s (Abstract.Kind l l d d)),
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
deriving instance (Data (s (Abstract.Context l l d d)), Data (s (Abstract.Type l l d d)),
                   Data (Abstract.Name λ), Data (Abstract.QualifiedName λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (Context λ l d s)
deriving instance (Show (s (Abstract.Context l l d d)), Show (s (Abstract.Type l l d d)),
                   Show (Abstract.QualifiedName λ), Show (Abstract.Name λ)) => Show (Context λ l d s)
deriving instance (Eq (s (Abstract.Context l l d d)), Eq (s (Abstract.Type l l d d)),
                   Eq (Abstract.QualifiedName λ), Eq (Abstract.Name λ)) => Eq (Context λ l d s)

deriving instance Typeable (Expression λ l d s)
deriving instance (Data (Abstract.SupportFor 'Extensions.RecordWildCards λ),
                   Data (s (Abstract.CaseAlternative l l d d)), Data (s (Abstract.Constructor l l d d)),
                   Data (s (Abstract.Expression l l d d)), Data (s (Abstract.GuardedExpression l l d d)),
                   Data (s (Abstract.Declaration l l d d)), Data (s (Abstract.FieldBinding l l d d)),
                   Data (s (Abstract.Pattern l l d d)), Data (s (Abstract.Statement l l d d)),
                   Data (s (Abstract.Type l l d d)), Data (s (Abstract.Value l l d d)),
                   Data (Abstract.QualifiedName λ), Data (Abstract.Name λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (Expression λ l d s)
deriving instance (Show (Abstract.SupportFor 'Extensions.RecordWildCards λ),
                   Show (s (Abstract.CaseAlternative l l d d)), Show (s (Abstract.Constructor l l d d)),
                   Show (s (Abstract.Expression l l d d)), Show (s (Abstract.GuardedExpression l l d d)),
                   Show (s (Abstract.Declaration l l d d)), Show (s (Abstract.FieldBinding l l d d)),
                   Show (s (Abstract.Pattern l l d d)), Show (s (Abstract.Statement l l d d)),
                   Show (s (Abstract.Type l l d d)), Show (s (Abstract.Value l l d d)),
                   Show (Abstract.QualifiedName λ), Show (Abstract.Name λ)) => Show (Expression λ l d s)
deriving instance (Eq (Abstract.SupportFor 'Extensions.RecordWildCards λ),
                   Eq (s (Abstract.CaseAlternative l l d d)), Eq (s (Abstract.Constructor l l d d)),
                   Eq (s (Abstract.Expression l l d d)), Eq (s (Abstract.GuardedExpression l l d d)),
                   Eq (s (Abstract.Declaration l l d d)), Eq (s (Abstract.FieldBinding l l d d)),
                   Eq (s (Abstract.Pattern l l d d)), Eq (s (Abstract.Statement l l d d)),
                   Eq (s (Abstract.Type l l d d)), Eq (s (Abstract.Value l l d d)),
                   Eq (Abstract.QualifiedName λ), Eq (Abstract.Name λ)) => Eq (Expression λ l d s)

deriving instance Typeable (FieldBinding λ l d s)
deriving instance (Data (s (Abstract.Expression l l d d)), Data (Abstract.QualifiedName λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (FieldBinding λ l d s)
deriving instance (Show (s (Abstract.Expression l l d d)), Show (Abstract.QualifiedName λ)) =>
                  Show (FieldBinding λ l d s)
deriving instance (Eq (s (Abstract.Expression l l d d)), Eq (Abstract.QualifiedName λ)) =>
                  Eq (FieldBinding λ l d s)

deriving instance Typeable (Pattern λ l d s)
deriving instance (Data (Abstract.SupportFor 'Extensions.RecordWildCards λ),
                   Data (Abstract.SupportFor 'Extensions.BangPatterns λ),
                   Data (s (Abstract.Constructor l l d d)), Data (s (Abstract.FieldPattern l l d d)),
                   Data (s (Abstract.Pattern l l d d)), Data (s (Abstract.Value l l d d)),
                   Data (s (Abstract.Type l l d d)),
                   Data (Abstract.Name λ), Data (Abstract.QualifiedName λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (Pattern λ l d s)
deriving instance (Show (Abstract.SupportFor 'Extensions.RecordWildCards λ),
                   Show (Abstract.SupportFor 'Extensions.BangPatterns λ),
                   Show (s (Abstract.Constructor l l d d)), Show (s (Abstract.FieldPattern l l d d)),
                   Show (s (Abstract.Pattern l l d d)), Show (s (Abstract.Value l l d d)),
                   Show (s (Abstract.Type l l d d)),
                   Show (Abstract.QualifiedName λ), Show (Abstract.Name λ)) => Show (Pattern λ l d s)
deriving instance (Eq (Abstract.SupportFor 'Extensions.RecordWildCards λ),
                   Eq (Abstract.SupportFor 'Extensions.BangPatterns λ),
                   Eq (s (Abstract.Constructor l l d d)), Eq (s (Abstract.FieldPattern l l d d)),
                   Eq (s (Abstract.Pattern l l d d)), Eq (s (Abstract.Value l l d d)), Eq (s (Abstract.Type l l d d)),
                   Eq (Abstract.QualifiedName λ), Eq (Abstract.Name λ)) => Eq (Pattern λ l d s)

deriving instance Typeable (FieldPattern λ l d s)
deriving instance (Data (s (Abstract.Pattern l l d d)), Data (Abstract.QualifiedName λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (FieldPattern λ l d s)
deriving instance (Show (s (Abstract.Pattern l l d d)), Show (Abstract.QualifiedName λ)) => Show (FieldPattern λ l d s)
deriving instance (Eq (s (Abstract.Pattern l l d d)), Eq (Abstract.QualifiedName λ)) => Eq (FieldPattern λ l d s)

deriving instance Typeable (Statement λ l d s)
deriving instance (Data (s (Abstract.Declaration l l d d)), Data (s (Abstract.Expression l l d d)),
                   Data (s (Abstract.Pattern l l d d)), Data (s (Abstract.Statement l l d d)),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (Statement λ l d s)
deriving instance (Show (s (Abstract.Declaration l l d d)), Show (s (Abstract.Expression l l d d)),
                   Show (s (Abstract.Pattern l l d d)), Show (s (Abstract.Statement l l d d)))
                   => Show (Statement λ l d s)
deriving instance (Eq (s (Abstract.Declaration l l d d)), Eq (s (Abstract.Expression l l d d)),
                   Eq (s (Abstract.Pattern l l d d)), Eq (s (Abstract.Statement l l d d))) => Eq (Statement λ l d s)

deriving instance Typeable (Value λ l d s)
deriving instance (Data (Abstract.SupportFor 'Extensions.MagicHash λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (Value λ l d s)
deriving instance Show (Abstract.SupportFor 'Extensions.MagicHash λ) => Show (Value λ l d s)
deriving instance Eq (Abstract.SupportFor 'Extensions.MagicHash λ) => Eq (Value λ l d s)


$(concat <$>
  (forM [Rank2.TH.deriveFunctor, Rank2.TH.deriveFoldable, Rank2.TH.deriveTraversable, Rank2.TH.unsafeDeriveApply,
         Transformation.Shallow.TH.deriveAll, Transformation.Deep.TH.deriveAll] $
   \derive-> mconcat <$> mapM derive
             [''Import, ''Declaration, ''FunctionalDependency, ''DataConstructor, ''GADTConstructor,
              ''DerivingClause, ''DerivingStrategy,
              ''Type, ''TypeLHS, ''TypeVarBinding, ''ClassInstanceLHS, ''Context,
              ''Expression, ''FieldBinding, ''Pattern, ''FieldPattern, ''Statement, ''Value]))
