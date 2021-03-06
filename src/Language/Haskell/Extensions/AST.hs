{-# Language DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings,
             StandaloneDeriving, TemplateHaskell, TypeFamilies, UndecidableInstances #-}

module Language.Haskell.Extensions.AST (Language(Language), Import(..), Members(..), ModuleMember(..),
                                        Declaration(..), DataConstructor(..),
                                        GADTConstructor(..), Expression(..), Pattern(..), Statement(..),
                                        ClassInstanceLHS(..), Context(..),
                                        Type(..), TypeLHS(..), TypeVarBinding(..), TypeRole(..), Value(..),
                                        module Report) where

import Control.Monad (forM)
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Data (Data, Typeable)
import Data.Text (Text)

import qualified Language.Haskell.Extensions.Abstract as Abstract
import qualified Language.Haskell.AST as Report
import Language.Haskell.AST (Module(..), EquationLHS(..), EquationRHS(..),
                             GuardedExpression(..), DerivingClause(..), Constructor(..),
                             FieldDeclaration(..), FieldBinding(..), FieldPattern(..), CaseAlternative(..),
                             CallingConvention(..), CallSafety(..), Associativity(..),
                             Name(..), ModuleName(..), QualifiedName(..),
                             ImportSpecification(..), ImportItem(..), Export(..))
import qualified Rank2.TH
import qualified Transformation.Deep.TH
import qualified Transformation.Shallow.TH

data Language = Language deriving (Data, Eq, Show)

instance Abstract.ExtendedHaskell Language where
   type GADTConstructor Language = GADTConstructor Language
   type Kind Language = Type Language
   type TypeVarBinding Language = TypeVarBinding Language
   type ModuleMember Language = ModuleMember Language
   type TypeRole Language = TypeRole Language
   hashLiteral = HashLiteral
   mdoExpression = MDoExpression
   parallelListComprehension = ParallelListComprehension
   tupleSectionExpression = TupleSectionExpression
   lambdaCaseExpression = LambdaCaseExpression
   multiWayIfExpression = MultiWayIfExpression
   recursiveStatement = RecursiveStatement
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

   simpleConstraint = SimpleConstraint
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

data Import ?? l d s = Import Bool Bool (Maybe Text) (Abstract.ModuleName ??) (Maybe (Abstract.ModuleName ??))
                             (Maybe (s (Abstract.ImportSpecification l l d d)))

data Members ?? = AllMembers
               | MemberList [Name ??]
               | ExplicitlyNamespacedMemberList [ModuleMember ??]
               deriving (Data, Eq, Show)

data ModuleMember ?? = DefaultMember (Name ??)
                    | PatternMember (Name ??)
                    | TypeMember (Name ??)
                    deriving (Data, Eq, Show)

data TypeRole ?? = InferredRole
                | NominalRole
                | RepresentationalRole
                | PhantomRole
                deriving (Data, Eq, Show)

data Declaration ?? l d s =
   ClassDeclaration (s (Abstract.Context l l d d)) (s (Abstract.TypeLHS l l d d)) [s (Abstract.Declaration l l d d)]
   | DataDeclaration (s (Abstract.Context l l d d)) (s (Abstract.TypeLHS l l d d)) (Maybe (s (Abstract.Kind l l d d)))
                     [s (Abstract.DataConstructor l l d d)] [s (Abstract.DerivingClause l l d d)]
   | GADTDeclaration (s (Abstract.TypeLHS l l d d)) (Maybe (s (Abstract.Kind l l d d)))
                     [s (Abstract.GADTConstructor l l d d)] [s (Abstract.DerivingClause l l d d)]
   | DefaultDeclaration [s (Abstract.Type l l d d)]
   | EquationDeclaration (s (Abstract.EquationLHS l l d d)) (s (Abstract.EquationRHS l l d d))
                         [s (Abstract.Declaration l l d d)]
   | FixityDeclaration (Associativity ??) (Maybe Int) (NonEmpty (Abstract.Name ??))
   | ForeignExport (CallingConvention ??) (Maybe Text) (Abstract.Name ??) (s (Abstract.Type l l d d))
   | ForeignImport (CallingConvention ??) (Maybe (CallSafety ??)) (Maybe Text) (Abstract.Name ??)
                   (s (Abstract.Type l l d d))
   | InstanceDeclaration [Abstract.TypeVarBinding ?? l d s] (s (Abstract.Context l l d d))
                         (s (Abstract.ClassInstanceLHS l l d d)) [s (Abstract.Declaration l l d d)]
   | NewtypeDeclaration (s (Abstract.Context l l d d)) (s (Abstract.TypeLHS l l d d))
                        (Maybe (s (Abstract.Kind l l d d))) (s (Abstract.DataConstructor l l d d))
                        [s (Abstract.DerivingClause l l d d)]
   | GADTNewtypeDeclaration (s (Abstract.TypeLHS l l d d)) (Maybe (s (Abstract.Kind l l d d)))
                            (s (Abstract.GADTConstructor l l d d)) [s (Abstract.DerivingClause l l d d)]
   | TypeSynonymDeclaration (s (Abstract.TypeLHS l l d d)) (s (Abstract.Type l l d d))
   | TypeSignature (NonEmpty (Abstract.Name ??)) (s (Abstract.Context l l d d)) (s (Abstract.Type l l d d))
   | DataFamilyDeclaration (s (Abstract.TypeLHS l l d d)) (Maybe (s (Abstract.Kind l l d d)))
   | OpenTypeFamilyDeclaration (s (Abstract.TypeLHS l l d d)) (Maybe (s (Abstract.Kind l l d d)))
   | ClosedTypeFamilyDeclaration (s (Abstract.TypeLHS l l d d)) (Maybe (s (Abstract.Kind l l d d)))
                                 [s (Abstract.Declaration l l d d)]
   | InjectiveOpenTypeFamilyDeclaration (s (Abstract.TypeLHS l l d d)) (Abstract.TypeVarBinding ?? l d s)
                                        (Maybe (Abstract.Name ??, NonEmpty (Abstract.Name ??)))
   | InjectiveClosedTypeFamilyDeclaration (s (Abstract.TypeLHS l l d d)) (Abstract.TypeVarBinding ?? l d s)
                                          (Maybe (Abstract.Name ??, NonEmpty (Abstract.Name ??)))
                                          [s (Abstract.Declaration l l d d)]
   | DataFamilyInstance [Abstract.TypeVarBinding ?? l d s] (s (Abstract.Context l l d d))
                        (s (Abstract.ClassInstanceLHS l l d d)) (Maybe (s (Abstract.Kind l l d d)))
                        [s (Abstract.DataConstructor l l d d)]
                        [s (Abstract.DerivingClause l l d d)]
   | NewtypeFamilyInstance [Abstract.TypeVarBinding ?? l d s] (s (Abstract.Context l l d d))
                           (s (Abstract.ClassInstanceLHS l l d d)) (Maybe (s (Abstract.Kind l l d d)))
                           (s (Abstract.DataConstructor l l d d)) [s (Abstract.DerivingClause l l d d)]
   | GADTDataFamilyInstance [Abstract.TypeVarBinding ?? l d s] (s (Abstract.ClassInstanceLHS l l d d))
                            (Maybe (s (Abstract.Kind l l d d)))
                            [s (Abstract.GADTConstructor l l d d)] [s (Abstract.DerivingClause l l d d)]
   | GADTNewtypeFamilyInstance [Abstract.TypeVarBinding ?? l d s] (s (Abstract.ClassInstanceLHS l l d d))
                               (Maybe (s (Abstract.Kind l l d d)))
                               (s (Abstract.GADTConstructor l l d d)) [s (Abstract.DerivingClause l l d d)]
   | TypeFamilyInstance [Abstract.TypeVarBinding ?? l d s] (s (Abstract.ClassInstanceLHS l l d d))
                        (s (Abstract.Type l l d d))
   | KindSignature (Abstract.Name ??) (s (Abstract.Context l l d d)) (s (Abstract.Kind l l d d))
   | TypeRoleDeclaration (Abstract.QualifiedName ??) [Abstract.TypeRole ??]

data GADTConstructor ?? l d s =
   GADTConstructors (NonEmpty (Abstract.Name ??)) [Abstract.TypeVarBinding ?? l d s]
                    (s (Abstract.Context l l d d)) (s (Abstract.Type l l d d))

data DataConstructor ?? l d s =
   Constructor (Abstract.Name ??) [s (Abstract.Type l l d d)]
   | RecordConstructor (Abstract.Name ??) [s (Abstract.FieldDeclaration l l d d)]
   | ExistentialConstructor [Abstract.TypeVarBinding ?? l d s] (s (Abstract.Context l l d d)) (s (Abstract.DataConstructor l l d d))


data Context ?? l d s =
   SimpleConstraint (Abstract.QualifiedName ??) (Abstract.Name ??)
   | ClassConstraint (Abstract.QualifiedName ??) [s (Abstract.Type l l d d)]
   | Constraints [s (Abstract.Context l l d d)]
   | InfixConstraint (s (Abstract.Type l l d d)) (Abstract.QualifiedName ??) (s (Abstract.Type l l d d))
   | TypeEqualityConstraint (s (Abstract.Type l l d d)) (s (Abstract.Type l l d d))
   | NoContext

data Type ?? l d s =
   ConstructorType (s (Abstract.Constructor l l d d))
   | FunctionConstructorType
   | FunctionType (s (Abstract.Type l l d d)) (s (Abstract.Type l l d d))
   | RecordFunctionType [s (Abstract.FieldDeclaration l l d d)] (s (Abstract.Type l l d d))
   | ListType (s (Abstract.Type l l d d))
   | StrictType (s (Abstract.Type l l d d))
   | TupleType (NonEmpty (s (Abstract.Type l l d d)))
   | TypeApplication (s (Abstract.Type l l d d)) (s (Abstract.Type l l d d))
   | InfixTypeApplication (s (Abstract.Type l l d d)) (Abstract.QualifiedName ??) (s (Abstract.Type l l d d))
   | TypeVariable (Abstract.Name ??)
   | ForallType [Abstract.TypeVarBinding ?? l d s] (s (Abstract.Type l l d d))
   | ForallKind [Abstract.TypeVarBinding ?? l d s] (s (Abstract.Kind l l d d))
   | ConstrainedType (s (Abstract.Context l l d d)) (s (Abstract.Type l l d d))
   | KindedType (s (Abstract.Type l l d d)) (s (Abstract.Kind l l d d))
   | TypeWildcard
   | TypeKind (s (Abstract.Type l l d d))
   | VisibleDependentType [Abstract.TypeVarBinding ?? l d s] (s (Abstract.Type l l d d))
   | GroundTypeKind
   | FunctionKind (s (Abstract.Kind l l d d)) (s (Abstract.Kind l l d d))
   | KindApplication (s (Abstract.Kind l l d d)) (s (Abstract.Kind l l d d))
   | InfixKindApplication (s (Abstract.Kind l l d d)) (Abstract.QualifiedName ??) (s (Abstract.Kind l l d d))
   | PromotedConstructorType (s (Abstract.Constructor l l d d))
   | PromotedTupleType (NonEmpty (s (Abstract.Type l l d d)))
   | PromotedListType [s (Abstract.Type l l d d)]
   | PromotedIntegerLiteral Integer
   | PromotedCharLiteral Char
   | PromotedStringLiteral Text
   | PromotedInfixTypeApplication (s (Abstract.Type l l d d)) (Abstract.QualifiedName ??) (s (Abstract.Type l l d d))
   | TupleKind (NonEmpty (s (Abstract.Kind l l d d)))
   | ListKind (s (Abstract.Kind l l d d))
   | TypeRepresentationKind (s (Abstract.Type l l d d))
   | ConstraintType (s (Abstract.Context l l d d))
   | VisibleKindApplication (s (Abstract.Type l l d d)) (s (Abstract.Kind l l d d))
   | VisibleKindKindApplication (s (Abstract.Kind l l d d)) (s (Abstract.Kind l l d d))

data TypeVarBinding ?? l d s =
   ExplicitlyKindedTypeVariable Bool (Abstract.Name ??) (s (Abstract.Kind l l d d))
   | ImplicitlyKindedTypeVariable Bool (Abstract.Name ??)

data TypeLHS ?? l d s =
   SimpleTypeLHS (Abstract.Name ??) [Abstract.TypeVarBinding ?? l d s]
   | SimpleTypeLHSApplication (s (Abstract.TypeLHS l l d d)) (Abstract.TypeVarBinding ?? l d s)

data ClassInstanceLHS ?? l d s =
   TypeClassInstanceLHS (Abstract.QualifiedName ??) (s (Abstract.Type l l d d))
   | ClassReferenceInstanceLHS (Abstract.QualifiedName ??)
   | InfixTypeClassInstanceLHS (s (Abstract.Type l l d d)) (Abstract.QualifiedName ??) (s (Abstract.Type l l d d))
   | ClassInstanceLHSApplication (s (Abstract.ClassInstanceLHS l l d d)) (s (Abstract.Type l l d d))
   | ClassInstanceLHSKindApplication (s (Abstract.ClassInstanceLHS l l d d)) (s (Abstract.Kind l l d d))

data Expression ?? l d s =
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
   | LeftSectionExpression (s (Abstract.Expression l l d d)) (Abstract.QualifiedName ??)
   | LambdaExpression [s (Abstract.Pattern l l d d)] (s (Abstract.Expression l l d d))
   | LetExpression [s (Abstract.Declaration l l d d)] (s (Abstract.Expression l l d d))
   | ListComprehension (s (Abstract.Expression l l d d)) (NonEmpty (s (Abstract.Statement l l d d)))
   | ParallelListComprehension (s (Abstract.Expression l l d d)) (NonEmpty (s (Abstract.Statement l l d d)))
                               (NonEmpty (s (Abstract.Statement l l d d))) [NonEmpty (s (Abstract.Statement l l d d))]
   | ListExpression [s (Abstract.Expression l l d d)]
   | LiteralExpression (s (Abstract.Value l l d d))
   | Negate
   | RecordExpression (s (Abstract.Expression l l d d)) [s (Abstract.FieldBinding l l d d)]
   | ReferenceExpression (Abstract.QualifiedName ??)
   | RightSectionExpression (Abstract.QualifiedName ??) (s (Abstract.Expression l l d d))
   | SequenceExpression (s (Abstract.Expression l l d d)) (Maybe (s (Abstract.Expression l l d d)))
                        (Maybe (s (Abstract.Expression l l d d)))
   | TupleExpression (NonEmpty (s (Abstract.Expression l l d d)))
   | TupleSectionExpression (NonEmpty (Maybe (s (Abstract.Expression l l d d))))
   | TypedExpression (s (Abstract.Expression l l d d)) (s (Abstract.Type l l d d))
   | VisibleTypeApplication (s (Abstract.Expression l l d d)) (s (Abstract.Type l l d d))

data Pattern ?? l d s =
   AsPattern (Abstract.Name ??) (s (Abstract.Pattern l l d d))
   | ConstructorPattern (s (Abstract.Constructor l l d d)) [s (Abstract.Type l l d d)] [s (Abstract.Pattern l l d d)]
   | InfixPattern (s (Abstract.Pattern l l d d)) (Abstract.QualifiedName ??) (s (Abstract.Pattern l l d d))
   | IrrefutablePattern (s (Abstract.Pattern l l d d))
   | ListPattern [s (Abstract.Pattern l l d d)]
   | LiteralPattern (s (Abstract.Value l l d d))
   | RecordPattern (Abstract.QualifiedName ??) [s (Abstract.FieldPattern l l d d)]
   | TuplePattern (NonEmpty (s (Abstract.Pattern l l d d)))
   | VariablePattern (Abstract.Name ??)
   | WildcardPattern

data Statement ?? l d s =
   BindStatement (s (Abstract.Pattern l l d d)) (s (Abstract.Expression l l d d))
   | ExpressionStatement (s (Abstract.Expression l l d d))
   | LetStatement [s (Abstract.Declaration l l d d)]
   | RecursiveStatement [s (Abstract.Statement l l d d)]

data Value ?? l (d :: * -> *) (s :: * -> *) =
   CharLiteral Char
   | FloatingLiteral Rational
   | IntegerLiteral Integer
   | StringLiteral Text
   | HashLiteral (Value ?? l d s)
   deriving (Data, Eq, Show)

deriving instance Typeable (Import ?? l d s)
deriving instance (Data (s (Abstract.ImportSpecification l l d d)), Data (Abstract.ModuleName ??),
                   Data ??, Typeable l, Typeable d, Typeable s) => Data (Import ?? l d s)
deriving instance (Show (s (Abstract.ImportSpecification l l d d)), Show (Abstract.ModuleName ??)) =>
                  Show (Import ?? l d s)
deriving instance (Eq (s (Abstract.ImportSpecification l l d d)), Eq (Abstract.ModuleName ??)) =>
                  Eq (Import ?? l d s)

deriving instance Typeable (Declaration ?? l d s)
deriving instance (Data (s (Abstract.Context l l d d)), Data (s (Abstract.Kind l l d d)),
                   Data (s (Abstract.DataConstructor l l d d)), Data (s (Abstract.GADTConstructor l l d d)),
                   Data (s (Abstract.Declaration l l d d)), Data (s (Abstract.DerivingClause l l d d)),
                   Data (s (Abstract.EquationLHS l l d d)), Data (s (Abstract.EquationRHS l l d d)),
                   Data (s (Abstract.Type l l d d)), Data (s (Abstract.TypeLHS l l d d)),
                   Data (Abstract.TypeVarBinding ?? l d s), Data (s (Abstract.ClassInstanceLHS l l d d)),
                   Data (Abstract.Name ??), Data (Abstract.QualifiedName ??), Data (Abstract.TypeRole ??),
                   Data ??, Typeable l, Typeable d, Typeable s) => Data (Declaration ?? l d s)
deriving instance (Show (s (Abstract.Context l l d d)), Show (s (Abstract.Kind l l d d)),
                   Show (s (Abstract.DataConstructor l l d d)), Show (s (Abstract.GADTConstructor l l d d)),
                   Show (s (Abstract.Declaration l l d d)), Show (s (Abstract.DerivingClause l l d d)),
                   Show (s (Abstract.EquationLHS l l d d)), Show (s (Abstract.EquationRHS l l d d)),
                   Show (s (Abstract.Type l l d d)), Show (s (Abstract.TypeLHS l l d d)),
                   Show (Abstract.TypeVarBinding ?? l d s), Show (s (Abstract.ClassInstanceLHS l l d d)),
                   Show (Abstract.Name ??), Show (Abstract.QualifiedName ??),
                   Show (Abstract.TypeRole ??)) => Show (Declaration ?? l d s)
deriving instance (Eq (s (Abstract.Context l l d d)), Eq (s (Abstract.Kind l l d d)),
                   Eq (s (Abstract.DataConstructor l l d d)), Eq (s (Abstract.GADTConstructor l l d d)),
                   Eq (s (Abstract.Declaration l l d d)), Eq (s (Abstract.DerivingClause l l d d)),
                   Eq (s (Abstract.EquationLHS l l d d)), Eq (s (Abstract.EquationRHS l l d d)),
                   Eq (s (Abstract.Type l l d d)), Eq (s (Abstract.TypeLHS l l d d)),
                   Eq (Abstract.TypeVarBinding ?? l d s), Eq (s (Abstract.ClassInstanceLHS l l d d)),
                   Eq (Abstract.Name ??), Eq (Abstract.QualifiedName ??),
                   Eq (Abstract.TypeRole ??)) => Eq (Declaration ?? l d s)

deriving instance Typeable (DataConstructor ?? l d s)
deriving instance (Data (s (Abstract.Context l l d d)), Data (s (Abstract.DataConstructor l l d d)),
                   Data (s (Abstract.FieldDeclaration l l d d)), Data (s (Abstract.Type l l d d)),
                   Data (Abstract.TypeVarBinding ?? l d s), Data (Abstract.Name ??),
                   Data ??, Typeable l, Typeable d, Typeable s) => Data (DataConstructor ?? l d s)
deriving instance (Show (s (Abstract.Context l l d d)), Show (s (Abstract.DataConstructor l l d d)),
                   Show (s (Abstract.FieldDeclaration l l d d)), Show (s (Abstract.Type l l d d)),
                   Show (Abstract.TypeVarBinding ?? l d s),
                   Show (Abstract.Name ??), Show (Abstract.Name ??)) => Show (DataConstructor ?? l d s)
deriving instance (Eq (s (Abstract.Context l l d d)), Eq (s (Abstract.DataConstructor l l d d)),
                   Eq (s (Abstract.FieldDeclaration l l d d)), Eq (s (Abstract.Type l l d d)),
                   Eq (Abstract.TypeVarBinding ?? l d s),
                   Eq (Abstract.Name ??), Eq (Abstract.Name ??)) => Eq (DataConstructor ?? l d s)

deriving instance Typeable (GADTConstructor ?? l d s)
deriving instance (Data (s (Abstract.Context l l d d)), Data (s (Abstract.Type l l d d)), Data (Abstract.Name ??),
                   Data (Abstract.TypeVarBinding ?? l d s),
                   Data ??, Typeable l, Typeable d, Typeable s) => Data (GADTConstructor ?? l d s)
deriving instance (Show (s (Abstract.Context l l d d)), Show (s (Abstract.Type l l d d)),
                   Show (Abstract.TypeVarBinding ?? l d s),
                   Show (Abstract.Name ??), Show (Abstract.Name ??)) => Show (GADTConstructor ?? l d s)
deriving instance (Eq (s (Abstract.Context l l d d)), Eq (s (Abstract.Type l l d d)),
                   Eq (Abstract.TypeVarBinding ?? l d s),
                   Eq (Abstract.Name ??), Eq (Abstract.Name ??)) => Eq (GADTConstructor ?? l d s)

deriving instance Typeable (Type ?? l d s)
deriving instance (Data (s (Abstract.Constructor l l d d)), Data (s (Abstract.Context l l d d)),
                   Data (s (Abstract.Kind l l d d)), Data (s (Abstract.Type l l d d)),
                   Data (s (Abstract.FieldDeclaration l l d d)),
                   Data (Abstract.TypeVarBinding ?? l d s),
                   Data (Abstract.Name ??), Data (Abstract.QualifiedName ??),
                   Data ??, Typeable l, Typeable d, Typeable s) => Data (Type ?? l d s)
deriving instance (Show (s (Abstract.Constructor l l d d)), Show (s (Abstract.Context l l d d)),
                   Show (s (Abstract.Kind l l d d)), Show (s (Abstract.Type l l d d)),
                   Show (s (Abstract.FieldDeclaration l l d d)),
                   Show (Abstract.TypeVarBinding ?? l d s),
                   Show (Abstract.Name ??), Show (Abstract.QualifiedName ??)) => Show (Type ?? l d s)
deriving instance (Eq (s (Abstract.Constructor l l d d)), Eq (s (Abstract.Context l l d d)),
                   Eq (s (Abstract.Kind l l d d)), Eq (s (Abstract.Type l l d d)),
                   Eq (s (Abstract.FieldDeclaration l l d d)),
                   Eq (Abstract.TypeVarBinding ?? l d s),
                   Eq (Abstract.Name ??), Eq (Abstract.QualifiedName ??)) => Eq (Type ?? l d s)

deriving instance Typeable (TypeVarBinding ?? l d s)
deriving instance (Data (s (Abstract.Kind l l d d)), Data (Abstract.Name ??),
                   Data ??, Typeable l, Typeable d, Typeable s) => Data (TypeVarBinding ?? l d s)
deriving instance (Show (s (Abstract.Kind l l d d)), Show (Abstract.Name ??)) => Show (TypeVarBinding ?? l d s)
deriving instance (Eq (s (Abstract.Kind l l d d)), Eq (Abstract.Name ??)) => Eq (TypeVarBinding ?? l d s)

deriving instance Typeable (TypeLHS ?? l d s)
deriving instance (Data (Abstract.TypeVarBinding ?? l d s),
                   Data (s (Abstract.Type l l d d)), Data (s (Abstract.TypeLHS l l d d)),
                   Data (Abstract.QualifiedName ??), Data (Abstract.Name ??),
                   Data ??, Typeable l, Typeable d, Typeable s) => Data (TypeLHS ?? l d s)
deriving instance (Show (Abstract.TypeVarBinding ?? l d s),
                   Show (s (Abstract.Type l l d d)), Show (s (Abstract.TypeLHS l l d d)),
                   Show (Abstract.QualifiedName ??), Show (Abstract.Name ??)) => Show (TypeLHS ?? l d s)
deriving instance (Eq (Abstract.TypeVarBinding ?? l d s),
                   Eq (s (Abstract.Type l l d d)), Eq (s (Abstract.TypeLHS l l d d)),
                   Eq (Abstract.QualifiedName ??), Eq (Abstract.Name ??)) => Eq (TypeLHS ?? l d s)

deriving instance Typeable (ClassInstanceLHS ?? l d s)
deriving instance (Data (s (Abstract.ClassInstanceLHS l l d d)), Data (s (Abstract.Type l l d d)),
                   Data (s (Abstract.Kind l l d d)), Data (Abstract.QualifiedName ??),
                   Data ??, Typeable l, Typeable d, Typeable s) => Data (ClassInstanceLHS ?? l d s)
deriving instance (Show (s (Abstract.ClassInstanceLHS l l d d)), Show (s (Abstract.Type l l d d)),
                   Show (s (Abstract.Kind l l d d)), Show (Abstract.QualifiedName ??)) => Show (ClassInstanceLHS ?? l d s)
deriving instance (Eq (s (Abstract.ClassInstanceLHS l l d d)), Eq (s (Abstract.Type l l d d)),
                   Eq (s (Abstract.Kind l l d d)), Eq (Abstract.QualifiedName ??)) => Eq (ClassInstanceLHS ?? l d s)

deriving instance Typeable (Context ?? l d s)
deriving instance (Data (s (Abstract.Context l l d d)), Data (s (Abstract.Type l l d d)),
                   Data (Abstract.Name ??), Data (Abstract.QualifiedName ??),
                   Data ??, Typeable l, Typeable d, Typeable s) => Data (Context ?? l d s)
deriving instance (Show (s (Abstract.Context l l d d)), Show (s (Abstract.Type l l d d)),
                   Show (Abstract.QualifiedName ??), Show (Abstract.Name ??)) => Show (Context ?? l d s)
deriving instance (Eq (s (Abstract.Context l l d d)), Eq (s (Abstract.Type l l d d)),
                   Eq (Abstract.QualifiedName ??), Eq (Abstract.Name ??)) => Eq (Context ?? l d s)

deriving instance Typeable (Expression ?? l d s)
deriving instance (Data (s (Abstract.CaseAlternative l l d d)), Data (s (Abstract.Constructor l l d d)),
                   Data (s (Abstract.Expression l l d d)), Data (s (Abstract.GuardedExpression l l d d)),
                   Data (s (Abstract.Declaration l l d d)), Data (s (Abstract.FieldBinding l l d d)),
                   Data (s (Abstract.Pattern l l d d)), Data (s (Abstract.Statement l l d d)),
                   Data (s (Abstract.Type l l d d)), Data (s (Abstract.Value l l d d)), Data (Abstract.QualifiedName ??),
                   Data ??, Typeable l, Typeable d, Typeable s) => Data (Expression ?? l d s)
deriving instance (Show (s (Abstract.CaseAlternative l l d d)), Show (s (Abstract.Constructor l l d d)),
                   Show (s (Abstract.Expression l l d d)), Show (s (Abstract.GuardedExpression l l d d)),
                   Show (s (Abstract.Declaration l l d d)), Show (s (Abstract.FieldBinding l l d d)),
                   Show (s (Abstract.Pattern l l d d)), Show (s (Abstract.Statement l l d d)),
                   Show (s (Abstract.Type l l d d)), Show (s (Abstract.Value l l d d)),
                   Show (Abstract.QualifiedName ??)) => Show (Expression ?? l d s)
deriving instance (Eq (s (Abstract.CaseAlternative l l d d)), Eq (s (Abstract.Constructor l l d d)),
                   Eq (s (Abstract.Expression l l d d)), Eq (s (Abstract.GuardedExpression l l d d)),
                   Eq (s (Abstract.Declaration l l d d)), Eq (s (Abstract.FieldBinding l l d d)),
                   Eq (s (Abstract.Pattern l l d d)), Eq (s (Abstract.Statement l l d d)),
                   Eq (s (Abstract.Type l l d d)), Eq (s (Abstract.Value l l d d)),
                   Eq (Abstract.QualifiedName ??)) => Eq (Expression ?? l d s)

deriving instance Typeable (Pattern ?? l d s)
deriving instance (Data (s (Abstract.Constructor l l d d)), Data (s (Abstract.FieldPattern l l d d)),
                   Data (s (Abstract.Pattern l l d d)), Data (s (Abstract.Value l l d d)),
                   Data (s (Abstract.Type l l d d)),
                   Data (Abstract.Name ??), Data (Abstract.QualifiedName ??),
                   Data ??, Typeable l, Typeable d, Typeable s) => Data (Pattern ?? l d s)
deriving instance (Show (s (Abstract.Constructor l l d d)), Show (s (Abstract.FieldPattern l l d d)),
                   Show (s (Abstract.Pattern l l d d)), Show (s (Abstract.Value l l d d)),
                   Show (s (Abstract.Type l l d d)),
                   Show (Abstract.QualifiedName ??), Show (Abstract.Name ??)) => Show (Pattern ?? l d s)
deriving instance (Eq (s (Abstract.Constructor l l d d)), Eq (s (Abstract.FieldPattern l l d d)),
                   Eq (s (Abstract.Pattern l l d d)), Eq (s (Abstract.Value l l d d)), Eq (s (Abstract.Type l l d d)),
                   Eq (Abstract.QualifiedName ??), Eq (Abstract.Name ??)) => Eq (Pattern ?? l d s)

deriving instance Typeable (Statement ?? l d s)
deriving instance (Data (s (Abstract.Declaration l l d d)), Data (s (Abstract.Expression l l d d)),
                   Data (s (Abstract.Pattern l l d d)), Data (s (Abstract.Statement l l d d)),
                   Data ??, Typeable l, Typeable d, Typeable s) => Data (Statement ?? l d s)
deriving instance (Show (s (Abstract.Declaration l l d d)), Show (s (Abstract.Expression l l d d)),
                   Show (s (Abstract.Pattern l l d d)), Show (s (Abstract.Statement l l d d)))
                   => Show (Statement ?? l d s)
deriving instance (Eq (s (Abstract.Declaration l l d d)), Eq (s (Abstract.Expression l l d d)),
                   Eq (s (Abstract.Pattern l l d d)), Eq (s (Abstract.Statement l l d d))) => Eq (Statement ?? l d s)

$(concat <$>
  (forM [Rank2.TH.deriveFunctor, Rank2.TH.deriveFoldable, Rank2.TH.deriveTraversable, Rank2.TH.unsafeDeriveApply,
         Transformation.Shallow.TH.deriveAll, Transformation.Deep.TH.deriveAll] $
   \derive-> mconcat <$> mapM derive
             [''Import, ''Declaration, ''DataConstructor, ''GADTConstructor, ''Type, ''TypeLHS, ''TypeVarBinding,
              ''ClassInstanceLHS, ''Context, ''Expression, ''Pattern, ''Statement, ''Value]))
