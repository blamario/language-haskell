{-# Language DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings,
             StandaloneDeriving, TemplateHaskell, TypeFamilies, UndecidableInstances #-}

module Language.Haskell.Extensions.AST (Language(Language), Import(..), Declaration(..), DataConstructor(..),
                                        GADTConstructor(..), Expression(..), Statement(..),
                                        ClassInstanceLHS(..), Context(..),
                                        Type(..), TypeLHS(..), TypeVarBinding(..), Value(..),
                                        module Report) where

import Control.Monad (forM)
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Data (Data, Typeable)
import Data.Text (Text)

import qualified Language.Haskell.Extensions.Abstract as Abstract
import qualified Language.Haskell.AST as Report
import Language.Haskell.AST (Module(..), EquationLHS(..), EquationRHS(..),
                             GuardedExpression(..), Pattern(..),
                             DerivingClause(..), Constructor(..),
                             FieldDeclaration(..), FieldBinding(..), FieldPattern(..), CaseAlternative(..),
                             CallingConvention(..), CallSafety(..), Associativity(..),
                             Name(..), ModuleName(..), QualifiedName(..),
                             ImportSpecification(..), ImportItem(..), Export(..), Members(..))
import qualified Rank2.TH
import qualified Transformation.Deep.TH
import qualified Transformation.Shallow.TH

data Language = Language deriving (Data, Eq, Show)

instance Abstract.ExtendedHaskell Language where
   type GADTConstructor Language = GADTConstructor Language
   type Kind Language = Type Language
   type TypeVarBinding Language = TypeVarBinding Language
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
   existentialConstructor = ExistentialConstructor
   explicitlyScopedInstanceDeclaration = InstanceDeclaration . toList
   forallType = ForallType
   kindedType = KindedType
   kindedDataDeclaration context lhs = DataDeclaration context lhs . Just
   kindedNewtypeDeclaration context lhs = NewtypeDeclaration context lhs . Just
   gadtDeclaration = GADTDeclaration
   gadtNewtypeDeclaration = GADTNewtypeDeclaration
   gadtConstructors = GADTConstructors
   recordFunctionType = RecordFunctionType

   explicitlyKindedTypeVariable = ExplicitlyKindedTypeVariable
   implicitlyKindedTypeVariable = ImplicitlyKindedTypeVariable
   constructorKind = ConstructorType
   kindVariable = TypeVariable
   functionKind = FunctionKind
   forallKind = ForallKind
   kindApplication = KindApplication
   groundTypeKind = GroundTypeKind
   typeEqualityConstraint = TypeEqualityConstraint

   dataFamilyDeclaration = DataFamilyDeclaration
   openTypeFamilyDeclaration = OpenTypeFamilyDeclaration
   closedTypeFamilyDeclaration = ClosedTypeFamilyDeclaration
   dataFamilyInstance = DataFamilyInstance
   newtypeFamilyInstance = NewtypeFamilyInstance
   gadtDataFamilyInstance = GADTDataFamilyInstance
   gadtNewtypeFamilyInstance = GADTNewtypeFamilyInstance
   typeFamilyInstance = TypeFamilyInstance
   multiParameterTypeClassInstanceLHS = MultiParameterTypeClassInstanceLHS

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
   constructorPattern = ConstructorPattern
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
   classConstraint = ClassConstraint
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

data Declaration λ l d s =
   ClassDeclaration (s (Abstract.Context l l d d)) (s (Abstract.TypeLHS l l d d)) [s (Abstract.Declaration l l d d)]
   | DataDeclaration (s (Abstract.Context l l d d)) (s (Abstract.TypeLHS l l d d)) (Maybe (s (Abstract.Kind l l d d)))
                     [s (Abstract.DataConstructor l l d d)] [s (Abstract.DerivingClause l l d d)]
   | GADTDeclaration (s (Abstract.TypeLHS l l d d)) (Maybe (s (Abstract.Kind l l d d)))
                     [s (Abstract.GADTConstructor l l d d)] [s (Abstract.DerivingClause l l d d)]
   | DefaultDeclaration [s (Abstract.Type l l d d)]
   | EquationDeclaration (s (Abstract.EquationLHS l l d d)) (s (Abstract.EquationRHS l l d d))
                         [s (Abstract.Declaration l l d d)]
   | FixityDeclaration (Associativity λ) (Maybe Int) (NonEmpty (Abstract.Name λ))
   | ForeignExport (CallingConvention λ) (Maybe Text) (Abstract.Name λ) (s (Abstract.Type l l d d))
   | ForeignImport (CallingConvention λ) (Maybe (CallSafety λ)) (Maybe Text) (Abstract.Name λ)
                   (s (Abstract.Type l l d d))
   | InstanceDeclaration [Abstract.TypeVarBinding λ l d s] (s (Abstract.Context l l d d))
                         (s (Abstract.ClassInstanceLHS l l d d)) [s (Abstract.Declaration l l d d)]
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
   | DataFamilyInstance [Abstract.TypeVarBinding λ l d s] (s (Abstract.Context l l d d))
                        (s (Abstract.ClassInstanceLHS l l d d)) [s (Abstract.DataConstructor l l d d)]
                        [s (Abstract.DerivingClause l l d d)]
   | NewtypeFamilyInstance [Abstract.TypeVarBinding λ l d s] (s (Abstract.Context l l d d))
                           (s (Abstract.ClassInstanceLHS l l d d)) (s (Abstract.DataConstructor l l d d))
                           [s (Abstract.DerivingClause l l d d)]
   | GADTDataFamilyInstance [Abstract.TypeVarBinding λ l d s] (s (Abstract.ClassInstanceLHS l l d d))
                            [s (Abstract.GADTConstructor l l d d)] [s (Abstract.DerivingClause l l d d)]
   | GADTNewtypeFamilyInstance [Abstract.TypeVarBinding λ l d s] (s (Abstract.ClassInstanceLHS l l d d))
                               (s (Abstract.GADTConstructor l l d d)) [s (Abstract.DerivingClause l l d d)]
   | TypeFamilyInstance [Abstract.TypeVarBinding λ l d s] (s (Abstract.ClassInstanceLHS l l d d))
                        (s (Abstract.Type l l d d))

data GADTConstructor λ l d s =
   GADTConstructors (NonEmpty (Abstract.Name λ)) [Abstract.TypeVarBinding λ l d s]
                    (s (Abstract.Context l l d d)) (s (Abstract.Type l l d d))

data DataConstructor λ l d s =
   Constructor (Abstract.Name λ) [s (Abstract.Type l l d d)]
   | RecordConstructor (Abstract.Name λ) [s (Abstract.FieldDeclaration l l d d)]
   | ExistentialConstructor [Abstract.TypeVarBinding λ l d s] (s (Abstract.Context l l d d)) (s (Abstract.DataConstructor l l d d))


data Context λ l d s =
   SimpleConstraint (Abstract.QualifiedName λ) (Abstract.Name λ)
   | ClassConstraint (Abstract.QualifiedName λ) (s (Abstract.Type l l d d))
   | Constraints [s (Abstract.Context l l d d)]
   | TypeEqualityConstraint (s (Abstract.Type l l d d)) (s (Abstract.Type l l d d))
   | NoContext

data Type λ l d s =
   ConstructorType (s (Abstract.Constructor l l d d))
   | FunctionConstructorType
   | FunctionType (s (Abstract.Type l l d d)) (s (Abstract.Type l l d d))
   | RecordFunctionType [s (Abstract.FieldDeclaration l l d d)] (s (Abstract.Type l l d d))
   | ListType (s (Abstract.Type l l d d))
   | StrictType (s (Abstract.Type l l d d))
   | TupleType (NonEmpty (s (Abstract.Type l l d d)))
   | TypeApplication (s (Abstract.Type l l d d)) (s (Abstract.Type l l d d))
   | InfixTypeApplication (s (Abstract.Type l l d d)) (Abstract.QualifiedName λ) (s (Abstract.Type l l d d))
   | TypeVariable (Abstract.Name λ)
   | ForallType [Abstract.TypeVarBinding λ l d s] (s (Abstract.Context l l d d)) (s (Abstract.Type l l d d))
   | ForallKind [Abstract.TypeVarBinding λ l d s] (s (Abstract.Kind l l d d))
   | KindedType (s (Abstract.Type l l d d)) (s (Abstract.Kind l l d d))
   | GroundTypeKind
   | FunctionKind (s (Abstract.Kind l l d d)) (s (Abstract.Kind l l d d))
   | KindApplication (s (Abstract.Kind l l d d)) (s (Abstract.Kind l l d d))

data TypeVarBinding λ l d s =
   ExplicitlyKindedTypeVariable (Abstract.Name λ) (s (Abstract.Kind l l d d))
   | ImplicitlyKindedTypeVariable (Abstract.Name λ)

data TypeLHS λ l d s =
   SimpleTypeLHS (Abstract.Name λ) [Abstract.TypeVarBinding λ l d s]
   | SimpleTypeLHSApplication (s (Abstract.TypeLHS l l d d)) (Abstract.TypeVarBinding λ l d s)

data ClassInstanceLHS λ l d s =
   TypeClassInstanceLHS (Abstract.QualifiedName λ) (s (Abstract.Type l l d d))
   | MultiParameterTypeClassInstanceLHS (Abstract.QualifiedName λ) [s (Abstract.Type l l d d)]

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

data Statement λ l d s =
   BindStatement (s (Abstract.Pattern l l d d)) (s (Abstract.Expression l l d d))
   | ExpressionStatement (s (Abstract.Expression l l d d))
   | LetStatement [s (Abstract.Declaration l l d d)]
   | RecursiveStatement [s (Abstract.Statement l l d d)]

data Value λ l (d :: * -> *) (s :: * -> *) =
   CharLiteral Char
   | FloatingLiteral Rational
   | IntegerLiteral Integer
   | StringLiteral Text
   | HashLiteral (Value λ l d s)
   deriving (Data, Eq, Show)

deriving instance Typeable (Import λ l d s)
deriving instance (Data (s (Abstract.ImportSpecification l l d d)), Data (Abstract.ModuleName λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (Import λ l d s)
deriving instance (Show (s (Abstract.ImportSpecification l l d d)), Show (Abstract.ModuleName λ)) =>
                  Show (Import λ l d s)
deriving instance (Eq (s (Abstract.ImportSpecification l l d d)), Eq (Abstract.ModuleName λ)) =>
                  Eq (Import λ l d s)

deriving instance Typeable (Declaration λ l d s)
deriving instance (Data (s (Abstract.Context l l d d)), Data (s (Abstract.Kind l l d d)),
                   Data (s (Abstract.DataConstructor l l d d)), Data (s (Abstract.GADTConstructor l l d d)),
                   Data (s (Abstract.Declaration l l d d)), Data (s (Abstract.DerivingClause l l d d)),
                   Data (s (Abstract.EquationLHS l l d d)), Data (s (Abstract.EquationRHS l l d d)),
                   Data (s (Abstract.Type l l d d)), Data (s (Abstract.TypeLHS l l d d)),
                   Data (Abstract.TypeVarBinding λ l d s), Data (s (Abstract.ClassInstanceLHS l l d d)),
                   Data (Abstract.Name λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (Declaration λ l d s)
deriving instance (Show (s (Abstract.Context l l d d)), Show (s (Abstract.Kind l l d d)),
                   Show (s (Abstract.DataConstructor l l d d)), Show (s (Abstract.GADTConstructor l l d d)),
                   Show (s (Abstract.Declaration l l d d)), Show (s (Abstract.DerivingClause l l d d)),
                   Show (s (Abstract.EquationLHS l l d d)), Show (s (Abstract.EquationRHS l l d d)),
                   Show (s (Abstract.Type l l d d)), Show (s (Abstract.TypeLHS l l d d)),
                   Show (Abstract.TypeVarBinding λ l d s), Show (s (Abstract.ClassInstanceLHS l l d d)),
                   Show (Abstract.Name λ)) => Show (Declaration λ l d s)
deriving instance (Eq (s (Abstract.Context l l d d)), Eq (s (Abstract.Kind l l d d)),
                   Eq (s (Abstract.DataConstructor l l d d)), Eq (s (Abstract.GADTConstructor l l d d)),
                   Eq (s (Abstract.Declaration l l d d)), Eq (s (Abstract.DerivingClause l l d d)),
                   Eq (s (Abstract.EquationLHS l l d d)), Eq (s (Abstract.EquationRHS l l d d)),
                   Eq (s (Abstract.Type l l d d)), Eq (s (Abstract.TypeLHS l l d d)),
                   Eq (Abstract.TypeVarBinding λ l d s), Eq (s (Abstract.ClassInstanceLHS l l d d)),
                   Eq (Abstract.Name λ)) => Eq (Declaration λ l d s)

deriving instance Typeable (DataConstructor λ l d s)
deriving instance (Data (s (Abstract.Context l l d d)), Data (s (Abstract.DataConstructor l l d d)),
                   Data (s (Abstract.FieldDeclaration l l d d)), Data (s (Abstract.Type l l d d)),
                   Data (Abstract.TypeVarBinding λ l d s), Data (Abstract.Name λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (DataConstructor λ l d s)
deriving instance (Show (s (Abstract.Context l l d d)), Show (s (Abstract.DataConstructor l l d d)),
                   Show (s (Abstract.FieldDeclaration l l d d)), Show (s (Abstract.Type l l d d)),
                   Show (Abstract.TypeVarBinding λ l d s),
                   Show (Abstract.Name λ), Show (Abstract.Name λ)) => Show (DataConstructor λ l d s)
deriving instance (Eq (s (Abstract.Context l l d d)), Eq (s (Abstract.DataConstructor l l d d)),
                   Eq (s (Abstract.FieldDeclaration l l d d)), Eq (s (Abstract.Type l l d d)),
                   Eq (Abstract.TypeVarBinding λ l d s),
                   Eq (Abstract.Name λ), Eq (Abstract.Name λ)) => Eq (DataConstructor λ l d s)

deriving instance Typeable (GADTConstructor λ l d s)
deriving instance (Data (s (Abstract.Context l l d d)), Data (s (Abstract.Type l l d d)), Data (Abstract.Name λ),
                   Data (Abstract.TypeVarBinding λ l d s),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (GADTConstructor λ l d s)
deriving instance (Show (s (Abstract.Context l l d d)), Show (s (Abstract.Type l l d d)),
                   Show (Abstract.TypeVarBinding λ l d s),
                   Show (Abstract.Name λ), Show (Abstract.Name λ)) => Show (GADTConstructor λ l d s)
deriving instance (Eq (s (Abstract.Context l l d d)), Eq (s (Abstract.Type l l d d)),
                   Eq (Abstract.TypeVarBinding λ l d s),
                   Eq (Abstract.Name λ), Eq (Abstract.Name λ)) => Eq (GADTConstructor λ l d s)

deriving instance Typeable (Type λ l d s)
deriving instance (Data (s (Abstract.Constructor l l d d)), Data (s (Abstract.Context l l d d)),
                   Data (s (Abstract.Kind l l d d)), Data (s (Abstract.Type l l d d)),
                   Data (s (Abstract.FieldDeclaration l l d d)),
                   Data (Abstract.TypeVarBinding λ l d s),
                   Data (Abstract.Name λ), Data (Abstract.QualifiedName λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (Type λ l d s)
deriving instance (Show (s (Abstract.Constructor l l d d)), Show (s (Abstract.Context l l d d)),
                   Show (s (Abstract.Kind l l d d)), Show (s (Abstract.Type l l d d)),
                   Show (s (Abstract.FieldDeclaration l l d d)),
                   Show (Abstract.TypeVarBinding λ l d s),
                   Show (Abstract.Name λ), Show (Abstract.QualifiedName λ)) => Show (Type λ l d s)
deriving instance (Eq (s (Abstract.Constructor l l d d)), Eq (s (Abstract.Context l l d d)),
                   Eq (s (Abstract.Kind l l d d)), Eq (s (Abstract.Type l l d d)),
                   Eq (s (Abstract.FieldDeclaration l l d d)),
                   Eq (Abstract.TypeVarBinding λ l d s),
                   Eq (Abstract.Name λ), Eq (Abstract.QualifiedName λ)) => Eq (Type λ l d s)

deriving instance Typeable (TypeVarBinding λ l d s)
deriving instance (Data (s (Abstract.Kind l l d d)), Data (Abstract.Name λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (TypeVarBinding λ l d s)
deriving instance (Show (s (Abstract.Kind l l d d)), Show (Abstract.Name λ)) => Show (TypeVarBinding λ l d s)
deriving instance (Eq (s (Abstract.Kind l l d d)), Eq (Abstract.Name λ)) => Eq (TypeVarBinding λ l d s)

deriving instance Typeable (TypeLHS λ l d s)
deriving instance (Data (Abstract.TypeVarBinding λ l d s),
                   Data (s (Abstract.Type l l d d)), Data (s (Abstract.TypeLHS l l d d)),
                   Data (Abstract.QualifiedName λ), Data (Abstract.Name λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (TypeLHS λ l d s)
deriving instance (Show (Abstract.TypeVarBinding λ l d s),
                   Show (s (Abstract.Type l l d d)), Show (s (Abstract.TypeLHS l l d d)),
                   Show (Abstract.QualifiedName λ), Show (Abstract.Name λ)) => Show (TypeLHS λ l d s)
deriving instance (Eq (Abstract.TypeVarBinding λ l d s),
                   Eq (s (Abstract.Type l l d d)), Eq (s (Abstract.TypeLHS l l d d)),
                   Eq (Abstract.QualifiedName λ), Eq (Abstract.Name λ)) => Eq (TypeLHS λ l d s)

deriving instance Typeable (ClassInstanceLHS λ l d s)
deriving instance (Data (s (Abstract.Type l l d d)), Data (Abstract.QualifiedName λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (ClassInstanceLHS λ l d s)
deriving instance (Show (s (Abstract.Type l l d d)), Show (Abstract.QualifiedName λ)) => Show (ClassInstanceLHS λ l d s)
deriving instance (Eq (s (Abstract.Type l l d d)), Eq (Abstract.QualifiedName λ)) => Eq (ClassInstanceLHS λ l d s)

deriving instance Typeable (Context λ l d s)
deriving instance (Data (s (Abstract.Context l l d d)), Data (s (Abstract.Type l l d d)),
                   Data (Abstract.Name λ), Data (Abstract.QualifiedName λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (Context λ l d s)
deriving instance (Show (s (Abstract.Context l l d d)), Show (s (Abstract.Type l l d d)),
                   Show (Abstract.QualifiedName λ), Show (Abstract.Name λ)) => Show (Context λ l d s)
deriving instance (Eq (s (Abstract.Context l l d d)), Eq (s (Abstract.Type l l d d)),
                   Eq (Abstract.QualifiedName λ), Eq (Abstract.Name λ)) => Eq (Context λ l d s)

deriving instance Typeable (Expression λ l d s)
deriving instance (Data (s (Abstract.CaseAlternative l l d d)), Data (s (Abstract.Constructor l l d d)),
                   Data (s (Abstract.Expression l l d d)), Data (s (Abstract.GuardedExpression l l d d)),
                   Data (s (Abstract.Declaration l l d d)), Data (s (Abstract.FieldBinding l l d d)),
                   Data (s (Abstract.Pattern l l d d)), Data (s (Abstract.Statement l l d d)),
                   Data (s (Abstract.Type l l d d)), Data (s (Abstract.Value l l d d)), Data (Abstract.QualifiedName λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (Expression λ l d s)
deriving instance (Show (s (Abstract.CaseAlternative l l d d)), Show (s (Abstract.Constructor l l d d)),
                   Show (s (Abstract.Expression l l d d)), Show (s (Abstract.GuardedExpression l l d d)),
                   Show (s (Abstract.Declaration l l d d)), Show (s (Abstract.FieldBinding l l d d)),
                   Show (s (Abstract.Pattern l l d d)), Show (s (Abstract.Statement l l d d)),
                   Show (s (Abstract.Type l l d d)), Show (s (Abstract.Value l l d d)),
                   Show (Abstract.QualifiedName λ)) => Show (Expression λ l d s)
deriving instance (Eq (s (Abstract.CaseAlternative l l d d)), Eq (s (Abstract.Constructor l l d d)),
                   Eq (s (Abstract.Expression l l d d)), Eq (s (Abstract.GuardedExpression l l d d)),
                   Eq (s (Abstract.Declaration l l d d)), Eq (s (Abstract.FieldBinding l l d d)),
                   Eq (s (Abstract.Pattern l l d d)), Eq (s (Abstract.Statement l l d d)),
                   Eq (s (Abstract.Type l l d d)), Eq (s (Abstract.Value l l d d)),
                   Eq (Abstract.QualifiedName λ)) => Eq (Expression λ l d s)

deriving instance Typeable (Statement λ l d s)
deriving instance (Data (s (Abstract.Declaration l l d d)), Data (s (Abstract.Expression l l d d)),
                   Data (s (Abstract.Pattern l l d d)), Data (s (Abstract.Statement l l d d)),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (Statement λ l d s)
deriving instance (Show (s (Abstract.Declaration l l d d)), Show (s (Abstract.Expression l l d d)),
                   Show (s (Abstract.Pattern l l d d)), Show (s (Abstract.Statement l l d d)))
                   => Show (Statement λ l d s)
deriving instance (Eq (s (Abstract.Declaration l l d d)), Eq (s (Abstract.Expression l l d d)),
                   Eq (s (Abstract.Pattern l l d d)), Eq (s (Abstract.Statement l l d d))) => Eq (Statement λ l d s)

$(concat <$>
  (forM [Rank2.TH.deriveFunctor, Rank2.TH.deriveFoldable, Rank2.TH.deriveTraversable, Rank2.TH.unsafeDeriveApply,
         Transformation.Shallow.TH.deriveAll, Transformation.Deep.TH.deriveAll] $
   \derive-> mconcat <$> mapM derive
             [''Import, ''Declaration, ''DataConstructor, ''GADTConstructor, ''Type, ''TypeLHS, ''TypeVarBinding,
              ''ClassInstanceLHS, ''Context, ''Expression, ''Statement, ''Value]))
