{-# Language OverloadedStrings, TypeFamilies #-}

module Language.Haskell.AST where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

import qualified Language.Haskell.Abstract as Abstract

data Language = Language

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
   type Fixity Language = Fixity Language
   type Members Language = Members Language
   type Name Language = Name Language
   type ModuleName Language = ModuleName Language
   type QualifiedName Language = QualifiedName Language

   anonymousModule = AnonymousModule
   namedModule = NamedModule

   exportClassOrType = ExportClassOrType
   exportVar = ExportVar
   reExportModule = ReExportModule

   importDeclaration = Import
   excludedImports = ImportSpecification False
   includedImports = ImportSpecification True
   importClassOrType = ImportClassOrType
   importVar = ImportVar

   allMembers = AllMembers
   memberList = MemberList

   classDeclaration = ClassDeclaration
   dataDeclaration = DataDeclaration
   defaultDeclaration = DefaultDeclaration
   equationDeclaration = EquationDeclaration
   fixityDeclaration = FixityDeclaration
   foreignExport = ForeignExport
   foreignImport = ForeignImport
   instanceDeclaration = InstanceDeclaration
   newtypeDeclaration = NewtypeDeclaration
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

   generalTypeLHS = GeneralTypeLHS
   simpleTypeLHS = SimpleTypeLHS

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

   infixNonAssociative = InfixNonAssociative
   infixLeft = InfixLeft
   infixRight = InfixRight

   cCall = CCall
   cppCall = CppCall
   dotNetCall = DotNetCall
   jvmCall = JvmCall
   stdCall = StdCall

   safeCall = SafeCall
   unsafeCall = UnsafeCall

data Module λ l d s =
   NamedModule (ModuleName λ) (Maybe [s (Abstract.Export l l d d)]) [s (Abstract.Import l l d d)]
               [s (Abstract.Declaration l l d d)]
   | AnonymousModule [s (Abstract.Import l l d d)] [s (Abstract.Declaration l l d d)]

data Declaration λ l d s =
   ClassDeclaration (s (Abstract.Context l l d d)) (s (Abstract.TypeLHS l l d d)) [s (Abstract.Declaration l l d d)]
   | DataDeclaration (s (Abstract.Context l l d d)) (s (Abstract.TypeLHS l l d d))
                     [s (Abstract.DataConstructor l l d d)] [s (Abstract.DerivingClause l l d d)]
   | DefaultDeclaration [s (Abstract.Type l l d d)]
   | EquationDeclaration (s (Abstract.EquationLHS l l d d)) (s (Abstract.EquationRHS l l d d))
                         [s (Abstract.Declaration l l d d)]
   | FixityDeclaration (Fixity λ) (Maybe Int) (NonEmpty (Abstract.Name λ))
   | ForeignExport (CallingConvention λ) (Maybe Text) (Abstract.Name λ) (s (Abstract.Type l l d d))
   | ForeignImport (CallingConvention λ) (Maybe (CallSafety λ)) (Maybe Text) (Abstract.Name λ)
                   (s (Abstract.Type l l d d))
   | InstanceDeclaration (s (Abstract.Context l l d d)) (s (Abstract.TypeLHS l l d d))
                         ([s (Abstract.Declaration l l d d)])
   | NewtypeDeclaration (s (Abstract.Context l l d d)) (s (Abstract.TypeLHS l l d d))
                        (s (Abstract.DataConstructor l l d d)) [s (Abstract.DerivingClause l l d d)]
   | TypeSynonymDeclaration (s (Abstract.TypeLHS l l d d)) (s (Abstract.Type l l d d))
   | TypeSignature (NonEmpty (Abstract.Name λ)) (s (Abstract.Context l l d d)) (s (Abstract.Type l l d d))

data Expression λ l d s =
   ApplyExpression (s (Abstract.Expression l l d d)) (s (Abstract.Expression l l d d))
   | ConditionalExpression (s (Abstract.Expression l l d d)) (s (Abstract.Expression l l d d))
                           (s (Abstract.Expression l l d d))
   | ConstructorExpression (s (Abstract.Constructor l l d d))
   | CaseExpression (s (Abstract.Expression l l d d)) [s (Abstract.CaseAlternative l l d d)]
   | DoExpression (s (Abstract.GuardedExpression l l d d))
   | InfixExpression (s (Abstract.Expression l l d d)) (s (Abstract.Expression l l d d))
                     (s (Abstract.Expression l l d d))
   | LeftSectionExpression (Abstract.QualifiedName λ) (s (Abstract.Expression l l d d))
   | LambdaExpression [s (Abstract.Pattern l l d d)] (s (Abstract.Expression l l d d))
   | LetExpression [s (Abstract.Declaration l l d d)] (s (Abstract.Expression l l d d))
   | ListComprehension (s (Abstract.Expression l l d d)) (NonEmpty (s (Abstract.Statement l l d d)))
   | ListExpression [s (Abstract.Expression l l d d)]
   | LiteralExpression (s (Abstract.Value l l d d))
   | Negate
   | RecordExpression (s (Abstract.Expression l l d d)) [s (Abstract.FieldBinding l l d d)]
   | ReferenceExpression (Abstract.QualifiedName λ)
   | RightSectionExpression (s (Abstract.Expression l l d d)) (Abstract.QualifiedName λ)
   | SequenceExpression (s (Abstract.Expression l l d d)) (Maybe (s (Abstract.Expression l l d d)))
                        (Maybe (s (Abstract.Expression l l d d)))
   | TupleExpression (NonEmpty (s (Abstract.Expression l l d d)))
   | TypedExpression (s (Abstract.Expression l l d d)) (s (Abstract.Type l l d d))

data Type λ l d s =
   ConstructorType (s (Abstract.Constructor l l d d))
   | FunctionConstructorType
   | FunctionType (s (Abstract.Type l l d d)) (s (Abstract.Type l l d d))
   | ListType (s (Abstract.Type l l d d))
   | StrictType (s (Abstract.Type l l d d))
   | TupleType (NonEmpty (s (Abstract.Type l l d d)))
   | TypeApplication (s (Abstract.Type l l d d)) (s (Abstract.Type l l d d))
   | TypeVariable (Abstract.Name λ)

data EquationLHS λ l d s =
   PrefixLHS (s (Abstract.EquationLHS l l d d)) (NonEmpty (s (Abstract.Pattern l l d d)))
   | InfixLHS (s (Abstract.Pattern l l d d)) (Abstract.Name λ) (s (Abstract.Pattern l l d d))
   | PatternLHS (s (Abstract.Pattern l l d d))
   | VariableLHS (Abstract.Name λ)

data EquationRHS λ l d s =
   GuardedRHS (NonEmpty (s (Abstract.GuardedExpression l l d d)))
   | NormalRHS (s (Abstract.Expression l l d d))

data GuardedExpression λ l d s = GuardedExpression [s (Abstract.Statement l l d d)] (s (Abstract.Expression  l l d d))

data Pattern λ l d s =
   AsPattern (Abstract.Name λ) (s (Abstract.Pattern l l d d))
   | ConstructorPattern (s (Abstract.Constructor l l d d)) [s (Abstract.Pattern l l d d)]
   | InfixPattern (s (Abstract.Pattern l l d d)) (Abstract.QualifiedName λ) (s (Abstract.Pattern l l d d))
   | IrrefutablePattern (s (Abstract.Pattern l l d d))
   | ListPattern [s (Abstract.Pattern l l d d)]
   | LiteralPattern (s (Abstract.Value l l d d))
   | RecordPattern (QualifiedName λ) [s (Abstract.FieldPattern l l d d)]
   | TuplePattern (NonEmpty (s (Abstract.Pattern l l d d)))
   | VariablePattern (Abstract.Name λ)
   | WildcardPattern

data Statement λ l d s =
   BindStatement (s (Abstract.Pattern l l d d)) (s (Abstract.Expression l l d d))
   | ExpressionStatement (s (Abstract.Expression l l d d))
   | LetStatement [s (Abstract.Declaration l l d d)]

data TypeLHS λ l d s =
   GeneralTypeLHS (QualifiedName λ) (s (Abstract.Type l l d d))
   | SimpleTypeLHS (Name λ) [Name λ]

data Import λ l d s = Import Bool (ModuleName λ) (Maybe (ModuleName λ))
                             (Maybe (s (Abstract.ImportSpecification l l d d)))

data ImportSpecification λ l d s = ImportSpecification Bool [s (Abstract.ImportItem l l d d)]

data ImportItem λ l (d :: * -> *) (s :: * -> *) =
   ImportClassOrType (Name λ) (Maybe (Members λ))
   | ImportVar (Name λ)
   deriving (Eq, Show)

data Export λ l (d :: * -> *) (s :: * -> *) =
   ExportClassOrType (QualifiedName λ) (Maybe (Members λ))
   | ExportVar (QualifiedName λ)
   | ReExportModule (ModuleName λ)

data Context λ l d s =
   SimpleConstraint (QualifiedName λ) (Name λ)
   | ClassConstraint (QualifiedName λ) (s (Abstract.Type l l d d))
   | Constraints [s (Abstract.Context l l d d)]
   | NoContext

data DataConstructor λ l d s =
   Constructor (Name λ) [s (Abstract.Type l l d d)]
   | RecordConstructor (Name λ) [s (Abstract.FieldDeclaration l l d d)]

data DerivingClause λ l (d :: * -> *) (s :: * -> *) = SimpleDerive (QualifiedName λ)
                                                      deriving (Eq, Show)

data FieldDeclaration λ l d s = ConstructorFields (NonEmpty (Name λ)) (s (Abstract.Type l l d d))
data FieldBinding λ l d s = FieldBinding (QualifiedName λ) (s (Abstract.Expression l l d d))
data FieldPattern λ l d s = FieldPattern (QualifiedName λ) (s (Abstract.Pattern l l d d))

data CaseAlternative λ l d s =
   CaseAlternative (s (Abstract.Pattern l l d d)) (s (Abstract.EquationRHS l l d d)) [s (Abstract.Declaration l l d d)]

data Constructor λ l (d :: * -> *) (s :: * -> *) =
   ConstructorReference (QualifiedName λ)
   | EmptyListConstructor
   | TupleConstructor Int
   | UnitConstructor
   deriving (Eq, Show)

data Value λ l (d :: * -> *) (s :: * -> *) =
   CharLiteral Char
   | FloatingLiteral Rational
   | IntegerLiteral Integer
   | StringLiteral Text
   deriving (Eq, Show)

data CallingConvention λ = CCall | CppCall | DotNetCall | JvmCall | StdCall deriving (Eq, Show)
data CallSafety λ = SafeCall | UnsafeCall deriving (Eq, Show)

data Fixity λ = InfixNonAssociative | InfixLeft | InfixRight deriving (Eq, Show)

data Members λ = AllMembers
               | MemberList [Name λ]
               deriving (Eq, Show)

newtype Name λ = Name Text deriving (Eq, Show)

newtype ModuleName λ = ModuleName (NonEmpty (Name λ)) deriving (Eq, Show)

data QualifiedName λ = QualifiedName (Maybe (ModuleName λ)) (Name λ) deriving (Eq, Show)
