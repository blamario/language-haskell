{-# Language DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings,
             StandaloneDeriving, TemplateHaskell, TypeFamilies, UndecidableInstances #-}

module Language.Haskell.AST where

import Control.Monad (forM)
import Data.List.NonEmpty (NonEmpty)
import Data.Data (Data, Typeable)
import Data.Text (Text)

import qualified Language.Haskell.Abstract as Abstract
import qualified Rank2.TH
import qualified Transformation.Deep.TH
import qualified Transformation.Shallow.TH

data Language = Language deriving (Data, Eq, Show)

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
   type Associativity Language = Associativity Language
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

data Module λ l d s =
   NamedModule (Abstract.ModuleName λ) (Maybe [s (Abstract.Export l l d d)]) [s (Abstract.Import l l d d)]
               [s (Abstract.Declaration l l d d)]
   | AnonymousModule [s (Abstract.Import l l d d)] [s (Abstract.Declaration l l d d)]

data Declaration λ l d s =
   ClassDeclaration (s (Abstract.Context l l d d)) (s (Abstract.TypeLHS l l d d)) [s (Abstract.Declaration l l d d)]
   | DataDeclaration (s (Abstract.Context l l d d)) (s (Abstract.TypeLHS l l d d))
                     [s (Abstract.DataConstructor l l d d)] [s (Abstract.DerivingClause l l d d)]
   | DefaultDeclaration [s (Abstract.Type l l d d)]
   | EquationDeclaration (s (Abstract.EquationLHS l l d d)) (s (Abstract.EquationRHS l l d d))
                         [s (Abstract.Declaration l l d d)]
   | FixityDeclaration (Associativity λ) (Maybe Int) (NonEmpty (Abstract.Name λ))
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
   | LeftSectionExpression (s (Abstract.Expression l l d d)) (Abstract.QualifiedName λ)
   | LambdaExpression [s (Abstract.Pattern l l d d)] (s (Abstract.Expression l l d d))
   | LetExpression [s (Abstract.Declaration l l d d)] (s (Abstract.Expression l l d d))
   | ListComprehension (s (Abstract.Expression l l d d)) (NonEmpty (s (Abstract.Statement l l d d)))
   | ListExpression [s (Abstract.Expression l l d d)]
   | LiteralExpression (s (Abstract.Value l l d d))
   | Negate
   | RecordExpression (s (Abstract.Expression l l d d)) [s (Abstract.FieldBinding l l d d)]
   | ReferenceExpression (Abstract.QualifiedName λ)
   | RightSectionExpression (Abstract.QualifiedName λ) (s (Abstract.Expression l l d d))
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
   | RecordPattern (Abstract.QualifiedName λ) [s (Abstract.FieldPattern l l d d)]
   | TuplePattern (NonEmpty (s (Abstract.Pattern l l d d)))
   | VariablePattern (Abstract.Name λ)
   | WildcardPattern

data Statement λ l d s =
   BindStatement (s (Abstract.Pattern l l d d)) (s (Abstract.Expression l l d d))
   | ExpressionStatement (s (Abstract.Expression l l d d))
   | LetStatement [s (Abstract.Declaration l l d d)]

data TypeLHS λ l d s =
   GeneralTypeLHS (Abstract.QualifiedName λ) (s (Abstract.Type l l d d))
   | SimpleTypeLHS (Abstract.Name λ) [Abstract.Name λ]

data Import λ l d s = Import Bool (Abstract.ModuleName λ) (Maybe (Abstract.ModuleName λ))
                             (Maybe (s (Abstract.ImportSpecification l l d d)))

data ImportSpecification λ l d s = ImportSpecification Bool [s (Abstract.ImportItem l l d d)]

data ImportItem λ l (d :: * -> *) (s :: * -> *) =
   ImportClassOrType (Abstract.Name λ) (Maybe (Abstract.Members λ))
   | ImportVar (Abstract.Name λ)

data Export λ l (d :: * -> *) (s :: * -> *) =
   ExportClassOrType (Abstract.QualifiedName λ) (Maybe (Abstract.Members λ))
   | ExportVar (Abstract.QualifiedName λ)
   | ReExportModule (Abstract.ModuleName λ)

data Context λ l d s =
   SimpleConstraint (Abstract.QualifiedName λ) (Abstract.Name λ)
   | ClassConstraint (Abstract.QualifiedName λ) (s (Abstract.Type l l d d))
   | Constraints [s (Abstract.Context l l d d)]
   | NoContext

data DataConstructor λ l d s =
   Constructor (Abstract.Name λ) [s (Abstract.Type l l d d)]
   | RecordConstructor (Abstract.Name λ) [s (Abstract.FieldDeclaration l l d d)]

data DerivingClause λ l (d :: * -> *) (s :: * -> *) = SimpleDerive (Abstract.QualifiedName λ)

data FieldDeclaration λ l d s = ConstructorFields (NonEmpty (Abstract.Name λ)) (s (Abstract.Type l l d d))
data FieldBinding λ l d s = FieldBinding (Abstract.QualifiedName λ) (s (Abstract.Expression l l d d))
data FieldPattern λ l d s = FieldPattern (Abstract.QualifiedName λ) (s (Abstract.Pattern l l d d))

data CaseAlternative λ l d s =
   CaseAlternative (s (Abstract.Pattern l l d d)) (s (Abstract.EquationRHS l l d d)) [s (Abstract.Declaration l l d d)]

data Constructor λ l (d :: * -> *) (s :: * -> *) =
   ConstructorReference (Abstract.QualifiedName λ)
   | EmptyListConstructor
   | TupleConstructor Int
   | UnitConstructor

data Value λ l (d :: * -> *) (s :: * -> *) =
   CharLiteral Char
   | FloatingLiteral Rational
   | IntegerLiteral Integer
   | StringLiteral Text
   deriving (Data, Eq, Show)

deriving instance Typeable (Module λ l d s)
deriving instance (Data (s (Abstract.Declaration l l d d)),
                   Data (s (Abstract.Export l l d d)), Data (s (Abstract.Import l l d d)),
                   Data (Abstract.ModuleName λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (Module λ l d s)
deriving instance (Show (s (Abstract.Declaration l l d d)),
                   Show (s (Abstract.Export l l d d)), Show (s (Abstract.Import l l d d)),
                   Show (Abstract.ModuleName λ)) => Show (Module λ l d s)
deriving instance (Eq (s (Abstract.Declaration l l d d)),
                   Eq (s (Abstract.Export l l d d)), Eq (s (Abstract.Import l l d d)),
                   Eq (Abstract.ModuleName λ)) => Eq (Module λ l d s)

deriving instance Typeable (Declaration λ l d s)
deriving instance (Data (s (Abstract.Context l l d d)), Data (s (Abstract.DataConstructor l l d d)),
                   Data (s (Abstract.Declaration l l d d)), Data (s (Abstract.DerivingClause l l d d)),
                   Data (s (Abstract.EquationLHS l l d d)), Data (s (Abstract.EquationRHS l l d d)),
                   Data (s (Abstract.Type l l d d)), Data (s (Abstract.TypeLHS l l d d)),
                   Data (Abstract.Name λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (Declaration λ l d s)
deriving instance (Show (s (Abstract.Context l l d d)), Show (s (Abstract.DataConstructor l l d d)),
                   Show (s (Abstract.Declaration l l d d)), Show (s (Abstract.DerivingClause l l d d)),
                   Show (s (Abstract.EquationLHS l l d d)), Show (s (Abstract.EquationRHS l l d d)),
                   Show (s (Abstract.Type l l d d)), Show (s (Abstract.TypeLHS l l d d)),
                   Show (Abstract.Name λ)) => Show (Declaration λ l d s)
deriving instance (Eq (s (Abstract.Context l l d d)), Eq (s (Abstract.DataConstructor l l d d)),
                   Eq (s (Abstract.Declaration l l d d)), Eq (s (Abstract.DerivingClause l l d d)),
                   Eq (s (Abstract.EquationLHS l l d d)), Eq (s (Abstract.EquationRHS l l d d)),
                   Eq (s (Abstract.Type l l d d)), Eq (s (Abstract.TypeLHS l l d d)),
                   Eq (Abstract.Name λ)) => Eq (Declaration λ l d s)

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

deriving instance Typeable (Type λ l d s)
deriving instance (Data (s (Abstract.Constructor l l d d)), Data (s (Abstract.Type l l d d)), Data (Abstract.Name λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (Type λ l d s)
deriving instance (Show (s (Abstract.Constructor l l d d)), Show (s (Abstract.Type l l d d)),
                   Show (Abstract.Name λ)) => Show (Type λ l d s)
deriving instance (Eq (s (Abstract.Constructor l l d d)), Eq (s (Abstract.Type l l d d)),
                   Eq (Abstract.Name λ)) => Eq (Type λ l d s)

deriving instance Typeable (EquationLHS λ l d s)
deriving instance (Data (s (Abstract.EquationLHS l l d d)), Data (s (Abstract.Pattern l l d d)), Data (Abstract.Name λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (EquationLHS λ l d s)
deriving instance (Show (s (Abstract.EquationLHS l l d d)), Show (s (Abstract.Pattern l l d d)),
                   Show (Abstract.Name λ)) => Show (EquationLHS λ l d s)
deriving instance (Eq (s (Abstract.EquationLHS l l d d)), Eq (s (Abstract.Pattern l l d d)),
                   Eq (Abstract.Name λ)) => Eq (EquationLHS λ l d s)

deriving instance Typeable (EquationRHS λ l d s)
deriving instance (Data (s (Abstract.GuardedExpression l l d d)), Data (s (Abstract.Expression l l d d)),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (EquationRHS λ l d s)
deriving instance (Show (s (Abstract.GuardedExpression l l d d)), Show (s (Abstract.Expression l l d d))) =>
                  Show (EquationRHS λ l d s)
deriving instance (Eq (s (Abstract.GuardedExpression l l d d)), Eq (s (Abstract.Expression l l d d))) =>
                  Eq (EquationRHS λ l d s)

deriving instance Typeable (GuardedExpression λ l d s)
deriving instance (Data (s (Abstract.Statement l l d d)), Data (s (Abstract.Expression l l d d)),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (GuardedExpression λ l d s)
deriving instance (Show (s (Abstract.Statement l l d d)), Show (s (Abstract.Expression l l d d))) =>
                  Show (GuardedExpression λ l d s)
deriving instance (Eq (s (Abstract.Statement l l d d)), Eq (s (Abstract.Expression l l d d))) =>
                  Eq (GuardedExpression λ l d s)

deriving instance Typeable (Export λ l d s)
deriving instance (Data (Abstract.Members λ), Data (Abstract.ModuleName λ), Data (Abstract.QualifiedName λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (Export λ l d s)
deriving instance (Show (Abstract.Members λ),
                   Show (Abstract.ModuleName λ), Show (Abstract.QualifiedName λ)) => Show (Export λ l d s)
deriving instance (Eq (Abstract.Members λ),
                   Eq (Abstract.ModuleName λ), Eq (Abstract.QualifiedName λ)) => Eq (Export λ l d s)

deriving instance Typeable (DerivingClause λ l d s)
deriving instance (Data (Abstract.QualifiedName λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (DerivingClause λ l d s)
deriving instance Show (Abstract.QualifiedName λ) => Show (DerivingClause λ l d s)
deriving instance Eq (Abstract.QualifiedName λ) => Eq (DerivingClause λ l d s)

deriving instance Typeable (Pattern λ l d s)
deriving instance (Data (s (Abstract.Constructor l l d d)), Data (s (Abstract.FieldPattern l l d d)),
                   Data (s (Abstract.Pattern l l d d)), Data (s (Abstract.Value l l d d)),
                   Data (Abstract.Name λ), Data (Abstract.QualifiedName λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (Pattern λ l d s)
deriving instance (Show (s (Abstract.Constructor l l d d)), Show (s (Abstract.FieldPattern l l d d)),
                   Show (s (Abstract.Pattern l l d d)), Show (s (Abstract.Value l l d d)),
                   Show (Abstract.QualifiedName λ), Show (Abstract.Name λ)) => Show (Pattern λ l d s)
deriving instance (Eq (s (Abstract.Constructor l l d d)), Eq (s (Abstract.FieldPattern l l d d)),
                   Eq (s (Abstract.Pattern l l d d)), Eq (s (Abstract.Value l l d d)),
                   Eq (Abstract.QualifiedName λ), Eq (Abstract.Name λ)) => Eq (Pattern λ l d s)

deriving instance Typeable (Statement λ l d s)
deriving instance (Data (s (Abstract.Declaration l l d d)), Data (s (Abstract.Expression l l d d)),
                   Data (s (Abstract.Pattern l l d d)),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (Statement λ l d s)
deriving instance (Show (s (Abstract.Declaration l l d d)), Show (s (Abstract.Expression l l d d)),
                   Show (s (Abstract.Pattern l l d d))) => Show (Statement λ l d s)
deriving instance (Eq (s (Abstract.Declaration l l d d)), Eq (s (Abstract.Expression l l d d)),
                   Eq (s (Abstract.Pattern l l d d))) => Eq (Statement λ l d s)

deriving instance Typeable (TypeLHS λ l d s)
deriving instance (Data (s (Abstract.Type l l d d)), Data (Abstract.QualifiedName λ), Data (Abstract.Name λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (TypeLHS λ l d s)
deriving instance (Show (s (Abstract.Type l l d d)),
                   Show (Abstract.QualifiedName λ), Show (Abstract.Name λ)) => Show (TypeLHS λ l d s)
deriving instance (Eq (s (Abstract.Type l l d d)),
                   Eq (Abstract.QualifiedName λ), Eq (Abstract.Name λ)) => Eq (TypeLHS λ l d s)

deriving instance Typeable (Import λ l d s)
deriving instance (Data (s (Abstract.ImportSpecification l l d d)), Data (Abstract.ModuleName λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (Import λ l d s)
deriving instance (Show (s (Abstract.ImportSpecification l l d d)), Show (Abstract.ModuleName λ)) =>
                  Show (Import λ l d s)
deriving instance (Eq (s (Abstract.ImportSpecification l l d d)), Eq (Abstract.ModuleName λ)) =>
                  Eq (Import λ l d s)

deriving instance Typeable (ImportSpecification λ l d s)
deriving instance (Data (s (Abstract.ImportItem l l d d)),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (ImportSpecification λ l d s)
deriving instance (Show (s (Abstract.ImportItem l l d d))) => Show (ImportSpecification λ l d s)
deriving instance (Eq (s (Abstract.ImportItem l l d d))) => Eq (ImportSpecification λ l d s)

deriving instance Typeable (ImportItem λ l d s)
deriving instance (Data (Abstract.Members λ), Data (Abstract.Name λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (ImportItem λ l d s)
deriving instance (Show (Abstract.Members λ), Show (Abstract.Name λ)) => Show (ImportItem λ l d s)
deriving instance (Eq (Abstract.Members λ), Eq (Abstract.Name λ)) => Eq (ImportItem λ l d s)

deriving instance Typeable (Context λ l d s)
deriving instance (Data (s (Abstract.Context l l d d)), Data (s (Abstract.Type l l d d)),
                   Data (Abstract.Name λ), Data (Abstract.QualifiedName λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (Context λ l d s)
deriving instance (Show (s (Abstract.Context l l d d)), Show (s (Abstract.Type l l d d)),
                   Show (Abstract.QualifiedName λ), Show (Abstract.Name λ)) => Show (Context λ l d s)
deriving instance (Eq (s (Abstract.Context l l d d)), Eq (s (Abstract.Type l l d d)),
                   Eq (Abstract.QualifiedName λ), Eq (Abstract.Name λ)) => Eq (Context λ l d s)

deriving instance Typeable (DataConstructor λ l d s)
deriving instance (Data (s (Abstract.FieldDeclaration l l d d)), Data (s (Abstract.Type l l d d)),
                   Data (Abstract.Name λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (DataConstructor λ l d s)
deriving instance (Show (s (Abstract.FieldDeclaration l l d d)), Show (s (Abstract.Type l l d d)),
                   Show (Abstract.Name λ), Show (Abstract.Name λ)) => Show (DataConstructor λ l d s)
deriving instance (Eq (s (Abstract.FieldDeclaration l l d d)), Eq (s (Abstract.Type l l d d)),
                   Eq (Abstract.Name λ), Eq (Abstract.Name λ)) => Eq (DataConstructor λ l d s)

deriving instance Typeable (FieldBinding λ l d s)
deriving instance (Data (s (Abstract.Expression l l d d)), Data (Abstract.QualifiedName λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (FieldBinding λ l d s)
deriving instance (Show (s (Abstract.Expression l l d d)), Show (Abstract.QualifiedName λ)) =>
                  Show (FieldBinding λ l d s)
deriving instance (Eq (s (Abstract.Expression l l d d)), Eq (Abstract.QualifiedName λ)) =>
                  Eq (FieldBinding λ l d s)

deriving instance Typeable (FieldDeclaration λ l d s)
deriving instance (Data (s (Abstract.Type l l d d)), Data (Abstract.Name λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (FieldDeclaration λ l d s)
deriving instance (Show (s (Abstract.Type l l d d)), Show (Abstract.Name λ)) => Show (FieldDeclaration λ l d s)
deriving instance (Eq (s (Abstract.Type l l d d)), Eq (Abstract.Name λ)) => Eq (FieldDeclaration λ l d s)

deriving instance Typeable (FieldPattern λ l d s)
deriving instance (Data (s (Abstract.Pattern l l d d)), Data (Abstract.QualifiedName λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (FieldPattern λ l d s)
deriving instance (Show (s (Abstract.Pattern l l d d)), Show (Abstract.QualifiedName λ)) => Show (FieldPattern λ l d s)
deriving instance (Eq (s (Abstract.Pattern l l d d)), Eq (Abstract.QualifiedName λ)) => Eq (FieldPattern λ l d s)

deriving instance Typeable (CaseAlternative λ l d s)
deriving instance (Data (s (Abstract.Pattern l l d d)), Data (s (Abstract.EquationRHS l l d d)),
                   Data (s (Abstract.Declaration l l d d)),
                   Typeable λ, Typeable l, Typeable d, Typeable s) => Data (CaseAlternative λ l d s)
deriving instance (Show (s (Abstract.Pattern l l d d)), Show (s (Abstract.EquationRHS l l d d)),
                   Show (s (Abstract.Declaration l l d d))) => Show (CaseAlternative λ l d s)
deriving instance (Eq (s (Abstract.Pattern l l d d)), Eq (s (Abstract.EquationRHS l l d d)),
                   Eq (s (Abstract.Declaration l l d d))) => Eq (CaseAlternative λ l d s)

deriving instance Data (Abstract.QualifiedName λ) => Typeable (Constructor λ l d s)
deriving instance (Data (Abstract.QualifiedName λ),
                   Data λ, Typeable l, Typeable d, Typeable s) => Data (Constructor λ l d s)
deriving instance Show (Abstract.QualifiedName λ) => Show (Constructor λ l d s)
deriving instance Eq (Abstract.QualifiedName λ) => Eq (Constructor λ l d s)

data CallingConvention λ = CCall | CppCall | DotNetCall | JvmCall | StdCall deriving (Data, Eq, Show)
data CallSafety λ = SafeCall | UnsafeCall deriving (Data, Eq, Show)

data Associativity λ = NonAssociative | LeftAssociative | RightAssociative deriving (Data, Eq, Show)

data Members λ = AllMembers
               | MemberList [Name λ]
               deriving (Data, Eq, Show)

newtype Name λ = Name Text deriving (Data, Eq, Ord, Show)

data QualifiedName λ = QualifiedName (Maybe (Abstract.ModuleName λ)) (Abstract.Name λ)

newtype ModuleName λ = ModuleName (NonEmpty (Abstract.Name λ))
deriving instance Data (Abstract.Name λ) => Typeable (ModuleName λ)
deriving instance (Data (Abstract.Name λ), Data λ) => Data (ModuleName λ)
deriving instance Show (Abstract.Name λ) => Show (ModuleName λ)
deriving instance Eq (Abstract.Name λ) => Eq (ModuleName λ)
deriving instance Ord (Abstract.Name λ) => Ord (ModuleName λ)

deriving instance (Data (Abstract.ModuleName λ), Data (Abstract.Name λ)) => Typeable (QualifiedName λ)
deriving instance (Data (Abstract.ModuleName λ), Data (Abstract.Name λ), Data λ) => Data (QualifiedName λ)
deriving instance (Show (Abstract.ModuleName λ), Show (Abstract.Name λ)) => Show (QualifiedName λ)
deriving instance (Eq (Abstract.ModuleName λ), Eq (Abstract.Name λ)) => Eq (QualifiedName λ)
deriving instance (Ord (Abstract.ModuleName λ), Ord (Abstract.Name λ)) => Ord (QualifiedName λ)

$(concat <$>
  (forM [Rank2.TH.deriveFunctor, Rank2.TH.deriveFoldable, Rank2.TH.deriveTraversable, Rank2.TH.unsafeDeriveApply,
         Transformation.Shallow.TH.deriveAll, Transformation.Deep.TH.deriveAll] $
   \derive-> mconcat <$> mapM derive
             [''Module, ''Export, ''Import, ''ImportItem, ''ImportSpecification,
              ''Declaration, ''DerivingClause, ''EquationLHS, ''EquationRHS, ''TypeLHS, ''Type, ''Context,
              ''Expression, ''Value, ''Pattern, ''GuardedExpression, ''Statement,
              ''FieldBinding, ''FieldDeclaration, ''FieldPattern, ''CaseAlternative, ''Constructor, ''DataConstructor]))
