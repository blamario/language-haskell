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

import Language.Haskell.Extensions (ExtensionSwitch)

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
   type ClassInstanceLHS Language = ClassInstanceLHS Language

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

   typeClassInstanceLHS = TypeClassInstanceLHS
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

data Module ?? l d s =
   NamedModule (Abstract.ModuleName ??) (Maybe [s (Abstract.Export l l d d)]) [s (Abstract.Import l l d d)]
               [s (Abstract.Declaration l l d d)]
   | AnonymousModule [s (Abstract.Import l l d d)] [s (Abstract.Declaration l l d d)]
   | ExtendedModule [ExtensionSwitch] (s (Abstract.Module l l d d))

data Declaration ?? l d s =
   ClassDeclaration (s (Abstract.Context l l d d)) (s (Abstract.TypeLHS l l d d)) [s (Abstract.Declaration l l d d)]
   | DataDeclaration (s (Abstract.Context l l d d)) (s (Abstract.TypeLHS l l d d))
                     [s (Abstract.DataConstructor l l d d)] [s (Abstract.DerivingClause l l d d)]
   | DefaultDeclaration [s (Abstract.Type l l d d)]
   | EquationDeclaration (s (Abstract.EquationLHS l l d d)) (s (Abstract.EquationRHS l l d d))
                         [s (Abstract.Declaration l l d d)]
   | FixityDeclaration (Associativity ??) (Maybe Int) (NonEmpty (Abstract.Name ??))
   | ForeignExport (CallingConvention ??) (Maybe Text) (Abstract.Name ??) (s (Abstract.Type l l d d))
   | ForeignImport (CallingConvention ??) (Maybe (CallSafety ??)) (Maybe Text) (Abstract.Name ??)
                   (s (Abstract.Type l l d d))
   | InstanceDeclaration (s (Abstract.Context l l d d)) (s (Abstract.ClassInstanceLHS l l d d))
                         ([s (Abstract.Declaration l l d d)])
   | NewtypeDeclaration (s (Abstract.Context l l d d)) (s (Abstract.TypeLHS l l d d))
                        (s (Abstract.DataConstructor l l d d)) [s (Abstract.DerivingClause l l d d)]
   | TypeSynonymDeclaration (s (Abstract.TypeLHS l l d d)) (s (Abstract.Type l l d d))
   | TypeSignature (NonEmpty (Abstract.Name ??)) (s (Abstract.Context l l d d)) (s (Abstract.Type l l d d))

data Expression ?? l d s =
   ApplyExpression (s (Abstract.Expression l l d d)) (s (Abstract.Expression l l d d))
   | ConditionalExpression (s (Abstract.Expression l l d d)) (s (Abstract.Expression l l d d))
                           (s (Abstract.Expression l l d d))
   | ConstructorExpression (s (Abstract.Constructor l l d d))
   | CaseExpression (s (Abstract.Expression l l d d)) [s (Abstract.CaseAlternative l l d d)]
   | DoExpression (s (Abstract.GuardedExpression l l d d))
   | InfixExpression (s (Abstract.Expression l l d d)) (s (Abstract.Expression l l d d))
                     (s (Abstract.Expression l l d d))
   | LeftSectionExpression (s (Abstract.Expression l l d d)) (Abstract.QualifiedName ??)
   | LambdaExpression [s (Abstract.Pattern l l d d)] (s (Abstract.Expression l l d d))
   | LetExpression [s (Abstract.Declaration l l d d)] (s (Abstract.Expression l l d d))
   | ListComprehension (s (Abstract.Expression l l d d)) (NonEmpty (s (Abstract.Statement l l d d)))
   | ListExpression [s (Abstract.Expression l l d d)]
   | LiteralExpression (s (Abstract.Value l l d d))
   | Negate
   | RecordExpression (s (Abstract.Expression l l d d)) [s (Abstract.FieldBinding l l d d)]
   | ReferenceExpression (Abstract.QualifiedName ??)
   | RightSectionExpression (Abstract.QualifiedName ??) (s (Abstract.Expression l l d d))
   | SequenceExpression (s (Abstract.Expression l l d d)) (Maybe (s (Abstract.Expression l l d d)))
                        (Maybe (s (Abstract.Expression l l d d)))
   | TupleExpression (NonEmpty (s (Abstract.Expression l l d d)))
   | TypedExpression (s (Abstract.Expression l l d d)) (s (Abstract.Type l l d d))

data Type ?? l d s =
   ConstructorType (s (Abstract.Constructor l l d d))
   | FunctionConstructorType
   | FunctionType (s (Abstract.Type l l d d)) (s (Abstract.Type l l d d))
   | ListType (s (Abstract.Type l l d d))
   | StrictType (s (Abstract.Type l l d d))
   | TupleType (NonEmpty (s (Abstract.Type l l d d)))
   | TypeApplication (s (Abstract.Type l l d d)) (s (Abstract.Type l l d d))
   | TypeVariable (Abstract.Name ??)

data EquationLHS ?? l d s =
   PrefixLHS (s (Abstract.EquationLHS l l d d)) (NonEmpty (s (Abstract.Pattern l l d d)))
   | InfixLHS (s (Abstract.Pattern l l d d)) (Abstract.Name ??) (s (Abstract.Pattern l l d d))
   | PatternLHS (s (Abstract.Pattern l l d d))
   | VariableLHS (Abstract.Name ??)

data EquationRHS ?? l d s =
   GuardedRHS (NonEmpty (s (Abstract.GuardedExpression l l d d)))
   | NormalRHS (s (Abstract.Expression l l d d))

data GuardedExpression ?? l d s = GuardedExpression [s (Abstract.Statement l l d d)] (s (Abstract.Expression  l l d d))

data Pattern ?? l d s =
   AsPattern (Abstract.Name ??) (s (Abstract.Pattern l l d d))
   | ConstructorPattern (s (Abstract.Constructor l l d d)) [s (Abstract.Pattern l l d d)]
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

data ClassInstanceLHS ?? l d s =
   TypeClassInstanceLHS (Abstract.QualifiedName ??) (s (Abstract.Type l l d d))

data TypeLHS ?? l (d :: * -> *) (s :: * -> *) =
   SimpleTypeLHS (Abstract.Name ??) [Abstract.Name ??]

data Import ?? l d s = Import Bool (Abstract.ModuleName ??) (Maybe (Abstract.ModuleName ??))
                             (Maybe (s (Abstract.ImportSpecification l l d d)))

data ImportSpecification ?? l d s = ImportSpecification Bool [s (Abstract.ImportItem l l d d)]

data ImportItem ?? l (d :: * -> *) (s :: * -> *) =
   ImportClassOrType (Abstract.Name ??) (Maybe (Abstract.Members ??))
   | ImportVar (Abstract.Name ??)

data Export ?? l (d :: * -> *) (s :: * -> *) =
   ExportClassOrType (Abstract.QualifiedName ??) (Maybe (Abstract.Members ??))
   | ExportVar (Abstract.QualifiedName ??)
   | ReExportModule (Abstract.ModuleName ??)

data Context ?? l d s =
   SimpleConstraint (Abstract.QualifiedName ??) (Abstract.Name ??)
   | ClassConstraint (Abstract.QualifiedName ??) (s (Abstract.Type l l d d))
   | Constraints [s (Abstract.Context l l d d)]
   | NoContext

data DataConstructor ?? l d s =
   Constructor (Abstract.Name ??) [s (Abstract.Type l l d d)]
   | RecordConstructor (Abstract.Name ??) [s (Abstract.FieldDeclaration l l d d)]

data DerivingClause ?? l (d :: * -> *) (s :: * -> *) = SimpleDerive (Abstract.QualifiedName ??)

data FieldDeclaration ?? l d s = ConstructorFields (NonEmpty (Abstract.Name ??)) (s (Abstract.Type l l d d))
data FieldBinding ?? l d s = FieldBinding (Abstract.QualifiedName ??) (s (Abstract.Expression l l d d))
data FieldPattern ?? l d s = FieldPattern (Abstract.QualifiedName ??) (s (Abstract.Pattern l l d d))

data CaseAlternative ?? l d s =
   CaseAlternative (s (Abstract.Pattern l l d d)) (s (Abstract.EquationRHS l l d d)) [s (Abstract.Declaration l l d d)]

data Constructor ?? l (d :: * -> *) (s :: * -> *) =
   ConstructorReference (Abstract.QualifiedName ??)
   | EmptyListConstructor
   | TupleConstructor Int
   | UnitConstructor

data Value ?? l (d :: * -> *) (s :: * -> *) =
   CharLiteral Char
   | FloatingLiteral Rational
   | IntegerLiteral Integer
   | StringLiteral Text
   deriving (Data, Eq, Show)

deriving instance Typeable (Module ?? l d s)
deriving instance (Data (s (Abstract.Module l l d d)), Data (s (Abstract.Declaration l l d d)),
                   Data (s (Abstract.Export l l d d)), Data (s (Abstract.Import l l d d)),
                   Data (Abstract.ModuleName ??),
                   Data ??, Typeable l, Typeable d, Typeable s) => Data (Module ?? l d s)
deriving instance (Show (s (Abstract.Module l l d d)), Show (s (Abstract.Declaration l l d d)),
                   Show (s (Abstract.Export l l d d)), Show (s (Abstract.Import l l d d)),
                   Show (Abstract.ModuleName ??)) => Show (Module ?? l d s)
deriving instance (Eq (s (Abstract.Module l l d d)), Eq (s (Abstract.Declaration l l d d)),
                   Eq (s (Abstract.Export l l d d)), Eq (s (Abstract.Import l l d d)),
                   Eq (Abstract.ModuleName ??)) => Eq (Module ?? l d s)

deriving instance Typeable (Declaration ?? l d s)
deriving instance (Data (s (Abstract.Context l l d d)), Data (s (Abstract.DataConstructor l l d d)),
                   Data (s (Abstract.Declaration l l d d)), Data (s (Abstract.DerivingClause l l d d)),
                   Data (s (Abstract.EquationLHS l l d d)), Data (s (Abstract.EquationRHS l l d d)),
                   Data (s (Abstract.Type l l d d)), Data (s (Abstract.TypeLHS l l d d)),
                   Data (s (Abstract.ClassInstanceLHS l l d d)),
                   Data (Abstract.Name ??),
                   Data ??, Typeable l, Typeable d, Typeable s) => Data (Declaration ?? l d s)
deriving instance (Show (s (Abstract.Context l l d d)), Show (s (Abstract.DataConstructor l l d d)),
                   Show (s (Abstract.Declaration l l d d)), Show (s (Abstract.DerivingClause l l d d)),
                   Show (s (Abstract.EquationLHS l l d d)), Show (s (Abstract.EquationRHS l l d d)),
                   Show (s (Abstract.Type l l d d)), Show (s (Abstract.TypeLHS l l d d)),
                   Show (s (Abstract.ClassInstanceLHS l l d d)),
                   Show (Abstract.Name ??)) => Show (Declaration ?? l d s)
deriving instance (Eq (s (Abstract.Context l l d d)), Eq (s (Abstract.DataConstructor l l d d)),
                   Eq (s (Abstract.Declaration l l d d)), Eq (s (Abstract.DerivingClause l l d d)),
                   Eq (s (Abstract.EquationLHS l l d d)), Eq (s (Abstract.EquationRHS l l d d)),
                   Eq (s (Abstract.Type l l d d)), Eq (s (Abstract.TypeLHS l l d d)),
                   Eq (s (Abstract.ClassInstanceLHS l l d d)),
                   Eq (Abstract.Name ??)) => Eq (Declaration ?? l d s)

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

deriving instance Typeable (Type ?? l d s)
deriving instance (Data (s (Abstract.Constructor l l d d)), Data (s (Abstract.Type l l d d)), Data (Abstract.Name ??),
                   Data ??, Typeable l, Typeable d, Typeable s) => Data (Type ?? l d s)
deriving instance (Show (s (Abstract.Constructor l l d d)), Show (s (Abstract.Type l l d d)),
                   Show (Abstract.Name ??)) => Show (Type ?? l d s)
deriving instance (Eq (s (Abstract.Constructor l l d d)), Eq (s (Abstract.Type l l d d)),
                   Eq (Abstract.Name ??)) => Eq (Type ?? l d s)

deriving instance Typeable (EquationLHS ?? l d s)
deriving instance (Data (s (Abstract.EquationLHS l l d d)), Data (s (Abstract.Pattern l l d d)), Data (Abstract.Name ??),
                   Data ??, Typeable l, Typeable d, Typeable s) => Data (EquationLHS ?? l d s)
deriving instance (Show (s (Abstract.EquationLHS l l d d)), Show (s (Abstract.Pattern l l d d)),
                   Show (Abstract.Name ??)) => Show (EquationLHS ?? l d s)
deriving instance (Eq (s (Abstract.EquationLHS l l d d)), Eq (s (Abstract.Pattern l l d d)),
                   Eq (Abstract.Name ??)) => Eq (EquationLHS ?? l d s)

deriving instance Typeable (EquationRHS ?? l d s)
deriving instance (Data (s (Abstract.GuardedExpression l l d d)), Data (s (Abstract.Expression l l d d)),
                   Data ??, Typeable l, Typeable d, Typeable s) => Data (EquationRHS ?? l d s)
deriving instance (Show (s (Abstract.GuardedExpression l l d d)), Show (s (Abstract.Expression l l d d))) =>
                  Show (EquationRHS ?? l d s)
deriving instance (Eq (s (Abstract.GuardedExpression l l d d)), Eq (s (Abstract.Expression l l d d))) =>
                  Eq (EquationRHS ?? l d s)

deriving instance Typeable (GuardedExpression ?? l d s)
deriving instance (Data (s (Abstract.Statement l l d d)), Data (s (Abstract.Expression l l d d)),
                   Data ??, Typeable l, Typeable d, Typeable s) => Data (GuardedExpression ?? l d s)
deriving instance (Show (s (Abstract.Statement l l d d)), Show (s (Abstract.Expression l l d d))) =>
                  Show (GuardedExpression ?? l d s)
deriving instance (Eq (s (Abstract.Statement l l d d)), Eq (s (Abstract.Expression l l d d))) =>
                  Eq (GuardedExpression ?? l d s)

deriving instance Typeable (Export ?? l d s)
deriving instance (Data (Abstract.Members ??), Data (Abstract.ModuleName ??), Data (Abstract.QualifiedName ??),
                   Data ??, Typeable l, Typeable d, Typeable s) => Data (Export ?? l d s)
deriving instance (Show (Abstract.Members ??),
                   Show (Abstract.ModuleName ??), Show (Abstract.QualifiedName ??)) => Show (Export ?? l d s)
deriving instance (Eq (Abstract.Members ??),
                   Eq (Abstract.ModuleName ??), Eq (Abstract.QualifiedName ??)) => Eq (Export ?? l d s)

deriving instance Typeable (DerivingClause ?? l d s)
deriving instance (Data (Abstract.QualifiedName ??),
                   Data ??, Typeable l, Typeable d, Typeable s) => Data (DerivingClause ?? l d s)
deriving instance Show (Abstract.QualifiedName ??) => Show (DerivingClause ?? l d s)
deriving instance Eq (Abstract.QualifiedName ??) => Eq (DerivingClause ?? l d s)

deriving instance Typeable (Pattern ?? l d s)
deriving instance (Data (s (Abstract.Constructor l l d d)), Data (s (Abstract.FieldPattern l l d d)),
                   Data (s (Abstract.Pattern l l d d)), Data (s (Abstract.Value l l d d)),
                   Data (Abstract.Name ??), Data (Abstract.QualifiedName ??),
                   Data ??, Typeable l, Typeable d, Typeable s) => Data (Pattern ?? l d s)
deriving instance (Show (s (Abstract.Constructor l l d d)), Show (s (Abstract.FieldPattern l l d d)),
                   Show (s (Abstract.Pattern l l d d)), Show (s (Abstract.Value l l d d)),
                   Show (Abstract.QualifiedName ??), Show (Abstract.Name ??)) => Show (Pattern ?? l d s)
deriving instance (Eq (s (Abstract.Constructor l l d d)), Eq (s (Abstract.FieldPattern l l d d)),
                   Eq (s (Abstract.Pattern l l d d)), Eq (s (Abstract.Value l l d d)),
                   Eq (Abstract.QualifiedName ??), Eq (Abstract.Name ??)) => Eq (Pattern ?? l d s)

deriving instance Typeable (Statement ?? l d s)
deriving instance (Data (s (Abstract.Declaration l l d d)), Data (s (Abstract.Expression l l d d)),
                   Data (s (Abstract.Pattern l l d d)),
                   Data ??, Typeable l, Typeable d, Typeable s) => Data (Statement ?? l d s)
deriving instance (Show (s (Abstract.Declaration l l d d)), Show (s (Abstract.Expression l l d d)),
                   Show (s (Abstract.Pattern l l d d))) => Show (Statement ?? l d s)
deriving instance (Eq (s (Abstract.Declaration l l d d)), Eq (s (Abstract.Expression l l d d)),
                   Eq (s (Abstract.Pattern l l d d))) => Eq (Statement ?? l d s)

deriving instance Typeable (TypeLHS ?? l d s)
deriving instance (Data (Abstract.Name ??), Data ??, Typeable l, Typeable d, Typeable s) => Data (TypeLHS ?? l d s)
deriving instance (Show (Abstract.Name ??)) => Show (TypeLHS ?? l d s)
deriving instance (Eq (Abstract.Name ??)) => Eq (TypeLHS ?? l d s)

deriving instance Typeable (ClassInstanceLHS ?? l d s)
deriving instance (Data (s (Abstract.Type l l d d)), Data (Abstract.QualifiedName ??),
                   Data ??, Typeable l, Typeable d, Typeable s) => Data (ClassInstanceLHS ?? l d s)
deriving instance (Show (s (Abstract.Type l l d d)), Show (Abstract.QualifiedName ??)) => Show (ClassInstanceLHS ?? l d s)
deriving instance (Eq (s (Abstract.Type l l d d)), Eq (Abstract.QualifiedName ??)) => Eq (ClassInstanceLHS ?? l d s)

deriving instance Typeable (Import ?? l d s)
deriving instance (Data (s (Abstract.ImportSpecification l l d d)), Data (Abstract.ModuleName ??),
                   Data ??, Typeable l, Typeable d, Typeable s) => Data (Import ?? l d s)
deriving instance (Show (s (Abstract.ImportSpecification l l d d)), Show (Abstract.ModuleName ??)) =>
                  Show (Import ?? l d s)
deriving instance (Eq (s (Abstract.ImportSpecification l l d d)), Eq (Abstract.ModuleName ??)) =>
                  Eq (Import ?? l d s)

deriving instance Typeable (ImportSpecification ?? l d s)
deriving instance (Data (s (Abstract.ImportItem l l d d)),
                   Data ??, Typeable l, Typeable d, Typeable s) => Data (ImportSpecification ?? l d s)
deriving instance (Show (s (Abstract.ImportItem l l d d))) => Show (ImportSpecification ?? l d s)
deriving instance (Eq (s (Abstract.ImportItem l l d d))) => Eq (ImportSpecification ?? l d s)

deriving instance Typeable (ImportItem ?? l d s)
deriving instance (Data (Abstract.Members ??), Data (Abstract.Name ??),
                   Data ??, Typeable l, Typeable d, Typeable s) => Data (ImportItem ?? l d s)
deriving instance (Show (Abstract.Members ??), Show (Abstract.Name ??)) => Show (ImportItem ?? l d s)
deriving instance (Eq (Abstract.Members ??), Eq (Abstract.Name ??)) => Eq (ImportItem ?? l d s)

deriving instance Typeable (Context ?? l d s)
deriving instance (Data (s (Abstract.Context l l d d)), Data (s (Abstract.Type l l d d)),
                   Data (Abstract.Name ??), Data (Abstract.QualifiedName ??),
                   Data ??, Typeable l, Typeable d, Typeable s) => Data (Context ?? l d s)
deriving instance (Show (s (Abstract.Context l l d d)), Show (s (Abstract.Type l l d d)),
                   Show (Abstract.QualifiedName ??), Show (Abstract.Name ??)) => Show (Context ?? l d s)
deriving instance (Eq (s (Abstract.Context l l d d)), Eq (s (Abstract.Type l l d d)),
                   Eq (Abstract.QualifiedName ??), Eq (Abstract.Name ??)) => Eq (Context ?? l d s)

deriving instance Typeable (DataConstructor ?? l d s)
deriving instance (Data (s (Abstract.FieldDeclaration l l d d)), Data (s (Abstract.Type l l d d)),
                   Data (Abstract.Name ??),
                   Data ??, Typeable l, Typeable d, Typeable s) => Data (DataConstructor ?? l d s)
deriving instance (Show (s (Abstract.FieldDeclaration l l d d)), Show (s (Abstract.Type l l d d)),
                   Show (Abstract.Name ??), Show (Abstract.Name ??)) => Show (DataConstructor ?? l d s)
deriving instance (Eq (s (Abstract.FieldDeclaration l l d d)), Eq (s (Abstract.Type l l d d)),
                   Eq (Abstract.Name ??), Eq (Abstract.Name ??)) => Eq (DataConstructor ?? l d s)

deriving instance Typeable (FieldBinding ?? l d s)
deriving instance (Data (s (Abstract.Expression l l d d)), Data (Abstract.QualifiedName ??),
                   Data ??, Typeable l, Typeable d, Typeable s) => Data (FieldBinding ?? l d s)
deriving instance (Show (s (Abstract.Expression l l d d)), Show (Abstract.QualifiedName ??)) =>
                  Show (FieldBinding ?? l d s)
deriving instance (Eq (s (Abstract.Expression l l d d)), Eq (Abstract.QualifiedName ??)) =>
                  Eq (FieldBinding ?? l d s)

deriving instance Typeable (FieldDeclaration ?? l d s)
deriving instance (Data (s (Abstract.Type l l d d)), Data (Abstract.Name ??),
                   Data ??, Typeable l, Typeable d, Typeable s) => Data (FieldDeclaration ?? l d s)
deriving instance (Show (s (Abstract.Type l l d d)), Show (Abstract.Name ??)) => Show (FieldDeclaration ?? l d s)
deriving instance (Eq (s (Abstract.Type l l d d)), Eq (Abstract.Name ??)) => Eq (FieldDeclaration ?? l d s)

deriving instance Typeable (FieldPattern ?? l d s)
deriving instance (Data (s (Abstract.Pattern l l d d)), Data (Abstract.QualifiedName ??),
                   Data ??, Typeable l, Typeable d, Typeable s) => Data (FieldPattern ?? l d s)
deriving instance (Show (s (Abstract.Pattern l l d d)), Show (Abstract.QualifiedName ??)) => Show (FieldPattern ?? l d s)
deriving instance (Eq (s (Abstract.Pattern l l d d)), Eq (Abstract.QualifiedName ??)) => Eq (FieldPattern ?? l d s)

deriving instance Typeable (CaseAlternative ?? l d s)
deriving instance (Data (s (Abstract.Pattern l l d d)), Data (s (Abstract.EquationRHS l l d d)),
                   Data (s (Abstract.Declaration l l d d)),
                   Typeable ??, Typeable l, Typeable d, Typeable s) => Data (CaseAlternative ?? l d s)
deriving instance (Show (s (Abstract.Pattern l l d d)), Show (s (Abstract.EquationRHS l l d d)),
                   Show (s (Abstract.Declaration l l d d))) => Show (CaseAlternative ?? l d s)
deriving instance (Eq (s (Abstract.Pattern l l d d)), Eq (s (Abstract.EquationRHS l l d d)),
                   Eq (s (Abstract.Declaration l l d d))) => Eq (CaseAlternative ?? l d s)

deriving instance Data (Abstract.QualifiedName ??) => Typeable (Constructor ?? l d s)
deriving instance (Data (Abstract.QualifiedName ??),
                   Data ??, Typeable l, Typeable d, Typeable s) => Data (Constructor ?? l d s)
deriving instance Show (Abstract.QualifiedName ??) => Show (Constructor ?? l d s)
deriving instance Eq (Abstract.QualifiedName ??) => Eq (Constructor ?? l d s)

data CallingConvention ?? = CCall | CppCall | DotNetCall | JvmCall | StdCall deriving (Data, Eq, Show)
data CallSafety ?? = SafeCall | UnsafeCall deriving (Data, Eq, Show)

data Associativity ?? = NonAssociative | LeftAssociative | RightAssociative deriving (Data, Eq, Show)

data Members ?? = AllMembers
               | MemberList [Name ??]
               deriving (Data, Eq, Show)

newtype Name ?? = Name Text deriving (Data, Eq, Ord, Show)

data QualifiedName ?? = QualifiedName (Maybe (Abstract.ModuleName ??)) (Abstract.Name ??)

newtype ModuleName ?? = ModuleName (NonEmpty (Abstract.Name ??))
deriving instance Data (Abstract.Name ??) => Typeable (ModuleName ??)
deriving instance (Data (Abstract.Name ??), Data ??) => Data (ModuleName ??)
deriving instance Show (Abstract.Name ??) => Show (ModuleName ??)
deriving instance Eq (Abstract.Name ??) => Eq (ModuleName ??)
deriving instance Ord (Abstract.Name ??) => Ord (ModuleName ??)

deriving instance (Data (Abstract.ModuleName ??), Data (Abstract.Name ??)) => Typeable (QualifiedName ??)
deriving instance (Data (Abstract.ModuleName ??), Data (Abstract.Name ??), Data ??) => Data (QualifiedName ??)
deriving instance (Show (Abstract.ModuleName ??), Show (Abstract.Name ??)) => Show (QualifiedName ??)
deriving instance (Eq (Abstract.ModuleName ??), Eq (Abstract.Name ??)) => Eq (QualifiedName ??)
deriving instance (Ord (Abstract.ModuleName ??), Ord (Abstract.Name ??)) => Ord (QualifiedName ??)

$(concat <$>
  (forM [Rank2.TH.deriveFunctor, Rank2.TH.deriveFoldable, Rank2.TH.deriveTraversable, Rank2.TH.unsafeDeriveApply,
         Transformation.Shallow.TH.deriveAll, Transformation.Deep.TH.deriveAll] $
   \derive-> mconcat <$> mapM derive
             [''Module, ''Export, ''Import, ''ImportItem, ''ImportSpecification,
              ''Declaration, ''DerivingClause, ''EquationLHS, ''EquationRHS,
              ''ClassInstanceLHS, ''TypeLHS, ''Type, ''Context,
              ''Expression, ''Value, ''Pattern, ''GuardedExpression, ''Statement,
              ''FieldBinding, ''FieldDeclaration, ''FieldPattern, ''CaseAlternative, ''Constructor, ''DataConstructor]))
