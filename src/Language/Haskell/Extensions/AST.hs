{-# Language DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings,
             StandaloneDeriving, TemplateHaskell, TypeFamilies, UndecidableInstances #-}

module Language.Haskell.Extensions.AST (Language(Language), Expression(..), Statement(..), Value(..),
                                        module Report) where

import Control.Monad (forM)
import Data.List.NonEmpty (NonEmpty)
import Data.Data (Data, Typeable)
import Data.Text (Text)

import qualified Language.Haskell.Extensions.Abstract as Abstract
import qualified Language.Haskell.AST as Report
import Language.Haskell.AST (Module(..), Declaration(..), Type(..), EquationLHS(..), EquationRHS(..),
                             GuardedExpression(..), Pattern(..),
                             TypeLHS(..), Context(..), DataConstructor(..), DerivingClause(..), Constructor(..),
                             FieldDeclaration(..), FieldBinding(..), FieldPattern(..), CaseAlternative(..),
                             CallingConvention(..), CallSafety(..), Associativity(..),
                             Name(..), ModuleName(..), QualifiedName(..),
                             Import(..), ImportSpecification(..), ImportItem(..), Export(..), Members(..))
import qualified Rank2.TH
import qualified Transformation.Deep.TH
import qualified Transformation.Shallow.TH

data Language = Language deriving (Data, Eq, Show)

instance Abstract.ExtendedHaskell Language where
   hashLiteral = HashLiteral
   mdoExpression = MDoExpression
   parallelListComprehension = ParallelListComprehension
   tupleSectionExpression = TupleSectionExpression
   lambdaCaseExpression = LambdaCaseExpression
   recursiveStatement = RecursiveStatement

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

data Expression λ l d s =
   ApplyExpression (s (Abstract.Expression l l d d)) (s (Abstract.Expression l l d d))
   | ConditionalExpression (s (Abstract.Expression l l d d)) (s (Abstract.Expression l l d d))
                           (s (Abstract.Expression l l d d))
   | ConstructorExpression (s (Abstract.Constructor l l d d))
   | CaseExpression (s (Abstract.Expression l l d d)) [s (Abstract.CaseAlternative l l d d)]
   | LambdaCaseExpression [s (Abstract.CaseAlternative l l d d)]
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
             [''Expression, ''Statement, ''Value]))
