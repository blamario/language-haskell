{-# Language FlexibleContexts, FlexibleInstances, OverloadedStrings, RankNTypes, ScopedTypeVariables, TemplateHaskell #-}

module Language.Haskell.Template where

import qualified Data.Char as Char
import Data.Foldable (foldl', toList)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import qualified Data.Text as Text
import Text.PrettyPrint (render)

import qualified Transformation

import Language.Haskell.AST
import Language.Haskell.TH

import qualified Language.Haskell.AST as AST
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.PprLib as Ppr
import Language.Haskell.TH.PprLib ((<+>), ($$))
import Language.Haskell.TH.Ppr as Ppr (ppr)

pprint :: PrettyViaTH a => a -> String
pprint = render . Ppr.to_HPJ_Doc . prettyViaTH

class PrettyViaTH a where
   prettyViaTH :: a -> Ppr.Doc

instance PrettyViaTH a => PrettyViaTH (x, a) where
   prettyViaTH = prettyViaTH . snd

instance PrettyViaTH (Module Language Language ((,) x) ((,) x)) where
   prettyViaTH (AnonymousModule imports declarations) =
      Ppr.vcat ((prettyViaTH <$> imports) ++ (prettyViaTH <$> declarations))
   prettyViaTH (NamedModule name exports imports declarations) =
      Ppr.text "module" <+> prettyViaTH name <+> maybe Ppr.empty showExports exports <+> Ppr.text "where"
      $$ prettyViaTH (AnonymousModule imports declarations :: Module Language Language ((,) x) ((,) x))
      where showExports xs = Ppr.parens (Ppr.sep $ Ppr.punctuate Ppr.comma (prettyViaTH <$> xs))

instance PrettyViaTH (Export Language Language ((,) x) ((,) x)) where
   prettyViaTH (ExportClassOrType name members) =
      prettyViaTH name Ppr.<> maybe Ppr.empty (Ppr.parens . prettyViaTH) members
   prettyViaTH (ExportVar name) = prettyViaTH name
   prettyViaTH (ReExportModule name) = Ppr.text "module" <+> prettyViaTH name

instance PrettyViaTH (Import Language Language ((,) x) ((,) x)) where
   prettyViaTH (Import qualified name alias imports) =
      Ppr.text "import" <+> (if qualified then Ppr.text "qualified" else Ppr.empty)
      <+> maybe Ppr.empty ((Ppr.text "as" <+>) . prettyViaTH) alias
      <+> maybe Ppr.empty prettyViaTH imports

instance PrettyViaTH (ImportSpecification Language Language ((,) x) ((,) x)) where
   prettyViaTH (ImportSpecification inclusive items) =
      (if inclusive then id else (Ppr.text "hiding" <+>))
      $ Ppr.parens (Ppr.sep $ Ppr.punctuate Ppr.comma $ prettyViaTH <$> items)

instance PrettyViaTH (ImportItem Language Language ((,) x) ((,) x)) where
   prettyViaTH (ImportClassOrType name members) =
      prettyViaTH name Ppr.<> maybe Ppr.empty (Ppr.parens . prettyViaTH) members
   prettyViaTH (ImportVar name) = prettyViaTH name

instance PrettyViaTH (Members Language) where
   prettyViaTH (MemberList names) = Ppr.sep (Ppr.punctuate Ppr.comma $ prettyViaTH <$> names)
   prettyViaTH AllMembers = Ppr.text ".."
   
instance PrettyViaTH (Declaration Language Language ((,) x) ((,) x)) where
   prettyViaTH x = Ppr.vcat (Ppr.ppr <$> declarationTemplates snd x)

instance PrettyViaTH (Expression Language Language ((,) x) ((,) x)) where
   prettyViaTH x = Ppr.ppr (expressionTemplate snd x)

instance PrettyViaTH (ModuleName Language) where
   prettyViaTH (ModuleName mods) = Ppr.ppr (mkName $ unpack $ Text.intercalate "." $ nameText <$> toList mods)

instance PrettyViaTH (AST.Name Language) where
   prettyViaTH x = Ppr.pprName (nameTemplate x)

instance PrettyViaTH (QualifiedName Language) where
   prettyViaTH x = Ppr.pprName (qnameTemplate x)


expressionTemplate :: (forall a. f a -> a) -> Expression Language Language f f -> Exp
expressionTemplate get (ApplyExpression f x) = AppE (expressionTemplate get $ get f) (expressionTemplate get $ get x)
expressionTemplate get (ConditionalExpression test true false) =
   CondE (expressionTemplate get $ get test) (expressionTemplate get $ get true) (expressionTemplate get $ get false)
expressionTemplate get (ConstructorExpression con) = case (get con)
   of ConstructorReference name -> ConE (qnameTemplate name)
      EmptyListConstructor -> ConE '[]
      TupleConstructor n -> TupE (replicate n Nothing)
      UnitConstructor -> ConE '()
expressionTemplate get (CaseExpression scrutinee alternatives) =
   CaseE (expressionTemplate get $ get scrutinee) (caseAlternativeTemplate get . get <$> alternatives)
expressionTemplate get (DoExpression statements) = DoE (guardedTemplate $ get statements)
   where guardedTemplate (GuardedExpression statements result) =
            (statementTemplate get . get <$> statements) ++ [NoBindS $ expressionTemplate get $ get result]
expressionTemplate get (InfixExpression left op right) =
   UInfixE (expressionTemplate get $ get left) (expressionTemplate get $ get op) (expressionTemplate get $ get right)
expressionTemplate get (LeftSectionExpression op right) =
   InfixE Nothing (nameReferenceTemplate op) (Just $ expressionTemplate get $ get right)
expressionTemplate get (LambdaExpression patterns body) =
   LamE (patternTemplate get . get <$> patterns) (expressionTemplate get $ get body)
expressionTemplate get (LetExpression bindings body) =
   LetE (foldMap (declarationTemplates get .get) bindings) (expressionTemplate get $ get body)
expressionTemplate get (ListComprehension element guards) =
   CompE (statementTemplate get <$> ((get <$> toList guards) ++ [ExpressionStatement element]))
expressionTemplate get (ListExpression items) = ListE (expressionTemplate get . get <$> items)
expressionTemplate get (LiteralExpression value) = LitE (literalTemplate $ get value)
expressionTemplate get Negate = VarE (mkName "-")
expressionTemplate get (RecordExpression record fields) =
   (case get record
    of ConstructorExpression con | ConstructorReference name <- get con -> RecConE (qnameTemplate name)
       e -> RecUpdE $ expressionTemplate get e)
   (fieldBindingTemplate get . get <$> fields)
expressionTemplate get (ReferenceExpression name) = VarE (qnameTemplate name)
expressionTemplate get (RightSectionExpression left op) =
   InfixE (Just $ expressionTemplate get $ get left) (nameReferenceTemplate op) Nothing
expressionTemplate get (SequenceExpression start next end) = ArithSeqE $
   case (expressionTemplate get . get <$> next, expressionTemplate get . get <$> end)
   of (Nothing, Nothing) -> FromR s
      (Just n, Nothing) -> FromThenR s n
      (Nothing, Just e) -> FromToR s e
      (Just n, Just e) -> FromThenToR s n e
   where s = expressionTemplate get $ get start
expressionTemplate get (TupleExpression items) = TupE (Just . expressionTemplate get . get <$> toList items)
expressionTemplate get (TypedExpression e signature) =
   SigE (expressionTemplate get $ get e) (typeTemplate get $ get signature)

caseAlternativeTemplate :: (forall a. f a -> a) -> CaseAlternative Language Language f f -> Match
caseAlternativeTemplate get (CaseAlternative lhs rhs wheres) =
   Match (patternTemplate get $ get lhs) (rhsTemplate get $ get rhs) (foldMap (declarationTemplates get . get) wheres)

declarationTemplates :: (forall a. f a -> a) -> Declaration Language Language f f -> [Dec]
declarationTemplates get (ClassDeclaration context lhs members)
   | SimpleTypeLHS con vars <- get lhs =
     [ClassD (contextTemplate get $ get context) (nameTemplate con) (PlainTV . nameTemplate <$> vars) []
             (foldMap (declarationTemplates get . get) members)]
declarationTemplates get (DataDeclaration context lhs constructors derivings)
   | SimpleTypeLHS con vars <- get lhs =
     [DataD (contextTemplate get $ get context) (nameTemplate con) (PlainTV . nameTemplate <$> vars)
            Nothing (dataConstructorTemplate get . get <$> constructors)
            [DerivClause Nothing $ derived . get <$> derivings]]
   where derived (SimpleDerive name) = ConT (qnameTemplate name)
declarationTemplates get (DefaultDeclaration types) = error "Template Haskell can't represent a default declaration"
declarationTemplates get (EquationDeclaration lhs rhs wheres)
   | VariableLHS name <- get lhs = [ValD (VarP $ nameTemplate name) body declarations]
   | PatternLHS pat <- get lhs = [ValD (patternTemplate get $ get pat) body declarations]
   | InfixLHS left name right <- get lhs =
     [FunD (nameTemplate name)
           [Clause [patternTemplate get $ get left, patternTemplate get $ get right] body declarations]]
   | PrefixLHS lhs' pats <- get lhs,
     [FunD name [Clause args body decs]] <- declarationTemplates get (EquationDeclaration lhs' rhs wheres) =
     [FunD name [Clause (args ++ (patternTemplate get . get <$> toList pats)) body decs]]
   where body = rhsTemplate get (get rhs)
         declarations = foldMap (declarationTemplates get . get) wheres
declarationTemplates get (FixityDeclaration fixity precedence names) =
   InfixD (Fixity (fromMaybe 9 precedence) (fixityTemplate fixity)) . nameTemplate <$> toList names
   where fixityTemplate InfixNonAssociative = InfixN
         fixityTemplate InfixLeft = InfixL
         fixityTemplate InfixRight = InfixR
declarationTemplates get (ForeignExport convention identification name t) =
   [ForeignD (ExportF (conventionTemplate convention) (foldMap unpack identification) (nameTemplate name)
                      (typeTemplate get $ get t))]
declarationTemplates get (ForeignImport convention safety identification name t) =
   [ForeignD (ImportF (conventionTemplate convention) (maybe Safe safetyTemplate safety) (foldMap unpack identification)
                      (nameTemplate name) (typeTemplate get $ get t))]
   where safetyTemplate SafeCall = Safe
         safetyTemplate UnsafeCall = Unsafe
declarationTemplates get (InstanceDeclaration context lhs wheres)
   | GeneralTypeLHS name t <- get lhs =
     [InstanceD Nothing (contextTemplate get $ get context)
                (AppT (ConT $ qnameTemplate name) $ typeTemplate get $ get t)
                (foldMap (declarationTemplates get . get) wheres)]
declarationTemplates get (NewtypeDeclaration context lhs constructor derivings)
   | SimpleTypeLHS con vars <- get lhs =
     [NewtypeD (contextTemplate get $ get context) (nameTemplate con) (PlainTV . nameTemplate <$> vars)
               Nothing (dataConstructorTemplate get . get $ constructor)
               [DerivClause Nothing $ derived . get <$> derivings]]
   where derived (SimpleDerive name) = ConT (qnameTemplate name)
declarationTemplates get (TypeSynonymDeclaration lhs t)
   | SimpleTypeLHS con vars <- get lhs =
     [TySynD (nameTemplate con) (PlainTV . nameTemplate <$> vars) (typeTemplate get $ get t)]
declarationTemplates get (TypeSignature names context t)
   | NoContext <- get context = [SigD (nameTemplate name) (typeTemplate get $ get t) | name <- toList names]

contextTemplate :: (forall a. f a -> a) -> Context Language Language f f -> Cxt
contextTemplate get (SimpleConstraint cls var) = [AppT (ConT $ qnameTemplate cls) (VarT $ nameTemplate var)]
contextTemplate get (ClassConstraint cls t) = [AppT (ConT $ qnameTemplate cls) (typeTemplate get $ get t)]
contextTemplate get (Constraints cs) = foldMap (contextTemplate get . get) cs
contextTemplate get NoContext = []

conventionTemplate :: CallingConvention l -> Callconv
conventionTemplate AST.CCall = TH.CCall
conventionTemplate AST.StdCall = TH.StdCall
conventionTemplate convention = error ("Calling Convention " ++ show convention ++ " is not supported by GHC")

dataConstructorTemplate :: (forall a. f a -> a) -> DataConstructor Language Language f f -> Con
dataConstructorTemplate get (Constructor name argTypes) =
   NormalC (nameTemplate name) (bangTypeTemplate . get <$> argTypes)
   where bangTypeTemplate (StrictType t) = (Bang NoSourceUnpackedness SourceStrict, typeTemplate get $ get t)
         bangTypeTemplate t = (Bang NoSourceUnpackedness NoSourceStrictness, typeTemplate get t)
dataConstructorTemplate get (RecordConstructor name fieldTypes) =
   RecC (nameTemplate name) (concat $ fieldTypeTemplate . get <$> fieldTypes)
   where fieldTypeTemplate (ConstructorFields names t)
            | StrictType t' <- get t = varBang SourceStrict t <$> toList names
            | otherwise = varBang NoSourceStrictness t <$> toList names
         varBang strictness t name = (nameTemplate name, Bang NoSourceUnpackedness strictness, typeTemplate get $ get t)

fieldBindingTemplate :: (forall a. f a -> a) -> FieldBinding Language Language f f -> FieldExp
fieldBindingTemplate get (FieldBinding name value) = (qnameTemplate name, expressionTemplate get $ get value)

literalTemplate :: Value Language Language f f -> Lit
literalTemplate (CharLiteral c) = CharL c
literalTemplate (FloatingLiteral x) = RationalL x
literalTemplate (IntegerLiteral n) = IntegerL n
literalTemplate (StringLiteral s) = StringL (unpack s)

patternTemplate :: (forall a. f a -> a) -> Pattern Language Language f f -> Pat
patternTemplate get (AsPattern name pat) = AsP (nameTemplate name) (patternTemplate get $ get pat)
patternTemplate get (ConstructorPattern con args) = case (get con) of
   ConstructorReference name -> ConP (qnameTemplate name) (patternTemplate get . get <$> args)
   EmptyListConstructor -> ListP (patternTemplate get . get <$> args)
   TupleConstructor n -> TupP (patternTemplate get . get <$> toList args)
   UnitConstructor -> TupP []
patternTemplate get (InfixPattern left op right) =
   InfixP (patternTemplate get $ get left) (qnameTemplate op) (patternTemplate get $ get right)
patternTemplate get (IrrefutablePattern pat) = TildeP (patternTemplate get $ get pat)
patternTemplate get (ListPattern items) = ListP (patternTemplate get . get <$> items)
patternTemplate get (LiteralPattern value) = LitP (literalTemplate $ get value)
patternTemplate get (RecordPattern constructor fields) =
   RecP (qnameTemplate constructor) (fieldPatternTemplate . get <$> fields)
   where fieldPatternTemplate (FieldPattern name pat) = (qnameTemplate name, patternTemplate get $ get pat)
patternTemplate get (TuplePattern items) = TupP (patternTemplate get . get <$> toList items)
patternTemplate get (VariablePattern name) = VarP (nameTemplate name)
patternTemplate get WildcardPattern = WildP

rhsTemplate :: (forall a. f a -> a) -> EquationRHS Language Language f f -> Body
rhsTemplate get (GuardedRHS guarded) = GuardedB (guardedTemplate . get <$> toList guarded)
   where guardedTemplate (GuardedExpression statements result) = (PatG $ statementTemplate get . get <$> statements,
                                                                  expressionTemplate get $ get result)
rhsTemplate get (NormalRHS result) = NormalB (expressionTemplate get $ get result)

statementTemplate :: (forall a. f a -> a) -> Statement Language Language f f -> Stmt
statementTemplate get (BindStatement left right) =
   BindS (patternTemplate get $ get left) (expressionTemplate get $ get right)
statementTemplate get (ExpressionStatement test) = NoBindS (expressionTemplate get $ get test)
statementTemplate get (LetStatement declarations) = LetS (foldMap (declarationTemplates get . get) declarations)

typeTemplate :: (forall a. f a -> a) -> AST.Type Language Language f f -> TH.Type
typeTemplate get (ConstructorType con) = case (get con) of
   ConstructorReference name -> ConT (qnameTemplate name)
   EmptyListConstructor -> ListT
   TupleConstructor n -> TupleT n
   UnitConstructor -> TupleT 0
typeTemplate get FunctionConstructorType = ArrowT
typeTemplate get (FunctionType from to) = ArrowT `AppT` typeTemplate get (get from) `AppT` typeTemplate get (get to)
typeTemplate get (ListType itemType) = AppT ListT (typeTemplate get $ get itemType)
typeTemplate get (StrictType t) = typeTemplate get (get t)
typeTemplate get (TupleType items) = foldl' AppT (TupleT $! length items) (typeTemplate get . get <$> items)
typeTemplate get (TypeApplication left right) = AppT (typeTemplate get $ get left) (typeTemplate get $ get right)
typeTemplate get (TypeVariable name) = VarT (nameTemplate name)

nameReferenceTemplate :: AST.QualifiedName Language -> Exp
nameReferenceTemplate name@(QualifiedName _ (AST.Name local))
   | not (Text.null local), c <- Text.head local, Char.isUpper c || c == ':' = ConE (qnameTemplate name)
   | otherwise = VarE (qnameTemplate name)

nameTemplate :: AST.Name Language -> TH.Name
nameTemplate (Name s) = mkName (unpack s)

qnameTemplate :: AST.QualifiedName Language -> TH.Name
qnameTemplate (QualifiedName Nothing name) = nameTemplate name
qnameTemplate (QualifiedName (Just (ModuleName m)) name) = mkName (unpack $ Text.intercalate "."
                                                                   $ nameText <$> toList m ++ [name])

nameText (Name s) = s


