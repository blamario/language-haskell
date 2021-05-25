{-# Language FlexibleContexts, FlexibleInstances, OverloadedStrings, RankNTypes,
             ScopedTypeVariables, TemplateHaskell #-}

module Language.Haskell.Template where

import qualified Data.Char as Char
import Data.Foldable (foldl', toList)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString as ByteString
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as Text
import Text.PrettyPrint (render)

import qualified Transformation

import Language.Haskell (Placed)
import Language.Haskell.Reserializer (ParsedLexemes(Trailing), lexemeText)
import Language.Haskell.Extensions.AST
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

class TemplateWrapper f where
   extract :: f a -> a
   isParenthesized :: f a -> Bool

instance TemplateWrapper Placed where
   extract = snd
   isParenthesized ((_, Trailing (lexeme:_), _), _) = "(" `Text.isPrefixOf` lexemeText lexeme
   isParenthesized _ = False

instance PrettyViaTH a => PrettyViaTH (x, a) where
   prettyViaTH = prettyViaTH . snd

instance PrettyViaTH (Module Language Language Placed Placed) where
   prettyViaTH (AnonymousModule imports declarations) =
      Ppr.vcat ((prettyViaTH <$> imports) ++ (prettyViaTH <$> declarations))
   prettyViaTH (NamedModule name exports imports declarations) =
      Ppr.text "module" <+> prettyViaTH name <+> maybe Ppr.empty showExports exports <+> Ppr.text "where"
      $$ prettyViaTH (AnonymousModule imports declarations :: Module Language Language Placed Placed)
      where showExports xs = Ppr.parens (Ppr.sep $ Ppr.punctuate Ppr.comma (prettyViaTH <$> xs))

instance PrettyViaTH (Export Language Language ((,) x) ((,) x)) where
   prettyViaTH (ExportClassOrType name members) =
      prettyViaTH name Ppr.<> maybe Ppr.empty (Ppr.parens . prettyViaTH) members
   prettyViaTH (ExportVar name) = prettyViaTH name
   prettyViaTH (ReExportModule name) = Ppr.text "module" <+> prettyViaTH name

instance PrettyViaTH (Import Language Language ((,) x) ((,) x)) where
   prettyViaTH (Import qualified name alias imports) =
      Ppr.text "import" <+> (if qualified then Ppr.text "qualified" else Ppr.empty)
      <+> prettyViaTH name
      <+> maybe Ppr.empty ((Ppr.text "as" <+>) . prettyViaTH) alias
      <+> maybe Ppr.empty prettyViaTH imports

instance PrettyViaTH (ImportSpecification Language Language ((,) x) ((,) x)) where
   prettyViaTH (ImportSpecification inclusive items) =
      (if inclusive then id else (Ppr.text "hiding" <+>))
      $ Ppr.parens (Ppr.sep $ Ppr.punctuate Ppr.comma $ prettyViaTH <$> items)

instance PrettyViaTH (ImportItem Language Language ((,) x) ((,) x)) where
   prettyViaTH (ImportClassOrType name members) =
      prettyViaTH name Ppr.<> maybe Ppr.empty (Ppr.parens . prettyViaTH) members
   prettyViaTH (ImportVar name@(AST.Name local))
      | not (Text.null local), c <- Text.head local, Char.isLetter c || c == '_' = prettyViaTH name
      | otherwise = Ppr.parens (prettyViaTH name)

instance PrettyViaTH (Members Language) where
   prettyViaTH (MemberList names) = Ppr.sep (Ppr.punctuate Ppr.comma $ prettyViaTH <$> names)
   prettyViaTH AllMembers = Ppr.text ".."
   
instance PrettyViaTH (Declaration Language Language Placed Placed) where
   prettyViaTH x = Ppr.vcat (Ppr.ppr <$> declarationTemplates x)

instance PrettyViaTH (Expression Language Language Placed Placed) where
   prettyViaTH x = Ppr.ppr (expressionTemplate x)

instance PrettyViaTH (ModuleName Language) where
   prettyViaTH (ModuleName mods) = Ppr.ppr (mkName $ unpack $ Text.intercalate "." $ nameText <$> toList mods)

instance PrettyViaTH (AST.Name Language) where
   prettyViaTH x = Ppr.pprName (nameTemplate x)

instance PrettyViaTH (QualifiedName Language) where
   prettyViaTH x = Ppr.pprName (qnameTemplate x)


expressionTemplate :: TemplateWrapper f => Expression Language Language f f -> Exp
expressionTemplate (ApplyExpression f x) = AppE (wrappedExpressionTemplate f) (wrappedExpressionTemplate x)
expressionTemplate (ConditionalExpression test true false) =
   CondE (wrappedExpressionTemplate test) (wrappedExpressionTemplate true) (wrappedExpressionTemplate false)
expressionTemplate (ConstructorExpression con) = case (extract con)
   of ConstructorReference name -> ConE (qnameTemplate name)
      EmptyListConstructor -> ListE []
      TupleConstructor n -> TupE (replicate n Nothing)
      UnitConstructor -> TupE []
expressionTemplate (CaseExpression scrutinee alternatives) =
   CaseE (wrappedExpressionTemplate scrutinee) (caseAlternativeTemplate . extract <$> alternatives)
expressionTemplate (MultiWayIfExpression alternatives) = MultiIfE (guardedTemplate . extract <$> alternatives)
   where guardedTemplate (GuardedExpression statements result) = (PatG $ statementTemplate . extract <$> statements,
                                                                  wrappedExpressionTemplate result)
expressionTemplate (LambdaCaseExpression alternatives) =
   LamCaseE (caseAlternativeTemplate . extract <$> alternatives)
expressionTemplate (DoExpression statements) = DoE (guardedTemplate $ extract statements)
expressionTemplate (MDoExpression statements) = MDoE (guardedTemplate $ extract statements)
expressionTemplate (InfixExpression left op right) =
   UInfixE (wrappedExpressionTemplate left) (wrappedExpressionTemplate op) (wrappedExpressionTemplate right)
expressionTemplate (LeftSectionExpression left op) =
   InfixE (Just $ wrappedExpressionTemplate left) (nameReferenceTemplate op) Nothing
expressionTemplate (LambdaExpression patterns body) =
   LamE (patternTemplate . extract <$> patterns) (wrappedExpressionTemplate body)
expressionTemplate (LetExpression bindings body) =
   LetE (foldMap (declarationTemplates . extract) bindings) (wrappedExpressionTemplate body)
expressionTemplate (ListComprehension element guards) =
   CompE (statementTemplate <$> ((extract <$> toList guards) ++ [ExpressionStatement element]))
expressionTemplate (ParallelListComprehension element guards1 guards2 guardses) =
   CompE [ParS (branch guards1 : branch guards2 : map branch guardses),
          statementTemplate (ExpressionStatement element)]
   where branch statements = statementTemplate <$> (extract <$> toList statements)
expressionTemplate (ListExpression items) = ListE (expressionTemplate . extract <$> items)
expressionTemplate (LiteralExpression value) = LitE (literalTemplate $ extract value)
expressionTemplate Negate = VarE (mkName "-")
expressionTemplate (RecordExpression record fields) =
   (case extract record
    of ConstructorExpression con | ConstructorReference name <- extract con -> RecConE (qnameTemplate name)
       e -> RecUpdE $ expressionTemplate e)
   (fieldBindingTemplate . extract <$> fields)
expressionTemplate (ReferenceExpression name) = VarE (qnameTemplate name)
expressionTemplate (RightSectionExpression op right) =
   InfixE Nothing (nameReferenceTemplate op) (Just $ wrappedExpressionTemplate right)
expressionTemplate (SequenceExpression start next end) = ArithSeqE $
   case (expressionTemplate . extract <$> next, expressionTemplate . extract <$> end)
   of (Nothing, Nothing) -> FromR s
      (Just n, Nothing) -> FromThenR s n
      (Nothing, Just e) -> FromToR s e
      (Just n, Just e) -> FromThenToR s n e
   where s = wrappedExpressionTemplate start
expressionTemplate (TupleExpression items) = TupE (Just . expressionTemplate . extract <$> toList items)
expressionTemplate (TupleSectionExpression items) = TupE ((expressionTemplate . extract <$>) <$> toList items)
expressionTemplate (TypedExpression e signature) = SigE (wrappedExpressionTemplate e) (typeTemplate $ extract signature)

guardedTemplate :: TemplateWrapper f => GuardedExpression Language Language f f -> [Stmt]
guardedTemplate (GuardedExpression statements result) =
   (statementTemplate . extract <$> statements) ++ [NoBindS $ wrappedExpressionTemplate result]

wrappedExpressionTemplate :: TemplateWrapper f => f (Expression Language Language f f) -> Exp
wrappedExpressionTemplate x = if isParenthesized x && not (syntactic e) then ParensE template else template
   where syntactic LeftSectionExpression{} = True
         syntactic RightSectionExpression{} = True
         syntactic ReferenceExpression{} = True
         syntactic TupleExpression{} = True
         syntactic _ = False
         template = expressionTemplate e
         e = extract x

caseAlternativeTemplate :: TemplateWrapper f => CaseAlternative Language Language f f -> Match
caseAlternativeTemplate (CaseAlternative lhs rhs wheres) =
   Match (patternTemplate $ extract lhs) (rhsTemplate $ extract rhs) (foldMap (declarationTemplates . extract) wheres)

declarationTemplates :: TemplateWrapper f => Declaration Language Language f f -> [Dec]
declarationTemplates (ClassDeclaration context lhs members)
   | SimpleTypeLHS con vars <- extract lhs =
     [ClassD (contextTemplate $ extract context) (nameTemplate con) (PlainTV . nameTemplate <$> vars) []
             (foldMap (declarationTemplates . extract) members)]
declarationTemplates (DataDeclaration context lhs constructors derivings)
   | SimpleTypeLHS con vars <- extract lhs =
     [DataD (contextTemplate $ extract context) (nameTemplate con) (PlainTV . nameTemplate <$> vars)
            Nothing (dataConstructorTemplate . extract <$> constructors)
            $ if null derivings then [] else [DerivClause Nothing $ derived . extract <$> derivings]]
   where derived (SimpleDerive name) = ConT (qnameTemplate name)
declarationTemplates (DefaultDeclaration types) = error "Template Haskell can't represent a default declaration"
declarationTemplates (EquationDeclaration lhs rhs wheres)
   | VariableLHS name <- extract lhs = [ValD (VarP $ nameTemplate name) body declarations]
   | PatternLHS pat <- extract lhs = [ValD (patternTemplate $ extract pat) body declarations]
   | InfixLHS left name right <- extract lhs =
     [FunD (nameTemplate name)
           [Clause [patternTemplate $ extract left, patternTemplate $ extract right] body declarations]]
   | PrefixLHS lhs' pats <- extract lhs = case declarationTemplates (EquationDeclaration lhs' rhs wheres) of
     [FunD name [Clause args body decs]] ->
        [FunD name [Clause (args ++ (patternTemplate . extract <$> toList pats)) body decs]]
     [ValD (VarP name) body decs] ->
        [FunD name [Clause (patternTemplate . extract <$> toList pats) body decs]]
   where body = rhsTemplate (extract rhs)
         declarations = foldMap (declarationTemplates . extract) wheres
declarationTemplates (FixityDeclaration fixity precedence names) =
   InfixD (Fixity (fromMaybe 9 precedence) (fixityTemplate fixity)) . nameTemplate <$> toList names
   where fixityTemplate NonAssociative = InfixN
         fixityTemplate LeftAssociative = InfixL
         fixityTemplate RightAssociative = InfixR
declarationTemplates (ForeignExport convention identification name t) =
   [ForeignD (ExportF (conventionTemplate convention) (foldMap unpack identification) (nameTemplate name)
                      (typeTemplate $ extract t))]
declarationTemplates (ForeignImport convention safety identification name t) =
   [ForeignD (ImportF (conventionTemplate convention) (maybe Safe safetyTemplate safety) (foldMap unpack identification)
                      (nameTemplate name) (typeTemplate $ extract t))]
   where safetyTemplate SafeCall = Safe
         safetyTemplate UnsafeCall = Unsafe
declarationTemplates (InstanceDeclaration context lhs wheres)
   | GeneralTypeLHS name t <- extract lhs =
     [InstanceD Nothing (contextTemplate $ extract context)
                (AppT (ConT $ qnameTemplate name) $ typeTemplate $ extract t)
                (foldMap (declarationTemplates . extract) wheres)]
declarationTemplates (NewtypeDeclaration context lhs constructor derivings)
   | SimpleTypeLHS con vars <- extract lhs =
     [NewtypeD (contextTemplate $ extract context) (nameTemplate con) (PlainTV . nameTemplate <$> vars)
               Nothing (dataConstructorTemplate . extract $ constructor)
               $ if null derivings then [] else [DerivClause Nothing $ derived . extract <$> derivings]]
   where derived (SimpleDerive name) = ConT (qnameTemplate name)
declarationTemplates (TypeSynonymDeclaration lhs t)
   | SimpleTypeLHS con vars <- extract lhs =
     [TySynD (nameTemplate con) (PlainTV . nameTemplate <$> vars) (typeTemplate $ extract t)]
declarationTemplates (TypeSignature names context t) =
   [SigD (nameTemplate name) (inContext $ typeTemplate $ extract t) | name <- toList names]
   where inContext = case extract context
                     of NoContext -> id
                        ctx -> ForallT (nub $ freeContextVars ctx <> freeTypeVars (extract t))
                                       (contextTemplate ctx)

contextTemplate :: TemplateWrapper f => Context Language Language f f -> Cxt
contextTemplate (SimpleConstraint cls var) = [AppT (ConT $ qnameTemplate cls) (VarT $ nameTemplate var)]
contextTemplate (ClassConstraint cls t) = [AppT (ConT $ qnameTemplate cls) (typeTemplate $ extract t)]
contextTemplate (Constraints cs) = foldMap (contextTemplate . extract) cs
contextTemplate NoContext = []

freeContextVars :: TemplateWrapper f => Context Language Language f f -> [TyVarBndr]
freeContextVars (SimpleConstraint cls var) = [PlainTV $ nameTemplate var]
freeContextVars (ClassConstraint cls t) = freeTypeVars (extract t)
freeContextVars (Constraints cs) = nub (foldMap (freeContextVars . extract) cs)
freeContextVars NoContext = []

conventionTemplate :: CallingConvention l -> Callconv
conventionTemplate AST.CCall = TH.CCall
conventionTemplate AST.StdCall = TH.StdCall
conventionTemplate convention = error ("Calling Convention " ++ show convention ++ " is not supported by GHC")

dataConstructorTemplate :: TemplateWrapper f => DataConstructor Language Language f f -> Con
dataConstructorTemplate (Constructor name@(AST.Name local) [left, right]) | ":" `Text.isPrefixOf` local =
   InfixC (bangTypeTemplate $ extract left) (nameTemplate name) (bangTypeTemplate $ extract right)
dataConstructorTemplate (Constructor name argTypes) =
   NormalC (nameTemplate name) (bangTypeTemplate . extract <$> argTypes)
dataConstructorTemplate (RecordConstructor name fieldTypes) =
   RecC (nameTemplate name) (concat $ fieldTypeTemplate . extract <$> fieldTypes)
   where fieldTypeTemplate (ConstructorFields names t)
            | StrictType t' <- extract t = varBang SourceStrict t <$> toList names
            | otherwise = varBang NoSourceStrictness t <$> toList names
         varBang strictness t name = (nameTemplate name, Bang NoSourceUnpackedness strictness, typeTemplate $ extract t)

fieldBindingTemplate :: TemplateWrapper f => FieldBinding Language Language f f -> FieldExp
fieldBindingTemplate (FieldBinding name value) = (qnameTemplate name, wrappedExpressionTemplate value)

literalTemplate :: TemplateWrapper f => Value Language Language f f -> Lit
literalTemplate (CharLiteral c) = CharL c
literalTemplate (FloatingLiteral x) = RationalL x
literalTemplate (IntegerLiteral n) = IntegerL n
literalTemplate (StringLiteral s) = StringL (unpack s)
literalTemplate (HashLiteral (CharLiteral c)) = CharPrimL c
literalTemplate (HashLiteral (FloatingLiteral x)) = FloatPrimL x
literalTemplate (HashLiteral (HashLiteral (FloatingLiteral x))) = DoublePrimL x
literalTemplate (HashLiteral (IntegerLiteral n)) = IntPrimL n
literalTemplate (HashLiteral (HashLiteral (IntegerLiteral n))) = WordPrimL n
literalTemplate (HashLiteral (StringLiteral s)) = StringPrimL (ByteString.unpack $ encodeUtf8 s)

patternTemplate :: TemplateWrapper f => Pattern Language Language f f -> Pat
patternTemplate (AsPattern name pat) = AsP (nameTemplate name) (patternTemplate $ extract pat)
patternTemplate (ConstructorPattern con args) = case (extract con) of
   ConstructorReference name -> ConP (qnameTemplate name) (patternTemplate . extract <$> args)
   EmptyListConstructor -> ListP (patternTemplate . extract <$> args)
   TupleConstructor n -> TupP (patternTemplate . extract <$> toList args)
   UnitConstructor -> TupP []
patternTemplate (InfixPattern left op right) =
   InfixP (patternTemplate $ extract left) (qnameTemplate op) (patternTemplate $ extract right)
patternTemplate (IrrefutablePattern pat) = TildeP (patternTemplate $ extract pat)
patternTemplate (ListPattern items) = ListP (patternTemplate . extract <$> items)
patternTemplate (LiteralPattern value) = LitP (literalTemplate $ extract value)
patternTemplate (RecordPattern constructor fields) =
   RecP (qnameTemplate constructor) (fieldPatternTemplate . extract <$> fields)
   where fieldPatternTemplate (FieldPattern name pat) = (qnameTemplate name, patternTemplate $ extract pat)
patternTemplate (TuplePattern items) = TupP (patternTemplate . extract <$> toList items)
patternTemplate (VariablePattern name) = VarP (nameTemplate name)
patternTemplate WildcardPattern = WildP

rhsTemplate :: TemplateWrapper f => EquationRHS Language Language f f -> Body
rhsTemplate (GuardedRHS guarded) = GuardedB (guardedTemplate . extract <$> toList guarded)
   where guardedTemplate (GuardedExpression statements result) = (PatG $ statementTemplate . extract <$> statements,
                                                                  wrappedExpressionTemplate result)
rhsTemplate (NormalRHS result) = NormalB (wrappedExpressionTemplate result)

statementTemplate :: TemplateWrapper f => Statement Language Language f f -> Stmt
statementTemplate (BindStatement left right) =
   BindS (patternTemplate $ extract left) (wrappedExpressionTemplate right)
statementTemplate (ExpressionStatement test) = NoBindS (wrappedExpressionTemplate test)
statementTemplate (LetStatement declarations) = LetS (foldMap (declarationTemplates . extract) declarations)
statementTemplate (RecursiveStatement statements) = RecS (statementTemplate . extract <$> statements)

bangTypeTemplate :: TemplateWrapper f => AST.Type Language Language f f -> (TH.Bang, TH.Type)
bangTypeTemplate (StrictType t) = (Bang NoSourceUnpackedness SourceStrict, typeTemplate $ extract t)
bangTypeTemplate t = (Bang NoSourceUnpackedness NoSourceStrictness, typeTemplate t)

typeTemplate :: TemplateWrapper f => AST.Type Language Language f f -> TH.Type
typeTemplate (ConstructorType con) = case (extract con) of
   ConstructorReference name -> ConT (qnameTemplate name)
   EmptyListConstructor -> ListT
   TupleConstructor n -> TupleT n
   UnitConstructor -> TupleT 0
typeTemplate FunctionConstructorType = ArrowT
typeTemplate (FunctionType from to) = ArrowT `AppT` typeTemplate (extract from) `AppT` typeTemplate (extract to)
typeTemplate (ListType itemType) = AppT ListT (typeTemplate $ extract itemType)
typeTemplate (StrictType t) = typeTemplate (extract t)
typeTemplate (TupleType items) = foldl' AppT (TupleT $! length items) (typeTemplate . extract <$> items)
typeTemplate (TypeApplication left right) = AppT (typeTemplate $ extract left) (typeTemplate $ extract right)
typeTemplate (TypeVariable name) = VarT (nameTemplate name)

freeTypeVars :: TemplateWrapper f => AST.Type Language Language f f -> [TyVarBndr]
freeTypeVars ConstructorType{} = []
freeTypeVars FunctionConstructorType = []
freeTypeVars (FunctionType from to) = nub (freeTypeVars (extract from) <> freeTypeVars (extract to))
freeTypeVars (ListType itemType) = freeTypeVars (extract itemType)
freeTypeVars (StrictType t) = freeTypeVars (extract t)
freeTypeVars (TupleType items) = nub (foldMap (freeTypeVars . extract) items)
freeTypeVars (TypeApplication left right) = nub (freeTypeVars (extract left) <> freeTypeVars (extract right))
freeTypeVars (TypeVariable name) = [PlainTV $ nameTemplate name]

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


