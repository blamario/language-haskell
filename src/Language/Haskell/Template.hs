{-# Language CPP, FlexibleContexts, FlexibleInstances, GADTs, OverloadedStrings, RankNTypes,
             ScopedTypeVariables, TemplateHaskell #-}

module Language.Haskell.Template where

import qualified Data.Char as Char
import Data.Foldable (foldl', toList)
import Data.List ((\\), nub)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString as ByteString
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as Text
import Text.PrettyPrint (render)

import Language.Haskell (Placed)
import Language.Haskell.Reserializer (ParsedLexemes(Trailing), lexemeText)
import Language.Haskell.Extensions (ExtensionSwitch(..))
import qualified Language.Haskell.Extensions as Extensions
import Language.Haskell.Extensions.AST as ExtAST
import Language.Haskell.TH hiding (Extension, doE, mdoE, safe)
import Language.Haskell.TH.Datatype.TyVarBndr

import qualified Language.Haskell.Abstract as Abstract
import qualified Language.Haskell.AST as AST
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.PprLib as Ppr
import Language.Haskell.TH.PprLib ((<+>), ($$))
import Language.Haskell.TH.Ppr as Ppr (ppr)
import Language.Haskell.TH.Syntax (VarBangType)

pprint :: PrettyViaTH a => a -> String
pprint = render . Ppr.to_HPJ_Doc . prettyViaTH

doE, mdoE :: [Stmt] -> Exp
#if MIN_VERSION_template_haskell(2,17,0)
doE = DoE Nothing
mdoE = MDoE Nothing
#else
doE = DoE
mdoE = MDoE
#endif

class PrettyViaTH a where
   prettyViaTH :: a -> Ppr.Doc

class Functor f => TemplateWrapper f where
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
   prettyViaTH (ExtendedModule extensions body) =
      Ppr.vcat [Ppr.text "{-# LANGUAGE" <+> Ppr.sep (Ppr.punctuate Ppr.comma $ prettyViaTH <$> extensions)
                <+> Ppr.text "#-}",
                prettyViaTH body]

instance PrettyViaTH ExtensionSwitch where
   prettyViaTH (ExtensionSwitch (Extensions.EmptyDataDeclarations, True)) = Ppr.text "EmptyDataDecls"
   prettyViaTH (ExtensionSwitch (x, True)) = Ppr.text (show x)
   prettyViaTH (ExtensionSwitch (x, False)) = Ppr.text "No" Ppr.<> prettyViaTH (ExtensionSwitch (x, True))

instance PrettyViaTH (Export Language Language ((,) x) ((,) x)) where
   prettyViaTH (ExportClassOrType name@(AST.QualifiedName _ (AST.Name local)) members)
      | Text.all (\c-> not $ Char.isLetter c || c == '_') local =
        Ppr.text "type" <+> Ppr.parens (prettyViaTH name) Ppr.<> prettyMembers
      | otherwise = prettyViaTH name Ppr.<> prettyMembers
      where prettyMembers = maybe Ppr.empty (Ppr.parens . prettyViaTH) members
   prettyViaTH (ExportVar name) = prettyViaTH name
   prettyViaTH (ReExportModule name) = Ppr.text "module" <+> prettyViaTH name

instance PrettyViaTH (Import Language Language ((,) x) ((,) x)) where
   prettyViaTH (Import safe qualified package name alias imports) =
      Ppr.text "import" <+> (if safe then Ppr.text "safe" else Ppr.empty)
      <+> (if qualified then Ppr.text "qualified" else Ppr.empty)
      <+> maybe Ppr.empty (Ppr.doubleQuotes . Ppr.text . unpack) package
      <+> prettyViaTH name
      <+> maybe Ppr.empty ((Ppr.text "as" <+>) . prettyViaTH) alias
      <+> maybe Ppr.empty prettyViaTH imports

instance PrettyViaTH (ImportSpecification Language Language ((,) x) ((,) x)) where
   prettyViaTH (ImportSpecification inclusive items) =
      (if inclusive then id else (Ppr.text "hiding" <+>))
      $ Ppr.parens (Ppr.sep $ Ppr.punctuate Ppr.comma $ prettyViaTH <$> items)

instance PrettyViaTH (ImportItem Language Language ((,) x) ((,) x)) where
   prettyViaTH (ImportClassOrType name@(AST.Name local) members)
      | Text.all (\c-> not $ Char.isLetter c || c == '_') local =
        Ppr.text "type" <+> Ppr.parens (prettyViaTH name) Ppr.<> prettyMembers
      | otherwise = prettyViaTH name Ppr.<> prettyMembers
      where prettyMembers = maybe Ppr.empty (Ppr.parens . prettyViaTH) members
   prettyViaTH (ImportVar name@(AST.Name local)) = prettyIdentifier name

instance PrettyViaTH (Members Language) where
   prettyViaTH (MemberList names) = Ppr.sep (Ppr.punctuate Ppr.comma $ prettyIdentifier <$> names)
   prettyViaTH (ExplicitlyNamespacedMemberList members) = Ppr.sep (Ppr.punctuate Ppr.comma $ prettyViaTH <$> members)
   prettyViaTH AllMembers = Ppr.text ".."

instance PrettyViaTH (ModuleMember Language) where
   prettyViaTH (DefaultMember name) = prettyIdentifier name
   prettyViaTH (PatternMember name) = Ppr.text "pattern" <+> prettyIdentifier name
   prettyViaTH (TypeMember name) = Ppr.text "type" <+> prettyIdentifier name

prettyIdentifier :: AST.Name Language -> Ppr.Doc
prettyIdentifier name@(AST.Name local)
   | Just (c, _) <- Text.uncons local, Char.isLetter c || c == '_' = prettyViaTH name
   | otherwise = Ppr.parens (prettyViaTH name)

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
expressionTemplate (MultiWayIfExpression alternatives) = MultiIfE (guardedTemplatePair . extract <$> alternatives)
expressionTemplate (LambdaCaseExpression alternatives) =
   LamCaseE (caseAlternativeTemplate . extract <$> alternatives)
expressionTemplate (DoExpression statements) = doE (guardedTemplate $ extract statements)
expressionTemplate (MDoExpression statements) = mdoE (guardedTemplate $ extract statements)
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

guardedTemplatePair :: TemplateWrapper f => GuardedExpression λ Language f f -> (Guard, Exp)
guardedTemplatePair (GuardedExpression statements result) = (PatG $ statementTemplate . extract <$> statements,
                                                             wrappedExpressionTemplate result)

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
   | Just (con, vars) <- extractSimpleTypeLHS lhs =
     [ClassD (contextTemplate $ extract context) (nameTemplate con) vars []
             (foldMap (declarationTemplates . extract) members)]
declarationTemplates (DataDeclaration context lhs kind constructors derivings)
   | Just (con, vars) <- extractSimpleTypeLHS lhs =
     [DataD (contextTemplate $ extract context) (nameTemplate con) vars
            (typeTemplate . extract <$> kind) (dataConstructorTemplate . extract <$> constructors)
            $ derivingsTemplate $ extract <$> derivings]
declarationTemplates (GADTDeclaration lhs kind constructors derivings)
   | Just (con, vars) <- extractSimpleTypeLHS lhs =
     [DataD [] (nameTemplate con) vars (typeTemplate . extract <$> kind)
            (gadtConstructorTemplate . extract <$> constructors)
            (derivingsTemplate $ extract <$> derivings)]
declarationTemplates DefaultDeclaration{} = error "Template Haskell can't represent a default declaration"
declarationTemplates (EquationDeclaration lhs rhs wheres)
   | VariableLHS name <- extract lhs = [ValD (VarP $ nameTemplate name) rhs' declarations]
   | PatternLHS pat <- extract lhs = [ValD (patternTemplate $ extract pat) rhs' declarations]
   | InfixLHS left name right <- extract lhs =
     [FunD (nameTemplate name)
           [Clause [patternTemplate $ extract left, patternTemplate $ extract right] rhs' declarations]]
   | PrefixLHS lhs' pats <- extract lhs = case declarationTemplates (EquationDeclaration lhs' rhs wheres) of
     [FunD name [Clause args body decs]] ->
        [FunD name [Clause (args ++ (patternTemplate . extract <$> toList pats)) body decs]]
     [ValD (VarP name) body decs] ->
        [FunD name [Clause (patternTemplate . extract <$> toList pats) body decs]]
   where rhs' = rhsTemplate (extract rhs)
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
declarationTemplates (InstanceDeclaration _vars context lhs wheres)
   | TypeClassInstanceLHS name t <- extract lhs =
     [InstanceD Nothing (contextTemplate $ extract context)
                (AppT (ConT $ qnameTemplate name) $ typeTemplate $ extract t)
                (foldMap (declarationTemplates . extract) wheres)]
declarationTemplates (NewtypeDeclaration context lhs kind constructor derivings)
   | Just (con, vars) <- extractSimpleTypeLHS lhs =
     [NewtypeD (contextTemplate $ extract context) (nameTemplate con) vars
               (typeTemplate . extract <$> kind) (dataConstructorTemplate . extract $ constructor)
               $ derivingsTemplate $ extract <$> derivings]
declarationTemplates (GADTNewtypeDeclaration lhs kind constructor derivings)
   | Just (con, vars) <- extractSimpleTypeLHS lhs =
     [NewtypeD [] (nameTemplate con) vars (typeTemplate . extract <$> kind)
            (gadtConstructorTemplate . extract $ constructor)
            (derivingsTemplate $ extract <$> derivings)]
declarationTemplates (TypeSynonymDeclaration lhs t)
   | Just (con, vars) <- extractSimpleTypeLHS lhs = [TySynD (nameTemplate con) vars (typeTemplate $ extract t)]
declarationTemplates (TypeSignature names context t) =
   [SigD (nameTemplate name) (inContext $ typeTemplate $ extract t) | name <- toList names]
   where inContext = case extract context
                     of NoContext -> id
                        ctx -> ForallT [] (contextTemplate ctx)

declarationTemplates (DataFamilyDeclaration lhs kind)
   | Just (con, vars) <- extractSimpleTypeLHS lhs
   = [DataFamilyD (nameTemplate con) vars (typeTemplate . extract <$> kind)]
declarationTemplates (OpenTypeFamilyDeclaration lhs kind)
   | Just (con, vars) <- extractSimpleTypeLHS lhs
   = [OpenTypeFamilyD $ TypeFamilyHead (nameTemplate con) vars (familyKindTemplate kind) Nothing]
declarationTemplates (ClosedTypeFamilyDeclaration lhs kind constructors)
   | Just (con, vars) <- extractSimpleTypeLHS lhs
   = [ClosedTypeFamilyD (TypeFamilyHead (nameTemplate con) vars (familyKindTemplate kind) Nothing)
                        (typeFamilyInstanceTemplate . extract <$> constructors)]
declarationTemplates (DataFamilyInstance vars context lhs kind constructors derivings) =
   [DataInstD (contextTemplate $ extract context)
              (if null vars then Nothing else Just $ typeVarBindingTemplate <$> vars)
              (lhsTypeTemplate $ extract lhs)
              (typeTemplate . extract <$> kind)
              (dataConstructorTemplate . extract <$> constructors)
              $ derivingsTemplate $ extract <$> derivings]
declarationTemplates (NewtypeFamilyInstance vars context lhs kind constructor derivings) =
   [NewtypeInstD (contextTemplate $ extract context)
                 (if null vars then Nothing else Just $ typeVarBindingTemplate <$> vars)
                 (lhsTypeTemplate $ extract lhs)
                 (typeTemplate . extract <$> kind)
                 (dataConstructorTemplate $ extract constructor)
                 $ derivingsTemplate $ extract <$> derivings]
declarationTemplates (GADTDataFamilyInstance vars lhs kind constructors derivings) =
   [DataInstD []
              (if null vars then Nothing else Just $ typeVarBindingTemplate <$> vars)
              (lhsTypeTemplate $ extract lhs)
              (typeTemplate . extract <$> kind)
              (gadtConstructorTemplate . extract <$> constructors)
              $ derivingsTemplate $ extract <$> derivings]
declarationTemplates (GADTNewtypeFamilyInstance vars lhs kind constructor derivings) =
   [NewtypeInstD []
                 (if null vars then Nothing else Just $ typeVarBindingTemplate <$> vars)
                 (lhsTypeTemplate $ extract lhs)
                 (typeTemplate . extract <$> kind)
                 (gadtConstructorTemplate $ extract constructor)
                 $ derivingsTemplate $ extract <$> derivings]
declarationTemplates d@TypeFamilyInstance{} = [TySynInstD $ typeFamilyInstanceTemplate d]

lhsTypeTemplate :: TemplateWrapper f => ExtAST.ClassInstanceLHS Language Language f f -> TH.Type
lhsTypeTemplate (TypeClassInstanceLHS name t) = AppT (ConT $ qnameTemplate name) (typeTemplate $ extract t)
lhsTypeTemplate (MultiParameterTypeClassInstanceLHS name types) =
  foldl' AppT (ConT $ qnameTemplate name) $ typeTemplate . extract <$> types
lhsTypeTemplate (InfixTypeClassInstanceLHS left name right) =
  InfixT (typeTemplate $ extract left) (qnameTemplate name) (typeTemplate $ extract right)

familyKindTemplate :: TemplateWrapper f => Maybe (f (ExtAST.Type Language Language f f)) -> FamilyResultSig
familyKindTemplate = maybe NoSig (KindSig . typeTemplate . extract)

typeFamilyInstanceTemplate :: TemplateWrapper f => Declaration Language Language f f -> TySynEqn
typeFamilyInstanceTemplate (TypeFamilyInstance vars lhs rhs) =
   TySynEqn (if null vars then Nothing else Just $ typeVarBindingTemplate <$> vars)
            (lhsTypeTemplate $ extract lhs)
            (typeTemplate $ extract rhs)
     
derivingsTemplate :: [DerivingClause Language Language f f] -> [DerivClause]
derivingsTemplate derivings =
   if null derivings then [] else [DerivClause Nothing $ derived <$> derivings]
   where derived (SimpleDerive name) = ConT (qnameTemplate name)

contextTemplate :: TemplateWrapper f => ExtAST.Context Language Language f f -> Cxt
contextTemplate (SimpleConstraint cls var) = [AppT (ConT $ qnameTemplate cls) (VarT $ nameTemplate var)]
contextTemplate (ClassConstraint cls t) = [AppT (ConT $ qnameTemplate cls) (typeTemplate $ extract t)]
contextTemplate (TypeEqualityConstraint t1 t2) =
   [EqualityT `AppT` typeTemplate (extract t1) `AppT` typeTemplate (extract t2)]
contextTemplate (Constraints cs) = foldMap (contextTemplate . extract) cs
contextTemplate NoContext = []

freeContextVars :: TemplateWrapper f => ExtAST.Context Language Language f f -> [TyVarBndrUnit]
freeContextVars (SimpleConstraint _cls var) = [plainTV $ nameTemplate var]
freeContextVars (ClassConstraint _cls t) = freeTypeVars (extract t)
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
dataConstructorTemplate (RecordConstructor recName fieldTypes) =
   RecC (nameTemplate recName) (foldMap (fieldTypeTemplate . extract) fieldTypes)
dataConstructorTemplate (ExistentialConstructor vars context con) =
  ForallC (changeTVFlags SpecifiedSpec $ nub $ (typeVarBindingTemplate <$> vars) <> freeConstructorVars con')
          (contextTemplate $ extract context)
          (dataConstructorTemplate con')
  where con' = extract con

gadtConstructorTemplate :: TemplateWrapper f => GADTConstructor Language Language f f -> Con
gadtConstructorTemplate (GADTConstructors names vars context t)
   | null vars, NoContext <- extract context = case extract t of
      FunctionType arg result -> normalTemplate (extract arg :) (extract result)
      RecordFunctionType fields result -> recordTemplate (extract <$> fields) (extract result)
      result -> normalTemplate id result
   | otherwise = ForallC (changeTVFlags SpecifiedSpec $ nub
                          $ (typeVarBindingTemplate <$> vars) <> freeTypeVars (extract t))
                         (contextTemplate $ extract context)
                         (gadtConstructorTemplate $ GADTConstructors names [] (NoContext <$ context) t)
   where normalTemplate args (FunctionType arg result) = normalTemplate (args . (extract arg :)) (extract result)
         normalTemplate args result =
            GadtC (nameTemplate <$> toList names) (bangTypeTemplate <$> args []) (typeTemplate result)
         recordTemplate fields result =
            RecGadtC (nameTemplate <$> toList names) (foldMap fieldTypeTemplate fields) (typeTemplate result)

fieldTypeTemplate :: TemplateWrapper f => FieldDeclaration Language Language f f -> [VarBangType]
fieldTypeTemplate (ConstructorFields names t)
   | StrictType{} <- extract t = varBang SourceStrict t <$> toList names
   | otherwise = varBang NoSourceStrictness t <$> toList names
   where varBang strictness t name = (nameTemplate name, Bang NoSourceUnpackedness strictness, typeTemplate $ extract t)

freeConstructorVars :: TemplateWrapper f => DataConstructor Language Language f f -> [TyVarBndrUnit]
freeConstructorVars (Constructor _ argTypes) = foldMap (freeTypeVars . extract) argTypes
freeConstructorVars (RecordConstructor _ fieldTypes) = foldMap (freeTypeVars . fieldsType . extract) fieldTypes
   where fieldsType (ConstructorFields _ t) = extract t
freeConstructorVars (ExistentialConstructor _ _ con) = freeConstructorVars (extract con)

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
literalTemplate (HashLiteral _) = error "Unexpected HashLiteral"

patternTemplate :: TemplateWrapper f => Pattern Language Language f f -> Pat
patternTemplate (AsPattern name pat) = AsP (nameTemplate name) (patternTemplate $ extract pat)
patternTemplate (ConstructorPattern con args) = case (extract con) of
   ConstructorReference name -> ConP (qnameTemplate name) (patternTemplate . extract <$> args)
   EmptyListConstructor -> ListP (patternTemplate . extract <$> args)
   TupleConstructor{} -> TupP (patternTemplate . extract <$> toList args)
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
rhsTemplate (GuardedRHS guarded) = GuardedB (guardedTemplatePair . extract <$> toList guarded)
rhsTemplate (NormalRHS result) = NormalB (wrappedExpressionTemplate result)

statementTemplate :: TemplateWrapper f => Statement Language Language f f -> Stmt
statementTemplate (BindStatement left right) =
   BindS (patternTemplate $ extract left) (wrappedExpressionTemplate right)
statementTemplate (ExpressionStatement test) = NoBindS (wrappedExpressionTemplate test)
statementTemplate (LetStatement declarations) = LetS (foldMap (declarationTemplates . extract) declarations)
statementTemplate (RecursiveStatement statements) = RecS (statementTemplate . extract <$> statements)

bangTypeTemplate :: TemplateWrapper f => ExtAST.Type Language Language f f -> (TH.Bang, TH.Type)
bangTypeTemplate (StrictType t) = (Bang NoSourceUnpackedness SourceStrict, typeTemplate $ extract t)
bangTypeTemplate t = (Bang NoSourceUnpackedness NoSourceStrictness, typeTemplate t)

typeTemplate :: TemplateWrapper f => ExtAST.Type Language Language f f -> TH.Type
typeTemplate (ConstructorType con) = case (extract con) of
   ConstructorReference name -> ConT (qnameTemplate name)
   EmptyListConstructor -> ListT
   TupleConstructor n -> TupleT n
   UnitConstructor -> TupleT 0
typeTemplate FunctionConstructorType = ArrowT
typeTemplate (FunctionType from to) = ArrowT `AppT` typeTemplate (extract from) `AppT` typeTemplate (extract to)
typeTemplate (FunctionKind from to) = ArrowT `AppT` typeTemplate (extract from) `AppT` typeTemplate (extract to)
typeTemplate (ListType itemType) = AppT ListT (typeTemplate $ extract itemType)
typeTemplate (StrictType t) = typeTemplate (extract t)
typeTemplate (TupleType items) = foldl' AppT (TupleT $! length items) (typeTemplate . extract <$> items)
typeTemplate (TypeApplication left right) = AppT (typeTemplate $ extract left) (typeTemplate $ extract right)
typeTemplate (KindApplication left right) = AppT (typeTemplate $ extract left) (typeTemplate $ extract right)
typeTemplate (InfixTypeApplication left op right) =
   InfixT (typeTemplate $ extract left) (qnameTemplate op) (typeTemplate $ extract right)
typeTemplate (TypeVariable name) = VarT (nameTemplate name)
typeTemplate TypeWildcard = WildCardT
typeTemplate (KindedType t kind) = SigT (typeTemplate $ extract t) (typeTemplate $ extract kind)
typeTemplate (ForallType vars context body) =
  ForallT (changeTVFlags SpecifiedSpec $ nub $ (typeVarBindingTemplate <$> vars) <> freeTypeVars type')
          (contextTemplate $ extract context)
          (typeTemplate type')
  where type' = extract body
typeTemplate (ForallKind vars body) =
  ForallT (changeTVFlags SpecifiedSpec $ nub $ (typeVarBindingTemplate <$> vars) <> freeTypeVars type') []
          (typeTemplate type')
  where type' = extract body
typeTemplate GroundTypeKind = StarT

freeTypeVars :: TemplateWrapper f => ExtAST.Type Language Language f f -> [TyVarBndrUnit]
freeTypeVars ConstructorType{} = []
freeTypeVars FunctionConstructorType = []
freeTypeVars (FunctionType from to) = nub (freeTypeVars (extract from) <> freeTypeVars (extract to))
freeTypeVars (ListType itemType) = freeTypeVars (extract itemType)
freeTypeVars (StrictType t) = freeTypeVars (extract t)
freeTypeVars (TupleType items) = nub (foldMap (freeTypeVars . extract) items)
freeTypeVars (TypeApplication left right) = nub (freeTypeVars (extract left) <> freeTypeVars (extract right))
freeTypeVars (InfixTypeApplication left _op right) = nub (freeTypeVars (extract left) <> freeTypeVars (extract right))
freeTypeVars (TypeVariable name) = [plainTV $ nameTemplate name]
freeTypeVars (KindedType t kind) = freeTypeVars (extract t) <> freeTypeVars (extract kind)
freeTypeVars (ForallType vars context body) =
  nub (freeContextVars (extract context) <> freeTypeVars (extract body)) \\ (typeVarBindingTemplate <$> vars)
freeTypeVars (FunctionKind from to) = nub (freeTypeVars (extract from) <> freeTypeVars (extract to))
freeTypeVars (KindApplication left right) = nub (freeTypeVars (extract left) <> freeTypeVars (extract right))
freeTypeVars (ForallKind vars body) = nub (freeTypeVars (extract body)) \\ (typeVarBindingTemplate <$> vars)
freeTypeVars (RecordFunctionType fields result) = nub (foldMap (freeTypeVars . fieldType . extract) fields
                                                       <> freeTypeVars (extract result))
   where fieldType (ConstructorFields _names t) = extract t

typeVarBindingTemplate :: TemplateWrapper f => ExtAST.TypeVarBinding Language Language f f -> TyVarBndrUnit
typeVarBindingTemplate (ExplicitlyKindedTypeVariable name kind) =
   kindedTV (nameTemplate name) (typeTemplate $ extract kind)
typeVarBindingTemplate (ImplicitlyKindedTypeVariable name) = plainTV (nameTemplate name)

nameReferenceTemplate :: AST.QualifiedName Language -> Exp
nameReferenceTemplate name@(QualifiedName _ (AST.Name local))
   | not (Text.null local), c <- Text.head local, Char.isUpper c || c == ':' = ConE (qnameTemplate name)
   | otherwise = VarE (qnameTemplate name)

nameTemplate :: AST.Name l -> TH.Name
nameTemplate (Name s) = mkName (unpack s)

qnameTemplate :: AST.QualifiedName Language -> TH.Name
qnameTemplate (QualifiedName Nothing name) = nameTemplate name
qnameTemplate (QualifiedName (Just (ModuleName m)) name) = mkName (unpack $ Text.intercalate "."
                                                                   $ nameText <$> toList m ++ [name])

extractSimpleTypeLHS :: forall l f. (Abstract.Name l ~ AST.Name l, Abstract.TypeLHS l ~ ExtAST.TypeLHS l,
                                 Abstract.Type l ~ ExtAST.Type l, l ~ Language, TemplateWrapper f)
               => f (ExtAST.TypeLHS l l f f) -> Maybe (AST.Name l, [TyVarBndrUnit])
extractSimpleTypeLHS = fromTypeLHS . extract
   where fromTypeLHS :: ExtAST.TypeLHS l l f f -> Maybe (AST.Name l, [TyVarBndrUnit])
         fromTypeLHS (SimpleTypeLHS con vars) = Just (con, typeVarBindingTemplate <$> vars)
         fromTypeLHS (SimpleTypeLHSApplication t var)
            | Just (con, vars) <- extractSimpleTypeLHS t = Just (con, vars ++ [typeVarBindingTemplate var])

nameText :: AST.Name λ -> Text
nameText (Name s) = s
