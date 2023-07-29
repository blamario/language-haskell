{-# Language CPP, DataKinds, FlexibleContexts, FlexibleInstances, GADTs, OverloadedStrings, RankNTypes,
             ScopedTypeVariables, TemplateHaskell, TypeOperators #-}
{-# Options_GHC -Werror=incomplete-patterns #-}

module Language.Haskell.Template where

import Data.Bifunctor (bimap)
import qualified Data.Char as Char
import Data.Foldable (foldl', toList)
import Data.Functor.Compose (Compose (getCompose))
import Data.Functor.Const (Const (Const))
import Data.List ((\\), nub)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString as ByteString
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as Text
import GHC.Exts (TYPE)
import qualified GHC.Types
import qualified Rank2
import Text.PrettyPrint (render)

import Language.Haskell (Bound, Placed)
import Language.Haskell.Reserializer (ParsedLexemes(Trailing), lexemeText)
import Language.Haskell.Extensions (ExtensionSwitch(..))
import qualified Language.Haskell.Extensions as Extensions
import Language.Haskell.Extensions.AST as ExtAST
import qualified Language.Haskell.Extensions.Reformulator as Reformulator
import Language.Haskell.Extensions.Translation (FullyTranslatable)
import Language.Haskell.TH hiding (Extension, doE, mdoE, safe)
import Language.Haskell.TH.Datatype.TyVarBndr (TyVarBndrSpec, TyVarBndrUnit,
                                               kindedTV, plainTV, kindedTVInferred, plainTVInferred,
                                               kindedTVSpecified, plainTVSpecified)
import Language.Haskell.TH.PprLib ((<+>), ($$))
import Language.Haskell.TH.Ppr as Ppr (ppr)
import Language.Haskell.TH.Syntax (VarBangType)

import qualified Language.Haskell.Abstract as Abstract
import qualified Language.Haskell.AST as AST
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.PprLib as Ppr

pprint :: (PrettyViaTH a, a ~ f (node Language Language f f), f ~ Reformulator.Wrap Language pos s,
           FullyTranslatable (Reformulator.ReformulationOf 'Extensions.RecordWildCards '[ 'Extensions.NamedFieldPuns ] Language Language pos s) node) => a -> String
pprint = render . Ppr.to_HPJ_Doc . prettyViaTH . Reformulator.dropRecordWildCards

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

instance TemplateWrapper Bound where
   extract = snd . snd . getCompose
   isParenthesized = isParenthesized . snd . getCompose

instance TemplateWrapper Placed where
   extract = snd
   isParenthesized ((_, Trailing (lexeme:_), _), _) = "(" `Text.isPrefixOf` lexemeText lexeme
   isParenthesized _ = False

instance PrettyViaTH a => PrettyViaTH (x, a) where
   prettyViaTH = prettyViaTH . snd

instance (Foldable f, PrettyViaTH a) => PrettyViaTH (Compose f ((,) x) a) where
   prettyViaTH = foldr ((<+>) . prettyViaTH) Ppr.empty . getCompose

instance TemplateWrapper f => PrettyViaTH (Module Language Language f f) where
   prettyViaTH (AnonymousModule imports declarations) =
      Ppr.vcat ((prettyViaTH . extract <$> imports) ++ (prettyViaTH . extract <$> declarations))
   prettyViaTH (NamedModule name exports imports declarations) =
      Ppr.text "module" <+> prettyViaTH name <+> maybe Ppr.empty showExports exports <+> Ppr.text "where"
      $$ prettyViaTH (AnonymousModule imports declarations :: Module Language Language f f)
      where showExports xs = Ppr.parens (Ppr.sep $ Ppr.punctuate Ppr.comma (prettyViaTH . extract <$> xs))
   prettyViaTH (ExtendedModule extensions body) =
      Ppr.vcat [Ppr.text "{-# LANGUAGE" <+> Ppr.sep (Ppr.punctuate Ppr.comma $ prettyViaTH <$> extensions)
                <+> Ppr.text "#-}",
                prettyViaTH $ extract body]

instance PrettyViaTH ExtensionSwitch where
   prettyViaTH (ExtensionSwitch (Extensions.EmptyDataDeclarations, True)) = Ppr.text "EmptyDataDecls"
   prettyViaTH (ExtensionSwitch (x, True)) = Ppr.text (show x)
   prettyViaTH (ExtensionSwitch (x, False)) = Ppr.text "No" Ppr.<> prettyViaTH (ExtensionSwitch (x, True))

instance PrettyViaTH (Export Language Language f f) where
   prettyViaTH (ExportClassOrType name@(AST.QualifiedName _ (AST.Name local)) members)
      | Text.all (\c-> not $ Char.isLetter c || c == '_') local =
        (if Text.take 1 local == ":" then id else (Ppr.text "type" <+>)) $
        Ppr.parens (prettyViaTH name) Ppr.<> prettyMembers
      | otherwise = prettyViaTH name Ppr.<> prettyMembers
      where prettyMembers = maybe Ppr.empty (Ppr.parens . prettyViaTH) members
   prettyViaTH (ExportVar name@(AST.QualifiedName _ (AST.Name local)))
      | Text.all (\c-> not $ Char.isLetter c || c == '_') local = Ppr.parens (prettyViaTH name)
      | otherwise = prettyViaTH name
   prettyViaTH (ReExportModule name) = Ppr.text "module" <+> prettyViaTH name

instance TemplateWrapper f => PrettyViaTH (Import Language Language f f) where
   prettyViaTH (Import safe qualified package name alias imports) =
      Ppr.text "import" <+> (if safe then Ppr.text "safe" else Ppr.empty)
      <+> (if qualified then Ppr.text "qualified" else Ppr.empty)
      <+> maybe Ppr.empty (Ppr.doubleQuotes . Ppr.text . unpack) package
      <+> prettyViaTH name
      <+> maybe Ppr.empty ((Ppr.text "as" <+>) . prettyViaTH) alias
      <+> maybe Ppr.empty (prettyViaTH . extract) imports

instance TemplateWrapper f => PrettyViaTH (ImportSpecification Language Language f f) where
   prettyViaTH (ImportSpecification inclusive items) =
      (if inclusive then id else (Ppr.text "hiding" <+>))
      $ Ppr.parens (Ppr.sep $ Ppr.punctuate Ppr.comma $ prettyViaTH . extract <$> items)

instance PrettyViaTH (ImportItem Language Language f f) where
   prettyViaTH (ImportClassOrType name@(AST.Name local) members)
      | Text.all (\c-> not $ Char.isLetter c || c == '_') local =
        (if Text.take 1 local == ":" then id else (Ppr.text "type" <+>)) $
        Ppr.parens (prettyViaTH name) Ppr.<> prettyMembers
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

instance TemplateWrapper f => PrettyViaTH (Declaration Language Language f f) where
   prettyViaTH x = Ppr.vcat (Ppr.ppr <$> declarationTemplates x)

instance TemplateWrapper f => PrettyViaTH (Expression Language Language f f) where
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
expressionTemplate Negate = VarE (mkName "Prelude.negate")
expressionTemplate (RecordExpression record fields) =
   (case extract record
    of ConstructorExpression con | ConstructorReference name <- extract con -> RecConE (qnameTemplate name)
       e -> RecUpdE $ expressionTemplate e)
   (fieldBindingTemplate . extract <$> fields)
expressionTemplate WildcardRecordExpression{} = error "TH doesn't support record wildcards"
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
expressionTemplate (VisibleTypeApplication e t) = AppTypeE (wrappedExpressionTemplate e) (typeTemplate $ extract t)
expressionTemplate (GetField e (Name field)) = GetFieldE (wrappedExpressionTemplate e) (Text.unpack field)
expressionTemplate (OverloadedLabel l) = LabelE (Text.unpack l)
expressionTemplate (FieldProjection fields) = ProjectionE (Text.unpack . getName <$> fields)

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
   | (con, vars) <- extractSimpleTypeLHS lhs =
     [ClassD (contextTemplate $ extract context) (nameTemplate con) vars []
             (foldMap (declarationTemplates . extract) members)]
declarationTemplates (DataDeclaration context lhs kind constructors derivings)
   | (con, vars) <- extractSimpleTypeLHS lhs =
     [DataD (contextTemplate $ extract context) (nameTemplate con) vars
            (typeTemplate . extract <$> kind) (dataConstructorTemplate . extract <$> constructors)
            $ derivingsTemplate $ extract <$> derivings]
declarationTemplates (GADTDeclaration lhs kind constructors derivings)
   | (con, vars) <- extractSimpleTypeLHS lhs =
     [DataD [] (nameTemplate con) vars (typeTemplate . extract <$> kind)
            (gadtConstructorTemplate . extract <$> constructors)
            (derivingsTemplate $ extract <$> derivings)]
declarationTemplates (DefaultDeclaration types) =
#if MIN_VERSION_template_haskell(2,19,0)
   [DefaultD (typeTemplate . extract <$> types)]
#else
   error "Template Haskell <2.19 can't represent a default declaration"
#endif
declarationTemplates (EquationDeclaration lhs rhs wheres) = case extract lhs of
   VariableLHS name -> [ValD (VarP $ nameTemplate name) rhs' declarations]
   PatternLHS pat -> [ValD (patternTemplate $ extract pat) rhs' declarations]
   InfixLHS left name right ->
      [FunD (nameTemplate name)
            [Clause [patternTemplate $ extract left, patternTemplate $ extract right] rhs' declarations]]
   PrefixLHS lhs' pats -> case declarationTemplates (EquationDeclaration lhs' rhs wheres) of
      [FunD name [Clause args body decs]] ->
         [FunD name [Clause (args ++ (patternTemplate . extract <$> toList pats)) body decs]]
      [ValD (VarP name) body decs] ->
         [FunD name [Clause (patternTemplate . extract <$> toList pats) body decs]]
      ds -> error ("An equation declaration should translate to a FunD or ValD, not " <> show ds)
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
declarationTemplates (InstanceDeclaration _vars context lhs wheres) =
     [InstanceD Nothing (contextTemplate $ extract context) (lhsTypeTemplate $ extract lhs)
                (foldMap (declarationTemplates . extract) wheres)]
declarationTemplates (NewtypeDeclaration context lhs kind constructor derivings)
   | (con, vars) <- extractSimpleTypeLHS lhs =
     [NewtypeD (contextTemplate $ extract context) (nameTemplate con) vars
               (typeTemplate . extract <$> kind) (dataConstructorTemplate . extract $ constructor)
               $ derivingsTemplate $ extract <$> derivings]
declarationTemplates (GADTNewtypeDeclaration lhs kind constructor derivings)
   | (con, vars) <- extractSimpleTypeLHS lhs =
     [NewtypeD [] (nameTemplate con) vars (typeTemplate . extract <$> kind)
            (gadtConstructorTemplate . extract $ constructor)
            (derivingsTemplate $ extract <$> derivings)]
declarationTemplates (TypeSynonymDeclaration lhs t)
   | (con, vars) <- extractSimpleTypeLHS lhs = [TySynD (nameTemplate con) vars (typeTemplate $ extract t)]
declarationTemplates (TypeSignature names context t) =
   [SigD (nameTemplate name) (inContext context $ typeTemplate $ extract t) | name <- toList names]
declarationTemplates (KindSignature name context k) =
   [KiSigD (nameTemplate name) (inContext context $ typeTemplate $ extract k)]

declarationTemplates (DataFamilyDeclaration lhs kind)
   | (con, vars) <- extractSimpleTypeLHS lhs
   = [DataFamilyD (nameTemplate con) vars (typeTemplate . extract <$> kind)]
declarationTemplates (OpenTypeFamilyDeclaration lhs kind)
   | (con, vars) <- extractSimpleTypeLHS lhs
   = [OpenTypeFamilyD $ TypeFamilyHead (nameTemplate con) vars (familyKindTemplate kind) Nothing]
declarationTemplates (ClosedTypeFamilyDeclaration lhs kind constructors)
   | (con, vars) <- extractSimpleTypeLHS lhs
   = [ClosedTypeFamilyD (TypeFamilyHead (nameTemplate con) vars (familyKindTemplate kind) Nothing)
                        (typeFamilyInstanceTemplate . extract <$> constructors)]
declarationTemplates (InjectiveOpenTypeFamilyDeclaration lhs binding injectivity)
   | (con, vars) <- extractSimpleTypeLHS lhs
   = [OpenTypeFamilyD (TypeFamilyHead (nameTemplate con) vars (TyVarSig $ typeVarBindingUnitTemplate binding)
                                      (uncurry InjectivityAnn . bimap nameTemplate (map nameTemplate . toList)
                                       <$> injectivity))]
declarationTemplates (InjectiveClosedTypeFamilyDeclaration lhs binding injectivity constructors)
   | (con, vars) <- extractSimpleTypeLHS lhs
   = [ClosedTypeFamilyD (TypeFamilyHead (nameTemplate con) vars (TyVarSig $ typeVarBindingUnitTemplate binding)
                                        (uncurry InjectivityAnn . bimap nameTemplate (map nameTemplate . toList)
                                         <$> injectivity))
                        (typeFamilyInstanceTemplate . extract <$> constructors)]
declarationTemplates (DataFamilyInstance vars context lhs kind constructors derivings) =
   [DataInstD (contextTemplate $ extract context)
              (if null vars then Nothing else Just $ typeVarBindingUnitTemplate <$> vars)
              (lhsTypeTemplate $ extract lhs)
              (typeTemplate . extract <$> kind)
              (dataConstructorTemplate . extract <$> constructors)
              $ derivingsTemplate $ extract <$> derivings]
declarationTemplates (NewtypeFamilyInstance vars context lhs kind constructor derivings) =
   [NewtypeInstD (contextTemplate $ extract context)
                 (if null vars then Nothing else Just $ typeVarBindingUnitTemplate <$> vars)
                 (lhsTypeTemplate $ extract lhs)
                 (typeTemplate . extract <$> kind)
                 (dataConstructorTemplate $ extract constructor)
                 $ derivingsTemplate $ extract <$> derivings]
declarationTemplates (GADTDataFamilyInstance vars lhs kind constructors derivings) =
   [DataInstD []
              (if null vars then Nothing else Just $ typeVarBindingUnitTemplate <$> vars)
              (lhsTypeTemplate $ extract lhs)
              (typeTemplate . extract <$> kind)
              (gadtConstructorTemplate . extract <$> constructors)
              $ derivingsTemplate $ extract <$> derivings]
declarationTemplates (GADTNewtypeFamilyInstance vars lhs kind constructor derivings) =
   [NewtypeInstD []
                 (if null vars then Nothing else Just $ typeVarBindingUnitTemplate <$> vars)
                 (lhsTypeTemplate $ extract lhs)
                 (typeTemplate . extract <$> kind)
                 (gadtConstructorTemplate $ extract constructor)
                 $ derivingsTemplate $ extract <$> derivings]
declarationTemplates d@TypeFamilyInstance{} = [TySynInstD $ typeFamilyInstanceTemplate d]
declarationTemplates (TypeRoleDeclaration name roles) =
   [RoleAnnotD (qnameTemplate name) (roleTemplate <$> roles)]
   where roleTemplate NominalRole = NominalR
         roleTemplate RepresentationalRole = RepresentationalR
         roleTemplate PhantomRole = PhantomR
         roleTemplate InferredRole = InferR
declarationTemplates (StandaloneDerivingDeclaration () context lhs) =
   [StandaloneDerivD Nothing (contextTemplate $ extract context) (lhsTypeTemplate $ extract lhs)]
declarationTemplates (StandaloneStrategicDerivingDeclaration () () strategy context lhs) =
   [StandaloneDerivD (Just $ strategyTemplate $ extract strategy) (contextTemplate $ extract context)
                     (lhsTypeTemplate $ extract lhs)]

lhsTypeTemplate :: TemplateWrapper f => ExtAST.ClassInstanceLHS Language Language f f -> TH.Type
lhsTypeTemplate (TypeClassInstanceLHS name t) = AppT (ConT $ qnameTemplate name) (typeTemplate $ extract t)
lhsTypeTemplate (ClassReferenceInstanceLHS name) = ConT (qnameTemplate name)
lhsTypeTemplate (ClassInstanceLHSApplication left right) =
  AppT (lhsTypeTemplate $ extract left) (typeTemplate $ extract right)
lhsTypeTemplate (ClassInstanceLHSKindApplication left right) =
  AppKindT (lhsTypeTemplate $ extract left) (typeTemplate $ extract right)
lhsTypeTemplate (InfixTypeClassInstanceLHS left name right) =
  InfixT (wrappedTypeTemplate left) (qnameTemplate name) (wrappedTypeTemplate right)

familyKindTemplate :: TemplateWrapper f => Maybe (f (ExtAST.Type Language Language f f)) -> FamilyResultSig
familyKindTemplate = maybe NoSig (KindSig . typeTemplate . extract)

typeFamilyInstanceTemplate :: TemplateWrapper f => Declaration Language Language f f -> TySynEqn
typeFamilyInstanceTemplate (TypeFamilyInstance vars lhs rhs) =
   TySynEqn (if null vars then Nothing else Just $ typeVarBindingUnitTemplate <$> vars)
            (lhsTypeTemplate $ extract lhs)
            (typeTemplate $ extract rhs)
typeFamilyInstanceTemplate d = error ("Expected a type family instance, got " <> show (const (Const ()) Rank2.<$> d))
     
derivingsTemplate :: TemplateWrapper f => [DerivingClause Language Language f f] -> [DerivClause]
derivingsTemplate = foldr derived []
   where derived (SimpleDerive name) (DerivClause Nothing ctx : rest) =
            DerivClause Nothing (ConT (qnameTemplate name) : ctx) : rest
         derived (SimpleDerive name) templates = DerivClause Nothing [ConT $ qnameTemplate name] : templates
         derived (StrategicDerive () strategy names) templates =
            DerivClause (Just $ strategyTemplate $ extract strategy) (ConT . qnameTemplate <$> names) :  templates

strategyTemplate :: DerivingStrategy Language Language f f -> DerivStrategy
strategyTemplate Stock = StockStrategy
strategyTemplate Newtype = NewtypeStrategy
strategyTemplate AnyClass = AnyclassStrategy

contextTemplate :: TemplateWrapper f => ExtAST.Context Language Language f f -> Cxt
contextTemplate (SimpleConstraint cls var) = [AppT (ConT $ qnameTemplate cls) (VarT $ nameTemplate var)]
contextTemplate (ClassConstraint cls ts) = [foldl' AppT (ConT $ qnameTemplate cls) (typeTemplate . extract <$> ts)]
contextTemplate (InfixConstraint left op right) =
   [InfixT (wrappedTypeTemplate left) (qnameTemplate op) (wrappedTypeTemplate right)]
contextTemplate (TypeEqualityConstraint t1 t2) =
   [EqualityT `AppT` typeTemplate (extract t1) `AppT` typeTemplate (extract t2)]
contextTemplate (Constraints cs) = foldMap (contextTemplate . extract) cs
contextTemplate NoContext = []

freeContextVars :: TemplateWrapper f => ExtAST.Context Language Language f f -> [TH.Name]
freeContextVars (SimpleConstraint _cls var) = [nameTemplate var]
freeContextVars (ClassConstraint _cls ts) = foldMap (freeTypeVars . extract) ts
freeContextVars (InfixConstraint left op right) = nub (freeTypeVars (extract left) <> freeTypeVars (extract right))
freeContextVars (Constraints cs) = nub (foldMap (freeContextVars . extract) cs)
freeContextVars (TypeEqualityConstraint left right) = nub (freeTypeVars (extract left) <> freeTypeVars (extract right))
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
  ForallC (nub $ typeVarBindingSpecTemplate <$> vars)
          (contextTemplate $ extract context)
          (dataConstructorTemplate con')
  where con' = extract con

gadtConstructorTemplate :: TemplateWrapper f => GADTConstructor Language Language f f -> Con
gadtConstructorTemplate (GADTConstructors names vars context t)
   | null vars, NoContext <- extract context = case extract t of
      FunctionType arg result -> normalTemplate (extract arg :) (extract result)
      RecordFunctionType fields result -> recordTemplate (extract <$> fields) (extract result)
      result -> normalTemplate id result
   | otherwise = ForallC (nub $ typeVarBindingSpecTemplate <$> vars)
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

freeConstructorVars :: TemplateWrapper f => DataConstructor Language Language f f -> [TyVarBndrSpec]
freeConstructorVars (Constructor _ argTypes) = foldMap (freeTypeVarBindings . extract) argTypes
freeConstructorVars (RecordConstructor _ fieldTypes) = foldMap (freeTypeVarBindings . fieldsType . extract) fieldTypes
   where fieldsType (ConstructorFields _ t) = extract t
freeConstructorVars (ExistentialConstructor _ _ con) = freeConstructorVars (extract con)

fieldBindingTemplate :: TemplateWrapper f => FieldBinding Language Language f f -> FieldExp
fieldBindingTemplate (FieldBinding name value) = (qnameTemplate name, wrappedExpressionTemplate value)
fieldBindingTemplate (PunnedFieldBinding name) = (qnameTemplate name, VarE $ qnameTemplate name)

literalTemplate :: TemplateWrapper f => Value Language Language f f -> Lit
literalTemplate (CharLiteral c) = CharL c
literalTemplate (FloatingLiteral x) = RationalL x
literalTemplate (IntegerLiteral n) = IntegerL n
literalTemplate (StringLiteral s) = StringL (unpack s)
literalTemplate (HashLiteral _ (CharLiteral c)) = CharPrimL c
literalTemplate (HashLiteral _ (FloatingLiteral x)) = FloatPrimL x
literalTemplate (HashLiteral _ (HashLiteral _ (FloatingLiteral x))) = DoublePrimL x
literalTemplate (HashLiteral _ (IntegerLiteral n)) = IntPrimL n
literalTemplate (HashLiteral _ (HashLiteral _ (IntegerLiteral n))) = WordPrimL n
literalTemplate (HashLiteral _ (StringLiteral s)) = StringPrimL (ByteString.unpack $ encodeUtf8 s)
literalTemplate (HashLiteral _ _) = error "Unexpected HashLiteral"

patternTemplate :: TemplateWrapper f => Pattern Language Language f f -> Pat
patternTemplate (AsPattern name pat) = AsP (nameTemplate name) (patternTemplate $ extract pat)
patternTemplate (ConstructorPattern con typeApps args) = case (extract con) of
   ConstructorReference name ->
#if MIN_VERSION_template_haskell(2,18,0)
      ConP (qnameTemplate name) (typeTemplate .extract <$> typeApps) (patternTemplate . extract <$> args)
#else
      ConP (qnameTemplate name) (patternTemplate . extract <$> args)
#endif
   EmptyListConstructor -> ListP (patternTemplate . extract <$> args)
   TupleConstructor{} -> TupP (patternTemplate . extract <$> toList args)
   UnitConstructor -> TupP []
patternTemplate (InfixPattern left op right) =
   InfixP (patternTemplate $ extract left) (qnameTemplate op) (patternTemplate $ extract right)
patternTemplate (IrrefutablePattern pat) = TildeP (patternTemplate $ extract pat)
patternTemplate (BangPattern () pat) = BangP (patternTemplate $ extract pat)
patternTemplate (ListPattern items) = ListP (patternTemplate . extract <$> items)
patternTemplate (LiteralPattern value) = LitP (literalTemplate $ extract value)
patternTemplate (RecordPattern constructor fields) =
   RecP (qnameTemplate constructor) (fieldPatternTemplate . extract <$> fields)
   where
     fieldPatternTemplate (FieldPattern name pat) = (qnameTemplate name, patternTemplate $ extract pat)
     fieldPatternTemplate (PunnedFieldPattern q@(QualifiedName _ name)) = (qnameTemplate q, VarP $ nameTemplate name)
patternTemplate WildcardRecordPattern{} = error "TH doesn't support record wildcards"
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
typeTemplate (RecordFunctionType fields result) = foldr (fieldsArrow . extract) (typeTemplate $ extract result) fields
   where fieldsArrow (ConstructorFields names t) rt = foldr (fieldArrow $ typeTemplate $ extract t) rt names
         fieldArrow t _ = (ArrowT `AppT` t `AppT`)
typeTemplate (FunctionKind from to) = ArrowT `AppT` typeTemplate (extract from) `AppT` typeTemplate (extract to)
#if MIN_VERSION_template_haskell(2,17,0)
typeTemplate (LinearFunctionType from to) =
   MulArrowT `AppT` PromotedT 'GHC.Types.One `AppT` typeTemplate (extract from) `AppT` typeTemplate (extract to)
typeTemplate (MultiplicityFunctionType from mult to) =
   MulArrowT `AppT` typeTemplate (extract mult) `AppT` typeTemplate (extract from) `AppT` typeTemplate (extract to)
#endif
typeTemplate (ListType itemType) = AppT ListT (typeTemplate $ extract itemType)
typeTemplate (StrictType t) = typeTemplate (extract t)
typeTemplate (TupleType items) = foldl' AppT (TupleT $! length items) (typeTemplate . extract <$> items)
typeTemplate (TypeApplication left right) = AppT (typeTemplate $ extract left) (typeTemplate $ extract right)
typeTemplate (KindApplication left right) = AppT (typeTemplate $ extract left) (typeTemplate $ extract right)
typeTemplate (InfixTypeApplication left op right) =
   InfixT (wrappedTypeTemplate left) (qnameTemplate op) (wrappedTypeTemplate right)
typeTemplate (InfixKindApplication left op right) =
   InfixT (wrappedTypeTemplate left) (qnameTemplate op) (wrappedTypeTemplate right)
typeTemplate (TypeVariable name) = VarT (nameTemplate name)
typeTemplate TypeWildcard = WildCardT
typeTemplate (TypeKind t) = typeTemplate (extract t)
typeTemplate (KindedType t kind) = SigT (typeTemplate $ extract t) (typeTemplate $ extract kind)
typeTemplate (ForallType vars body) =
   ForallT varBindings [] (typeTemplate type')
   where type' = extract body
         varBindings = typeVarBindingSpecTemplate <$> vars
         bindingVars = bindingVarName <$> vars
typeTemplate (ForallKind vars body) =
   ForallT varBindings [] (typeTemplate type')
   where type' = extract body
         varBindings = typeVarBindingSpecTemplate <$> vars
         bindingVars = bindingVarName <$> vars
typeTemplate (ConstrainedType context body) =
   ForallT [] (contextTemplate $ extract context) (typeTemplate $ extract body)
typeTemplate (VisibleDependentType vars body) =
   ForallVisT (varBindings <> (plainTV <$> nub (freeTypeVars type') \\ bindingVars))
              (typeTemplate type')
   where type' = extract body
         varBindings = typeVarBindingUnitTemplate <$> vars
         bindingVars = bindingVarName <$> vars
typeTemplate GroundTypeKind = StarT
typeTemplate (PromotedConstructorType con) = case (extract con) of
   ConstructorReference name -> PromotedT (qnameTemplate name)
   EmptyListConstructor -> PromotedNilT
   TupleConstructor n -> PromotedTupleT n
   UnitConstructor -> PromotedTupleT 0
typeTemplate (PromotedTupleType items) =
   foldl' AppT (PromotedTupleT $! length items) (typeTemplate . extract <$> items)
typeTemplate (PromotedListType items) =
   foldr (AppT . AppT PromotedConsT) PromotedNilT (typeTemplate . extract <$> items)
typeTemplate (PromotedIntegerLiteral n) = LitT (NumTyLit n)
typeTemplate (PromotedStringLiteral s) = LitT (StrTyLit $ unpack s)
#if MIN_VERSION_template_haskell(2,18,0)
typeTemplate (PromotedCharLiteral c) = LitT (CharTyLit c)
#endif
typeTemplate (TupleKind items) = foldl' AppT (TupleT $! length items) (typeTemplate . extract <$> items)
typeTemplate (ListKind itemType) = AppT ListT (typeTemplate $ extract itemType)
typeTemplate (TypeRepresentationKind t) = AppT (ConT ''TYPE) (typeTemplate $ extract t)
typeTemplate (PromotedInfixTypeApplication left op right) =
   PromotedT (qnameTemplate op) `AppT` typeTemplate (extract left) `AppT` typeTemplate (extract right)
typeTemplate (ConstraintType c) = case contextTemplate (extract c) of
   [c1] -> c1
   cs -> foldl' AppT (TupleT $! length cs) cs
typeTemplate (VisibleKindApplication t k) = AppKindT (typeTemplate $ extract t) (typeTemplate $ extract k)
typeTemplate (VisibleKindKindApplication t k) = AppKindT (typeTemplate $ extract t) (typeTemplate $ extract k)

wrappedTypeTemplate :: TemplateWrapper f => f (ExtAST.Type Language Language f f) -> TH.Type
wrappedTypeTemplate x = (if isParenthesized x then ParensT else id) (typeTemplate $ extract x)

freeTypeVarBindings :: TemplateWrapper f => ExtAST.Type Language Language f f -> [TyVarBndrSpec]
freeTypeVarBindings = map plainTVInferred . freeTypeVars

freeTypeVars :: TemplateWrapper f => ExtAST.Type Language Language f f -> [TH.Name]
freeTypeVars ConstructorType{} = []
freeTypeVars FunctionConstructorType = []
freeTypeVars TypeWildcard{} = []
freeTypeVars GroundTypeKind{} = []
freeTypeVars (TypeKind t) = freeTypeVars (extract t)
freeTypeVars (FunctionType from to) = nub (freeTypeVars (extract from) <> freeTypeVars (extract to))
freeTypeVars (LinearFunctionType from to) = nub (freeTypeVars (extract from) <> freeTypeVars (extract to))
freeTypeVars (MultiplicityFunctionType from mult to) =
   nub (freeTypeVars (extract from) <> freeTypeVars (extract mult) <> freeTypeVars (extract to))
freeTypeVars (ListType itemType) = freeTypeVars (extract itemType)
freeTypeVars (StrictType t) = freeTypeVars (extract t)
freeTypeVars (TupleType items) = nub (foldMap (freeTypeVars . extract) items)
freeTypeVars (TypeApplication left right) = nub (freeTypeVars (extract left) <> freeTypeVars (extract right))
freeTypeVars (VisibleKindApplication t kind) = freeTypeVars (extract t) <> freeTypeVars (extract kind)
freeTypeVars (VisibleKindKindApplication k1 k2) = freeTypeVars (extract k1) <> freeTypeVars (extract k2)
freeTypeVars (InfixTypeApplication left _op right) = nub (freeTypeVars (extract left) <> freeTypeVars (extract right))
freeTypeVars (TypeVariable name) = [nameTemplate name]
freeTypeVars (KindedType t kind) = freeTypeVars (extract t) <> freeTypeVars (extract kind)
freeTypeVars (ForallType vars body) = nub (freeTypeVars $ extract body) \\ (bindingVarName <$> vars)
freeTypeVars (ConstrainedType context body) = nub (freeContextVars (extract context) <> freeTypeVars (extract body))
freeTypeVars (FunctionKind from to) = nub (freeTypeVars (extract from) <> freeTypeVars (extract to))
freeTypeVars (KindApplication left right) = nub (freeTypeVars (extract left) <> freeTypeVars (extract right))
freeTypeVars (InfixKindApplication left _op right) = nub (freeTypeVars (extract left) <> freeTypeVars (extract right))
freeTypeVars (ForallKind vars body) = nub (freeTypeVars (extract body)) \\ (bindingVarName <$> vars)
freeTypeVars (VisibleDependentType vars body) = nub (freeTypeVars (extract body)) \\ (bindingVarName <$> vars)
freeTypeVars (TupleKind items) = nub (foldMap (freeTypeVars . extract) items)
freeTypeVars (ListKind itemType) = freeTypeVars (extract itemType)
freeTypeVars (TypeRepresentationKind t) = freeTypeVars (extract t)
freeTypeVars (ConstraintType ctx) = freeContextVars (extract ctx)
freeTypeVars (RecordFunctionType fields result) = nub (foldMap (freeTypeVars . fieldType . extract) fields
                                                       <> freeTypeVars (extract result))
   where fieldType (ConstructorFields _names t) = extract t
freeTypeVars PromotedConstructorType{} = []
freeTypeVars (PromotedTupleType items) = nub (foldMap (freeTypeVars . extract) items)
freeTypeVars (PromotedListType items) = nub (foldMap (freeTypeVars . extract) items)
freeTypeVars (PromotedInfixTypeApplication left _op right) =
   nub (freeTypeVars (extract left) <> freeTypeVars (extract right))
freeTypeVars PromotedIntegerLiteral{} = []
freeTypeVars PromotedCharLiteral{} = []
freeTypeVars PromotedStringLiteral{} = []

typeVarBindingUnitTemplate :: TemplateWrapper f => ExtAST.TypeVarBinding Language Language f f -> TyVarBndrUnit
typeVarBindingUnitTemplate (ExplicitlyKindedTypeVariable _ name kind) =
   kindedTV (nameTemplate name) (typeTemplate $ extract kind)
typeVarBindingUnitTemplate (ImplicitlyKindedTypeVariable _ name) = plainTV (nameTemplate name)

typeVarBindingSpecTemplate :: TemplateWrapper f => ExtAST.TypeVarBinding Language Language f f -> TyVarBndrSpec
typeVarBindingSpecTemplate (ExplicitlyKindedTypeVariable False name kind) =
   kindedTVSpecified (nameTemplate name) (typeTemplate $ extract kind)
typeVarBindingSpecTemplate (ImplicitlyKindedTypeVariable False name) = plainTVSpecified (nameTemplate name)
typeVarBindingSpecTemplate (ExplicitlyKindedTypeVariable True name kind) =
   kindedTVInferred (nameTemplate name) (typeTemplate $ extract kind)
typeVarBindingSpecTemplate (ImplicitlyKindedTypeVariable True name) = plainTVInferred (nameTemplate name)

bindingVarName :: ExtAST.TypeVarBinding Language Language f f -> TH.Name
bindingVarName (ExplicitlyKindedTypeVariable _ name _) = nameTemplate name
bindingVarName (ImplicitlyKindedTypeVariable _ name) = nameTemplate name

inContext :: TemplateWrapper f => f (ExtAST.Context Language Language f f) -> TH.Type -> TH.Type
inContext context = case extract context
                     of NoContext -> id
                        ctx -> ForallT [] (contextTemplate ctx)

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

baseName :: AST.QualifiedName Language -> AST.Name Language
baseName (QualifiedName _ name) = name

extractSimpleTypeLHS :: forall l f. (Abstract.Name l ~ AST.Name l, Abstract.TypeLHS l ~ ExtAST.TypeLHS l,
                                 Abstract.Type l ~ ExtAST.Type l, l ~ Language, TemplateWrapper f)
               => f (ExtAST.TypeLHS l l f f) -> (AST.Name l, [TyVarBndrUnit])
extractSimpleTypeLHS = fromTypeLHS . extract
   where fromTypeLHS :: ExtAST.TypeLHS l l f f -> (AST.Name l, [TyVarBndrUnit])
         fromTypeLHS (SimpleTypeLHS con vars) = (con, typeVarBindingUnitTemplate <$> vars)
         fromTypeLHS (SimpleTypeLHSApplication t var)
            | (con, vars) <- extractSimpleTypeLHS t = (con, vars ++ [typeVarBindingUnitTemplate var])

nameText :: AST.Name λ -> Text
nameText (Name s) = s
