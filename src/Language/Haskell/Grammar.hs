{-# Language FlexibleContexts, FlexibleInstances, OverloadedStrings,
             Rank2Types, RecordWildCards,
             ScopedTypeVariables, TemplateHaskell, TupleSections, TypeSynonymInstances #-}

module Language.Haskell.Grammar where

import Control.Applicative
import Control.Monad (void)
import qualified Control.Monad
import qualified Data.Char as Char (chr, isAscii, isAlphaNum, isDigit, isHexDigit, isLetter, isLower, isOctDigit,
                                    isSpace, isSymbol, isUpper, ord)
import Data.Data (Data)
import Data.Either (isLeft, partitionEithers)
import Data.Foldable (maximumBy, toList)
import Data.Functor.Compose (getCompose)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Ord (comparing)
import Data.String (fromString)
import qualified Data.Monoid.Textual as Textual
import Data.Monoid.Null (null)
import Data.Monoid.Textual (TextualMonoid, characterPrefix, toString)
import Data.Monoid.Instances.Positioned (LinePositioned, column, extract)
import Data.Semigroup.Cancellative (isPrefixOf)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text (Text)
import Numeric (readOct, readDec, readHex, readFloat)
import Witherable (filter, mapMaybe)
import Text.Grampa
import Text.Grampa.ContextFree.LeftRecursive.Transformer (ParserT, lift, tmap)
import qualified Text.Parser.Char
import Text.Parser.Combinators (eof, sepBy, sepBy1, sepByNonEmpty, try)
import Text.Parser.Token (braces, brackets, comma, parens, semi)
import qualified Rank2.TH

import qualified Language.Haskell.Abstract as Abstract
import qualified Language.Haskell.AST as AST (Language)
import Language.Haskell.Reserializer (ParsedLexemes(..), Lexeme(..), TokenType(..))

import Prelude hiding (exponent, filter, null)

type Parser = ParserT ((,) [[Lexeme]])

data HaskellGrammar l f p = HaskellGrammar {
   haskellModule :: p (f (Abstract.Module l l f f)),
   body :: p ([f (Abstract.Import l l f f)], [f (Abstract.Declaration l l f f)]),
   exports :: p [f (Abstract.Export l l f f)],
   export :: p (Abstract.Export l l f f),
   importDeclaration :: p (Abstract.Import l l f f),
   importSpecification :: p (Abstract.ImportSpecification l l f f),
   importItem :: p (Abstract.ImportItem l l f f),
   members :: p (Abstract.Members l),
   cname :: p (Abstract.Name l),
   topLevelDeclaration :: p (Abstract.Declaration l l f f),
   declarations :: p [f (Abstract.Declaration l l f f)],
   declaration :: p (Abstract.Declaration l l f f),
   inClassDeclaration :: p (Abstract.Declaration l l f f),
   inInstanceDeclaration :: p (Abstract.Declaration l l f f),
   generalDeclaration :: p (Abstract.Declaration l l f f),
   whereClauses :: p [f (Abstract.Declaration l l f f)],
   variables :: p (NonEmpty (Abstract.Name l)),
   fixity :: p (Abstract.Fixity l),
   typeTerm :: p (Abstract.Type l l f f),
   bType :: p (Abstract.Type l l f f),
   aType :: p (Abstract.Type l l f f),
   generalTypeConstructor :: p (Abstract.Type l l f f),
   context :: p (Abstract.Context l l f f),
   classConstraint :: p (Abstract.Context l l f f),
   typeApplications :: p (Abstract.Type l l f f),
   simpleContext :: p (Abstract.Context l l f f),
   simpleConstraint :: p (Abstract.Context l l f f),
   simpleType :: p (Abstract.TypeLHS l l f f),
   declaredConstructors :: p [f (Abstract.DataConstructor l l f f)],
   declaredConstructor :: p (Abstract.DataConstructor l l f f),
   infixConstructorArgType :: p (Abstract.Type l l f f),
   newConstructor :: p (Abstract.DataConstructor l l f f),
   fieldDeclaration :: p (Abstract.FieldDeclaration l l f f),
   derivingClause :: p [f (Abstract.DerivingClause l l f f)],
   instanceDesignator :: p (Abstract.Type l l f f),
   typeVarApplications :: p (Abstract.Type l l f f),
   typeVarTuple :: p (NonEmpty (f (Abstract.Type l l f f))),
   foreignDeclaration :: p (Abstract.Declaration l l f f),
   callingConvention :: p (Abstract.CallingConvention l),
   safety :: p (Abstract.CallSafety l),
   foreignType :: p (Abstract.Type l l f f),
   foreignReturnType :: p (Abstract.Type l l f f),
   foreignArgType :: p (Abstract.Type l l f f),
   functionLHS :: p (Abstract.EquationLHS l l f f),
   rhs :: p (Abstract.EquationRHS l l f f),
   guards :: p (NonEmpty (f (Abstract.Statement l l f f))),
   guard :: p (Abstract.Statement l l f f),
   expression :: p (f (Abstract.Expression l l f f)),
   infixExpression :: p (f (Abstract.Expression l l f f)),
   lExpression :: p (f (Abstract.Expression l l f f)),
   dExpression :: p (f (Abstract.Expression l l f f)),
   fExpression :: p (f (Abstract.Expression l l f f)),
   aExpression :: p (f (Abstract.Expression l l f f)),
   alternative :: p (Abstract.CaseAlternative l l f f),
   statements :: p (Abstract.GuardedExpression l l f f),
   statement :: p (Either (f (Abstract.Statement l l f f)) (f (Abstract.Expression l l f f))),
   fieldBinding :: p (Abstract.FieldBinding l l f f),
   pattern :: p (Abstract.Pattern l l f f),
   lPattern :: p (Abstract.Pattern l l f f),
   aPattern :: p (Abstract.Pattern l l f f),
   fieldPattern :: p (Abstract.FieldPattern l l f f),
   generalConstructor :: p (Abstract.Constructor l l f f),
   variable :: p (Abstract.Name l),
   qualifiedVariable :: p (Abstract.QualifiedName l),
   constructor :: p (Abstract.Name l),
   qualifiedConstructor :: p (Abstract.QualifiedName l),
   variableOperator :: p (Abstract.Name l),
   qualifiedVariableOperator :: p (Abstract.QualifiedName l),
   constructorOperator :: p (Abstract.Name l),
   qualifiedConstructorOperator :: p (Abstract.QualifiedName l),
   operator :: p (Abstract.Name l),
   qualifiedOperator :: p (Abstract.QualifiedName l),
   literal :: p (Abstract.Value l l f f)
}

grammar2010 :: (LexicalParsing (Parser (HaskellGrammar AST.Language NodeWrap) t), Ord t, Show t, OutlineMonoid t)
            => Grammar (HaskellGrammar AST.Language NodeWrap) Parser t
grammar2010 = fixGrammar grammar
   
grammar :: forall l g t. (Abstract.Haskell l, LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t)
        => GrammarBuilder (HaskellGrammar l NodeWrap) g Parser t
grammar g@HaskellGrammar{..} = HaskellGrammar{
   haskellModule = wrap (whiteSpace
                         *> (uncurry <$> (Abstract.namedModule <$ keyword "module" <*> moduleId
                                                               <*> optional exports <* keyword "where"
                                          <|> pure Abstract.anonymousModule)
                             <*> body)),
   body = let ordered impdecs
                 | null rightImports = Just (leftImports, rightDeclarations)
                 | otherwise = Nothing
                 where (lefts, rest) = span isLeft impdecs
                       (leftImports, _) = partitionEithers lefts
                       (rightImports, rightDeclarations) = partitionEithers rest
          in mapMaybe ordered (blockOf (Left <$> wrap importDeclaration <|> Right <$> wrap topLevelDeclaration))
             <?> "imports followed by declarations",

-- module 	→ 	module modid [exports] where body 
-- 	| 	body
-- body 	→ 	{ impdecls ; topdecls }
-- 	| 	{ impdecls }
-- 	| 	{ topdecls }

   exports = parens (wrap export `sepBy` comma),
   export = Abstract.exportVar <$> qualifiedVariable
            <|> Abstract.exportClassOrType <$> qualifiedTypeConstructor <*> optional members
            <|> Abstract.reExportModule <$> moduleId,
   importDeclaration = Abstract.importDeclaration <$ keyword "import"
                       <*> (True <$ keyword "qualified" <|> pure False) <*> moduleId
                       <*> optional (keyword "as" *> moduleId) <*> optional (wrap importSpecification),
   importSpecification = (pure Abstract.includedImports <|> Abstract.excludedImports <$ keyword "hiding")
                         <*> parens (wrap importItem `sepBy` comma <* optional comma),
   importItem = Abstract.importVar <$> variable
                <|> Abstract.importClassOrType <$> typeConstructor <*> optional members,
   members = parens (Abstract.allMembers <$ delimiter ".."
                     <|> Abstract.memberList <$> (cname `sepBy` comma) <* optional comma),
   cname = variable <|> constructor,

-- impdecls 	→ 	impdecl1 ; … ; impdecln 	    (n ≥ 1)
-- exports 	→ 	( export1 , … , exportn [ , ] ) 	    (n ≥ 0)
-- export 	→ 	qvar
-- 	| 	qtycon [(..) | ( cname1 , … , cnamen )] 	    (n ≥ 0)
-- 	| 	qtycls [(..) | ( qvar1 , … , qvarn )] 	    (n ≥ 0)
-- 	| 	module modid
--     
-- impdecl 	→ 	import [qualified] modid [as modid] [impspec]
-- 	| 		    (empty declaration)
-- impspec 	→ 	( import1 , … , importn [ , ] ) 	    (n ≥ 0)
-- 	| 	hiding ( import1 , … , importn [ , ] ) 	    (n ≥ 0)
-- import 	→ 	var
-- 	| 	tycon [ (..) | ( cname1 , … , cnamen )] 	    (n ≥ 0)
-- 	| 	tycls [(..) | ( var1 , … , varn )] 	    (n ≥ 0)
-- cname 	→ 	var | con

   topLevelDeclaration =
      Abstract.typeSynonymDeclaration <$ keyword "type" <*> wrap simpleType <* delimiter "=" <*> wrap typeTerm
      <|> Abstract.dataDeclaration <$ keyword "data" <*> wrap (context <* delimiter "=>" <|> pure Abstract.noContext)
          <*> wrap simpleType <*> (delimiter "=" *> declaredConstructors <|> pure []) <*> derivingClause
      <|> Abstract.newtypeDeclaration <$ keyword "newtype" <*> wrap (context <* delimiter "=>" <|> pure Abstract.noContext)
          <*> wrap simpleType <* delimiter "=" <*> wrap newConstructor <*> derivingClause
      <|> Abstract.classDeclaration <$ keyword "class" <*> wrap (simpleContext <* delimiter "=>" <|> pure Abstract.noContext)
          <*> wrap (Abstract.simpleTypeLHS <$> typeClass <*> ((:[]) <$> typeVar))
          <*> (keyword "where" *> blockOf (wrap inClassDeclaration) <|> pure [])
      <|> Abstract.instanceDeclaration <$ keyword "instance"
          <*> wrap (simpleContext <* delimiter "=>" <|> pure Abstract.noContext)
          <*> wrap (Abstract.generalTypeLHS <$> qualifiedTypeClass <*> wrap instanceDesignator)
          <*> (keyword "where" *> blockOf (wrap inInstanceDeclaration) <|> pure [])
      <|> Abstract.defaultDeclaration <$ keyword "default" <*> parens (wrap typeTerm `sepBy` comma)
      <|> foreignDeclaration
      <|> declaration,
       
-- topdecls 	→ 	topdecl1 ; … ; topdecln 	    (n ≥ 0)
-- topdecl 	→ 	type simpletype = type
-- 	| 	data [context =>] simpletype [= constrs] [deriving]
-- 	| 	newtype [context =>] simpletype = newconstr [deriving]
-- 	| 	class [scontext =>] tycls tyvar [where cdecls]
-- 	| 	instance [scontext =>] qtycls inst [where idecls]
-- 	| 	default (type1 , … , typen) 	    (n ≥ 0)
-- 	| 	foreign fdecl
-- 	| 	decl

   declarations = blockOf (wrap declaration),
   declaration = generalDeclaration
                 <|> Abstract.equationDeclaration <$> wrap (functionLHS <|> Abstract.patternLHS <$> wrap pattern)
                                                  <*> wrap rhs <*> whereClauses,
   inClassDeclaration = generalDeclaration <|> inInstanceDeclaration,
   inInstanceDeclaration = Abstract.equationDeclaration <$> wrap (functionLHS <|> Abstract.variableLHS <$> variable)
                                                        <*> wrap rhs <*> whereClauses,
   generalDeclaration =
      Abstract.typeSignature <$> variables <* delimiter "::"
                             <*> wrap (context <* delimiter "=>" <|> pure Abstract.noContext) <*> wrap typeTerm
      <|> Abstract.fixityDeclaration <$> fixity <*> optional (fromIntegral <$> integer)
                                     <*> (operator `sepByNonEmpty` comma),
   whereClauses = keyword "where" *> declarations <|> pure [],
   variables = variable `sepByNonEmpty` comma,
   fixity = Abstract.infixLeft <$ keyword "infixl"
            <|> Abstract.infixRight <$ keyword "infixr"
            <|> Abstract.infixNonAssociative <$ keyword "infix",
   
-- decls 	→ 	{ decl1 ; … ; decln } 	    (n ≥ 0)
-- decl 	→ 	gendecl
-- 	| 	(funlhs | pat) rhs
-- cdecls 	→ 	{ cdecl1 ; … ; cdecln } 	    (n ≥ 0)
-- cdecl 	→ 	gendecl
-- 	| 	(funlhs | var) rhs
-- idecls 	→ 	{ idecl1 ; … ; idecln } 	    (n ≥ 0)
-- idecl 	→ 	(funlhs | var) rhs
-- 	| 		    (empty)
-- gendecl 	→ 	vars :: [context =>] type 	    (type signature)
-- 	| 	fixity [integer] ops 	    (fixity declaration)
-- 	| 		    (empty declaration)
-- ops 	→ 	op1 , … , opn 	    (n ≥ 1)
-- vars 	→ 	var1 , …, varn 	    (n ≥ 1)
-- fixity 	→ 	infixl | infixr | infix

   typeTerm = Abstract.functionType <$> wrap bType <* delimiter "->" <*> wrap typeTerm <|> bType,
   bType = Abstract.typeApplication <$> wrap bType <*> wrap aType <|> aType,
   aType = generalTypeConstructor
           <|> Abstract.typeVariable <$> typeVar
           <|> Abstract.tupleType <$> parens ((:|) <$> wrap typeTerm <*> some (comma *> wrap typeTerm))
           <|> Abstract.listType <$> brackets (wrap typeTerm)
           <|> parens typeTerm,
   generalTypeConstructor = Abstract.constructorType <$> wrap generalConstructor
                            <|> Abstract.functionConstructorType <$ parens (delimiter "->"),
   
-- type 	→ 	btype [-> type] 	    (function type)
-- btype 	→ 	[btype] atype 	    (type application)
-- atype 	→ 	gtycon
-- 	| 	tyvar
-- 	| 	( type1 , … , typek ) 	    (tuple type, k ≥ 2)
-- 	| 	[ type ] 	    (list type)
-- 	| 	( type ) 	    (parenthesized constructor)
-- gtycon 	→ 	qtycon
-- 	| 	() 	    (unit type)
-- 	| 	[] 	    (list constructor)
-- 	| 	(->) 	    (function constructor)
-- 	| 	(,{,}) 	    (tupling constructors)

   context = classConstraint <|> Abstract.constraints <$> parens (wrap classConstraint `sepBy` comma),
   classConstraint = simpleConstraint
                     <|> Abstract.classConstraint <$> qualifiedTypeClass <*> parens (wrap typeApplications),
   typeApplications = Abstract.typeApplication <$> wrap (Abstract.typeVariable <$> typeVar <|> typeApplications)
                                               <*> wrap aType,
   simpleContext = simpleConstraint <|> Abstract.constraints <$> parens (wrap simpleConstraint `sepBy` comma),
   simpleConstraint = Abstract.simpleConstraint <$> qualifiedTypeClass <*> typeVar,
   
-- context 	→ 	class
-- 	| 	( class1 , … , classn ) 	    (n ≥ 0)
-- class 	→ 	qtycls tyvar
-- 	| 	qtycls ( tyvar atype1 … atypen ) 	    (n ≥ 1)
-- scontext 	→ 	simpleclass
-- 	| 	( simpleclass1 , … , simpleclassn ) 	    (n ≥ 0)
-- simpleclass 	→ 	qtycls tyvar

   simpleType = Abstract.simpleTypeLHS <$> typeConstructor <*> many typeVar,
   declaredConstructors = wrap declaredConstructor `sepBy1` delimiter "|",
   declaredConstructor = Abstract.constructor <$> constructor
                                              <*> many (wrap
                                                        $ aType <|> Abstract.strictType <$ delimiter "!" <*> wrap aType)
                         <|> wrap infixConstructorArgType
                             <**> (constructorOperator
                                   <**> (wrap infixConstructorArgType
                                         <**> (pure $ \right op left-> Abstract.constructor op [left, right])))
                         <|> Abstract.recordConstructor <$> constructor
                                                        <*> braces (wrap fieldDeclaration `sepBy` comma),
   infixConstructorArgType = bType <|> Abstract.strictType <$ delimiter "!" <*> wrap aType,
   newConstructor = Abstract.constructor <$> constructor <*> ((:[]) <$> wrap aType)
                    <|> Abstract.recordConstructor <$> constructor
                        <*> braces ((:[]) <$> wrap (Abstract.constructorFields <$> ((:|[]) <$> variable)
                                                    <* delimiter "::" <*> wrap typeTerm)),
   fieldDeclaration = Abstract.constructorFields <$> variables <* delimiter "::"
                      <*> wrap (typeTerm <|> Abstract.strictType <$ delimiter "!" <*> wrap aType),

-- simpletype 	→ 	tycon tyvar1 … tyvark 	    (k ≥ 0)
-- constrs 	→ 	constr1 | … | constrn 	    (n ≥ 1)
-- constr 	→ 	con [!] atype1 … [!] atypek 	    (arity con  =  k, k ≥ 0)
-- 	| 	(btype | ! atype) conop (btype | ! atype) 	    (infix conop)
-- 	| 	con { fielddecl1 , … , fielddecln } 	    (n ≥ 0)
-- newconstr 	→ 	con atype
-- 	| 	con { var :: type }
-- fielddecl 	→ 	vars :: (type | ! atype)

   derivingClause = keyword "deriving"
                    *> (pure <$> wrap (Abstract.simpleDerive <$> qualifiedTypeClass)
                        <|> parens (wrap (Abstract.simpleDerive <$> qualifiedTypeClass) `sepBy` comma))
                    <|> pure [],
   instanceDesignator = generalTypeConstructor
                        <|> parens (typeVarApplications <|> Abstract.tupleType <$> typeVarTuple)
                        <|> Abstract.listType <$> brackets (wrap $ Abstract.typeVariable <$> typeVar)
                        <|> parens (Abstract.functionType <$> wrap (Abstract.typeVariable <$> typeVar) <* delimiter "->"
                                                          <*> wrap (Abstract.typeVariable <$> typeVar)),
   typeVarApplications = generalTypeConstructor
                         <|> Abstract.typeApplication <$> wrap typeVarApplications
                                                      <*> wrap (Abstract.typeVariable <$> typeVar),
   typeVarTuple = (:|) <$> wrap (Abstract.typeVariable <$> typeVar)
                       <*> some (comma *> wrap (Abstract.typeVariable <$> typeVar)),
   
-- deriving 	→ 	deriving (dclass | (dclass1, … , dclassn)) 	    (n ≥ 0)
-- dclass 	→ 	qtycls
-- inst 	→ 	gtycon
-- 	| 	( gtycon tyvar1 … tyvark ) 	    (k ≥ 0, tyvars distinct)
-- 	| 	( tyvar1 , … , tyvark ) 	    (k ≥ 2, tyvars distinct)
-- 	| 	[ tyvar ]
-- 	| 	( tyvar1 -> tyvar2 ) 	    tyvar1 and tyvar2 distinct

   foreignDeclaration = Abstract.foreignImport <$ keyword "import" <*> callingConvention <*> optional safety
                                               <*> optional stringLiteral <*> variable <* delimiter "::"
                                               <*> wrap foreignType
                        <|> Abstract.foreignExport <$ keyword "export" <*> callingConvention
                                                   <*> optional stringLiteral <*> variable <* delimiter "::"
                                                   <*> wrap foreignType,
   callingConvention = Abstract.cCall <$ keyword "ccall" <|> Abstract.stdCall <$ keyword "stdcall"
                       <|> Abstract.cppCall <$ keyword "cplusplus" <|> Abstract.jvmCall <$ keyword "jvm"
                       <|> Abstract.dotNetCall <$ keyword "dotnet",
   safety = Abstract.safeCall <$ keyword "safe" <|> Abstract.unsafeCall <$ keyword "unsafe",
   foreignType = Abstract.functionType <$> wrap foreignArgType <* delimiter "->" <*> wrap foreignType
                 <|> foreignReturnType,
   foreignReturnType = foreignArgType
                       <|> Abstract.constructorType <$> wrap (Abstract.unitConstructor
                                                              <$ delimiter "(" <* delimiter ")"),
   foreignArgType = Abstract.constructorType <$> wrap (Abstract.constructorReference <$> qualifiedTypeConstructor)
                    <|> Abstract.typeApplication <$> wrap foreignArgType <*> wrap (Abstract.strictType <$> wrap aType),

-- fdecl 	→ 	import callconv [safety] impent var :: ftype 	    (define variable)
-- 	| 	export callconv expent var :: ftype 	    (expose variable)
-- callconv 	→ 	ccall | stdcall | cplusplus 	    (calling convention)
-- 	| 	jvm | dotnet
-- 	| 	 system-specific calling conventions
-- impent 	→ 	[string] 	    (see Section 8.5.1)
-- expent 	→ 	[string] 	    (see Section 8.5.1)
-- safety 	→ 	unsafe | safe
-- ftype 	→ 	frtype
-- 	| 	fatype  →  ftype
-- frtype 	→ 	fatype
-- 	| 	()
-- fatype 	→ 	qtycon atype1 … atypek 	    (k  ≥  0)

   functionLHS = Abstract.prefixLHS <$> wrap (Abstract.variableLHS <$> variable <|> parens functionLHS)
                                    <*> (NonEmpty.fromList <$> some (wrap aPattern))
                 <|> Abstract.infixLHS <$> wrap pattern <*> variableOperator <*> wrap pattern,
   rhs = Abstract.normalRHS <$ delimiter "=" <*> expression
         <|> Abstract.guardedRHS . NonEmpty.fromList
             <$> some (wrap $ Abstract.guardedExpression . toList <$> guards <* delimiter "=" <*> expression),
   guards = delimiter "|" *> wrap guard `sepByNonEmpty` comma,
   guard = Abstract.bindStatement <$> wrap pattern <* delimiter "<-" <*> infixExpression
           <|> Abstract.letStatement <$ keyword "let" <*> declarations
           <|> Abstract.expressionStatement <$> infixExpression,

-- funlhs 	→ 	var apat { apat }
-- 	| 	pat varop pat
-- 	| 	( funlhs ) apat { apat }
-- rhs 	→ 	= exp [where decls]
-- 	| 	gdrhs [where decls]
-- gdrhs 	→ 	guards = exp [gdrhs]
-- guards 	→ 	| guard1, …, guardn 	    (n ≥ 1)
-- guard 	→ 	pat <- infixexp 	    (pattern guard)
-- 	| 	let decls 	    (local declaration)
-- 	| 	infixexp 	    (boolean guard)

   expression = wrap (Abstract.typedExpression <$> infixExpression <* delimiter "::" <*> wrap typeTerm)
                <|> infixExpression,
   infixExpression = wrap (Abstract.infixExpression <$> dExpression
                                                    <*> wrap (Abstract.referenceExpression <$> qualifiedOperator)
                                                    <*> infixExpression
                           <|> Abstract.applyExpression <$> wrap (Abstract.negate <$ delimiter "-") <*> infixExpression)
                     <|> lExpression,
   lExpression = wrap (Abstract.lambdaExpression <$ delimiter "\\" <*> some (wrap aPattern) <* delimiter "->"
                                                 <*> expression
                       <|> Abstract.letExpression <$ keyword "let" <*> declarations <* keyword "in" <*> expression
                       <|> Abstract.conditionalExpression <$ keyword "if" <*> expression <* optional semi
                                                          <* keyword "then" <*> expression <* optional semi
                                                          <* keyword "else" <*> expression)
                 <|> dExpression,
   dExpression = wrap (Abstract.caseExpression <$ keyword "case" <*> expression <* delimiter "of"
                                                    <*> blockOf (wrap alternative)
                       <|> Abstract.doExpression <$ keyword "do" <*> wrap statements)
                 <|> fExpression,
   fExpression = wrap (Abstract.applyExpression <$> fExpression <*> aExpression) <|> aExpression,
   aExpression = wrap (Abstract.referenceExpression <$> qualifiedVariable
                       <|> Abstract.constructorExpression <$> wrap generalConstructor
                       <|> Abstract.literalExpression <$> wrap literal
                       <|> Abstract.tupleExpression <$> parens ((:|) <$> expression <*> some (comma *> expression))
                       <|> brackets (Abstract.listExpression <$> (expression `sepBy1` comma)
                                     <|> Abstract.sequenceExpression <$> expression
                                         <*> optional (comma *> expression)
                                         <* delimiter ".." <*> optional expression
                                     <|> Abstract.listComprehension <$> expression
                                         <* delimiter "|"
                                         <*> (either id (rewrap Abstract.expressionStatement) <$> statement)
                                             `sepByNonEmpty` comma)
                       <|> parens (Abstract.leftSectionExpression <$> infixExpression <*> qualifiedOperator
                                   <|> Abstract.rightSectionExpression
                                          <$> (notFollowedBy (string "-" <* notSatisfyChar isSymbol)
                                               *> qualifiedOperator)
                                          <*> infixExpression)
                       <|> Abstract.recordExpression <$> wrap (Abstract.constructorExpression
                                                               <$> wrap (Abstract.constructorReference
                                                                         <$> qualifiedConstructor))
                                                     <*> braces (pure [])
                       <|> Abstract.recordExpression <$> aExpression <*> braces (wrap fieldBinding `sepBy1` comma))
                 <|> parens expression,
   alternative = Abstract.caseAlternative <$> wrap pattern
                 <*> wrap (Abstract.normalRHS <$ delimiter "->" <*> expression
                           <|> Abstract.guardedRHS . NonEmpty.fromList
                               <$> some (wrap $ Abstract.guardedExpression . toList <$> guards <* delimiter "->"
                                                                                    <*> expression))
                 <*> whereClauses,
   statements = braces (Abstract.guardedExpression
                        <$> many (either id (rewrap Abstract.expressionStatement) <$> statement <* some semi)
                        <*> expression <* optional semi)
                <|> (blockOf statement >>= \stats-> if null stats then fail "empty do block"
                     else Abstract.guardedExpression (either id (rewrap Abstract.expressionStatement) <$> init stats)
                          <$> either (const $ fail "do block must end with an expression") pure (last stats)),
   statement = Left <$> wrap (Abstract.bindStatement <$> wrap pattern <* delimiter "<-" <*> expression
                              <|> Abstract.letStatement <$ keyword "let" <*> declarations)
               <|> Right <$> expression,
   fieldBinding = Abstract.fieldBinding <$> qualifiedVariable <*> expression,
                
-- exp 	→ 	infixexp :: [context =>] type 	    (expression type signature)
-- 	| 	infixexp
-- infixexp 	→ 	lexp qop infixexp 	    (infix operator application)
-- 	| 	- infixexp 	    (prefix negation)
-- 	| 	lexp
-- lexp 	→ 	\ apat1 … apatn -> exp 	    (lambda abstraction, n ≥ 1)
-- 	| 	let decls in exp 	    (let expression)
-- 	| 	if exp [;] then exp [;] else exp 	    (conditional)
-- 	| 	case exp of { alts } 	    (case expression)
-- 	| 	do { stmts } 	    (do expression)
-- 	| 	fexp
-- fexp 	→ 	[fexp] aexp 	    (function application)
-- aexp 	→ 	qvar 	    (variable)
-- 	| 	gcon 	    (general constructor)
-- 	| 	literal
-- 	| 	( exp ) 	    (parenthesized expression)
-- 	| 	( exp1 , … , expk ) 	    (tuple, k ≥ 2)
-- 	| 	[ exp1 , … , expk ] 	    (list, k ≥ 1)
-- 	| 	[ exp1 [, exp2] .. [exp3] ] 	    (arithmetic sequence)
-- 	| 	[ exp | qual1 , … , qualn ] 	    (list comprehension, n ≥ 1)
-- 	| 	( infixexp qop ) 	    (left section)
-- 	| 	( qop⟨-⟩ infixexp ) 	    (right section)
-- 	| 	qcon { fbind1 , … , fbindn } 	    (labeled construction, n ≥ 0)
-- 	| 	aexp⟨qcon⟩ { fbind1 , … , fbindn } 	    (labeled update, n  ≥  1)
-- qual 	→ 	pat <- exp 	    (generator)
-- 	| 	let decls 	    (local declaration)
-- 	| 	exp 	    (guard)
-- alts 	→ 	alt1 ; … ; altn 	    (n ≥ 1)
-- alt 	→ 	pat -> exp [where decls]
-- 	| 	pat gdpat [where decls]
-- 	| 		    (empty alternative)
-- gdpat 	→ 	guards -> exp [ gdpat ]
-- stmts 	→ 	stmt1 … stmtn exp [;] 	    (n ≥ 0)
-- stmt 	→ 	exp ;
-- 	| 	pat <- exp ;
-- 	| 	let decls ;
-- 	| 	; 	    (empty statement)
-- fbind 	→ 	qvar = exp

   pattern = Abstract.infixPattern <$> wrap lPattern <*> qualifiedConstructorOperator <*> wrap pattern <|> lPattern,
   lPattern = aPattern
              <|> Abstract.literalPattern <$> wrap ((Abstract.integerLiteral . negate) <$ delimiter "-" <*> integer)
              <|> Abstract.literalPattern <$> wrap ((Abstract.floatingLiteral . negate) <$ delimiter "-" <*> float)
              <|> Abstract.constructorPattern <$> wrap generalConstructor <*> some (wrap aPattern),
   aPattern = Abstract.variablePattern <$> variable
              <|> Abstract.asPattern <$> variable <* delimiter "@" <*> wrap aPattern
              <|> Abstract.constructorPattern <$> wrap generalConstructor <*> pure []
              <|> Abstract.recordPattern <$> qualifiedConstructor <*> braces (wrap fieldPattern `sepBy` comma)
              <|> Abstract.literalPattern <$> wrap literal
              <|> Abstract.wildcardPattern <$ keyword "_"
              <|> parens (pattern
                          <|> Abstract.tuplePattern
                              <$> ((:|) <$> wrap pattern <*> some (delimiter "," *> wrap pattern)))
              <|> Abstract.listPattern <$> brackets (wrap pattern `sepBy` comma)
              <|> Abstract.irrefutablePattern <$ delimiter "~" <*> wrap aPattern,
   fieldPattern = Abstract.fieldPattern <$> qualifiedVariable <* delimiter "=" <*> wrap pattern,
   generalConstructor = Abstract.constructorReference <$> qualifiedConstructor
                        <|> Abstract.unitConstructor <$ delimiter "(" <* delimiter ")"
                        <|> Abstract.emptyListConstructor <$ delimiter "[" <* delimiter "]"
                        <|> Abstract.tupleConstructor . length <$> parens (some comma),

-- pat 	→ 	lpat qconop pat 	    (infix constructor)
-- 	| 	lpat
-- lpat 	→ 	apat
-- 	| 	- (integer | float) 	    (negative literal)
-- 	| 	gcon apat1 … apatk 	    (arity gcon  =  k, k ≥ 1)
-- apat 	→ 	var [ @ apat] 	    (as pattern)
-- 	| 	gcon 	    (arity gcon  =  0)
-- 	| 	qcon { fpat1 , … , fpatk } 	    (labeled pattern, k ≥ 0)
-- 	| 	literal
-- 	| 	_ 	    (wildcard)
-- 	| 	( pat ) 	    (parenthesized pattern)
-- 	| 	( pat1 , … , patk ) 	    (tuple pattern, k ≥ 2)
-- 	| 	[ pat1 , … , patk ] 	    (list pattern, k ≥ 1)
-- 	| 	~ apat 	    (irrefutable pattern)
-- fpat 	→ 	qvar = pat
-- gcon 	→ 	()
-- 	| 	[]
-- 	| 	(,{,})
-- 	| 	qcon
 
   variable = variableIdentifier <|> parens variableSymbol,
   qualifiedVariable = qualifiedVariableIdentifier <|> parens qualifiedVariableSymbol,
   constructor = constructorIdentifier <|> parens constructorSymbol,
   qualifiedConstructor = qualifiedConstructorIdentifier <|> parens qualifiedConstructorSymbol,
   variableOperator = variableSymbol <|> delimiter "`" *> variableIdentifier <* delimiter "`",
   qualifiedVariableOperator = qualifiedVariableSymbol
                               <|> delimiter "`" *> qualifiedVariableIdentifier <* delimiter "`",
   constructorOperator = constructorSymbol <|> delimiter "`" *> constructorIdentifier <* delimiter "`",
   qualifiedConstructorOperator = qualifiedConstructorSymbol
                                  <|> delimiter "`" *> qualifiedConstructorIdentifier <* delimiter "`",
   operator = variableOperator <|> constructorOperator,
   qualifiedOperator = qualifiedVariableOperator <|> qualifiedConstructorOperator <?> "qualified operator",
   
-- var 	→ 	varid | ( varsym ) 	    (variable)
-- qvar 	→ 	qvarid | ( qvarsym ) 	    (qualified variable)
-- con 	→ 	conid | ( consym ) 	    (constructor)
-- qcon 	→ 	qconid | ( gconsym ) 	    (qualified constructor)
-- varop 	→ 	varsym | `  varid ` 	    (variable operator)
-- qvarop 	→ 	qvarsym | `  qvarid ` 	    (qualified variable operator)
-- conop 	→ 	consym | `  conid ` 	    (constructor operator)
-- qconop 	→ 	gconsym | `  qconid ` 	    (qualified constructor operator)
-- op 	→ 	varop | conop 	    (operator)
-- qop 	→ 	qvarop | qconop 	    (qualified operator)
-- gconsym 	→ 	: | qconsym 

-- Lexical Syntax

   literal = Abstract.integerLiteral <$> integer <|> Abstract.floatingLiteral <$> float
             <|> Abstract.charLiteral <$> charLiteral <|> Abstract.stringLiteral <$> stringLiteral
}

-- literal 	→ 	integer | float | char | string
-- special 	→ 	( | ) | , | ; | [ | ] | ` | { | }
 
-- whitespace 	→ 	whitestuff {whitestuff}
-- whitestuff 	→ 	whitechar | comment | ncomment
-- whitechar 	→ 	newline | vertab | space | tab | uniWhite
-- newline 	→ 	return linefeed | return | linefeed | formfeed
-- return 	→ 	a carriage return
-- linefeed 	→ 	a line feed
-- vertab 	→ 	a vertical tab
-- formfeed 	→ 	a form feed
-- space 	→ 	a space
-- tab 	→ 	a horizontal tab
-- uniWhite 	→ 	any Unicode character defined as whitespace
 
-- comment 	→ 	dashes [ any⟨symbol⟩ {any} ] newline
-- dashes 	→ 	-- {-}
-- opencom 	→ 	{-
-- closecom 	→ 	-}
-- ncomment 	→ 	opencom ANY seq {ncomment ANY seq} closecom
-- ANY seq 	→ 	{ANY }⟨{ANY } ( opencom | closecom ) {ANY }⟩
-- ANY 	→ 	graphic | whitechar
-- any 	→ 	graphic | space | tab
-- graphic 	→ 	small | large | symbol | digit | special | " | '
 
-- small 	→ 	ascSmall | uniSmall | _
-- ascSmall 	→ 	a | b | … | z
-- uniSmall 	→ 	any Unicode lowercase letter
 
-- large 	→ 	ascLarge | uniLarge
-- ascLarge 	→ 	A | B | … | Z
-- uniLarge 	→ 	any uppercase or titlecase Unicode letter
-- symbol 	→ 	ascSymbol | uniSymbol⟨special | _ | " | '⟩
 
-- ascSymbol 	→ 	! | # | $ | % | & | ⋆ | + | . | / | < | = | > | ? | @
-- 	| 	\ | ^ | | | - | ~ | :
-- uniSymbol 	→ 	any Unicode symbol or punctuation
-- digit 	→ 	ascDigit | uniDigit
-- ascDigit 	→ 	0 | 1 | … | 9
-- uniDigit 	→ 	any Unicode decimal digit
-- octit 	→ 	0 | 1 | … | 7
-- hexit 	→ 	digit | A | … | F | a | … | f
 

constructorIdentifier, constructorSymbol, typeClass, typeConstructor, typeVar, variableIdentifier,
   variableSymbol :: (Abstract.Haskell l, LexicalParsing (Parser g t), Ord t, Show t, TextualMonoid t)
                  => Parser g t (Abstract.Name l)
variableIdentifier = token (Abstract.name . Text.pack . toString mempty <$> variableLexeme)
constructorIdentifier = token (Abstract.name . Text.pack . toString mempty <$> constructorLexeme)
variableSymbol = token (Abstract.name . Text.pack . toString mempty <$> variableSymbolLexeme)
constructorSymbol = token (Abstract.name . Text.pack . toString mempty <$> constructorSymbolLexeme)

variableLexeme, constructorLexeme, variableSymbolLexeme, constructorSymbolLexeme,
   identifierTail :: (Ord t, Show t, TextualMonoid t) => Parser g t t
variableLexeme = filter (`Set.notMember` reservedWords) (satisfyCharInput varStart <> identifierTail) <?> "variable"
   where varStart c = Char.isLower c ||  c == '_'
constructorLexeme = satisfyCharInput Char.isUpper <> identifierTail <?> "constructor"
variableSymbolLexeme = filter validSymbol (takeCharsWhile1 isSymbol) <?> "variable symbol"
   where validSymbol tok = Textual.characterPrefix tok /= Just ':' && tok `Set.notMember` reservedOperators
constructorSymbolLexeme = filter (`Set.notMember` reservedOperators) (string ":" <> takeCharsWhile isSymbol)
identifierTail = takeCharsWhile isNameTailChar

reservedWords, reservedOperators :: (Ord t, TextualMonoid t) => Set.Set t
reservedWords = Set.fromList ["case", "class", "data", "default", "deriving", "do", "else",
                              "foreign", "if", "import", "in", "infix", "infixl",
                              "infixr", "instance", "let", "module", "newtype", "of",
                              "then", "type", "where", "_"]
reservedOperators = Set.fromList ["--", "..", ":", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>"]

asciiSymbols :: Set.Set Char
asciiSymbols = Set.fromList "!#$%&⋆+./<=>?@\\^|-~:"

-- varid 	→ 	(small {small | large | digit | ' })⟨reservedid⟩
-- conid 	→ 	large {small | large | digit | ' }
-- reservedid 	→ 	case | class | data | default | deriving | do | else
-- 	| 	foreign | if | import | in | infix | infixl
-- 	| 	infixr | instance | let | module | newtype | of
-- 	| 	then | type | where | _
-- varsym 	→ 	( symbol⟨:⟩ {symbol} )⟨reservedop | dashes⟩
-- consym 	→ 	( : {symbol})⟨reservedop⟩
-- reservedop 	→ 	.. | : | :: | = | \ | | | <- | -> |  @ | ~ | =>

typeVar = variableIdentifier
typeConstructor = constructorIdentifier
typeClass = constructorIdentifier

moduleId :: (Abstract.Haskell l, LexicalParsing (Parser g t), Ord t, Show t, TextualMonoid t)
         => Parser g t (Abstract.ModuleName l)
moduleId = Abstract.moduleName <$> token moduleLexeme

moduleLexeme :: (Abstract.Haskell l, LexicalParsing (Parser g t), Ord t, Show t, TextualMonoid t)
             => Parser g t (NonEmpty (Abstract.Name l))
moduleLexeme = (Abstract.name . Text.pack . toString mempty <$> constructorLexeme) `sepByNonEmpty` string "."

qualifier :: (Abstract.Haskell l, LexicalParsing (Parser g t), Ord t, Show t, TextualMonoid t)
          => Parser g t (Abstract.Name l -> Abstract.QualifiedName l)
qualifier = Abstract.qualifiedName <$> takeOptional (Abstract.moduleName <$> moduleLexeme <* string ".")

qualifiedConstructorIdentifier, qualifiedConstructorSymbol, qualifiedTypeClass, qualifiedTypeConstructor,
   qualifiedVariableIdentifier, qualifiedVariableSymbol
   :: (Abstract.Haskell l, LexicalParsing (Parser g t), Ord t, Show t, TextualMonoid t)
   => Parser g t (Abstract.QualifiedName l)
qualifiedVariableIdentifier = token (qualifier <*> variableIdentifier)
qualifiedConstructorIdentifier = token (qualifier <*> constructorIdentifier)
qualifiedTypeConstructor = qualifiedConstructorIdentifier
qualifiedTypeClass = qualifiedConstructorIdentifier
qualifiedVariableSymbol = token (qualifier <*> variableSymbol)
qualifiedConstructorSymbol = token (qualifier <*> constructorSymbol)

-- varid 	     	    (variables)
-- conid 	     	    (constructors)
-- tyvar 	→ 	varid 	    (type variables)
-- tycon 	→ 	conid 	    (type constructors)
-- tycls 	→ 	conid 	    (type classes)
-- modid 	→ 	{conid .} conid 	    (modules)
-- qvarid 	→ 	[ modid . ] varid
-- qconid 	→ 	[ modid . ] conid
-- qtycon 	→ 	[ modid . ] tycon
-- qtycls 	→ 	[ modid . ] tycls
-- qvarsym 	→ 	[ modid . ] varsym
-- qconsym 	→ 	[ modid . ] consym

decimal, octal, hexadecimal, exponent :: (Show t, TextualMonoid t) => Parser g t t
integer :: (LexicalParsing (Parser g t), Show t, TextualMonoid t) => Parser g t Integer
float :: (LexicalParsing (Parser g t), Show t, TextualMonoid t) => Parser g t Rational
decimal = takeCharsWhile1 Char.isDigit <?> "decimal number"
octal = takeCharsWhile1 Char.isOctDigit <?> "octal number"
hexadecimal = takeCharsWhile1 Char.isHexDigit <?> "hexadecimal number"
integer = fst . head
          <$> token (Numeric.readDec . toString mempty <$> decimal <* notFollowedBy exponent
                     <|> (string "0o" <|> string "0O") *> (Numeric.readOct . toString mempty <$> octal)
                     <|> (string "0x" <|> string "0X") *> (Numeric.readHex . toString mempty <$> hexadecimal))
float = fst . head . Numeric.readFloat . toString mempty
        <$> token (decimal <> string "." <> decimal <> (exponent <<|> mempty)
                   <|> decimal <> exponent)
exponent = (string "e" <|> string "E") <> (string "+" <|> string "-" <|> mempty) <> decimal

-- decimal 	→ 	digit{digit}
-- octal 	→ 	octit{octit}
-- hexadecimal 	→ 	hexit{hexit}
-- integer 	→ 	decimal
-- 	| 	0o octal | 0O octal
-- 	| 	0x hexadecimal | 0X hexadecimal
-- float 	→ 	decimal . decimal [exponent]
-- 	| 	decimal exponent
-- exponent 	→ 	(e | E) [+ | -] decimal

charLiteral :: (LexicalParsing (Parser g t), Show t, TextualMonoid t) => Parser g t Char
stringLiteral :: (LexicalParsing (Parser g t), Show t, TextualMonoid t) => Parser g t Text
charLiteral = token (char '\''
                     *> (Text.Parser.Char.satisfy (\c-> c == ' ' || not (Char.isSpace c) && c /= '\'' && c /= '\\')
                         <|> escape)
                     <* char '\'')
stringLiteral = Text.pack . toString mempty <$>
                token (char '"'
                       *> concatMany (takeCharsWhile1 (\c-> c == ' ' || not (Char.isSpace c) && c /= '"' && c /= '\\')
                                      <|> Textual.singleton <$> escape
                                      <|> char '\\'
                                          *> (char '&' <|> takeCharsWhile1 Char.isSpace *> char '\\')
                                          *> pure "")
                           <* char '"')
                <?> "string literal"

escape, asciiEscape, charEscape,
   controlEscape :: (LexicalParsing (Parser g t), Show t, TextualMonoid t) => Parser g t Char
escape = string "\\" *> (charEscape <|> asciiEscape
                         <|> Char.chr . fst . head <$> (Numeric.readDec . toString mempty <$> decimal
                                                        <|> string "o" *> (Numeric.readOct . toString mempty <$> octal)
                                                        <|> string "x"
                                                            *> (Numeric.readHex . toString mempty <$> hexadecimal)))
charEscape = '\a' <$ char 'a'
             <|> '\b' <$ char 'b'
             <|> '\f' <$ char 'f'
             <|> '\n' <$ char 'n'
             <|> '\r' <$ char 'r'
             <|> '\t' <$ char 't'
             <|> '\v' <$ char 'v'
             <|> '\\' <$ char '\\'
             <|> char '\\'
             <|> char '"'
             <|> char '\''
asciiEscape = char '^' *> controlEscape
              <|> '\NUL' <$ string "NUL"
              <|> '\SOH' <$ string "SOH"
              <|> '\STX' <$ string "STX" 
              <|> '\ETX' <$ string "ETX" 
              <|> '\EOT' <$ string "EOT" 
              <|> '\ENQ' <$ string "ENQ" 
              <|> '\ACK' <$ string "ACK"
              <|> '\BEL' <$ string "BEL" 
              <|> '\BS' <$ string "BS" 
              <|> '\HT' <$ string "HT" 
              <|> '\LF' <$ string "LF" 
              <|> '\VT' <$ string "VT" 
              <|> '\FF' <$ string "FF" 
              <|> '\CR' <$ string "CR" 
              <|> '\SO' <$ string "SO" 
              <|> '\SI' <$ string "SI" 
              <|> '\DLE' <$ string "DLE"
              <|> '\DC1' <$ string "DC1" 
              <|> '\DC2' <$ string "DC2" 
              <|> '\DC3' <$ string "DC3" 
              <|> '\DC4' <$ string "DC4" 
              <|> '\NAK' <$ string "NAK" 
              <|> '\SYN' <$ string "SYN" 
              <|> '\ETB' <$ string "ETB" 
              <|> '\CAN' <$ string "CAN"
              <|> '\EM' <$ string "EM" 
              <|> '\SUB' <$ string "SUB" 
              <|> '\ESC' <$ string "ESC" 
              <|> '\FS' <$ string "FS" 
              <|> '\GS' <$ string "GS" 
              <|> '\RS' <$ string "RS" 
              <|> '\US' <$ string "US" 
              <|> '\SP' <$ string "SP" 
              <|> '\DEL' <$ string "DEL"
controlEscape = Char.chr . (-64 +) . Char.ord <$> Text.Parser.Char.satisfy (\c-> c >= '@' && c <= '_')

-- char 	→ 	' (graphic⟨' | \⟩ | space | escape⟨\&⟩) '
-- string 	→ 	" {graphic⟨" | \⟩ | space | escape | gap} "
-- escape 	→ 	\ ( charesc | ascii | decimal | o octal | x hexadecimal )
-- charesc 	→ 	a | b | f | n | r | t | v | \ | " | ' | &
-- ascii 	→ 	^cntrl | NUL | SOH | STX | ETX | EOT | ENQ | ACK
-- 	| 	BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
-- 	| 	DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN
-- 	| 	EM | SUB | ESC | FS | GS | RS | US | SP | DEL
-- cntrl 	→ 	ascLarge | @ | [ | \ | ] | ^ | _
-- gap 	→ 	\ whitechar {whitechar} \

type NodeWrap = (,) (Position, ParsedLexemes, Position)

wrap :: TextualMonoid t => Parser g t a -> Parser g t (NodeWrap a)
wrap = (\p-> liftA3 surround getSourcePos p getSourcePos) . tmap store . ((,) (Trailing []) <$>)
   where surround start (lexemes, p) end = ((start, lexemes, end), p)
         store (wss, (Trailing ws', a)) = (mempty, (Trailing $ ws' <> concat wss, a))

rewrap :: (NodeWrap a -> b) -> NodeWrap a -> NodeWrap b
rewrap f node@((start, _, end), _) = ((start, mempty, end), f node)

instance TokenParsing (Parser (HaskellGrammar l f) (LinePositioned Text)) where
   someSpace = someLexicalSpace
   token = lexicalToken

instance LexicalParsing (Parser (HaskellGrammar l f) (LinePositioned Text)) where
   lexicalComment = do c <- comment
                       lift ([[Comment $ extract c]], ())
   lexicalWhiteSpace = whiteSpace
   isIdentifierStartChar = Char.isLetter
   isIdentifierFollowChar = isNameTailChar
   identifierToken word = lexicalToken (filter (`notElem` reservedWords) word)
   lexicalToken p = snd <$> tmap addOtherToken (match p) <* lexicalWhiteSpace
      where addOtherToken ([], (i, x)) = ([[Token Other $ extract i]], (i, x))
            addOtherToken (t, (i, x)) = (t, (i, x))
   keyword s = lexicalToken (string s
                             *> notSatisfyChar isNameTailChar
                             <* lift ([[Token Keyword $ extract s]], ()))
               <?> ("keyword " <> show s)

isLineChar, isNameTailChar, isSymbol :: Char -> Bool
isLineChar c = c /= '\n' && c /= '\r' && c /= '\f'
isNameTailChar c = Char.isAlphaNum c || c == '_' || c == '\''
isSymbol c = if Char.isAscii c then c `Set.member` asciiSymbols else Char.isSymbol c

delimiter :: (Show t, TextualMonoid t) => LexicalParsing (Parser g t) => t -> Parser g t t
delimiter s = lexicalToken (string s <* lift ([[Token Delimiter $ Text.pack $ toString mempty s]], ()))
              <?> ("delimiter " <> show s)

whiteSpace :: (Show t, TextualMonoid t) => LexicalParsing (Parser g t) => Parser g t ()
whiteSpace = spaceChars *> skipMany (lexicalComment *> spaceChars) <?> "whitespace"
   where spaceChars = (takeCharsWhile1 Char.isSpace
                       >>= \ws-> lift ([[WhiteSpace $ Text.pack $ toString mempty ws]], ()))
                      <<|> pure ()

comment :: (Show t, TextualMonoid t) => Parser g t t
comment = try (blockComment <|> (string "--" <* notSatisfyChar isSymbol) <> takeCharsWhile isLineChar)
          <?> "comment"
   where isCommentChar c = c /= '-' && c /= '{'
         blockComment =
            string "{-"
            <> concatMany (blockComment <<|> (notFollowedBy (string "-}") *> anyToken) <> takeCharsWhile isCommentChar)
            <> string "-}"

blockOf :: (Ord t, Show t, OutlineMonoid t, TokenParsing (Parser g t)) => Parser g t a -> Parser g t [a]
blockOf p = braces (p `startSepEndBy` semi) <|> (inputColumn >>= alignedBlock pure)
   where alignedBlock cont indent =
            do item <- mapMaybe (uncurry $ oneExtendedLine indent) (match p)
               -- don't stop at a higher indent unless there's a terminator
               void (filter (indent >=) inputColumn)
                  <<|> lookAhead (void (Text.Parser.Char.satisfy (`elem` terminators))
                                  <|> (string "else" <|> string "in"
                                       <|> string "of" <|> string "where") *> notSatisfyChar isNameTailChar
                                  <|> eof)
               indent' <- inputColumn
               let cont' = cont . (item :)
               if indent == indent'
                  then many semi *> alignedBlock cont' indent
                  else some semi *> alignedBlock cont' indent <|> cont' []
            <|> cont []
         oneExtendedLine indent input a =
            if null nextLine then Just a
            else case compare (currentColumn nextLine) indent
                 of LT -> Nothing
                    EQ | Textual.takeWhile_ False Char.isLetter nextLine `Set.notMember` reservedWords -> Nothing
                    _ -> oneExtendedLine indent nextLine a
            where nextLine = nextLineOf input
         nextLineOf = dropComments . Textual.dropWhile_ True Char.isSpace . Textual.dropWhile_ True isLineChar
         dropComments s
            | "--" `isPrefixOf` s || "{-" `isPrefixOf` s =
                either (const s) (nextLineOf . snd . snd . head) $ getCompose $ getCompose $ getCompose
                $ simply parsePrefix comment s
            | otherwise = s
         terminators :: [Char]
         terminators = ",;)]}"
-- | Parses a sequence of zero or more occurrences of @p@, separated and optionally started or ended by one or more of
-- @sep@.
startSepEndBy :: Alternative m => m a -> m sep -> m [a]
startSepEndBy p sep = (:) <$> p <*> (sep *> startSepEndBy p sep <|> pure [])
                      <|> sep *> startSepEndBy p sep
                      <|> pure []

class TextualMonoid t => OutlineMonoid t where
   currentColumn :: t -> Int

instance OutlineMonoid (LinePositioned Text) where
   currentColumn = column

inputColumn :: OutlineMonoid t => Parser g t Int
inputColumn = currentColumn <$> getInput

$(Rank2.TH.deriveAll ''HaskellGrammar)
