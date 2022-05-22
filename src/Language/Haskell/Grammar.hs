{-# Language FlexibleContexts, FlexibleInstances, OverloadedStrings,
             Rank2Types, RecordWildCards,
             ScopedTypeVariables, TemplateHaskell, TupleSections, TypeSynonymInstances #-}
{-# OPTIONS_GHC -ddump-splices -ddump-to-file #-}

module Language.Haskell.Grammar where

import Control.Applicative
import Control.Monad (void)
import qualified Data.Char as Char
import Data.Either (lefts, isLeft, partitionEithers)
import Data.Foldable (toList)
import Data.Functor.Compose (Compose(Compose))
import Data.Functor.Identity (Identity)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Ord (Down)
import qualified Data.Monoid.Factorial as Factorial
import qualified Data.Monoid.Textual as Textual
import Data.Monoid.Null (null)
import Data.Monoid.Textual (TextualMonoid, characterPrefix, toString)
import Data.Monoid.Instances.Positioned (LinePositioned, column)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text (Text)
import Numeric (readOct, readDec, readHex, readFloat)
import Witherable (filter, mapMaybe)
import qualified Text.Grampa
import Text.Grampa hiding (keyword)
import Text.Grampa.ContextFree.LeftRecursive.Transformer (ParserT, lift, tmap)
import qualified Text.Parser.Char
import Text.Parser.Combinators (eof, sepBy, sepBy1, sepByNonEmpty, try)
import Text.Parser.Token (braces, brackets, comma, parens)
import qualified Rank2.TH
import qualified Transformation.Deep as Deep
import qualified Transformation.Rank2

import qualified Language.Haskell.Abstract as Abstract
import qualified Language.Haskell.Reserializer as Reserializer
import Language.Haskell.Reserializer (ParsedLexemes(..), Lexeme(..), Serialization, TokenType(..), lexemes)

import Prelude hiding (exponent, filter, null)

type Parser g s = ParserT ((,) [[Lexeme s]]) g s

data HaskellGrammar l t f p = HaskellGrammar {
   haskellModule :: p (f (Abstract.Module l l f f)),
   moduleLevel :: ModuleLevelGrammar l f p,
   declarationLevel :: DeclarationGrammar l f p,
   body :: p ([f (Abstract.Import l l f f)], [f (Abstract.Declaration l l f f)]),
   typeTerm, bType, aType :: p (Abstract.Type l l f f),
   generalTypeConstructor :: p (Abstract.Type l l f f),
   rhs :: p (Abstract.EquationRHS l l f f),
   guards, qualifiers :: p (NonEmpty (f (Abstract.Statement l l f f))),
   guard, qualifier :: p (Abstract.Statement l l f f),
   expression, infixExpression, leftInfixExpression :: p (f (Abstract.Expression l l f f)),
   lExpression, dExpression, fExpression, aExpression :: p (f (Abstract.Expression l l f f)),
   bareExpression, openBlockExpression, closedBlockExpresion :: p (Abstract.Expression l l f f),
   alternatives :: p [f (Abstract.CaseAlternative l l f f)],
   alternative :: p (Abstract.CaseAlternative l l f f),
   statements :: p (Abstract.GuardedExpression l l f f),
   statement :: p (Deep.Sum (Abstract.Statement l l) (Abstract.Expression l l) f f),
   fieldBinding :: p (Abstract.FieldBinding l l f f),
   pattern, lPattern, aPattern :: p (Abstract.Pattern l l f f),
   fieldPattern :: p (Abstract.FieldPattern l l f f),
   generalConstructor :: p (Abstract.Constructor l l f f),
   variable, constructor, variableOperator, constructorOperator, operator :: p (Abstract.Name l),
   qualifiedVariable, qualifiedConstructor :: p (Abstract.QualifiedName l),
   qualifiedVariableOperator, qualifiedConstructorOperator, qualifiedOperator :: p (Abstract.QualifiedName l),
   qualifiedConstructorIdentifier, qualifiedConstructorSymbol, qualifiedTypeConstructor,
      qualifiedVariableIdentifier, qualifiedVariableSymbol :: p (Abstract.QualifiedName l),
   constructorIdentifier, constructorSymbol,
   typeConstructor, typeVar, variableIdentifier, variableSymbol :: p (Abstract.Name l),
   literal :: p (Abstract.Value l l f f),
   doubleColon, rightDoubleArrow, rightArrow, leftArrow :: p (),
   integer, integerLexeme :: p Integer,
   float, floatLexeme :: p Rational,
   decimal, octal, hexadecimal, exponent :: p t,
   charLiteral, charLexeme, escape :: p Char,
   stringLiteral, stringLexeme :: p Text
}

data ModuleLevelGrammar l f p = ModuleLevelGrammar {
   exports :: p [f (Abstract.Export l l f f)],
   export :: p (Abstract.Export l l f f),
   importDeclaration :: p (Abstract.Import l l f f),
   importSpecification :: p (Abstract.ImportSpecification l l f f),
   importItem :: p (Abstract.ImportItem l l f f),
   members :: p (Abstract.Members l),
   cname :: p (Abstract.Name l)
}

data DeclarationGrammar l f p = DeclarationGrammar {
   topLevelDeclaration :: p (Abstract.Declaration l l f f),
   declarations :: p [f (Abstract.Declaration l l f f)],
   declaration :: p (Abstract.Declaration l l f f),
   inClassDeclaration :: p (Abstract.Declaration l l f f),
   inInstanceDeclaration :: p (Abstract.Declaration l l f f),
   equationDeclaration :: p (Abstract.Declaration l l f f),
   generalDeclaration :: p (Abstract.Declaration l l f f),
   whereClauses :: p [f (Abstract.Declaration l l f f)],
   variables :: p (NonEmpty (Abstract.Name l)),
   fixity :: p (Abstract.Associativity l),
   declaredConstructors :: p [f (Abstract.DataConstructor l l f f)],
   declaredConstructor :: p (Abstract.DataConstructor l l f f),
   infixConstructorArgType :: p (Abstract.Type l l f f),
   newConstructor :: p (Abstract.DataConstructor l l f f),
   fieldDeclaration :: p (Abstract.FieldDeclaration l l f f),
   optionalContext, optionalTypeSignatureContext, context, constraint, classConstraint :: p (Abstract.Context l l f f),
   typeApplications :: p (Abstract.Type l l f f),
   simpleType :: p (Abstract.TypeLHS l l f f),
   derivingClause :: p [f (Abstract.DerivingClause l l f f)],
   instanceDesignator :: p (Abstract.ClassInstanceLHS l l f f),
   instanceTypeDesignator :: p (Abstract.Type l l f f),
   typeVarApplications :: p (Abstract.Type l l f f),
   typeVarTuple :: p (NonEmpty (f (Abstract.Type l l f f))),
   foreignDeclaration :: p (Abstract.Declaration l l f f),
   callingConvention :: p (Abstract.CallingConvention l),
   safety :: p (Abstract.CallSafety l),
   foreignType :: p (Abstract.Type l l f f),
   foreignReturnType :: p (Abstract.Type l l f f),
   foreignArgType :: p (Abstract.Type l l f f),
   functionLHS :: p (Abstract.EquationLHS l l f f),
   qualifiedTypeClass :: p (Abstract.QualifiedName l),
   typeClass :: p (Abstract.Name l)
}

$(Rank2.TH.deriveAll ''HaskellGrammar)
$(Rank2.TH.deriveAll ''ModuleLevelGrammar)
$(Rank2.TH.deriveAll ''DeclarationGrammar)

grammar2010 :: (Abstract.Haskell l, LexicalParsing (Parser (HaskellGrammar l t (NodeWrap t)) t),
                Ord t, Show t, OutlineMonoid t,
                Deep.Foldable (Serialization (Down Int) t) (Abstract.CaseAlternative l l),
                Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l),
                Deep.Foldable (Serialization (Down Int) t) (Abstract.Expression l l),
                Deep.Foldable (Serialization (Down Int) t) (Abstract.Import l l),
                Deep.Foldable (Serialization (Down Int) t) (Abstract.Statement l l))
            => Grammar (HaskellGrammar l t (NodeWrap t)) (ParserT ((,) [[Lexeme t]])) t
grammar2010 = fixGrammar grammar

grammar :: forall l g t. (Abstract.Haskell l, LexicalParsing (Parser g t), Ord t, Show t, OutlineMonoid t,
                      Deep.Foldable (Serialization (Down Int) t) (Abstract.CaseAlternative l l),
                      Deep.Foldable (Serialization (Down Int) t) (Abstract.Declaration l l),
                      Deep.Foldable (Serialization (Down Int) t) (Abstract.Expression l l),
                      Deep.Foldable (Serialization (Down Int) t) (Abstract.Import l l),
                      Deep.Foldable (Serialization (Down Int) t) (Abstract.Statement l l))
        => GrammarBuilder (HaskellGrammar l t (NodeWrap t)) g (ParserT ((,) [[Lexeme t]])) t
grammar HaskellGrammar{moduleLevel= ModuleLevelGrammar{..},
                       declarationLevel= DeclarationGrammar{..},
                       ..} = HaskellGrammar{
   haskellModule = wrap (optional (char '\xfeff') *> whiteSpace
                         *> (uncurry <$> (Abstract.namedModule <$ keyword "module" <*> moduleId
                                                               <*> optional exports <* keyword "where"
                                          <|> pure Abstract.anonymousModule)
                             <*> body)),
   body = let ordered impdecs
                 | null rightImports = Just (leftImports, rightDeclarations)
                 | otherwise = Nothing
                 where (prefix, rest) = span isLeft (Deep.eitherFromSum . unwrap <$> impdecs)
                       leftImports = lefts prefix
                       (rightImports, rightDeclarations) = partitionEithers rest
          in mapMaybe ordered (blockOf (Deep.InL <$> wrap importDeclaration
                                        <|> Deep.InR <$> wrap topLevelDeclaration))
             <?> "imports followed by declarations",

-- module 	→ 	module modid [exports] where body 
-- 	| 	body
-- body 	→ 	{ impdecls ; topdecls }
-- 	| 	{ impdecls }
-- 	| 	{ topdecls }
   moduleLevel= ModuleLevelGrammar{
      exports = parens (wrap export `sepBy` comma),
      export = Abstract.exportVar <$> qualifiedVariable
               <|> Abstract.exportClassOrType <$> qualifiedTypeConstructor <*> optional members
               <|> Abstract.reExportModule <$ keyword "module" <*> moduleId,
      importDeclaration = Abstract.importDeclaration <$ keyword "import"
                          <*> (True <$ keyword "qualified" <|> pure False) <*> moduleId
                          <*> optional (keyword "as" *> moduleId) <*> optional (wrap importSpecification),
      importSpecification = (pure Abstract.includedImports <|> Abstract.excludedImports <$ keyword "hiding")
                            <*> parens (wrap importItem `sepBy` comma <* optional comma),
      importItem = Abstract.importVar <$> variable
                   <|> Abstract.importClassOrType <$> typeConstructor <*> optional members,
      members = parens (Abstract.allMembers <$ delimiter ".."
                        <|> Abstract.memberList <$> (cname `sepBy` comma) <* optional comma),
      cname = variable <|> constructor},

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

   declarationLevel= DeclarationGrammar{
      topLevelDeclaration =
         Abstract.typeSynonymDeclaration <$ keyword "type" <*> wrap simpleType <* delimiter "=" <*> wrap typeTerm
         <|> Abstract.dataDeclaration <$ keyword "data"
             <*> wrap optionalContext
             <*> wrap simpleType <*> (delimiter "=" *> declaredConstructors <|> pure []) <*> derivingClause
         <|> Abstract.newtypeDeclaration <$ keyword "newtype"
             <*> wrap optionalContext
             <*> wrap simpleType <* delimiter "=" <*> wrap newConstructor <*> derivingClause
         <|> Abstract.classDeclaration <$ keyword "class"
             <*> wrap optionalContext
             <*> wrap (Abstract.simpleTypeLHS <$> typeClass <*> ((:[]) <$> typeVar))
             <*> (keyword "where" *> blockOf inClassDeclaration <|> pure [])
         <|> Abstract.instanceDeclaration <$ keyword "instance"
             <*> wrap optionalContext
             <*> wrap instanceDesignator
             <*> (keyword "where" *> blockOf inInstanceDeclaration <|> pure [])
         <|> Abstract.defaultDeclaration <$ keyword "default" <*> parens (wrap typeTerm `sepBy` comma)
         <|> keyword "foreign" *> foreignDeclaration
         <|> declaration,

      declarations = blockOf declaration,
      declaration = generalDeclaration
                    <|> Abstract.equationDeclaration <$> wrap (functionLHS <|> Abstract.patternLHS <$> wrap pattern)
                                                     <*> wrap rhs <*> whereClauses,
      inClassDeclaration = generalDeclaration <|> equationDeclaration,
      inInstanceDeclaration = equationDeclaration,
      equationDeclaration = Abstract.equationDeclaration <$> wrap (functionLHS <|> Abstract.variableLHS <$> variable)
                                                         <*> wrap rhs <*> whereClauses,
      generalDeclaration =
         Abstract.typeSignature <$> variables <* doubleColon <*> wrap optionalTypeSignatureContext <*> wrap typeTerm
         <|> Abstract.fixityDeclaration <$> fixity <*> optional (fromIntegral <$> integer)
                                        <*> (operator `sepByNonEmpty` comma),
      optionalTypeSignatureContext = optionalContext,
      whereClauses = keyword "where" *> declarations <|> pure [],
      variables = variable `sepByNonEmpty` comma,
      fixity = Abstract.leftAssociative <$ keyword "infixl"
               <|> Abstract.rightAssociative <$ keyword "infixr"
               <|> Abstract.nonAssociative <$ keyword "infix",

   -- topdecls 	→ 	topdecl1 ; … ; topdecln 	    (n ≥ 0)
   -- topdecl 	→ 	type simpletype = type
   -- 	| 	data [context =>] simpletype [= constrs] [deriving]
   -- 	| 	newtype [context =>] simpletype = newconstr [deriving]
   -- 	| 	class [scontext =>] tycls tyvar [where cdecls]
   -- 	| 	instance [scontext =>] qtycls inst [where idecls]
   -- 	| 	default (type1 , … , typen) 	    (n ≥ 0)
   -- 	| 	foreign fdecl
   -- 	| 	decl
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
                                                       <* doubleColon <*> wrap typeTerm)),
      fieldDeclaration = Abstract.constructorFields <$> variables <* doubleColon
                         <*> wrap (typeTerm <|> Abstract.strictType <$ delimiter "!" <*> wrap aType),

   -- constrs 	→ 	constr1 | … | constrn 	    (n ≥ 1)
   -- constr 	→ 	con [!] atype1 … [!] atypek 	    (arity con  =  k, k ≥ 0)
   -- 	| 	(btype | ! atype) conop (btype | ! atype) 	    (infix conop)
   -- 	| 	con { fielddecl1 , … , fielddecln } 	    (n ≥ 0)
   -- newconstr 	→ 	con atype
   -- 	| 	con { var :: type }
   -- fielddecl 	→ 	vars :: (type | ! atype)

      optionalContext = context <* rightDoubleArrow <|> pure Abstract.noContext,
      context = constraint <|> Abstract.constraints <$> parens (wrap constraint `sepBy` comma),
      constraint = classConstraint,
      classConstraint = Abstract.simpleConstraint <$> qualifiedTypeClass <*> typeVar
                        <|> Abstract.classConstraint <$> qualifiedTypeClass <*> parens (wrap typeApplications),
      typeApplications = Abstract.typeApplication <$> wrap (Abstract.typeVariable <$> typeVar <|> typeApplications)
                                                  <*> wrap aType,
      simpleType = Abstract.simpleTypeLHS <$> typeConstructor <*> many typeVar,

   -- context 	→ 	class
   -- 	| 	( class1 , … , classn ) 	    (n ≥ 0)
   -- class 	→ 	qtycls tyvar
   -- 	| 	qtycls ( tyvar atype1 … atypen ) 	    (n ≥ 1)
   -- scontext 	→ 	simpleclass
   -- 	| 	( simpleclass1 , … , simpleclassn ) 	    (n ≥ 0)
   -- simpleclass 	→ 	qtycls tyvar
   -- simpletype 	→ 	tycon tyvar1 … tyvark 	    (k ≥ 0)

      derivingClause = keyword "deriving"
                       *> (pure <$> wrap (Abstract.simpleDerive <$> qualifiedTypeClass)
                           <|> parens (wrap (Abstract.simpleDerive <$> qualifiedTypeClass) `sepBy` comma))
                       <|> pure [],
      instanceDesignator = Abstract.typeClassInstanceLHS <$> qualifiedTypeClass <*> wrap instanceTypeDesignator,
      instanceTypeDesignator =
         generalTypeConstructor
         <|> Abstract.listType <$> brackets (wrap $ Abstract.typeVariable <$> typeVar)
         <|> parens (typeVarApplications
                     <|> Abstract.tupleType <$> typeVarTuple
                     <|> Abstract.functionType <$> wrap (Abstract.typeVariable <$> typeVar) <* rightArrow
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
                                                  <*> optional stringLiteral <*> variable <* doubleColon
                                                  <*> wrap foreignType
                           <|> Abstract.foreignExport <$ keyword "export" <*> callingConvention
                                                      <*> optional stringLiteral <*> variable <* doubleColon
                                                      <*> wrap foreignType,
      callingConvention = Abstract.cCall <$ keyword "ccall" <|> Abstract.stdCall <$ keyword "stdcall"
                          <|> Abstract.cppCall <$ keyword "cplusplus" <|> Abstract.jvmCall <$ keyword "jvm"
                          <|> Abstract.dotNetCall <$ keyword "dotnet",
      safety = Abstract.safeCall <$ keyword "safe" <|> Abstract.unsafeCall <$ keyword "unsafe",
      foreignType = Abstract.functionType <$> wrap foreignArgType <* rightArrow <*> wrap foreignType
                    <|> foreignReturnType,
      foreignReturnType = foreignArgType
                          <|> Abstract.constructorType <$> wrap (Abstract.unitConstructor
                                                                 <$ terminator "(" <* terminator ")"),
      foreignArgType = Abstract.constructorType <$> wrap (Abstract.constructorReference <$> qualifiedTypeConstructor)
                       <|> Abstract.typeApplication
                           <$> wrap foreignArgType
                           <*> wrap (Abstract.strictType <$> wrap aType),

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
   -- funlhs 	→ 	var apat { apat }
   -- 	| 	pat varop pat
   -- 	| 	( funlhs ) apat { apat }

      qualifiedTypeClass = qualifiedConstructorIdentifier,
      typeClass = constructorIdentifier
   -- tycls 	→ 	conid 	    (type classes)
   -- qtycls 	→ 	[ modid . ] tycls
   },

   typeTerm = Abstract.functionType <$> wrap bType <* rightArrow <*> wrap typeTerm <|> bType,
   bType = Abstract.typeApplication <$> wrap bType <*> wrap aType <|> aType,
   aType = generalTypeConstructor
           <|> Abstract.typeVariable <$> typeVar
           <|> Abstract.tupleType <$> parens ((:|) <$> wrap typeTerm <*> some (comma *> wrap typeTerm))
           <|> Abstract.listType <$> brackets (wrap typeTerm)
           <|> parens typeTerm,
   generalTypeConstructor = Abstract.constructorType <$> wrap generalConstructor
                            <|> Abstract.functionConstructorType <$ parens rightArrow,
   
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

   rhs = Abstract.normalRHS <$ delimiter "=" <*> expression
         <|> Abstract.guardedRHS . NonEmpty.fromList
             <$> some (wrap $ Abstract.guardedExpression . toList <$> guards <* delimiter "=" <*> expression),
   guards = delimiter "|" *> wrap guard `sepByNonEmpty` comma,
   guard = Abstract.bindStatement <$> wrap pattern <* leftArrow <*> infixExpression
           <|> Abstract.letStatement <$ keyword "let" <*> declarations
           <|> Abstract.expressionStatement <$> infixExpression,

-- rhs 	→ 	= exp [where decls]
-- 	| 	gdrhs [where decls]
-- gdrhs 	→ 	guards = exp [gdrhs]
-- guards 	→ 	| guard1, …, guardn 	    (n ≥ 1)
-- guard 	→ 	pat <- infixexp 	    (pattern guard)
-- 	| 	let decls 	    (local declaration)
-- 	| 	infixexp 	    (boolean guard)

   expression = wrap (Abstract.typedExpression <$> leftInfixExpression <* doubleColon <*> wrap typeTerm)
                <|> infixExpression,
   -- infixExpression doesn't allow a conditional, let, or lambda expression on its left side
   infixExpression = wrap (Abstract.infixExpression
                              <$> dExpression
                              <*> wrap (Abstract.referenceExpression <$> qualifiedOperator)
                              <*> infixExpression
                           <|> Abstract.applyExpression <$> wrap (Abstract.negate <$ delimiter "-") <*> lExpression)
                     <|> lExpression,
   -- leftInfixExpression doesn't allow a conditional, let, or lambda expression on either side
   leftInfixExpression = wrap (Abstract.infixExpression
                                  <$> dExpression
                                  <*> wrap (Abstract.referenceExpression <$> qualifiedOperator)
                                  <*> leftInfixExpression
                               <|> Abstract.applyExpression <$> wrap (Abstract.negate <$ delimiter "-") <*> dExpression)
                         <|> dExpression,
   lExpression = wrap openBlockExpression <|> dExpression,
   openBlockExpression = Abstract.lambdaExpression <$ delimiter "\\" <*> some (wrap aPattern) <* rightArrow
                                                   <*> expression
                         <|> Abstract.letExpression <$ keyword "let" <*> declarations <* keyword "in" <*> expression
                         <|> Abstract.conditionalExpression <$ keyword "if" <*> expression <* optional semi
                                                            <* keyword "then" <*> expression <* optional semi
                                                            <* keyword "else" <*> expression,
   dExpression = wrap closedBlockExpresion <|> fExpression,
   closedBlockExpresion = Abstract.caseExpression <$ keyword "case" <*> expression <* keyword "of" <*> alternatives
                          <|> Abstract.doExpression <$ keyword "do" <*> wrap statements,
   fExpression = wrap (Abstract.applyExpression <$> fExpression <*> aExpression) <|> aExpression,
   aExpression = wrap bareExpression <|> Reserializer.joinWrapped <$> wrap (parens expression),
   bareExpression = Abstract.referenceExpression <$> qualifiedVariable
                    <|> Abstract.constructorExpression <$> wrap generalConstructor
                    <|> Abstract.literalExpression <$> wrap literal
                    <|> Abstract.tupleExpression <$> parens ((:|) <$> expression <*> some (comma *> expression))
                    <|> brackets (Abstract.listExpression <$> (expression `sepBy1` comma)
                                  <|> Abstract.sequenceExpression <$> expression
                                      <*> optional (comma *> expression)
                                      <* delimiter ".." <*> optional expression
                                  <|> Abstract.listComprehension <$> expression <*> qualifiers)
                    <|> parens (Abstract.leftSectionExpression <$> infixExpression <*> qualifiedOperator
                                <|> Abstract.rightSectionExpression
                                       <$> (notFollowedBy (string "-" <* notSatisfyChar isSymbol) *> qualifiedOperator)
                                       <*> infixExpression)
                    <|> Abstract.recordExpression
                           <$> wrap (Abstract.constructorExpression
                                        <$> wrap (Abstract.constructorReference <$> qualifiedConstructor))
                           <*> braces (pure [])
                    <|> Abstract.recordExpression <$> aExpression <*> braces (wrap fieldBinding `sepBy1` comma),
   qualifiers = delimiter "|" *> wrap qualifier `sepByNonEmpty` comma,
   qualifier = Abstract.bindStatement <$> wrap pattern <* leftArrow <*> expression
               <|> Abstract.letStatement <$ keyword "let" <*> declarations
               <|> Abstract.expressionStatement <$> expression,
   alternatives = filter (not . null) (blockOf alternative) <?> "non-empty case alternatives",
   alternative = Abstract.caseAlternative <$> wrap pattern
                 <*> wrap (Abstract.normalRHS <$ rightArrow <*> expression
                           <|> Abstract.guardedRHS . NonEmpty.fromList
                               <$> some (wrap $ Abstract.guardedExpression . toList <$> guards <* rightArrow
                                                                                    <*> expression))
                 <*> whereClauses,
   statements = blockOf statement >>= \stats->
                   if null stats then fail "empty do block"
                   else Abstract.guardedExpression
                           (either id (rewrap Abstract.expressionStatement) . Deep.eitherFromSum . unwrap
                            <$> init stats)
                        <$> either (const $ fail "do block must end with an expression") pure
                                   (Deep.eitherFromSum $ unwrap $ last stats),
   statement = Deep.InL <$> wrap (Abstract.bindStatement <$> wrap pattern <* leftArrow <*> expression
                                  <|> Abstract.letStatement <$ keyword "let" <*> declarations)
               <|> Deep.InR <$> expression,
   fieldBinding = Abstract.fieldBinding <$> qualifiedVariable <* delimiter "=" <*> expression,
                
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
                              <$> ((:|) <$> wrap pattern <*> some (terminator "," *> wrap pattern)))
              <|> Abstract.listPattern <$> brackets (wrap pattern `sepBy1` comma)
              <|> Abstract.irrefutablePattern <$ delimiter "~" <*> wrap aPattern,
   fieldPattern = Abstract.fieldPattern <$> qualifiedVariable <* delimiter "=" <*> wrap pattern,
   generalConstructor = Abstract.constructorReference <$> qualifiedConstructor
                        <|> Abstract.unitConstructor <$ terminator "(" <* terminator ")"
                        <|> Abstract.emptyListConstructor <$ terminator "[" <* terminator "]"
                        <|> Abstract.tupleConstructor . succ . length <$> parens (some comma),

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
   variableOperator = variableSymbol <|> terminator "`" *> variableIdentifier <* terminator "`",
   qualifiedVariableOperator = qualifiedVariableSymbol
                               <|> terminator "`" *> qualifiedVariableIdentifier <* terminator "`",
   constructorOperator = constructorSymbol <|> terminator "`" *> constructorIdentifier <* terminator "`",
   qualifiedConstructorOperator = qualifiedConstructorSymbol
                                  <|> terminator "`" *> qualifiedConstructorIdentifier <* terminator "`",
   operator = variableOperator <|> constructorOperator,
   qualifiedOperator = qualifiedVariableOperator <|> qualifiedConstructorOperator <?> "qualified operator",
   qualifiedVariableIdentifier = token (nameQualifier <*> variableIdentifier),
   qualifiedConstructorIdentifier = token (nameQualifier <*> constructorIdentifier),
   qualifiedTypeConstructor = qualifiedConstructorIdentifier,
   qualifiedVariableSymbol = token (nameQualifier <*> variableSymbol),
   qualifiedConstructorSymbol = token (nameQualifier <*> constructorSymbol
                                       <|> Abstract.qualifiedName Nothing . Abstract.name . Text.pack . toString mempty
                                           <$> string ":" <* notSatisfyChar isSymbol),

   typeVar = variableIdentifier,
   typeConstructor = constructorIdentifier,
   variableIdentifier = nameToken variableLexeme,
   constructorIdentifier = nameToken constructorLexeme,
   variableSymbol = nameToken variableSymbolLexeme,
   constructorSymbol = nameToken constructorSymbolLexeme,

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
-- varid 	     	    (variables)
-- conid 	     	    (constructors)
-- tyvar 	→ 	varid 	    (type variables)
-- tycon 	→ 	conid 	    (type constructors)
-- modid 	→ 	{conid .} conid 	    (modules)
-- qvarid 	→ 	[ modid . ] varid
-- qconid 	→ 	[ modid . ] conid
-- qtycon 	→ 	[ modid . ] tycon
-- qvarsym 	→ 	[ modid . ] varsym
-- qconsym 	→ 	[ modid . ] consym

-- Lexical Syntax

   literal = Abstract.integerLiteral <$> integer <|> Abstract.floatingLiteral <$> float
             <|> Abstract.charLiteral <$> charLiteral <|> Abstract.stringLiteral <$> stringLiteral,

   doubleColon = delimiter "::",
   rightDoubleArrow = delimiter "=>",
   rightArrow = delimiter "->",
   leftArrow = delimiter "<-",
   integer = token integerLexeme,
   float = token floatLexeme,
   integerLexeme = fst . head
                   <$> ((string "0o" <|> string "0O") *> (Numeric.readOct . toString mempty <$> octal)
                        <<|> (string "0x" <|> string "0X") *> (Numeric.readHex . toString mempty <$> hexadecimal)
                        <<|> Numeric.readDec . toString mempty <$> decimal
                             <* notFollowedBy (string "." *> decimal <|> exponent)),
   floatLexeme = fst . head . Numeric.readFloat . toString mempty
                 <$> (decimal <> string "." <> decimal <> (exponent <<|> mempty)
                      <|> decimal <> exponent),

   decimal = takeCharsWhile1 Char.isDigit <?> "decimal number",
   octal = takeCharsWhile1 Char.isOctDigit <?> "octal number",
   hexadecimal = takeCharsWhile1 Char.isHexDigit <?> "hexadecimal number",
   exponent = (string "e" <|> string "E") <> (string "+" <|> string "-" <|> mempty) <> decimal,

   charLiteral = token charLexeme,
   charLexeme = char '\''
                *> (Text.Parser.Char.satisfy (\c-> c == ' ' || not (Char.isSpace c) && c /= '\'' && c /= '\\')
                    <|> escape)
                <* char '\'',
   stringLiteral = token stringLexeme <?> "string literal",
   stringLexeme = Text.pack . toString mempty <$>
                  (char '"'
                   *> concatMany (takeCharsWhile1 (\c-> c == ' ' || not (Char.isSpace c) && c /= '"' && c /= '\\')
                                  <|> Textual.singleton <$> escape
                                  <|> char '\\'
                                      *> (char '&' <|> takeCharsWhile1 Char.isSpace *> char '\\')
                                      *> pure "")
                   <* char '"'),
   escape = string "\\" *> (charEscape <|> asciiEscape
                            <|> Char.chr . fst . head <$> (Numeric.readDec . toString mempty <$> decimal
                                                           <|> string "o" *> (Numeric.readOct . toString mempty <$> octal)
                                                           <|> string "x"
                                                            *> (Numeric.readHex . toString mempty <$> hexadecimal)))
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

-- decimal 	→ 	digit{digit}
-- octal 	→ 	octit{octit}
-- hexadecimal 	→ 	hexit{hexit}
-- integer 	→ 	decimal
-- 	| 	0o octal | 0O octal
-- 	| 	0x hexadecimal | 0X hexadecimal
-- float 	→ 	decimal . decimal [exponent]
-- 	| 	decimal exponent
-- exponent 	→ 	(e | E) [+ | -] decimal
 
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
asciiSymbols = Set.fromList "!#$%&*+./<=>?@\\^|-~:"

-- varid 	→ 	(small {small | large | digit | ' })⟨reservedid⟩
-- conid 	→ 	large {small | large | digit | ' }
-- reservedid 	→ 	case | class | data | default | deriving | do | else
-- 	| 	foreign | if | import | in | infix | infixl
-- 	| 	infixr | instance | let | module | newtype | of
-- 	| 	then | type | where | _
-- varsym 	→ 	( symbol⟨:⟩ {symbol} )⟨reservedop | dashes⟩
-- consym 	→ 	( : {symbol})⟨reservedop⟩
-- reservedop 	→ 	.. | : | :: | = | \ | | | <- | -> |  @ | ~ | =>

moduleId :: (Abstract.Haskell l, LexicalParsing (Parser g t), Ord t, Show t, TextualMonoid t)
         => Parser g t (Abstract.ModuleName l)
moduleId = Abstract.moduleName <$> token moduleLexeme

moduleLexeme :: (Abstract.Haskell l, LexicalParsing (Parser g t), Ord t, Show t, TextualMonoid t)
             => Parser g t (NonEmpty (Abstract.Name l))
moduleLexeme = (Abstract.name . Text.pack . toString mempty <$> constructorLexeme <?> "module name")
               `sepByNonEmpty` string "."

nameQualifier :: (Abstract.Haskell l, LexicalParsing (Parser g t), Ord t, Show t, TextualMonoid t)
              => Parser g t (Abstract.Name l -> Abstract.QualifiedName l)
nameQualifier =
   Abstract.qualifiedName
   <$> takeOptional (storeToken (Abstract.moduleName <$> moduleLexeme <* string ".")
                     <* notFollowedBy (constructorLexeme *> string "."
                                       <|> filter (`elem` reservedWords) (takeCharsWhile1 Char.isLower)))

asciiEscape, charEscape, controlEscape :: (LexicalParsing (Parser g t), Show t, TextualMonoid t) => Parser g t Char
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

type NodeWrap s = Reserializer.Wrapped (Down Int) s

wrap :: (Ord t, TextualMonoid t) => Parser g t a -> Parser g t (NodeWrap t a)
wrap = (\p-> liftA3 surround getSourcePos p getSourcePos)
         . tmap store . ((,) (Trailing []) <$>)
   where store (wss, (Trailing [], a)) = (mempty, (Trailing (concat wss), a))
         surround start (ls, val) end = ((start, ls, end), val)

rewrap :: (NodeWrap t a -> b) -> NodeWrap t a -> NodeWrap t b
rewrap f node@((start, _, end), _) = ((start, mempty, end), f node)

unwrap :: NodeWrap t a -> a
unwrap (_, x) = x

instance (Ord t, Show t, TextualMonoid t) => TokenParsing (Parser g t) where
   someSpace = someLexicalSpace
   token = lexicalToken

instance (Ord t, Show t, TextualMonoid t) => LexicalParsing (Parser g t) where
   lexicalComment = comment
   lexicalWhiteSpace = whiteSpace
   isIdentifierStartChar = Char.isLetter
   isIdentifierFollowChar = isNameTailChar
   identifierToken word = lexicalToken (filter (`notElem` reservedWords) word)
   lexicalToken p = storeToken p <* lexicalWhiteSpace
   keyword = keyword

keyword :: (Ord s, Show s, TextualMonoid s, LexicalParsing (Parser g s)) => s -> Parser g s ()
keyword s = lexicalToken (string s
                          *> notSatisfyChar isNameTailChar
                          <* lift ([[Token Keyword s]], ()))
            <?> ("keyword " <> show s)

storeToken :: (Ord t, TextualMonoid t) => Parser g t a -> Parser g t a
storeToken p = snd <$> tmap addOtherToken (match p)
   where addOtherToken ([], (i, x)) = ([[Token Other i]], (i, x))
         addOtherToken (t, (i, x)) = (t, (i, x))

isLineChar, isNameTailChar, isSymbol :: Char -> Bool
isLineChar c = c /= '\n' && c /= '\r' && c /= '\f'
isNameTailChar c = Char.isAlphaNum c || c == '_' || c == '\''
isSymbol c = if Char.isAscii c then c `Set.member` asciiSymbols else Char.isSymbol c || Char.isPunctuation c

delimiter, terminator :: (Ord t, Show t, TextualMonoid t) => LexicalParsing (Parser g t) => t -> Parser g t ()
delimiter s = void (lexicalToken $
                    string s
                    <* notSatisfyChar isSymbol
                    <* lift ([[Token Delimiter s]], ()))
              <?> ("delimiter " <> show s)
terminator s = void (lexicalToken $ string s <* lift ([[Token Delimiter s]], ()))
               <?> ("terminating delimiter " <> show s)

nameToken :: (Abstract.Haskell l, TextualMonoid t, TokenParsing (Parser g t))
          => Parser g t t -> Parser g t (Abstract.Name l)
nameToken p = token (Abstract.name . Text.pack . toString mempty <$> p)

whiteSpace :: (Ord t, Show t, TextualMonoid t) => LexicalParsing (Parser g t) => Parser g t ()
whiteSpace = spaceChars *> skipAll (lexicalComment *> spaceChars) <?> "whitespace"
   where spaceChars = ((takeCharsWhile1 Char.isSpace
                       >>= \ws-> lift ([[WhiteSpace ws]], ())) <?> "whitespace")
                      <<|> pure ()

comment :: (Ord t, Show t, TextualMonoid t) => Parser g t ()
comment = do c <- try (blockComment
                       <|> (string "--" <> takeCharsWhile (== '-') <* notSatisfyChar isSymbol)
                           <> takeCharsWhile isLineChar) <?> "comment"
             lift ([[Comment c]], ())
   where isCommentChar c = c /= '-' && c /= '{'
         blockComment =
            string "{-"
            <> concatMany (blockComment <<|> (notFollowedBy (string "-}") *> anyToken) <> takeCharsWhile isCommentChar)
            <> string "-}"

blockOf :: (Ord t, Show t, OutlineMonoid t, TokenParsing (Parser g t),
            Deep.Foldable (Serialization (Down Int) t) node)
        => Parser g t (node (NodeWrap t) (NodeWrap t)) -> Parser g t [NodeWrap t (node (NodeWrap t) (NodeWrap t))]
blockOf p = braces (wrap p `startSepEndBy` semi) <|> (inputColumn >>= alignedBlock optional pure)
   where alignedBlock opt cont indent =
            do rest <- getInput
               maybeItem <- opt (filter (oneExtendedLine indent rest) $ wrap p)
               case maybeItem of
                  Nothing -> cont []
                  Just item -> do
                     -- don't stop at a higher indent unless there's a terminator
                     void (filter (indent >=) inputColumn)
                        <<|> lookAhead (void (Text.Parser.Char.satisfy (`elem` terminators))
                                        <|> (string "else" <|> string "in"
                                             <|> string "of" <|> string "where") *> notSatisfyChar isNameTailChar
                                        <|> eof)
                     indent' <- inputColumn
                     let cont' = cont . (item :)
                     if indent == indent'
                        then many semi *> alignedBlock takeOptional cont' indent
                        else some semi *> alignedBlock takeOptional cont' indent <<|> cont' []
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

inputColumn :: (Ord t, OutlineMonoid t) => Parser g t Int
inputColumn = currentColumn <$> getInput

oneExtendedLine :: (Ord t, Show t, OutlineMonoid t,
                    Deep.Foldable (Serialization (Down Int) t) node)
                => Int -> t -> NodeWrap t (node (NodeWrap t) (NodeWrap t)) -> Bool
oneExtendedLine indent _input node =
   allIndented (lexemes node)
   where allIndented (WhiteSpace _ : Token Delimiter _tok : rest) = allIndented rest
         allIndented (WhiteSpace ws : Token _ tok : rest)
            | Textual.all isLineChar ws = allIndented rest
            | tokenIndent < indent = False
            | tokenIndent == indent && tok `Set.notMember` reservedWords
              && all (`notElem` terminators) (characterPrefix tok) = False
            where tokenIndent = currentColumn (Factorial.dropWhile (const True) ws)
         allIndented (_ : rest) = allIndented rest
         allIndented [] = True
         terminators :: [Char]
         terminators = ",;)]}"
