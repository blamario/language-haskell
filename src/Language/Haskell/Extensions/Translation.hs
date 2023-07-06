{-# Language ConstraintKinds, DefaultSignatures, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
             ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}

module Language.Haskell.Extensions.Translation where

import Data.Coerce (coerce)
import qualified Language.Haskell.Extensions.Abstract as Abstract
import qualified Language.Haskell.Extensions.AST as AST

class NameTranslation t where
   type Origin t :: Abstract.Language
   type Target t :: Abstract.Language
   translateName :: t -> AST.Name (Origin t) -> Abstract.Name (Target t)
   translateModuleName :: t -> AST.ModuleName (Origin t) -> Abstract.ModuleName (Target t)
   translateQualifiedName :: t -> AST.QualifiedName (Origin t) -> Abstract.QualifiedName (Target t)
   default translateName :: Abstract.Name (Target t) ~ AST.Name (Target t)
                         => t -> AST.Name (Origin t) -> Abstract.Name (Target t)
   default translateModuleName :: (Abstract.Name (Target t) ~ AST.Name (Target t),
                                   Abstract.ModuleName (Target t) ~ AST.ModuleName (Target t))
                               => t -> AST.ModuleName (Origin t) -> Abstract.ModuleName (Target t)
   default translateQualifiedName :: (Abstract.Name (Target t) ~ AST.Name (Target t),
                                      Abstract.ModuleName (Target t) ~ AST.ModuleName (Target t),
                                      Abstract.QualifiedName (Target t) ~ AST.QualifiedName (Target t))
                                  => t -> AST.QualifiedName (Origin t) -> Abstract.QualifiedName (Target t)
   translateName = const coerce
   translateModuleName = const coerce
   translateQualifiedName = const coerce

class NameTranslation t => Translation t (node :: Abstract.TreeNodeKind) where
   translate :: t -> node (Origin t) l d s -> node (Target t) l d s

class DeeplyTranslatable t (node :: Abstract.TreeNodeKind) where
   translateDeeply :: Functor d => t -> node (Origin t) (Origin t) d d -> node (Origin t) (Target t) d d

type FullyTranslatable t node = (Translation t node, DeeplyTranslatable t node)

translateFully :: (FullyTranslatable t node, Functor d) =>
                  t -> d (node (Origin t) (Origin t) d d) -> d (node (Target t) (Target t) d d)
translateFully t = (translate t . translateDeeply t <$>)

instance (Translation t AST.Module,
          FullyTranslatable t AST.Export, FullyTranslatable t AST.Import, FullyTranslatable t AST.Declaration,
          Abstract.Module (Origin t) ~ AST.Module (Origin t), Abstract.Module (Target t) ~ AST.Module (Target t),
          Abstract.ModuleName (Origin t) ~ AST.ModuleName (Origin t),
          Abstract.ModuleName (Target t) ~ AST.ModuleName (Target t),
          Abstract.Export (Origin t) ~ AST.Export (Origin t), Abstract.Export (Target t) ~ AST.Export (Target t),
          Abstract.Import (Origin t) ~ AST.Import (Origin t), Abstract.Import (Target t) ~ AST.Import (Target t),
          Abstract.Declaration (Origin t) ~ AST.Declaration (Origin t),
          Abstract.Declaration (Target t) ~ AST.Declaration (Target t)) => DeeplyTranslatable t AST.Module where
   translateDeeply t (AST.NamedModule modName exports imports declarations) =
      AST.NamedModule modName
         ((translateFully t <$>) <$> exports)
         (translateFully t <$> imports)
         (translateFully t <$> declarations)
   translateDeeply t (AST.AnonymousModule imports declarations) =
      AST.AnonymousModule (translateFully t <$> imports) (translateFully t <$> declarations)
   translateDeeply t (AST.ExtendedModule extensions m) =
      AST.ExtendedModule extensions (translateFully t m)

instance (FullyTranslatable t AST.ImportSpecification,
          Abstract.Import (Target t) ~ AST.Import (Target t),
          Abstract.ModuleName (Origin t) ~ AST.ModuleName (Origin t),
          Abstract.ImportSpecification (Origin t) ~ AST.ImportSpecification (Origin t),
          Abstract.ImportSpecification (Target t) ~ AST.ImportSpecification (Target t)) =>
         DeeplyTranslatable t AST.Import where
   translateDeeply t (AST.Import safe qualified package name alias detail) =
      AST.Import safe qualified package name alias (translateFully t <$> detail)

instance DeeplyTranslatable t AST.Value where
   translateDeeply t (AST.CharLiteral l)     = AST.CharLiteral l
   translateDeeply t (AST.FloatingLiteral l) = AST.FloatingLiteral l
   translateDeeply t (AST.IntegerLiteral l)  = AST.IntegerLiteral l
   translateDeeply t (AST.StringLiteral l)   = AST.StringLiteral l
   translateDeeply t (AST.HashLiteral sup l)  = AST.HashLiteral sup (translateDeeply t l)
  
