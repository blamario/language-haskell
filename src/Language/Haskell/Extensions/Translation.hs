{-# Language ConstraintKinds, DataKinds, DefaultSignatures,
             FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
             ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Language.Haskell.Extensions.Translation where

import Data.Coerce (coerce)
import Data.Foldable1 (Foldable1)
import qualified Data.Foldable1 as Foldable1
import qualified Language.Haskell.Extensions.Abstract as Abstract
import qualified Language.Haskell.Extensions.AST as AST
import qualified Language.Haskell.Extensions as Extensions

class NameTranslation t where
   type Origin t :: Abstract.Language
   type Target t :: Abstract.Language
   translateName :: t -> Abstract.Name (Origin t) -> Abstract.Name (Target t)
   translateModuleName :: t -> Abstract.ModuleName (Origin t) -> Abstract.ModuleName (Target t)
   translateQualifiedName :: t -> Abstract.QualifiedName (Origin t) -> Abstract.QualifiedName (Target t)
   default translateName :: (Abstract.Name (Origin t) ~ AST.Name (Origin t),
                             Abstract.Name (Target t) ~ AST.Name (Target t))
                         => t -> Abstract.Name (Origin t) -> Abstract.Name (Target t)
   default translateModuleName :: (Abstract.Name (Target t) ~ AST.Name (Target t),
                                   Abstract.ModuleName (Origin t) ~ AST.ModuleName (Origin t),
                                   Abstract.ModuleName (Target t) ~ AST.ModuleName (Target t))
                               => t -> Abstract.ModuleName (Origin t) -> Abstract.ModuleName (Target t)
   default translateQualifiedName :: (Abstract.Name (Target t) ~ AST.Name (Target t),
                                      Abstract.ModuleName (Target t) ~ AST.ModuleName (Target t),
                                      Abstract.QualifiedName (Origin t) ~ AST.QualifiedName (Origin t),
                                      Abstract.QualifiedName (Target t) ~ AST.QualifiedName (Target t))
                                  => t -> Abstract.QualifiedName (Origin t) -> Abstract.QualifiedName (Target t)
   translateName = const coerce
   translateModuleName = const coerce
   translateQualifiedName = const coerce


class WrapTranslation t where
   type Wrap t :: Abstract.NodeWrap

class (NameTranslation t, WrapTranslation t) => Translation t (node :: Abstract.TreeNodeKind) where
   translate :: t -> node (Origin t) (Origin t) (Wrap t) (Wrap t) -> node (Target t) (Origin t) (Wrap t) (Wrap t)
   translateWrapped :: t -> Wrap t (node (Origin t) (Origin t) (Wrap t) (Wrap t)) -> Wrap t (node (Target t) (Origin t) (Wrap t) (Wrap t))
   default translate :: (Applicative (Wrap t), Foldable1 (Wrap t)) => t -> node (Origin t) (Origin t) (Wrap t) (Wrap t) -> node (Target t) (Origin t) (Wrap t) (Wrap t)
   default translateWrapped :: Functor (Wrap t) => t -> Wrap t (node (Origin t) (Origin t) (Wrap t) (Wrap t)) -> Wrap t (node (Target t) (Origin t) (Wrap t) (Wrap t))
   translate t = Foldable1.head . translateWrapped t . pure
   translateWrapped = fmap . translate

class WrapTranslation t => DeeplyTranslatable t (node :: Abstract.TreeNodeKind) where
   translateDeeply :: Functor (Wrap t)
                   => t -> node l (Origin t) (Wrap t) (Wrap t) -> node l (Target t) (Wrap t) (Wrap t)

translateFully :: (FullyTranslatable t node, Functor (Wrap t)) =>
                  t -> Wrap t (node (Origin t) (Origin t) (Wrap t) (Wrap t)) -> Wrap t (node (Target t) (Target t) (Wrap t) (Wrap t))
translateFully t = (translateDeeply t <$>) . translateWrapped t

type FullyTranslatable t node = (WrapTranslation t, Translation t node, DeeplyTranslatable t node)

-- DeeplyTranslatable instances

instance (Translation t AST.Module,
          FullyTranslatable t AST.Export, FullyTranslatable t AST.Import, FullyTranslatable t AST.Declaration,
          Abstract.Module (Origin t) ~ AST.Module (Origin t), Abstract.Module (Target t) ~ AST.Module (Target t),
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
   translateDeeply t (AST.ExtendedModule extensions m) = AST.ExtendedModule extensions (translateFully t m)

instance (FullyTranslatable t AST.ImportSpecification,
          Abstract.Import (Target t) ~ AST.Import (Target t),
          Abstract.ModuleName (Origin t) ~ AST.ModuleName (Origin t),
          Abstract.ImportSpecification (Origin t) ~ AST.ImportSpecification (Origin t),
          Abstract.ImportSpecification (Target t) ~ AST.ImportSpecification (Target t)) =>
         DeeplyTranslatable t AST.Import where
   translateDeeply t (AST.Import safe qualified package name alias detail) =
      AST.Import safe qualified package name alias (translateFully t <$> detail)

instance (Translation t AST.Expression,
          FullyTranslatable t AST.CaseAlternative, FullyTranslatable t AST.Constructor,
          FullyTranslatable t AST.Declaration, FullyTranslatable t AST.FieldBinding,
          FullyTranslatable t AST.GuardedExpression, FullyTranslatable t AST.Pattern,
          FullyTranslatable t AST.Statement, FullyTranslatable t AST.Type, FullyTranslatable t AST.Value,
          Abstract.Expression (Origin t) ~ AST.Expression (Origin t),
          Abstract.Expression (Target t) ~ AST.Expression (Target t),
          Abstract.CaseAlternative (Origin t) ~ AST.CaseAlternative (Origin t),
          Abstract.CaseAlternative (Target t) ~ AST.CaseAlternative (Target t),
          Abstract.Constructor (Origin t) ~ AST.Constructor (Origin t),
          Abstract.Constructor (Target t) ~ AST.Constructor (Target t),
          Abstract.Declaration (Origin t) ~ AST.Declaration (Origin t),
          Abstract.Declaration (Target t) ~ AST.Declaration (Target t),
          Abstract.FieldBinding (Origin t) ~ AST.FieldBinding (Origin t),
          Abstract.FieldBinding (Target t) ~ AST.FieldBinding (Target t),
          Abstract.GuardedExpression (Origin t) ~ AST.GuardedExpression (Origin t),
          Abstract.GuardedExpression (Target t) ~ AST.GuardedExpression (Target t),
          Abstract.Pattern (Origin t) ~ AST.Pattern (Origin t),
          Abstract.Pattern (Target t) ~ AST.Pattern (Target t),
          Abstract.Statement (Origin t) ~ AST.Statement (Origin t),
          Abstract.Statement (Target t) ~ AST.Statement (Target t),
          Abstract.Type (Origin t) ~ AST.Type (Origin t), Abstract.Type (Target t) ~ AST.Type (Target t),
          Abstract.Value (Origin t) ~ AST.Value (Origin t), Abstract.Value (Target t) ~ AST.Value (Target t)) =>
         DeeplyTranslatable t AST.Expression where
   translateDeeply t (AST.ApplyExpression f x) = AST.ApplyExpression (translateFully t f) (translateFully t x)
   translateDeeply t (AST.ConditionalExpression cond true false) =
      AST.ConditionalExpression (translateFully t cond) (translateFully t true) (translateFully t false)
   translateDeeply t (AST.ConstructorExpression con) = AST.ConstructorExpression (translateFully t con)
   translateDeeply t (AST.CaseExpression scrutinee alts) =
      AST.CaseExpression (translateFully t scrutinee) (translateFully t <$> alts)
   translateDeeply t (AST.LambdaCaseExpression alts) = AST.LambdaCaseExpression (translateFully t <$> alts)
   translateDeeply t (AST.MultiWayIfExpression alts) = AST.MultiWayIfExpression (translateFully t <$> alts)
   translateDeeply t (AST.DoExpression body) = AST.DoExpression (translateFully t body)
   translateDeeply t (AST.MDoExpression body) = AST.MDoExpression (translateFully t body)
   translateDeeply t (AST.InfixExpression left op right) =
      AST.InfixExpression (translateFully t left) (translateFully t op) (translateFully t right)
   translateDeeply t (AST.LeftSectionExpression left op) = AST.LeftSectionExpression (translateFully t left) op
   translateDeeply t (AST.LambdaExpression patterns body) =
      AST.LambdaExpression (translateFully t <$> patterns) (translateFully t body)
   translateDeeply t (AST.LetExpression bindings body) =
      AST.LetExpression (translateFully t <$> bindings) (translateFully t body)
   translateDeeply t (AST.ListComprehension element guards) =
      AST.ListComprehension (translateFully t element) (translateFully t <$> guards)
   translateDeeply t (AST.ParallelListComprehension element guards1 guards2 guardses) =
      AST.ParallelListComprehension
         (translateFully t element) (translateFully t <$> guards1)
         (translateFully t <$> guards2) ((translateFully t <$>) <$> guardses)
   translateDeeply t (AST.ListExpression items) = AST.ListExpression (translateFully t <$> items)
   translateDeeply t (AST.LiteralExpression value) = AST.LiteralExpression (translateFully t value)
   translateDeeply _ AST.Negate = AST.Negate
   translateDeeply t (AST.RecordExpression record fields) =
      AST.RecordExpression (translateFully t record) (translateFully t <$> fields)
   translateDeeply _ (AST.ReferenceExpression name) = AST.ReferenceExpression name
   translateDeeply t (AST.RightSectionExpression op right) = AST.RightSectionExpression op (translateFully t right)
   translateDeeply t (AST.SequenceExpression start next end) =
      AST.SequenceExpression (translateFully t start) (translateFully t <$> next) (translateFully t <$> end)
   translateDeeply t (AST.TupleExpression items) = AST.TupleExpression (translateFully t <$> items)
   translateDeeply t (AST.TupleSectionExpression items) =
      AST.TupleSectionExpression ((translateFully t <$>) <$> items)
   translateDeeply t (AST.TypedExpression x signature) =
      AST.TypedExpression (translateFully t x) (translateFully t signature)
   translateDeeply t (AST.VisibleTypeApplication x ty) =
      AST.VisibleTypeApplication (translateFully t x) (translateFully t ty)
   translateDeeply _ (AST.OverloadedLabel l) = AST.OverloadedLabel l
   translateDeeply t (AST.GetField record name) = AST.GetField (translateFully t record) name
   translateDeeply _ (AST.FieldProjection fields) = AST.FieldProjection fields
   translateDeeply t (AST.WildcardRecordExpression sup con fields) =
      AST.WildcardRecordExpression sup con (translateFully t <$> fields)

instance WrapTranslation t => DeeplyTranslatable t AST.Value where
   translateDeeply _ (AST.CharLiteral l)     = AST.CharLiteral l
   translateDeeply _ (AST.FloatingLiteral l) = AST.FloatingLiteral l
   translateDeeply _ (AST.IntegerLiteral l)  = AST.IntegerLiteral l
   translateDeeply _ (AST.StringLiteral l)   = AST.StringLiteral l
   translateDeeply t (AST.HashLiteral sup l) = AST.HashLiteral sup (translateDeeply t l)

-- Default overlappable Translation instances

instance {-# overlappable #-} (NameTranslation t, WrapTranslation t, Functor (Wrap t)) => Translation t AST.Module where
   translate t (AST.NamedModule modName exports imports declarations) =
      AST.NamedModule (translateModuleName t modName) exports imports declarations
   translate _ (AST.AnonymousModule imports declarations) = AST.AnonymousModule imports declarations
   translate _ (AST.ExtendedModule extensions m) = AST.ExtendedModule extensions m

instance {-# overlappable #-} (NameTranslation t, WrapTranslation t, Functor (Wrap t)) => Translation t AST.Import where
   translate t (AST.Import safe qualified package name alias detail) =
      AST.Import safe qualified package (translateModuleName t name) (translateModuleName t <$> alias) detail

instance {-# overlappable #-}
   (NameTranslation t, WrapTranslation t, Functor (Wrap t),
    Abstract.SupportFor 'Extensions.RecordWildCards (Origin t)
    ~ Abstract.SupportFor 'Extensions.RecordWildCards (Target t)) =>
   Translation t AST.Expression where
   translate _ (AST.ApplyExpression f x) = AST.ApplyExpression f x
   translate _ (AST.ConditionalExpression cond true false) = AST.ConditionalExpression cond true false
   translate _ (AST.ConstructorExpression con) = AST.ConstructorExpression con
   translate _ (AST.CaseExpression scrutinee alts) = AST.CaseExpression scrutinee alts
   translate _ (AST.LambdaCaseExpression alts) = AST.LambdaCaseExpression alts
   translate _ (AST.MultiWayIfExpression alts) = AST.MultiWayIfExpression alts
   translate _ (AST.DoExpression statements) = AST.DoExpression statements
   translate _ (AST.MDoExpression statements) = AST.MDoExpression statements
   translate _ (AST.InfixExpression left op right) = AST.InfixExpression left op right
   translate t (AST.LeftSectionExpression left op) = AST.LeftSectionExpression left (translateQualifiedName t op)
   translate _ (AST.LambdaExpression pat body) = AST.LambdaExpression pat body
   translate _ (AST.LetExpression bindings body) = AST.LetExpression bindings body
   translate _ (AST.ListComprehension element guards) = AST.ListComprehension element guards
   translate _ (AST.ParallelListComprehension element guards1 guards2 guardses) = AST.ParallelListComprehension element guards1 guards2 guardses
   translate _ (AST.ListExpression items) = AST.ListExpression items
   translate _ (AST.LiteralExpression value) = AST.LiteralExpression value
   translate _ AST.Negate = AST.Negate
   translate _ (AST.RecordExpression record fields) = AST.RecordExpression record fields
   translate t (AST.ReferenceExpression name) = AST.ReferenceExpression (translateQualifiedName t name)
   translate t (AST.RightSectionExpression op right) = AST.RightSectionExpression (translateQualifiedName t op) right
   translate _ (AST.SequenceExpression start next end) = AST.SequenceExpression start next end
   translate _ (AST.TupleExpression items) = AST.TupleExpression items
   translate _ (AST.TupleSectionExpression items) = AST.TupleSectionExpression items
   translate _ (AST.TypedExpression x signature) = AST.TypedExpression x signature
   translate _ (AST.VisibleTypeApplication x ty) = AST.VisibleTypeApplication x ty
   translate _ (AST.OverloadedLabel l) = AST.OverloadedLabel l
   translate t (AST.GetField record name) = AST.GetField record (translateName t name)
   translate t (AST.FieldProjection fields) = AST.FieldProjection (translateName t <$> fields)
   translate t (AST.WildcardRecordExpression sup con fields) =
      AST.WildcardRecordExpression sup (translateQualifiedName t con) fields

instance {-# overlappable #-}
   (NameTranslation t, WrapTranslation t, Functor (Wrap t),
    Abstract.SupportFor 'Extensions.MagicHash (Origin t) ~ Abstract.SupportFor 'Extensions.MagicHash (Target t)) =>
   Translation t AST.Value where
   translate _ (AST.CharLiteral l)     = AST.CharLiteral l
   translate _ (AST.FloatingLiteral l) = AST.FloatingLiteral l
   translate _ (AST.IntegerLiteral l)  = AST.IntegerLiteral l
   translate _ (AST.StringLiteral l)   = AST.StringLiteral l
   translate t (AST.HashLiteral sup l) = AST.HashLiteral sup (translate t l)
