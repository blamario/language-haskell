{-# Language ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes,
             TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}

module Language.Haskell.Extensions.Reformulator where

import Data.Functor.Compose (Compose (Compose))
import qualified Transformation
import Transformation (Transformation)
import qualified Transformation.Deep as Deep
import qualified Transformation.Full as Full

import Language.Haskell.Abstract (Haskell (Module))
import Language.Haskell.Extensions.Abstract (DeeplyFunctor, ExtendedWithAllOf)
import Language.Haskell.Extensions (Extension)
import qualified Language.Haskell.Extensions as Extensions
import qualified Language.Haskell.Binder as Binder
import qualified Language.Haskell.Reserializer as Reserializer

type family Without (e :: Extension) (es :: [Extension]) where
   Without _ '[] = '[]
   Without e (e ': es) = Without e es
   Without e1 (e2 ': es) = e2 ': Without e1 es

type family Difference (xs :: [Extension]) (ys :: [Extension]) where
   Difference xs '[] = xs
   Difference xs (y ': ys) = Difference (Without y xs) ys

type Transpiler λ1 λ2 f = Module λ1 λ1 f f -> Module λ2 λ2 f f

type Reformulator xs ys λ c f = ExtendedWithAllOf xs λ =>
   forall l. (Haskell l, ExtendedWithAllOf (Difference ys xs) l, c λ l) => Transpiler λ l f


type Wrap l pos s = Binder.WithEnvironment l (Reserializer.Wrapped pos s)

data ReformulationOf (e :: Extension) λ l pos s = Reformulation

instance Transformation (ReformulationOf e λ l pos s) where
   type Domain (ReformulationOf e λ l pos s) = Wrap λ pos s
   type Codomain (ReformulationOf e λ l pos s) = Wrap l pos s

type SameWrap e pos s l1 l2 = (Binder.WithEnvironment l1 ~ Binder.WithEnvironment l2,
                               DeeplyFunctor (ReformulationOf e l1 l2 pos s) l2)
  

dropRecordWildCards :: Reformulator '[Extensions.RecordWildCards] '[] λ (SameWrap 'Extensions.RecordWildCards pos s) (Wrap λ pos s)
dropRecordWildCards = Deep.fmap (Reformulation @'Extensions.RecordWildCards)


instance {-# overlappable #-} SameWrap e pos s λ l =>
   ReformulationOf e λ l pos s `Transformation.At` g (Wrap λ pos s) (Wrap λ pos s) where
   Reformulation $ Compose (env, (s, node)) = Compose (env, (s, node))



instance (Deep.Functor (ReformulationOf e λ l pos s) g,
          Transformation.At (ReformulationOf e λ l pos s) (g (Wrap λ pos s) (Wrap λ pos s))) =>
         Full.Functor (ReformulationOf e λ l pos s) g where
   (<$>) = Full.mapDownDefault
