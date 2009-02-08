{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}

module Utilities where

import Test.QuickCheck

import qualified Data.Vector as DV
import qualified Data.Vector.Unboxed as DVU
import qualified Data.Vector.IVector as DVI
import Data.Vector.Unboxed.Unbox (Unbox)


instance Arbitrary a => Arbitrary (DV.Vector a) where
    arbitrary = fmap DV.fromList arbitrary
    coarbitrary = coarbitrary . DV.toList

instance (Arbitrary a, Unbox a) => Arbitrary (DVU.Vector a) where
    arbitrary = fmap DVU.fromList arbitrary
    coarbitrary = coarbitrary . DVU.toList


class Model a b | a -> b where
  -- | Convert a concrete value into an abstract model
  model :: a -> b

-- The meat of the models
instance            Model (DV.Vector a)  [a] where model = DV.toList
instance Unbox a => Model (DVU.Vector a) [a] where model = DVU.toList

-- Identity models
instance Model Bool     Bool     where model = id
instance Model Int      Int      where model = id
instance Model Ordering Ordering where model = id

-- Functorish models
-- All of these need UndecidableInstances although they are actually well founded. Oh well.
instance Model a b              => Model (Maybe a) (Maybe b) where model        = fmap model
instance (Model a c, Model b d) => Model (a, b) (c, d)       where model (a, b) = (model a, model b)
instance (Model c a, Model b d) => Model (a -> b) (c -> d)   where model f = model . f . model


eq1 f g = \a     -> model (f a)     == g (model a)
eq2 f g = \a b   -> model (f a b)   == g (model a) (model b)
eq3 f g = \a b c -> model (f a b c) == g (model a) (model b) (model c)

eqNotNull1 f g = \x     -> (not (DVI.null x)) ==> eq1 f g x
eqNotNull2 f g = \x y   -> (not (DVI.null y)) ==> eq2 f g x y
eqNotNull3 f g = \x y z -> (not (DVI.null z)) ==> eq3 f g x y z
