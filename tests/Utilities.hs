{-# LANGUAGE FlexibleInstances #-}
module Utilities where

import Test.QuickCheck

import qualified Data.Vector as DV
import qualified Data.Vector.Generic as DVG
import qualified Data.Vector.Primitive as DVP
import qualified Data.Vector.Fusion.Stream as S

import Data.List ( sortBy )


instance Show a => Show (S.Stream a) where
    show s = "Data.Vector.Fusion.Stream.fromList " ++ show (S.toList s)


instance Arbitrary a => Arbitrary (DV.Vector a) where
    arbitrary = fmap DV.fromList arbitrary
    coarbitrary = coarbitrary . DV.toList

instance (Arbitrary a, DVP.Prim a) => Arbitrary (DVP.Vector a) where
    arbitrary = fmap DVP.fromList arbitrary
    coarbitrary = coarbitrary . DVP.toList

instance Arbitrary a => Arbitrary (S.Stream a) where
    arbitrary = fmap S.fromList arbitrary
    coarbitrary = coarbitrary . S.toList


class Modelled a where
  type Model a
  -- | Convert a concrete value into an abstract model
  model :: a -> Model a
  unmodel :: Model a -> a

-- The meat of the models
instance Modelled (DV.Vector a) where
  type Model (DV.Vector a) = [a]
  model = DV.toList
  unmodel = DV.fromList

instance DVP.Prim a => Modelled (DVP.Vector a) where
  type Model (DVP.Vector a) = [a]
  model = DVP.toList
  unmodel = DVP.fromList

-- Identity models

#define id_Modelled(ty) \
instance Modelled ty where { type Model ty = ty; model = id; unmodel = id }

id_Modelled(Bool)
id_Modelled(Int)
id_Modelled(Float)
id_Modelled(Double)
id_Modelled(Ordering)

-- Functorish models
-- All of these need UndecidableInstances although they are actually well founded. Oh well.
instance Modelled a => Modelled (Maybe a) where
  type Model (Maybe a) = Maybe (Model a)
  model = fmap model
  unmodel = fmap unmodel

instance Modelled a => Modelled [a] where
  type Model [a] = [Model a]
  model = fmap model
  unmodel = fmap unmodel

instance (Modelled a, Modelled b) => Modelled (a,b) where
  type Model (a,b) = (Model a, Model b)
  model (a,b) = (model a, model b)
  unmodel (a,b) = (unmodel a, unmodel b)

instance (Modelled a, Modelled b, Modelled c) => Modelled (a,b,c) where
  type Model (a,b,c) = (Model a, Model b, Model c)
  model (a,b,c) = (model a, model b, model c)
  unmodel (a,b,c) = (unmodel a, unmodel b, unmodel c)

instance (Modelled a, Modelled b) => Modelled (a -> b) where
  type Model (a -> b) = Model a -> Model b
  model f = model . f . unmodel
  unmodel f = unmodel . f . model

class (Predicate (EqTest a), Testable (EqTest a)) => EqTestable a where
  type EqTest a

  equal :: a -> a -> EqTest a

#define EqTestable0(ty) \
instance EqTestable (ty) where { type EqTest (ty) = Bool; equal = (==) }

EqTestable0(Bool)
EqTestable0(Int)
EqTestable0(Float)
EqTestable0(Double)
EqTestable0(Ordering)

#define EqTestable1(ty) \
instance Eq a => EqTestable (ty a) where { type EqTest (ty a) = Bool; equal = (==) }

EqTestable1(Maybe)
EqTestable1([])
EqTestable1(DV.Vector)
EqTestable1(S.Stream)

instance (Eq a, DVP.Prim a) => EqTestable (DVP.Vector a) where
  type EqTest (DVP.Vector a) = Bool
  equal = (==)

instance (Eq a, Eq b) => EqTestable (a,b) where
  type EqTest (a,b) = Bool
  equal = (==)

instance (Eq a, Eq b, Eq c) => EqTestable (a,b,c) where
  type EqTest (a,b,c) = Bool
  equal = (==)

instance (Arbitrary a, Show a, EqTestable b) => EqTestable (a -> b) where
  type EqTest (a -> b) = a -> EqTest b

  equal f g x = f x `equal` g x

infix 4 `eq`
eq :: (Modelled a, EqTestable a) => a -> Model a -> EqTest a
x `eq` y = x `equal` unmodel y

class Testable (Prop f) => Predicate f where
  type Pred f
  type Prop f

  infixr 0 ===>
  (===>) :: Pred f -> f -> Prop f

instance Predicate Bool where
  type Pred Bool = Bool
  type Prop Bool = Property

  (===>) = (==>)

instance (Arbitrary a, Show a, Predicate f) => Predicate (a -> f) where
  type Pred (a -> f) = a -> Pred f
  type Prop (a -> f) = a -> Prop f

  p ===> f = \x -> p x ===> f x

notNull2 _ xs = not $ DVG.null xs

-- Generators
index_value_pairs :: Arbitrary a => Int -> Gen [(Int,a)]
index_value_pairs 0 = return [] 
index_value_pairs m = sized $ \n ->
  do
    len <- choose (0,n)
    is <- sequence [choose (0,m-1) | i <- [1..len]]
    xs <- vector len
    return $ zip is xs

indices :: Int -> Gen [Int]
indices 0 = return []
indices m = sized $ \n ->
  do
    len <- choose (0,n)
    sequence [choose (0,m-1) | i <- [1..len]]


-- Additional list functions
singleton x = [x]
snoc xs x = xs ++ [x]
slice xs i n = take n (drop i xs)
backpermute xs is = map (xs!!) is
prescanl f z = init . scanl f z
postscanl f z = tail . scanl f z

accum :: (a -> b -> a) -> [a] -> [(Int,b)] -> [a]
accum f xs ps = go xs ps' 0
  where
    ps' = sortBy (\p q -> compare (fst p) (fst q)) ps

    go (x:xs) ((i,y) : ps) j
      | i == j     = go (f x y : xs) ps j
    go (x:xs) ps j = x : go xs ps (j+1)
    go [] _ _      = []  

(//) :: [a] -> [(Int, a)] -> [a]
xs // ps = go xs ps' 0
  where
    ps' = sortBy (\p q -> compare (fst p) (fst q)) ps

    go (x:xs) ((i,y) : ps) j
      | i == j     = go (y:xs) ps j
    go (x:xs) ps j = x : go xs ps (j+1)
    go [] _ _      = []

