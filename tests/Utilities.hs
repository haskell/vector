{-# LANGUAGE FlexibleInstances, GADTs #-}
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

instance CoArbitrary a => CoArbitrary (DV.Vector a) where
    coarbitrary = coarbitrary . DV.toList

instance (Arbitrary a, DVP.Prim a) => Arbitrary (DVP.Vector a) where
    arbitrary = fmap DVP.fromList arbitrary

instance (CoArbitrary a, DVP.Prim a) => CoArbitrary (DVP.Vector a) where
    coarbitrary = coarbitrary . DVP.toList

instance Arbitrary a => Arbitrary (S.Stream a) where
    arbitrary = fmap S.fromList arbitrary

instance CoArbitrary a => CoArbitrary (S.Stream a) where
    coarbitrary = coarbitrary . S.toList

class (Testable (Pty a), Conclusion (Pty a)) => TestData a where
  type Pty a
  type Model a
  model :: a -> Model a
  unmodel :: Model a -> a

instance TestData (DV.Vector a) where
  type Pty (DV.Vector a) = Property
  type Model (DV.Vector a) = [a]
  model = DV.toList
  unmodel = DV.fromList

instance DVP.Prim a => TestData (DVP.Vector a) where
  type Pty (DVP.Vector a) = Property
  type Model (DVP.Vector a) = [a]
  model = DVP.toList
  unmodel = DVP.fromList

-- Identity models

#define id_TestData(ty) \
instance TestData ty where { type Pty ty = Property; type Model ty = ty; model = id; unmodel = id }

id_TestData(Bool)
id_TestData(Int)
id_TestData(Float)
id_TestData(Double)
id_TestData(Ordering)

-- Functorish models
-- All of these need UndecidableInstances although they are actually well founded. Oh well.
instance TestData a => TestData (Maybe a) where
  type Pty (Maybe a) = Property
  type Model (Maybe a) = Maybe (Model a)
  model = fmap model
  unmodel = fmap unmodel

instance TestData a => TestData [a] where
  type Pty [a] = Property
  type Model [a] = [Model a]
  model = fmap model
  unmodel = fmap unmodel

instance (TestData a, TestData b) => TestData (a,b) where
  type Pty (a,b) = Property
  type Model (a,b) = (Model a, Model b)
  model (a,b) = (model a, model b)
  unmodel (a,b) = (unmodel a, unmodel b)

instance (TestData a, TestData b, TestData c) => TestData (a,b,c) where
  type Pty (a,b,c) = Property
  type Model (a,b,c) = (Model a, Model b, Model c)
  model (a,b,c) = (model a, model b, model c)
  unmodel (a,b,c) = (unmodel a, unmodel b, unmodel c)

instance (Arbitrary a, Show a, TestData a, TestData b) => TestData (a -> b) where
  type Pty (a -> b) = a -> Pty b
  type Model (a -> b) = Model a -> Model b
  model f = model . f . unmodel
  unmodel f = unmodel . f . model

data P a where
  P :: TestData a => Pty a -> P a

unP :: P a -> Pty a
unP (P p) = p

instance Testable (P a) where
  property (P a) = property a

class TestData a => EqTestable a p where
  equal :: a -> a -> p

instance (Eq a, TestData a) => EqTestable a Property where
  equal x y = property (x==y)

instance (Arbitrary a, Show a, TestData a, EqTestable b p) => EqTestable (a -> b) (a -> p) where
  equal f g = \x -> equal (f x) (g x)

infix 4 `eq`
eq :: EqTestable a (Pty a) => a -> Model a -> P a
eq x y = P (equal x (unmodel y))

class Conclusion p where
  type Predicate p

  predicate :: Predicate p -> p -> p

instance Conclusion Property where
  type Predicate Property = Bool

  predicate = (==>)

instance Conclusion p => Conclusion (a -> p) where
  type Predicate (a -> p) = a -> Predicate p

  predicate f p = \x -> predicate (f x) (p x)

infixr 0 ===>
(===>) :: Predicate (Pty a) -> P a -> P a
p ===> P a = P (predicate p a)

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

