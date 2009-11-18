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


class Model a b | a -> b where
  -- | Convert a concrete value into an abstract model
  model :: a -> b

-- The meat of the models
instance               Model (DV.Vector a)  [a] where model = DV.toList
instance DVP.Prim a => Model (DVP.Vector a) [a] where model = DVP.toList

-- Identity models
instance Model Bool     Bool     where model = id
instance Model Int      Int      where model = id
instance Model Float    Float    where model = id
instance Model Double   Double   where model = id
instance Model Ordering Ordering where model = id

-- Functorish models
-- All of these need UndecidableInstances although they are actually well founded. Oh well.
instance Model a b                            => Model (Maybe a) (Maybe b)    where model           = fmap model
instance Model a b                            => Model [a] [b]                where model           = fmap model
instance (Model a a', Model b b')             => Model (a, b) (a', b')        where model (a, b)    = (model a, model b)
instance (Model a a', Model b b', Model c c') => Model (a, b, c) (a', b', c') where model (a, b, c) = (model a, model b, model c)
instance (Model c a, Model b d)               => Model (a -> b) (c -> d)      where model f         = model . f . model


eq0 f g =             model f           == g
eq1 f g = \a       -> model (f a)       == g (model a)
eq2 f g = \a b     -> model (f a b)     == g (model a) (model b)
eq3 f g = \a b c   -> model (f a b c)   == g (model a) (model b) (model c)
eq4 f g = \a b c d -> model (f a b c d) == g (model a) (model b) (model c) (model d)

eqNotNull1 f g = \a       -> (not (DVG.null a)) ==> eq1 f g a
eqNotNull2 f g = \a b     -> (not (DVG.null b)) ==> eq2 f g a b
eqNotNull3 f g = \a b c   -> (not (DVG.null c)) ==> eq3 f g a b c
eqNotNull4 f g = \a b c d -> (not (DVG.null d)) ==> eq4 f g a b c d

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

