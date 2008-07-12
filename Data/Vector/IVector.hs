{-# LANGUAGE Rank2Types, MultiParamTypeClasses, BangPatterns, CPP #-}

#include "phases.h"

module Data.Vector.IVector
where

import qualified Data.Vector.MVector as MVector
import           Data.Vector.MVector ( MVector )

import qualified Data.Vector.Stream as Stream
import           Data.Vector.Stream ( Stream )
import           Data.Vector.Stream.Size

import Prelude hiding ( length,
                        replicate, (++),
                        head, last,
                        init, tail, take, drop,
                        map, zipWith,
                        filter, takeWhile, dropWhile,
                        foldl, foldl1, foldr, foldr1 )

class IVector v a where
  create       :: (forall mv m. MVector mv m a => m (mv m a)) -> v a

  length       :: v a -> Int
  unsafeSlice  :: v a -> Int -> Int -> v a

  unsafeIndex  :: v a -> Int -> (a -> b) -> b

stream :: IVector v a => v a -> Stream a
{-# INLINE_STREAM stream #-}
stream !v = Stream.unfold get 0 `Stream.sized` Exact n
  where
    n = length v

    {-# INLINE get #-}
    get i | i < n     = unsafeIndex v i $ \x -> Just (x, i+1)
          | otherwise = Nothing

unstream :: IVector v a => Stream a -> v a
{-# INLINE_STREAM unstream #-}
unstream s = create (MVector.unstream s)

{-# RULES

"stream/unstream [Vector.IVector]" forall s.
  stream (unstream s) = s

 #-}

-- Construction
-- ------------

empty :: IVector v a => v a
{-# INLINE empty #-}
empty = unstream Stream.empty

singleton :: IVector v a => a -> v a
{-# INLINE singleton #-}
singleton x = unstream (Stream.singleton x)

replicate :: IVector v a => Int -> a -> v a
{-# INLINE replicate #-}
replicate n = unstream . Stream.replicate n

cons :: IVector v a => a -> v a -> v a
{-# INLINE cons #-}
cons x = unstream . Stream.cons x . stream

snoc :: IVector v a => v a -> a -> v a
{-# INLINE snoc #-}
snoc v = unstream . Stream.snoc (stream v)

infixr 5 ++
(++) :: IVector v a => v a -> v a -> v a
{-# INLINE (++) #-}
v ++ w = unstream (stream v Stream.++ stream w)

-- Subarrays
-- ---------

take :: IVector v a => Int -> v a -> v a
{-# INLINE take #-}
take n = unstream . Stream.take n . stream

drop :: IVector v a => Int -> v a -> v a
{-# INLINE drop #-}
drop n = unstream . Stream.drop n . stream

-- Mapping/zipping
-- ---------------

map :: (IVector v a, IVector v b) => (a -> b) -> v a -> v b
{-# INLINE map #-}
map f = unstream . Stream.map f . stream

zipWith :: (IVector v a, IVector v b, IVector v c) => (a -> b -> c) -> v a -> v b -> v c
{-# INLINE zipWith #-}
zipWith f xs ys = unstream (Stream.zipWith f (stream xs) (stream ys))

-- Filtering
-- ---------

filter :: IVector v a => (a -> Bool) -> v a -> v a
{-# INLINE filter #-}
filter f = unstream . Stream.filter f . stream

takeWhile :: IVector v a => (a -> Bool) -> v a -> v a
{-# INLINE takeWhile #-}
takeWhile f = unstream . Stream.takeWhile f . stream

dropWhile :: IVector v a => (a -> Bool) -> v a -> v a
{-# INLINE dropWhile #-}
dropWhile f = unstream . Stream.dropWhile f . stream

-- Folding
-- -------

foldl :: IVector v b => (a -> b -> a) -> a -> v b -> a
{-# INLINE foldl #-}
foldl f z = Stream.foldl f z . stream

foldl1 :: IVector v a => (a -> a -> a) -> v a -> a
{-# INLINE foldl1 #-}
foldl1 f = Stream.foldl1 f . stream

foldl' :: IVector v b => (a -> b -> a) -> a -> v b -> a
{-# INLINE foldl' #-}
foldl' f z = Stream.foldl' f z . stream

foldl1' :: IVector v a => (a -> a -> a) -> v a -> a
{-# INLINE foldl1' #-}
foldl1' f = Stream.foldl1' f . stream

foldr :: IVector v a => (a -> b -> b) -> b -> v a -> b
{-# INLINE foldr #-}
foldr f z = Stream.foldr f z . stream

foldr1 :: IVector v a => (a -> a -> a) -> v a -> a
{-# INLINE foldr1 #-}
foldr1 f = Stream.foldr1 f . stream

toList :: IVector v a => v a -> [a]
{-# INLINE toList #-}
toList = Stream.toList . stream

fromList :: IVector v a => [a] -> v a
{-# INLINE fromList #-}
fromList = unstream . Stream.fromList

