{-# LANGUAGE Rank2Types, MultiParamTypeClasses, BangPatterns, CPP #-}

#include "phases.h"

module Data.Vector.Base
where

import qualified Data.Vector.Base.Mutable as Mut

import qualified Data.Vector.Stream as Stream
import           Data.Vector.Stream ( Stream )
import           Data.Vector.Stream.Size

import Prelude hiding ( length, map, zipWith, sum )

class Base v a where
  create       :: (forall mv m. Mut.Base mv m a => m (mv m a)) -> v a

  length       :: v a -> Int
  unsafeSlice  :: v a -> Int -> Int -> v a

  unsafeIndex  :: v a -> Int -> (a -> b) -> b

stream :: Base v a => v a -> Stream a
{-# INLINE_STREAM stream #-}
stream !v = Stream.unfold get 0 `Stream.sized` Exact n
  where
    n = length v

    {-# INLINE get #-}
    get i | i < n     = unsafeIndex v i $ \x -> Just (x, i+1)
          | otherwise = Nothing

unstream :: Base v a => Stream a -> v a
{-# INLINE_STREAM unstream #-}
unstream s = create (Mut.unstream s)

{-# RULES

"stream/unstream [Vector.Base]" forall s.
  stream (unstream s) = s

 #-}

infixr ++
(++) :: Base v a => v a -> v a -> v a
{-# INLINE (++) #-}
v ++ w = unstream (stream v Stream.++ stream w)

take :: Base v a => Int -> v a -> v a
{-# INLINE take #-}
take n = unstream . Stream.take n . stream

drop :: Base v a => Int -> v a -> v a
{-# INLINE drop #-}
drop n = unstream . Stream.drop n . stream

map :: (Base v a, Base v b) => (a -> b) -> v a -> v b
{-# INLINE map #-}
map f = unstream . Stream.map f . stream

zipWith :: (Base v a, Base v b, Base v c) => (a -> b -> c) -> v a -> v b -> v c
{-# INLINE zipWith #-}
zipWith f xs ys = unstream (Stream.zipWith f (stream xs) (stream ys))

filter :: Base v a => (a -> Bool) -> v a -> v a
{-# INLINE filter #-}
filter f = unstream . Stream.filter f . stream

takeWhile :: Base v a => (a -> Bool) -> v a -> v a
{-# INLINE takeWhile #-}
takeWhile f = unstream . Stream.takeWhile f . stream

dropWhile :: Base v a => (a -> Bool) -> v a -> v a
{-# INLINE dropWhile #-}
dropWhile f = unstream . Stream.dropWhile f . stream

foldl' :: Base v b => (a -> b -> a) -> a -> v b -> a
{-# INLINE foldl' #-}
foldl' f z = Stream.foldl' f z . stream

sum :: (Base v a, Num a) => v a -> a
{-# INLINE sum #-}
sum = foldl' (+) 0

toList :: Base v a => v a -> [a]
{-# INLINE toList #-}
toList = Stream.toList . stream

fromList :: Base v a => [a] -> v a
{-# INLINE fromList #-}
fromList = unstream . Stream.fromList

