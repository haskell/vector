{-# LANGUAGE TypeFamilies, FlexibleContexts, RankNTypes, MultiParamTypeClasses, BangPatterns, CPP #-}

#include "phases.h"

module Data.Vector.Base
where

import qualified Data.Vector.Base.Mutable as Mut

import qualified Data.Vector.Stream as Stream
import           Data.Vector.Stream ( Stream )
import           Data.Vector.Stream.Size

import Prelude hiding ( length, map, zipWith, sum )

class Base v a where
  create       :: (forall mv. Mut.Base mv a => Mut.Trans mv (mv a)) -> v a

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

map :: (Base v a, Base v b) => (a -> b) -> v a -> v b
{-# INLINE map #-}
map f = unstream . Stream.map f . stream

zipWith :: (Base v a, Base v b, Base v c) => (a -> b -> c) -> v a -> v b -> v c
{-# INLINE zipWith #-}
zipWith f xs ys = unstream (Stream.zipWith f (stream xs) (stream ys))

foldl' :: Base v b => (a -> b -> a) -> a -> v b -> a
{-# INLINE foldl' #-}
foldl' f z = Stream.foldl' f z . stream

sum :: (Base v a, Num a) => v a -> a
{-# INLINE sum #-}
sum = foldl' (+) 0

