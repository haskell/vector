{-# LANGUAGE Rank2Types, MultiParamTypeClasses #-}
-- |
-- Module      : Data.Vector.IVector
-- Copyright   : (c) Roman Leshchinskiy 2008
-- License     : BSD-style
--
-- Maintainer  : rl@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : non-portable
-- 
-- Generic interface to pure vectors
--

#include "phases.h"

module Data.Vector.IVector (
  -- * Immutable vectors
  IVector,

  -- * Length information
  length,

  -- * Construction
  empty, singleton, cons, snoc, replicate, (++),

  -- * Subvectors
  slice, subvector, takeSlice, take, dropSlice, drop,

  -- * Mapping and zipping
  map, zipWith,

  -- * Filtering
  filter, takeWhileSlice, takeWhile, dropWhileSlice, dropWhile,

  -- * Searching
  elem, notElem, find, findIndex,

  -- * Folding
  foldl, foldl1, foldl', foldl1', foldr, foldr1,

  -- * Conversion to/from lists
  toList, fromList,

  -- * Conversion to/from Streams
  stream, unstream,

  -- * MVector-based initialisation
  create,

  -- * Unsafe functions
  unsafeSlice, unsafeIndex
) where

import qualified Data.Vector.MVector as MVector
import           Data.Vector.MVector ( MVector )

import qualified Data.Vector.Stream as Stream
import           Data.Vector.Stream ( Stream )
import           Data.Vector.Stream.Size

import Control.Exception ( assert )

import Prelude hiding ( length,
                        replicate, (++),
                        head, last,
                        init, tail, take, drop,
                        map, zipWith,
                        filter, takeWhile, dropWhile,
                        elem, notElem,
                        foldl, foldl1, foldr, foldr1 )

-- | Class of immutable vectors. Just like with 'MVector', the type of the
-- elements can be restricted by using GADTs.
--
class IVector v a where
  -- | Construct a pure vector from a monadic initialiser.
  create       :: (forall mv m. MVector mv m a => m (mv m a)) -> v a

  -- | Length of the vector
  length       :: v a -> Int

  -- | Yield a part of the vector without copying it. No range checks!
  unsafeSlice  :: v a -> Int -> Int -> v a

  -- | Apply the given function to the element at the given position. This
  -- interface prevents us from being too lazy. Suppose we had
  --
  -- > unsafeIndex' :: v a -> Int -> a
  --
  -- instead. Now, if we wanted to copy a vector, we'd do something like
  --
  -- > copy mv v ... = ... unsafeWrite mv i (unsafeIndex' v i) ...
  --
  -- For lazy vectors, the indexing would not be evaluated which means that we
  -- would retain a reference to the original vector in each element we write.
  -- This would be bad!
  --
  -- With 'unsafeIndex', we can do
  --
  -- > copy mv v ... = ... unsafeIndex v i (unsafeWrite mv i) ...
  --
  -- which does not have this problem.
  --
  unsafeIndex  :: v a -> Int -> (a -> b) -> b

-- | Convert a vector to a 'Stream'
stream :: IVector v a => v a -> Stream a
{-# INLINE_STREAM stream #-}
stream v = v `seq` (Stream.unfold get 0 `Stream.sized` Exact n)
  where
    n = length v

    {-# INLINE get #-}
    get i | i < n     = unsafeIndex v i $ \x -> Just (x, i+1)
          | otherwise = Nothing

-- | Create a vector from a 'Stream'
unstream :: IVector v a => Stream a -> v a
{-# INLINE_STREAM unstream #-}
unstream s = create (MVector.unstream s)

{-# RULES

"stream/unstream [Vector.IVector]" forall s.
  stream (unstream s) = s

 #-}

-- Construction
-- ------------

-- | Empty vector
empty :: IVector v a => v a
{-# INLINE empty #-}
empty = unstream Stream.empty

-- | Vector with exaclty one element
singleton :: IVector v a => a -> v a
{-# INLINE singleton #-}
singleton x = unstream (Stream.singleton x)

-- | Vector of the given length with the given value in each position
replicate :: IVector v a => Int -> a -> v a
{-# INLINE replicate #-}
replicate n = unstream . Stream.replicate n

-- | Prepend an element
cons :: IVector v a => a -> v a -> v a
{-# INLINE cons #-}
cons x = unstream . Stream.cons x . stream

-- | Append an element
snoc :: IVector v a => v a -> a -> v a
{-# INLINE snoc #-}
snoc v = unstream . Stream.snoc (stream v)

infixr 5 ++
-- | Concatenate two vectors
(++) :: IVector v a => v a -> v a -> v a
{-# INLINE (++) #-}
v ++ w = unstream (stream v Stream.++ stream w)

-- Subarrays
-- ---------

-- | Yield a part of the vector without copying it. Safer version of
-- 'unsafeSlice'.
slice :: IVector v a => v a -> Int   -- ^ starting index
                            -> Int   -- ^ length
                            -> v a
{-# INLINE slice #-}
slice v i n = assert (i >= 0 && n >= 0  && i+n <= length v)
            $ unsafeSlice v i n

-- | Yield the first @n@ elements without copying.
takeSlice :: IVector v a => Int -> v a -> v a
{-# INLINE takeSlice #-}
takeSlice n v = slice v 0 n

-- | Copy the first @n@ elements to a new vector.
take :: IVector v a => Int -> v a -> v a
{-# INLINE take #-}
take n = unstream . Stream.take n . stream

-- | Yield all but the first @n@ elements without copying.
dropSlice :: IVector v a => Int -> v a -> v a
{-# INLINE dropSlice #-}
dropSlice n v = slice v n (length v - n)

-- | Copy all but the first @n@ elements to a new vectors.
drop :: IVector v a => Int -> v a -> v a
{-# INLINE drop #-}
drop n = unstream . Stream.drop n . stream

-- Mapping/zipping
-- ---------------

-- | Map a function over a vector
map :: (IVector v a, IVector v b) => (a -> b) -> v a -> v b
{-# INLINE map #-}
map f = unstream . Stream.map f . stream

-- | Zip two vectors with the given function.
zipWith :: (IVector v a, IVector v b, IVector v c) => (a -> b -> c) -> v a -> v b -> v c
{-# INLINE zipWith #-}
zipWith f xs ys = unstream (Stream.zipWith f (stream xs) (stream ys))

-- Filtering
-- ---------

-- | Drop elements which do not satisfy the predicate
filter :: IVector v a => (a -> Bool) -> v a -> v a
{-# INLINE filter #-}
filter f = unstream . Stream.filter f . stream

-- | Yield the longest prefix of elements satisfying the predicate without
-- copying.
takeWhileSlice :: IVector v a => (a -> Bool) -> v a -> v a
{-# INLINE takeWhileSlice #-}
takeWhileSlice f v = case findIndex (not . f) v of
                       Just n  -> takeSlice n v
                       Nothing -> v

-- | Copy the longest prefix of elements satisfying the predicate to a new
-- vector
takeWhile :: IVector v a => (a -> Bool) -> v a -> v a
{-# INLINE takeWhile #-}
takeWhile f = unstream . Stream.takeWhile f . stream

-- | Drop the longest prefix of elements that satisfy the predicate without
-- copying
dropWhileSlice :: IVector v a => (a -> Bool) -> v a -> v a
{-# INLINE dropWhileSlice #-}
dropWhileSlice f v = case findIndex (not . f) v of
                       Just n  -> dropSlice n v
                       Nothing -> v

-- | Drop the longest prefix of elements that satisfy the predicate and copy
-- the rest to a new vector.
dropWhile :: IVector v a => (a -> Bool) -> v a -> v a
{-# INLINE dropWhile #-}
dropWhile f = unstream . Stream.dropWhile f . stream

-- Searching
-- ---------

infix 4 `elem`
-- | Check whether the vector contains an element
elem :: (IVector v a, Eq a) => a -> v a -> Bool
{-# INLINE elem #-}
elem x = Stream.elem x . stream

infix 4 `notElem`
-- | Inverse of `elem`
notElem :: (IVector v a, Eq a) => a -> v a -> Bool
{-# INLINE notElem #-}
notElem x = Stream.notElem x . stream

-- | Yield 'Just' the first element matching the predicate or 'Nothing' if no
-- such element exists.
find :: IVector v a => (a -> Bool) -> v a -> Maybe a
{-# INLINE find #-}
find f = Stream.find f . stream

-- | Yield 'Just' the index of the first element matching the predicate or
-- 'Nothing' if no such element exists.
findIndex :: IVector v a => (a -> Bool) -> v a -> Maybe Int
{-# INLINE findIndex #-}
findIndex f = Stream.findIndex f . stream

-- Folding
-- -------

-- | Left fold
foldl :: IVector v b => (a -> b -> a) -> a -> v b -> a
{-# INLINE foldl #-}
foldl f z = Stream.foldl f z . stream

-- | Lefgt fold on non-empty vectors
foldl1 :: IVector v a => (a -> a -> a) -> v a -> a
{-# INLINE foldl1 #-}
foldl1 f = Stream.foldl1 f . stream

-- | Left fold with strict accumulator
foldl' :: IVector v b => (a -> b -> a) -> a -> v b -> a
{-# INLINE foldl' #-}
foldl' f z = Stream.foldl' f z . stream

-- | Left fold on non-empty vectors with strict accumulator
foldl1' :: IVector v a => (a -> a -> a) -> v a -> a
{-# INLINE foldl1' #-}
foldl1' f = Stream.foldl1' f . stream

-- | Right fold
foldr :: IVector v a => (a -> b -> b) -> b -> v a -> b
{-# INLINE foldr #-}
foldr f z = Stream.foldr f z . stream

-- | Right fold on non-empty vectors
foldr1 :: IVector v a => (a -> a -> a) -> v a -> a
{-# INLINE foldr1 #-}
foldr1 f = Stream.foldr1 f . stream

-- | Convert a vector to a list
toList :: IVector v a => v a -> [a]
{-# INLINE toList #-}
toList = Stream.toList . stream

-- | Convert a list to a vector
fromList :: IVector v a => [a] -> v a
{-# INLINE fromList #-}
fromList = unstream . Stream.fromList

