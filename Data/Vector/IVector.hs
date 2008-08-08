{-# LANGUAGE Rank2Types, MultiParamTypeClasses, FlexibleContexts,
             ScopedTypeVariables #-}
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

  -- * Accessing individual elements
  (!), head, last,

  -- * Subvectors
  slice, extract, takeSlice, take, dropSlice, drop,

  -- * Permutations
  (//), update, bpermute,

  -- * Mapping and zipping
  map, zipWith, zip,

  -- * Comparisons
  eq, cmp,

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
  new,

  -- * Unsafe functions
  unsafeSlice, unsafeIndex,

  -- * Utility functions
  vlength, vnew
) where

import qualified Data.Vector.MVector as MVector
import           Data.Vector.MVector ( MVector )

import qualified Data.Vector.MVector.New as New
import           Data.Vector.MVector.New ( New )

import qualified Data.Vector.Fusion.Stream as Stream
import           Data.Vector.Fusion.Stream ( Stream )
import qualified Data.Vector.Fusion.MStream as MStream
import           Data.Vector.Fusion.MStream ( MStream )
import           Data.Vector.Fusion.Stream.Size

import Control.Exception ( assert )

import Prelude hiding ( length,
                        replicate, (++),
                        head, last,
                        init, tail, take, drop,
                        map, zipWith, zip,
                        filter, takeWhile, dropWhile,
                        elem, notElem,
                        foldl, foldl1, foldr, foldr1 )

-- | Class of immutable vectors.
--
class IVector v a where
  -- | Construct a pure vector from a monadic initialiser (not fusible!)
  vnew         :: (forall mv m. MVector mv m a => m (mv a)) -> v a

  -- | Length of the vector (not fusible!)
  vlength      :: v a -> Int

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

-- Fusion
-- ------

-- | Construct a pure vector from a monadic initialiser 
new :: IVector v a => New a -> v a
{-# INLINE_STREAM new #-}
new m = vnew (New.run m)

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
{-# INLINE unstream #-}
unstream s = new (New.unstream s)

{-# RULES

"stream/unstream [IVector]" forall s.
  stream (new (New.unstream s)) = s

"New.unstream/stream/new [IVector]" forall p.
  New.unstream (stream (new p)) = p

 #-}

inplace :: (Stream a -> Stream a)
        -> (forall m. Monad m => MStream m a -> MStream m a)
        -> Stream a -> Stream a
{-# INLINE_STREAM inplace #-}
inplace f _ s = f s

{-# RULES

"inplace [IVector]"
  forall f (mf :: forall m. Monad m => MStream m a -> MStream m a) m.
  New.unstream (inplace f mf (stream (new m))) = New.inplace mf m

"inplace/inplace [IVector]"
  forall f (mf :: forall m. Monad m => MStream m a -> MStream m a)
         g (mg :: forall m. Monad m => MStream m a -> MStream m a) s.
  inplace f mf (inplace g mg s) = inplace (f . g) (mf . mg) s

 #-}

-- Length
-- ------

length :: IVector v a => v a -> Int
{-# INLINE_STREAM length #-}
length v = vlength v

{-# RULES

"length/unstream [IVector]" forall s.
  length (new (New.unstream s)) = Stream.length s

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

-- Accessing individual elements
-- -----------------------------

-- | Indexing
(!) :: IVector v a => v a -> Int -> a
{-# INLINE_STREAM (!) #-}
v ! i = assert (i >= 0 && i < length v)
      $ unsafeIndex v i id

-- | First element
head :: IVector v a => v a -> a
{-# INLINE_STREAM head #-}
head v = v ! 0

-- | Last element
last :: IVector v a => v a -> a
{-# INLINE_STREAM last #-}
last v = v ! (length v - 1)

{-# RULES

"(!)/unstream [IVector]" forall i s.
  new (New.unstream s) ! i = s Stream.!! i

"head/unstream [IVector]" forall s.
  head (new (New.unstream s)) = Stream.head s

"last/unstream [IVector]" forall s.
  last (new (New.unstream s)) = Stream.last s

 #-}

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

-- | Copy @n@ elements starting at the given position to a new vector.
extract :: IVector v a => v a -> Int  -- ^ starting index
                              -> Int  -- ^ length
                              -> v a
{-# INLINE extract #-}
extract v i n = unstream (Stream.extract (stream v) i n)

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

{-# RULES

"slice/extract [IVector]" forall i n s.
  slice (new (New.unstream s)) i n = extract (new (New.unstream s)) i n

"takeSlice/unstream [IVector]" forall n s.
  takeSlice n (new (New.unstream s)) = take n (new (New.unstream s))

"dropSlice/unstream [IVector]" forall n s.
  dropSlice n (new (New.unstream s)) = drop n (new (New.unstream s))

  #-}

-- Permutations
-- ------------

(//) :: IVector v a => v a -> [(Int, a)] -> v a
{-# INLINE (//) #-}
v // us = new (New.update (New.unstream (stream v))
                          (Stream.fromList us))

update :: (IVector v a, IVector v (Int, a)) => v a -> v (Int, a) -> v a
{-# INLINE update #-}
update v w = new (New.update (New.unstream (stream v)) (stream w))

bpermute :: (IVector v a, IVector v Int) => v a -> v Int -> v a
{-# INLINE bpermute #-}
bpermute v is = is `seq` map (v!) is

-- Mapping/zipping
-- ---------------

-- | Map a function over a vector
map :: (IVector v a, IVector v b) => (a -> b) -> v a -> v b
{-# INLINE map #-}
map f = unstream . Stream.map f . stream

inplace_map :: IVector v a => (a -> a) -> v a -> v a
{-# INLINE inplace_map #-}
inplace_map f = unstream . inplace (Stream.map f) (MStream.map f) . stream

{-# RULES

"map->inplace_map [IVector]" map = inplace_map

 #-}

-- | Zip two vectors with the given function.
zipWith :: (IVector v a, IVector v b, IVector v c) => (a -> b -> c) -> v a -> v b -> v c
{-# INLINE zipWith #-}
zipWith f xs ys = unstream (Stream.zipWith f (stream xs) (stream ys))

zip :: (IVector v a, IVector v b, IVector v (a,b)) => v a -> v b -> v (a, b)
{-# INLINE zip #-}
zip = zipWith (,)

-- Comparisons
-- -----------

eq :: (IVector v a, Eq a) => v a -> v a -> Bool
{-# INLINE eq #-}
xs `eq` ys = stream xs == stream ys

cmp :: (IVector v a, Ord a) => v a -> v a -> Ordering
{-# INLINE cmp #-}
cmp xs ys = compare (stream xs) (stream ys)

-- Filtering
-- ---------

-- | Drop elements which do not satisfy the predicate
filter :: IVector v a => (a -> Bool) -> v a -> v a
{-# INLINE filter #-}
filter f = unstream . inplace (Stream.filter f) (MStream.filter f) . stream

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

{-# RULES

"takeWhileSlice/unstream" forall f s.
  takeWhileSlice f (new (New.unstream s)) = takeWhile f (new (New.unstream s))

"dropWhileSlice/unstream" forall f s.
  dropWhileSlice f (new (New.unstream s)) = dropWhile f (new (New.unstream s))

 #-}

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

