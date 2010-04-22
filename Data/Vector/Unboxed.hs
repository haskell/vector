{-# LANGUAGE Rank2Types #-}

-- |
-- Module      : Data.Vector.Unboxed
-- Copyright   : (c) Roman Leshchinskiy 2009-2010
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
--
-- Adaptive unboxed vectors
--

module Data.Vector.Unboxed (
  Vector, MVector(..), Unbox,

  -- * Length information
  length, null,

  -- * Construction
  empty, singleton, cons, snoc, replicate, generate, (++), force,

  -- * Accessing individual elements
  (!), head, last, indexM, headM, lastM,
  unsafeIndex, unsafeHead, unsafeLast,
  unsafeIndexM, unsafeHeadM, unsafeLastM,

  -- * Subvectors
  slice, init, tail, take, drop,
  unsafeSlice, unsafeInit, unsafeTail, unsafeTake, unsafeDrop,

  -- * Permutations
  accum, accumulate, accumulate_,
  (//), update, update_,
  backpermute, reverse,
  unsafeAccum, unsafeAccumulate, unsafeAccumulate_,
  unsafeUpd, unsafeUpdate, unsafeUpdate_,
  unsafeBackpermute,

  -- * Mapping
  map, imap, concatMap,

  -- * Zipping and unzipping
  zipWith, zipWith3, zipWith4, zipWith5, zipWith6,
  izipWith, izipWith3, izipWith4, izipWith5, izipWith6,
  zip, zip3, zip4, zip5, zip6,
  unzip, unzip3, unzip4, unzip5, unzip6,

  -- * Filtering
  filter, ifilter, takeWhile, dropWhile,
  partition, unstablePartition, span, break,

  -- * Searching
  elem, notElem, find, findIndex, findIndices, elemIndex, elemIndices,

  -- * Folding
  foldl, foldl1, foldl', foldl1', foldr, foldr1, foldr', foldr1',
  ifoldl, ifoldl', ifoldr, ifoldr',

  -- * Specialised folds
  all, any, and, or,
  sum, product,
  maximum, maximumBy, minimum, minimumBy,
  minIndex, minIndexBy, maxIndex, maxIndexBy,

  -- * Unfolding
  unfoldr, unfoldrN,

  -- * Scans
  prescanl, prescanl',
  postscanl, postscanl',
  scanl, scanl', scanl1, scanl1',
  prescanr, prescanr',
  postscanr, postscanr',
  scanr, scanr', scanr1, scanr1',

  -- * Enumeration
  enumFromN, enumFromStepN, enumFromTo, enumFromThenTo,

  -- * Conversion to/from lists
  toList, fromList, fromListN,

  -- * Monadic operations
  replicateM, mapM, mapM_, forM, forM_, zipWithM, zipWithM_, filterM,
  foldM, foldM', fold1M, fold1M',

  -- * Destructive operations
  create, modify, copy, unsafeCopy
) where

import Data.Vector.Unboxed.Base
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Fusion.Stream as Stream

import Control.Monad.ST ( ST )
import Control.Monad.Primitive

import Prelude hiding ( length, null,
                        replicate, (++),
                        head, last,
                        init, tail, take, drop, reverse,
                        map, concatMap,
                        zipWith, zipWith3, zip, zip3, unzip, unzip3,
                        filter, takeWhile, dropWhile, span, break,
                        elem, notElem,
                        foldl, foldl1, foldr, foldr1,
                        all, any, and, or, sum, product, minimum, maximum,
                        scanl, scanl1, scanr, scanr1,
                        enumFromTo, enumFromThenTo,
                        mapM, mapM_ )
import qualified Prelude

#include "vector.h"

-- See [HACKS:Eq and Ord instances]
instance (Unbox a, Eq a) => Eq (Vector a) where
  {-# INLINE (==) #-}
  xs == ys = Stream.eq (G.stream xs) (G.stream ys)

  {-# INLINE (/=) #-}
  xs /= ys = not (Stream.eq (G.stream xs) (G.stream ys))

-- See [HACKS:Eq and Ord instances]
instance (Unbox a, Ord a) => Ord (Vector a) where
  {-# INLINE compare #-}
  compare xs ys = Stream.cmp (G.stream xs) (G.stream ys)

  {-# INLINE (<) #-}
  xs < ys = Stream.cmp (G.stream xs) (G.stream ys) == LT

  {-# INLINE (<=) #-}
  xs <= ys = Stream.cmp (G.stream xs) (G.stream ys) /= GT

  {-# INLINE (>) #-}
  xs > ys = Stream.cmp (G.stream xs) (G.stream ys) == GT

  {-# INLINE (>=) #-}
  xs >= ys = Stream.cmp (G.stream xs) (G.stream ys) /= LT

instance (Show a, Unbox a) => Show (Vector a) where
    show = (Prelude.++ " :: Data.Vector.Unboxed.Vector") . ("fromList " Prelude.++) . show . toList

-- Length
-- ------

length :: Unbox a => Vector a -> Int
{-# INLINE length #-}
length = G.length

null :: Unbox a => Vector a -> Bool
{-# INLINE null #-}
null = G.null

-- Construction
-- ------------

-- | Empty vector
empty :: Unbox a => Vector a
{-# INLINE empty #-}
empty = G.empty

-- | Vector with exaclty one element
singleton :: Unbox a => a -> Vector a
{-# INLINE singleton #-}
singleton = G.singleton

-- | Vector of the given length with the given value in each position
replicate :: Unbox a => Int -> a -> Vector a
{-# INLINE replicate #-}
replicate = G.replicate

-- | Generate a vector of the given length by applying the function to each
-- index
generate :: Unbox a => Int -> (Int -> a) -> Vector a
{-# INLINE generate #-}
generate = G.generate

-- | Prepend an element
cons :: Unbox a => a -> Vector a -> Vector a
{-# INLINE cons #-}
cons = G.cons

-- | Append an element
snoc :: Unbox a => Vector a -> a -> Vector a
{-# INLINE snoc #-}
snoc = G.snoc

infixr 5 ++
-- | Concatenate two vectors
(++) :: Unbox a => Vector a -> Vector a -> Vector a
{-# INLINE (++) #-}
(++) = (G.++)

-- | Create a copy of a vector. Useful when dealing with slices.
force :: Unbox a => Vector a -> Vector a
{-# INLINE force #-}
force = G.force

-- Accessing individual elements
-- -----------------------------

-- | Indexing
(!) :: Unbox a => Vector a -> Int -> a
{-# INLINE (!) #-}
(!) = (G.!)

-- | First element
head :: Unbox a => Vector a -> a
{-# INLINE head #-}
head = G.head

-- | Last element
last :: Unbox a => Vector a -> a
{-# INLINE last #-}
last = G.last

-- | Unsafe indexing without bounds checking
unsafeIndex :: Unbox a => Vector a -> Int -> a
{-# INLINE unsafeIndex #-}
unsafeIndex = G.unsafeIndex

-- | Yield the first element of a vector without checking if the vector is
-- empty
unsafeHead :: Unbox a => Vector a -> a
{-# INLINE unsafeHead #-}
unsafeHead = G.unsafeHead

-- | Yield the last element of a vector without checking if the vector is
-- empty
unsafeLast :: Unbox a => Vector a -> a
{-# INLINE unsafeLast #-}
unsafeLast = G.unsafeLast

-- | Monadic indexing which can be strict in the vector while remaining lazy in
-- the element
indexM :: (Unbox a, Monad m) => Vector a -> Int -> m a
{-# INLINE indexM #-}
indexM = G.indexM

headM :: (Unbox a, Monad m) => Vector a -> m a
{-# INLINE headM #-}
headM = G.headM

lastM :: (Unbox a, Monad m) => Vector a -> m a
{-# INLINE lastM #-}
lastM = G.lastM

-- | Unsafe monadic indexing without bounds checks
unsafeIndexM :: (Unbox a, Monad m) => Vector a -> Int -> m a
{-# INLINE unsafeIndexM #-}
unsafeIndexM = G.unsafeIndexM

unsafeHeadM :: (Unbox a, Monad m) => Vector a -> m a
{-# INLINE unsafeHeadM #-}
unsafeHeadM = G.unsafeHeadM

unsafeLastM :: (Unbox a, Monad m) => Vector a -> m a
{-# INLINE unsafeLastM #-}
unsafeLastM = G.unsafeLastM

-- Subarrays
-- ---------

-- | Yield a part of the vector without copying it. Safer version of
-- 'basicUnsafeSlice'.
slice :: Unbox a => Int   -- ^ starting index
                 -> Int   -- ^ length
                 -> Vector a
                 -> Vector a
{-# INLINE slice #-}
slice = G.slice

-- | Yield all but the last element without copying.
init :: Unbox a => Vector a -> Vector a
{-# INLINE init #-}
init = G.init

-- | All but the first element (without copying).
tail :: Unbox a => Vector a -> Vector a
{-# INLINE tail #-}
tail = G.tail

-- | Yield the first @n@ elements without copying.
take :: Unbox a => Int -> Vector a -> Vector a
{-# INLINE take #-}
take = G.take

-- | Yield all but the first @n@ elements without copying.
drop :: Unbox a => Int -> Vector a -> Vector a
{-# INLINE drop #-}
drop = G.drop

-- | Unsafely yield a part of the vector without copying it and without
-- performing bounds checks.
unsafeSlice :: Unbox a => Int   -- ^ starting index
                       -> Int   -- ^ length
                       -> Vector a
                       -> Vector a
{-# INLINE unsafeSlice #-}
unsafeSlice = G.unsafeSlice

unsafeInit :: Unbox a => Vector a -> Vector a
{-# INLINE unsafeInit #-}
unsafeInit = G.unsafeInit

unsafeTail :: Unbox a => Vector a -> Vector a
{-# INLINE unsafeTail #-}
unsafeTail = G.unsafeTail

unsafeTake :: Unbox a => Int -> Vector a -> Vector a
{-# INLINE unsafeTake #-}
unsafeTake = G.unsafeTake

unsafeDrop :: Unbox a => Int -> Vector a -> Vector a
{-# INLINE unsafeDrop #-}
unsafeDrop = G.unsafeDrop

-- Permutations
-- ------------

unsafeAccum :: Unbox a => (a -> b -> a) -> Vector a -> [(Int,b)] -> Vector a
{-# INLINE unsafeAccum #-}
unsafeAccum = G.unsafeAccum

unsafeAccumulate :: (Unbox a, Unbox b)
                => (a -> b -> a) -> Vector a -> Vector (Int,b) -> Vector a
{-# INLINE unsafeAccumulate #-}
unsafeAccumulate = G.unsafeAccumulate

unsafeAccumulate_ :: (Unbox a, Unbox b) =>
               (a -> b -> a) -> Vector a -> Vector Int -> Vector b -> Vector a
{-# INLINE unsafeAccumulate_ #-}
unsafeAccumulate_ = G.unsafeAccumulate_

accum :: Unbox a => (a -> b -> a) -> Vector a -> [(Int,b)] -> Vector a
{-# INLINE accum #-}
accum = G.accum

accumulate :: (Unbox a, Unbox b)
                => (a -> b -> a) -> Vector a -> Vector (Int,b) -> Vector a
{-# INLINE accumulate #-}
accumulate = G.accumulate

accumulate_ :: (Unbox a, Unbox b) =>
               (a -> b -> a) -> Vector a -> Vector Int -> Vector b -> Vector a
{-# INLINE accumulate_ #-}
accumulate_ = G.accumulate_

unsafeUpd :: Unbox a => Vector a -> [(Int, a)] -> Vector a
{-# INLINE unsafeUpd #-}
unsafeUpd = G.unsafeUpd

unsafeUpdate :: Unbox a => Vector a -> Vector (Int, a) -> Vector a
{-# INLINE unsafeUpdate #-}
unsafeUpdate = G.unsafeUpdate

unsafeUpdate_ :: Unbox a => Vector a -> Vector Int -> Vector a -> Vector a
{-# INLINE unsafeUpdate_ #-}
unsafeUpdate_ = G.unsafeUpdate_

(//) :: Unbox a => Vector a -> [(Int, a)] -> Vector a
{-# INLINE (//) #-}
(//) = (G.//)

update :: Unbox a => Vector a -> Vector (Int, a) -> Vector a
{-# INLINE update #-}
update = G.update

update_ :: Unbox a => Vector a -> Vector Int -> Vector a -> Vector a
{-# INLINE update_ #-}
update_ = G.update_

backpermute :: Unbox a => Vector a -> Vector Int -> Vector a
{-# INLINE backpermute #-}
backpermute = G.backpermute

unsafeBackpermute :: Unbox a => Vector a -> Vector Int -> Vector a
{-# INLINE unsafeBackpermute #-}
unsafeBackpermute = G.unsafeBackpermute

reverse :: Unbox a => Vector a -> Vector a
{-# INLINE reverse #-}
reverse = G.reverse

-- Mapping
-- -------

-- | Map a function over a vector
map :: (Unbox a, Unbox b) => (a -> b) -> Vector a -> Vector b
{-# INLINE map #-}
map = G.map

-- | Apply a function to every index/value pair
imap :: (Unbox a, Unbox b) => (Int -> a -> b) -> Vector a -> Vector b
{-# INLINE imap #-}
imap = G.imap

concatMap :: (Unbox a, Unbox b) => (a -> Vector b) -> Vector a -> Vector b
{-# INLINE concatMap #-}
concatMap = G.concatMap

-- Zipping/unzipping
-- -----------------

-- | Zip two vectors with the given function.
zipWith :: (Unbox a, Unbox b, Unbox c)
        => (a -> b -> c) -> Vector a -> Vector b -> Vector c
{-# INLINE zipWith #-}
zipWith = G.zipWith

-- | Zip three vectors with the given function.
zipWith3 :: (Unbox a, Unbox b, Unbox c, Unbox d)
         => (a -> b -> c -> d) -> Vector a -> Vector b -> Vector c -> Vector d
{-# INLINE zipWith3 #-}
zipWith3 = G.zipWith3

zipWith4 :: (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e)
         => (a -> b -> c -> d -> e)
         -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
{-# INLINE zipWith4 #-}
zipWith4 = G.zipWith4

zipWith5 :: (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e, Unbox f)
         => (a -> b -> c -> d -> e -> f)
         -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
         -> Vector f
{-# INLINE zipWith5 #-}
zipWith5 = G.zipWith5

zipWith6 :: (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e, Unbox f, Unbox g)
         => (a -> b -> c -> d -> e -> f -> g)
         -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
         -> Vector f -> Vector g
{-# INLINE zipWith6 #-}
zipWith6 = G.zipWith6

-- | Zip two vectors and their indices with the given function.
izipWith :: (Unbox a, Unbox b, Unbox c)
         => (Int -> a -> b -> c) -> Vector a -> Vector b -> Vector c
{-# INLINE izipWith #-}
izipWith = G.izipWith

-- | Zip three vectors and their indices with the given function.
izipWith3 :: (Unbox a, Unbox b, Unbox c, Unbox d)
          => (Int -> a -> b -> c -> d)
          -> Vector a -> Vector b -> Vector c -> Vector d
{-# INLINE izipWith3 #-}
izipWith3 = G.izipWith3

izipWith4 :: (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e)
          => (Int -> a -> b -> c -> d -> e)
          -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
{-# INLINE izipWith4 #-}
izipWith4 = G.izipWith4

izipWith5 :: (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e, Unbox f)
          => (Int -> a -> b -> c -> d -> e -> f)
          -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
          -> Vector f
{-# INLINE izipWith5 #-}
izipWith5 = G.izipWith5

izipWith6 :: (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e, Unbox f, Unbox g)
          => (Int -> a -> b -> c -> d -> e -> f -> g)
          -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
          -> Vector f -> Vector g
{-# INLINE izipWith6 #-}
izipWith6 = G.izipWith6

-- Filtering
-- ---------

-- | Drop elements which do not satisfy the predicate
filter :: Unbox a => (a -> Bool) -> Vector a -> Vector a
{-# INLINE filter #-}
filter = G.filter

-- | Drop elements that do not satisfy the predicate (applied to values and
-- their indices)
ifilter :: Unbox a => (Int -> a -> Bool) -> Vector a -> Vector a
{-# INLINE ifilter #-}
ifilter = G.ifilter

-- | Yield the longest prefix of elements satisfying the predicate.
takeWhile :: Unbox a => (a -> Bool) -> Vector a -> Vector a
{-# INLINE takeWhile #-}
takeWhile = G.takeWhile

-- | Drop the longest prefix of elements that satisfy the predicate.
dropWhile :: Unbox a => (a -> Bool) -> Vector a -> Vector a
{-# INLINE dropWhile #-}
dropWhile = G.dropWhile

-- | Split the vector in two parts, the first one containing those elements
-- that satisfy the predicate and the second one those that don't. The
-- relative order of the elements is preserved at the cost of a (sometimes)
-- reduced performance compared to 'unstablePartition'.
partition :: Unbox a => (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE partition #-}
partition = G.partition

-- | Split the vector in two parts, the first one containing those elements
-- that satisfy the predicate and the second one those that don't. The order
-- of the elements is not preserved.
unstablePartition :: Unbox a => (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE unstablePartition #-}
unstablePartition = G.unstablePartition

-- | Split the vector into the longest prefix of elements that satisfy the
-- predicate and the rest.
span :: Unbox a => (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE span #-}
span = G.span

-- | Split the vector into the longest prefix of elements that do not satisfy
-- the predicate and the rest.
break :: Unbox a => (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE break #-}
break = G.break

-- Searching
-- ---------

infix 4 `elem`
-- | Check whether the vector contains an element
elem :: (Unbox a, Eq a) => a -> Vector a -> Bool
{-# INLINE elem #-}
elem = G.elem

infix 4 `notElem`
-- | Inverse of `elem`
notElem :: (Unbox a, Eq a) => a -> Vector a -> Bool
{-# INLINE notElem #-}
notElem = G.notElem

-- | Yield 'Just' the first element matching the predicate or 'Nothing' if no
-- such element exists.
find :: Unbox a => (a -> Bool) -> Vector a -> Maybe a
{-# INLINE find #-}
find = G.find

-- | Yield 'Just' the index of the first element matching the predicate or
-- 'Nothing' if no such element exists.
findIndex :: Unbox a => (a -> Bool) -> Vector a -> Maybe Int
{-# INLINE findIndex #-}
findIndex = G.findIndex

-- | Yield the indices of elements satisfying the predicate
findIndices :: Unbox a => (a -> Bool) -> Vector a -> Vector Int
{-# INLINE findIndices #-}
findIndices = G.findIndices

-- | Yield 'Just' the index of the first occurence of the given element or
-- 'Nothing' if the vector does not contain the element
elemIndex :: (Unbox a, Eq a) => a -> Vector a -> Maybe Int
{-# INLINE elemIndex #-}
elemIndex = G.elemIndex

-- | Yield the indices of all occurences of the given element
elemIndices :: (Unbox a, Eq a) => a -> Vector a -> Vector Int
{-# INLINE elemIndices #-}
elemIndices = G.elemIndices

-- Folding
-- -------

-- | Left fold
foldl :: Unbox b => (a -> b -> a) -> a -> Vector b -> a
{-# INLINE foldl #-}
foldl = G.foldl

-- | Lefgt fold on non-empty vectors
foldl1 :: Unbox a => (a -> a -> a) -> Vector a -> a
{-# INLINE foldl1 #-}
foldl1 = G.foldl1

-- | Left fold with strict accumulator
foldl' :: Unbox b => (a -> b -> a) -> a -> Vector b -> a
{-# INLINE foldl' #-}
foldl' = G.foldl'

-- | Left fold on non-empty vectors with strict accumulator
foldl1' :: Unbox a => (a -> a -> a) -> Vector a -> a
{-# INLINE foldl1' #-}
foldl1' = G.foldl1'

-- | Right fold
foldr :: Unbox a => (a -> b -> b) -> b -> Vector a -> b
{-# INLINE foldr #-}
foldr = G.foldr

-- | Right fold on non-empty vectors
foldr1 :: Unbox a => (a -> a -> a) -> Vector a -> a
{-# INLINE foldr1 #-}
foldr1 = G.foldr1

-- | Right fold with a strict accumulator
foldr' :: Unbox a => (a -> b -> b) -> b -> Vector a -> b
{-# INLINE foldr' #-}
foldr' = G.foldr'

-- | Right fold on non-empty vectors with strict accumulator
foldr1' :: Unbox a => (a -> a -> a) -> Vector a -> a
{-# INLINE foldr1' #-}
foldr1' = G.foldr1'

-- | Left fold (function applied to each element and its index)
ifoldl :: Unbox b => (a -> Int -> b -> a) -> a -> Vector b -> a
{-# INLINE ifoldl #-}
ifoldl = G.ifoldl

-- | Left fold with strict accumulator (function applied to each element and
-- its index)
ifoldl' :: Unbox b => (a -> Int -> b -> a) -> a -> Vector b -> a
{-# INLINE ifoldl' #-}
ifoldl' = G.ifoldl'

-- | Right fold (function applied to each element and its index)
ifoldr :: Unbox a => (Int -> a -> b -> b) -> b -> Vector a -> b
{-# INLINE ifoldr #-}
ifoldr = G.ifoldr

-- | Right fold with strict accumulator (function applied to each element and
-- its index)
ifoldr' :: Unbox a => (Int -> a -> b -> b) -> b -> Vector a -> b
{-# INLINE ifoldr' #-}
ifoldr' = G.ifoldr'

-- Specialised folds
-- -----------------

all :: Unbox a => (a -> Bool) -> Vector a -> Bool
{-# INLINE all #-}
all = G.all

any :: Unbox a => (a -> Bool) -> Vector a -> Bool
{-# INLINE any #-}
any = G.any

and :: Vector Bool -> Bool
{-# INLINE and #-}
and = G.and

or :: Vector Bool -> Bool
{-# INLINE or #-}
or = G.or

sum :: (Unbox a, Num a) => Vector a -> a
{-# INLINE sum #-}
sum = G.sum

product :: (Unbox a, Num a) => Vector a -> a
{-# INLINE product #-}
product = G.product

maximum :: (Unbox a, Ord a) => Vector a -> a
{-# INLINE maximum #-}
maximum = G.maximum

maximumBy :: Unbox a => (a -> a -> Ordering) -> Vector a -> a
{-# INLINE maximumBy #-}
maximumBy = G.maximumBy

minimum :: (Unbox a, Ord a) => Vector a -> a
{-# INLINE minimum #-}
minimum = G.minimum

minimumBy :: Unbox a => (a -> a -> Ordering) -> Vector a -> a
{-# INLINE minimumBy #-}
minimumBy = G.minimumBy

maxIndex :: (Unbox a, Ord a) => Vector a -> Int
{-# INLINE maxIndex #-}
maxIndex = G.maxIndex

maxIndexBy :: Unbox a => (a -> a -> Ordering) -> Vector a -> Int
{-# INLINE maxIndexBy #-}
maxIndexBy = G.maxIndexBy

minIndex :: (Unbox a, Ord a) => Vector a -> Int
{-# INLINE minIndex #-}
minIndex = G.minIndex

minIndexBy :: Unbox a => (a -> a -> Ordering) -> Vector a -> Int
{-# INLINE minIndexBy #-}
minIndexBy = G.minIndexBy

-- Unfolding
-- ---------

-- | The 'unfoldr' function is a \`dual\' to 'foldr': while 'foldr'
-- reduces a vector to a summary value, 'unfoldr' builds a list from
-- a seed value.  The function takes the element and returns 'Nothing'
-- if it is done generating the vector or returns 'Just' @(a,b)@, in which
-- case, @a@ is a prepended to the vector and @b@ is used as the next
-- element in a recursive call.
--
-- A simple use of unfoldr:
--
-- > unfoldr (\b -> if b == 0 then Nothing else Just (b, b-1)) 10
-- >  [10,9,8,7,6,5,4,3,2,1]
--
unfoldr :: Unbox a => (b -> Maybe (a, b)) -> b -> Vector a
{-# INLINE unfoldr #-}
unfoldr = G.unfoldr

-- | Unfold at most @n@ elements
unfoldrN :: Unbox a => Int -> (b -> Maybe (a, b)) -> b -> Vector a
{-# INLINE unfoldrN #-}
unfoldrN = G.unfoldrN

-- Scans
-- -----

-- | Prefix scan
prescanl :: (Unbox a, Unbox b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE prescanl #-}
prescanl = G.prescanl

-- | Prefix scan with strict accumulator
prescanl' :: (Unbox a, Unbox b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE prescanl' #-}
prescanl' = G.prescanl'

-- | Suffix scan
postscanl :: (Unbox a, Unbox b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE postscanl #-}
postscanl = G.postscanl

-- | Suffix scan with strict accumulator
postscanl' :: (Unbox a, Unbox b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE postscanl' #-}
postscanl' = G.postscanl'

-- | Haskell-style scan
scanl :: (Unbox a, Unbox b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE scanl #-}
scanl = G.scanl

-- | Haskell-style scan with strict accumulator
scanl' :: (Unbox a, Unbox b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE scanl' #-}
scanl' = G.scanl'

-- | Scan over a non-empty 'Vector'
scanl1 :: Unbox a => (a -> a -> a) -> Vector a -> Vector a
{-# INLINE scanl1 #-}
scanl1 = G.scanl1

-- | Scan over a non-empty 'Vector' with a strict accumulator
scanl1' :: Unbox a => (a -> a -> a) -> Vector a -> Vector a
{-# INLINE scanl1' #-}
scanl1' = G.scanl1'


-- | Prefix right-to-left scan
prescanr :: (Unbox a, Unbox b) => (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE prescanr #-}
prescanr = G.prescanr

-- | Prefix right-to-left scan with strict accumulator
prescanr' :: (Unbox a, Unbox b) => (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE prescanr' #-}
prescanr' = G.prescanr'

-- | Suffix right-to-left scan
postscanr :: (Unbox a, Unbox b) => (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE postscanr #-}
postscanr = G.postscanr

-- | Suffix right-to-left scan with strict accumulator
postscanr' :: (Unbox a, Unbox b) => (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE postscanr' #-}
postscanr' = G.postscanr'

-- | Haskell-style right-to-left scan
scanr :: (Unbox a, Unbox b) => (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE scanr #-}
scanr = G.scanr

-- | Haskell-style right-to-left scan with strict accumulator
scanr' :: (Unbox a, Unbox b) => (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE scanr' #-}
scanr' = G.scanr'

-- | Right-to-left scan over a non-empty vector
scanr1 :: Unbox a => (a -> a -> a) -> Vector a -> Vector a
{-# INLINE scanr1 #-}
scanr1 = G.scanr1

-- | Right-to-left scan over a non-empty vector with a strict accumulator
scanr1' :: Unbox a => (a -> a -> a) -> Vector a -> Vector a
{-# INLINE scanr1' #-}
scanr1' = G.scanr1'

-- Enumeration
-- -----------

-- | Yield a vector of the given length containing the values @x@, @x+1@ etc.
-- This operation is usually more efficient than 'enumFromTo'.
enumFromN :: (Unbox a, Num a) => a -> Int -> Vector a
{-# INLINE enumFromN #-}
enumFromN = G.enumFromN

-- | Yield a vector of the given length containing the values @x@, @x+y@,
-- @x+y+y@ etc. This operations is usually more efficient than
-- 'enumFromThenTo'.
enumFromStepN :: (Unbox a, Num a) => a -> a -> Int -> Vector a
{-# INLINE enumFromStepN #-}
enumFromStepN = G.enumFromStepN

-- | Enumerate values from @x@ to @y@.
--
-- /WARNING:/ This operation can be very inefficient. If at all possible, use
-- 'enumFromN' instead.
enumFromTo :: (Unbox a, Enum a) => a -> a -> Vector a
{-# INLINE enumFromTo #-}
enumFromTo = G.enumFromTo

-- | Enumerate values from @x@ to @y@ with a specific step @z@.
--
-- /WARNING:/ This operation can be very inefficient. If at all possible, use
-- 'enumFromStepN' instead.
enumFromThenTo :: (Unbox a, Enum a) => a -> a -> a -> Vector a
{-# INLINE enumFromThenTo #-}
enumFromThenTo = G.enumFromThenTo

-- Conversion to/from lists
-- ------------------------

-- | Convert a vector to a list
toList :: Unbox a => Vector a -> [a]
{-# INLINE toList #-}
toList = G.toList

-- | Convert a list to a vector
fromList :: Unbox a => [a] -> Vector a
{-# INLINE fromList #-}
fromList = G.fromList

-- | Convert the first @n@ elements of a list to a vector
--
-- > fromListN n xs = fromList (take n xs)
fromListN :: Unbox a => Int -> [a] -> Vector a
{-# INLINE fromListN #-}
fromListN = G.fromListN

-- Monadic operations
-- ------------------

-- | Perform the monadic action the given number of times and store the
-- results in a vector.
replicateM :: (Monad m, Unbox a) => Int -> m a -> m (Vector a)
{-# INLINE replicateM #-}
replicateM = G.replicateM

-- | Apply the monadic action to all elements of the vector, yielding a vector
-- of results
mapM :: (Monad m, Unbox a, Unbox b) => (a -> m b) -> Vector a -> m (Vector b)
{-# INLINE mapM #-}
mapM = G.mapM

-- | Apply the monadic action to all elements of a vector and ignore the
-- results
mapM_ :: (Monad m, Unbox a) => (a -> m b) -> Vector a -> m ()
{-# INLINE mapM_ #-}
mapM_ = G.mapM_

-- | Apply the monadic action to all elements of the vector, yielding a vector
-- of results
forM :: (Monad m, Unbox a, Unbox b) => Vector a -> (a -> m b) -> m (Vector b)
{-# INLINE forM #-}
forM = G.forM

-- | Apply the monadic action to all elements of a vector and ignore the
-- results
forM_ :: (Monad m, Unbox a) => Vector a -> (a -> m b) -> m ()
{-# INLINE forM_ #-}
forM_ = G.forM_

-- | Zip the two vectors with the monadic action and yield a vector of results
zipWithM :: (Monad m, Unbox a, Unbox b, Unbox c)
         => (a -> b -> m c) -> Vector a -> Vector b -> m (Vector c)
{-# INLINE zipWithM #-}
zipWithM = G.zipWithM

-- | Zip the two vectors with the monadic action and ignore the results
zipWithM_ :: (Monad m, Unbox a, Unbox b)
          => (a -> b -> m c) -> Vector a -> Vector b -> m ()
{-# INLINE zipWithM_ #-}
zipWithM_ = G.zipWithM_

-- | Drop elements that do not satisfy the monadic predicate
filterM :: (Monad m, Unbox a) => (a -> m Bool) -> Vector a -> m (Vector a)
{-# INLINE filterM #-}
filterM = G.filterM

-- | Monadic fold
foldM :: (Monad m, Unbox b) => (a -> b -> m a) -> a -> Vector b -> m a
{-# INLINE foldM #-}
foldM = G.foldM

-- | Monadic fold over non-empty vectors
fold1M :: (Monad m, Unbox a) => (a -> a -> m a) -> Vector a -> m a
{-# INLINE fold1M #-}
fold1M = G.fold1M

-- | Monadic fold with strict accumulator
foldM' :: (Monad m, Unbox b) => (a -> b -> m a) -> a -> Vector b -> m a
{-# INLINE foldM' #-}
foldM' = G.foldM'

-- | Monad fold over non-empty vectors with strict accumulator
fold1M' :: (Monad m, Unbox a) => (a -> a -> m a) -> Vector a -> m a
{-# INLINE fold1M' #-}
fold1M' = G.fold1M'

-- Destructive operations
-- ----------------------

-- | Destructively initialise a vector.
create :: Unbox a => (forall s. ST s (MVector s a)) -> Vector a
{-# INLINE create #-}
create = G.create

-- | Apply a destructive operation to a vector. The operation is applied to a
-- copy of the vector unless it can be safely performed in place.
modify :: Unbox a => (forall s. MVector s a -> ST s ()) -> Vector a -> Vector a
{-# INLINE modify #-}
modify = G.modify

-- | Copy an immutable vector into a mutable one. The two vectors must have
-- the same length. This is not checked.
unsafeCopy
  :: (Unbox a, PrimMonad m) => MVector (PrimState m) a -> Vector a -> m ()
{-# INLINE unsafeCopy #-}
unsafeCopy = G.unsafeCopy
           
-- | Copy an immutable vector into a mutable one. The two vectors must have the
-- same length.
copy :: (Unbox a, PrimMonad m) => MVector (PrimState m) a -> Vector a -> m ()
{-# INLINE copy #-}
copy = G.copy


#define DEFINE_IMMUTABLE
#include "unbox-tuple-instances"

