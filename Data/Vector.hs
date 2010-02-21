{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

-- |
-- Module      : Data.Vector
-- Copyright   : (c) Roman Leshchinskiy 2008-2009
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
-- 
-- A library for boxed vectors (that is, polymorphic arrays capable of
-- holding any Haskell value). The vectors come in two flavors:
--
--  * mutable
--
--  * immutable
--
-- and support a rich interface of both list-like operations, and bulk
-- array operations.
--
-- For unboxed arrays, use the 'Data.Vector.Unboxed' interface.
--

module Data.Vector (

  -- * The pure and mutable array types
  Vector, MVector,

  -- * Constructing vectors
  empty,
  singleton,
  cons,
  snoc,
  (++),
  replicate,
  generate,
  copy,

  -- * Operations based on length information
  length,
  null,

  -- * Accessing individual elements
  (!),
  head,
  last,

  -- ** Accessors in a monad
  indexM,
  headM,
  lastM,

  -- ** Accessor functions with no bounds checking
  unsafeIndex, unsafeHead, unsafeLast,
  unsafeIndexM, unsafeHeadM, unsafeLastM,

  -- * Subvectors
  init,
  tail,
  take,
  drop,
  slice,

  -- * Subvector construction without bounds checks
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
  toList, fromList
) where

import qualified Data.Vector.Generic as G
import           Data.Vector.Mutable  ( MVector(..) )
import           Data.Primitive.Array
import qualified Data.Vector.Fusion.Stream as Stream

import Control.Monad ( liftM )

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
                        enumFromTo, enumFromThenTo )

import qualified Prelude

-- | Boxed vectors, supporting efficient slicing.
data Vector a = Vector {-# UNPACK #-} !Int
                       {-# UNPACK #-} !Int
                       {-# UNPACK #-} !(Array a)

instance Show a => Show (Vector a) where
    show = (Prelude.++ " :: Data.Vector.Vector") . ("fromList " Prelude.++) . show . toList

type instance G.Mutable Vector = MVector

instance G.Vector Vector a where
  {-# INLINE unsafeFreeze #-}
  unsafeFreeze (MVector i n marr)
    = Vector i n `liftM` unsafeFreezeArray marr

  {-# INLINE basicLength #-}
  basicLength (Vector _ n _) = n

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice j n (Vector i _ arr) = Vector (i+j) n arr

  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (Vector i _ arr) j = indexArrayM arr (i+j)

-- See [HACKS:Eq and Ord instances]
instance Eq a => Eq (Vector a) where
  {-# INLINE (==) #-}
  xs == ys = Stream.eq (G.stream xs) (G.stream ys)

  {-# INLINE (/=) #-}
  xs /= ys = not (Stream.eq (G.stream xs) (G.stream ys))

-- See [HACKS:Eq and Ord instances]
instance Ord a => Ord (Vector a) where
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

-- Length
-- ------

-- |/O(1)/. Yield the length of a vector as an 'Int'
length :: Vector a -> Int
{-# INLINE length #-}
length = G.length

-- |/O(1)/. 'null' tests whether the given array is empty.
null :: Vector a -> Bool
{-# INLINE null #-}
null = G.null

-- Construction
-- ------------

-- |/O(1)/. 'empty' builds a vector of size zero.
empty :: Vector a
{-# INLINE empty #-}
empty = G.empty

-- |/O(1)/, Vector with exactly one element
singleton :: a -> Vector a
{-# INLINE singleton #-}
singleton = G.singleton

-- |/O(n)/. @'replicate' n e@ yields a vector of length @n@ storing @e@ at each position
replicate :: Int -> a -> Vector a
{-# INLINE replicate #-}
replicate = G.replicate

-- |/O(n)/, Generate a vector of the given length by applying a (pure)
-- generator function to each index
generate :: Int -> (Int -> a) -> Vector a
{-# INLINE generate #-}
generate = G.generate

-- |/O(n)/, Prepend an element to an array.
cons :: a -> Vector a -> Vector a
{-# INLINE cons #-}
cons = G.cons

-- |/O(n)/, Append an element to an array.
snoc :: Vector a -> a -> Vector a
{-# INLINE snoc #-}
snoc = G.snoc

infixr 5 ++

-- |/O(n)/, Concatenate two vectors
(++) :: Vector a -> Vector a -> Vector a
{-# INLINE (++) #-}
(++) = (G.++)

-- |/O(n)/, Create a copy of a vector.
-- @copy@ is useful when dealing with slices, as the garbage collector
-- may be able to free the original vector if no further references are held.
--
copy :: Vector a -> Vector a
{-# INLINE copy #-}
copy = G.copy

-- Accessing individual elements
-- -----------------------------

-- |/O(1)/. Read the element in the vector at the given index.
(!) :: Vector a -> Int -> a
{-# INLINE (!) #-}
(!) = (G.!)

-- |/O(1)/. 'head' returns the first element of the vector
head :: Vector a -> a
{-# INLINE head #-}
head = G.head

-- |/O(n)/. 'last' yields the last element of an array.
last :: Vector a -> a
{-# INLINE last #-}
last = G.last

-- |/O(1)/, Unsafe indexing without bounds checking
--
-- By not performing bounds checks, this function may be faster when
-- this function is used in an inner loop)
--
unsafeIndex :: Vector a -> Int -> a
{-# INLINE unsafeIndex #-}
unsafeIndex = G.unsafeIndex

-- |/O(1)/, Yield the first element of a vector without checking if the vector is empty
--
-- By not performing bounds checks, this function may be faster when
-- this function is used in an inner loop)
unsafeHead :: Vector a -> a
{-# INLINE unsafeHead #-}
unsafeHead = G.unsafeHead

-- | Yield the last element of a vector without checking if the vector is empty
--
-- By not performing bounds checks, this function may be faster when
-- this function is used in an inner loop)
unsafeLast :: Vector a -> a
{-# INLINE unsafeLast #-}
unsafeLast = G.unsafeLast

-- | Monadic indexing which can be strict in the vector while remaining lazy in the element
indexM :: Monad m => Vector a -> Int -> m a
{-# INLINE indexM #-}
indexM = G.indexM

-- | Monadic head which can be strict in the vector while remaining lazy in the element
headM :: Monad m => Vector a -> m a
{-# INLINE headM #-}
headM = G.headM

-- | Monadic last which can be strict in the vector while remaining lazy in the element
lastM :: Monad m => Vector a -> m a
{-# INLINE lastM #-}
lastM = G.lastM

-- | Unsafe monadic indexing without bounds checks
unsafeIndexM :: Monad m => Vector a -> Int -> m a
{-# INLINE unsafeIndexM #-}
unsafeIndexM = G.unsafeIndexM

-- | Unsafe monadic head (access the first element) without bounds checks
unsafeHeadM :: Monad m => Vector a -> m a
{-# INLINE unsafeHeadM #-}
unsafeHeadM = G.unsafeHeadM

-- | Unsafe monadic last (access the last element) without bounds checks
unsafeLastM :: Monad m => Vector a -> m a
{-# INLINE unsafeLastM #-}
unsafeLastM = G.unsafeLastM

-- Subarrays
-- ---------

-- | /O(1)/, Yield a part of the vector without copying it.
--
slice :: Int   -- ^ starting index
      -> Int   -- ^ length
      -> Vector a
      -> Vector a
{-# INLINE slice #-}
slice = G.slice

-- |/O(1)/, Yield all but the last element without copying.
init :: Vector a -> Vector a
{-# INLINE init #-}
init = G.init

-- |/O(1), Yield all but the first element (without copying).
tail :: Vector a -> Vector a
{-# INLINE tail #-}
tail = G.tail

-- |/O(1)/, Yield the first @n@ elements without copying.
take :: Int -> Vector a -> Vector a
{-# INLINE take #-}
take = G.take

-- |/O(1)/, Yield all but the first @n@ elements without copying.
drop :: Int -> Vector a -> Vector a
{-# INLINE drop #-}
drop = G.drop

-- |/O(1)/, Unsafely yield a part of the vector without copying it and without
-- performing bounds checks.
unsafeSlice :: Int   -- ^ starting index
            -> Int   -- ^ length
            -> Vector a
            -> Vector a
{-# INLINE unsafeSlice #-}
unsafeSlice = G.unsafeSlice

-- |/O(1)/, Zero-copying 'init' without bounds checks.
unsafeInit :: Vector a -> Vector a
{-# INLINE unsafeInit #-}
unsafeInit = G.unsafeInit

-- |/O(1)/, Zero-copying 'tail' without bounds checks.
unsafeTail :: Vector a -> Vector a
{-# INLINE unsafeTail #-}
unsafeTail = G.unsafeTail

-- |/O(1)/, Zero-copying 'take' without bounds checks.
unsafeTake :: Int -> Vector a -> Vector a
{-# INLINE unsafeTake #-}
unsafeTake = G.unsafeTake

-- |/O(1)/, Zero-copying 'drop' without bounds checks.
unsafeDrop :: Int -> Vector a -> Vector a
{-# INLINE unsafeDrop #-}
unsafeDrop = G.unsafeDrop

-- Permutations
-- ------------

-- TODO there is no documentation for the accum* family of functions

-- | TODO unsafeAccum.
unsafeAccum :: (a -> b -> a) -> Vector a -> [(Int,b)] -> Vector a
{-# INLINE unsafeAccum #-}
unsafeAccum = G.unsafeAccum

-- | TODO unsafeAccumulate
unsafeAccumulate :: (a -> b -> a) -> Vector a -> Vector (Int,b) -> Vector a
{-# INLINE unsafeAccumulate #-}
unsafeAccumulate = G.unsafeAccumulate

-- | TODO unsafeAccumulate_
unsafeAccumulate_
  :: (a -> b -> a) -> Vector a -> Vector Int -> Vector b -> Vector a
{-# INLINE unsafeAccumulate_ #-}
unsafeAccumulate_ = G.unsafeAccumulate_

-- | TODO accum
accum :: (a -> b -> a) -> Vector a -> [(Int,b)] -> Vector a
{-# INLINE accum #-}
accum = G.accum

-- | TODO accumulate
accumulate :: (a -> b -> a) -> Vector a -> Vector (Int,b) -> Vector a
{-# INLINE accumulate #-}
accumulate = G.accumulate

-- | TODO accumulate_
accumulate_ :: (a -> b -> a) -> Vector a -> Vector Int -> Vector b -> Vector a
{-# INLINE accumulate_ #-}
accumulate_ = G.accumulate_

-- | TODO unsafeUpd
unsafeUpd :: Vector a -> [(Int, a)] -> Vector a
{-# INLINE unsafeUpd #-}
unsafeUpd = G.unsafeUpd

-- | TODO unsafeUpdate
unsafeUpdate :: Vector a -> Vector (Int, a) -> Vector a
{-# INLINE unsafeUpdate #-}
unsafeUpdate = G.unsafeUpdate

-- | TODO unsafeUpdate_
unsafeUpdate_ :: Vector a -> Vector Int -> Vector a -> Vector a
{-# INLINE unsafeUpdate_ #-}
unsafeUpdate_ = G.unsafeUpdate_

-- | TODO (//)
(//) :: Vector a -> [(Int, a)] -> Vector a
{-# INLINE (//) #-}
(//) = (G.//)

-- | TODO update
update :: Vector a -> Vector (Int, a) -> Vector a
{-# INLINE update #-}
update = G.update

-- | TODO update_
update_ :: Vector a -> Vector Int -> Vector a -> Vector a
{-# INLINE update_ #-}
update_ = G.update_

-- | backpermute, courtesy Blelloch. The back-permute is a gather\/get operation.
backpermute :: Vector a -> Vector Int -> Vector a
{-# INLINE backpermute #-}
backpermute = G.backpermute

-- | TODO unsafeBackpermute
unsafeBackpermute :: Vector a -> Vector Int -> Vector a
{-# INLINE unsafeBackpermute #-}
unsafeBackpermute = G.unsafeBackpermute

-- | /O(n)/, reverse the elements of the given vector.
reverse :: Vector a -> Vector a
{-# INLINE reverse #-}
reverse = G.reverse

-- Mapping
-- -------

-- | /O(n)/, Map a function over a vector
map :: (a -> b) -> Vector a -> Vector b
{-# INLINE map #-}
map = G.map

-- | /O(n)/, Apply a function to every index/value pair yielding a new vector
imap :: (Int -> a -> b) -> Vector a -> Vector b
{-# INLINE imap #-}
imap = G.imap

-- | /O(n)/, generate a vector from each element of the input vector, then join the results.
concatMap :: (a -> Vector b) -> Vector a -> Vector b
{-# INLINE concatMap #-}
concatMap = G.concatMap

-- Zipping/unzipping
-- -----------------

-- |/O(n)/, Zip two vectors with the given function.
zipWith :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
{-# INLINE zipWith #-}
zipWith = G.zipWith

-- |/O(n)/, Zip three vectors with the given function.
zipWith3 :: (a -> b -> c -> d) -> Vector a -> Vector b -> Vector c -> Vector d
{-# INLINE zipWith3 #-}
zipWith3 = G.zipWith3

-- |/O(n)/, Zip four vectors with the given function.
zipWith4 :: (a -> b -> c -> d -> e)
          -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
{-# INLINE zipWith4 #-}
zipWith4 = G.zipWith4

-- |/O(n)/, Zip five vectors with the given function.
zipWith5 :: (a -> b -> c -> d -> e -> f)
          -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
          -> Vector f
{-# INLINE zipWith5 #-}
zipWith5 = G.zipWith5

-- |/O(n)/, Zip six vectors with the given function.
zipWith6 :: (a -> b -> c -> d -> e -> f -> g)
          -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
          -> Vector f -> Vector g
{-# INLINE zipWith6 #-}
zipWith6 = G.zipWith6

-- |/O(n)/, Zip two vectors and their indices with the given function.
izipWith :: (Int -> a -> b -> c) -> Vector a -> Vector b -> Vector c
{-# INLINE izipWith #-}
izipWith = G.izipWith

-- |/O(n)/, Zip three vectors and their indices with the given function.
izipWith3 :: (Int -> a -> b -> c -> d)
          -> Vector a -> Vector b -> Vector c -> Vector d
{-# INLINE izipWith3 #-}
izipWith3 = G.izipWith3

-- |/O(n)/, Zip four vectors and their indices with the given function.
izipWith4 :: (Int -> a -> b -> c -> d -> e)
          -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
{-# INLINE izipWith4 #-}
izipWith4 = G.izipWith4

-- |/O(n)/, Zip five vectors and their indices with the given function.
izipWith5 :: (Int -> a -> b -> c -> d -> e -> f)
          -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
          -> Vector f
{-# INLINE izipWith5 #-}
izipWith5 = G.izipWith5

-- |/O(n)/, Zip six vectors and their indices with the given function.
izipWith6 :: (Int -> a -> b -> c -> d -> e -> f -> g)
          -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
          -> Vector f -> Vector g
{-# INLINE izipWith6 #-}
izipWith6 = G.izipWith6

-- | Elementwise pairing of array elements. 
zip :: Vector a -> Vector b -> Vector (a, b)
{-# INLINE zip #-}
zip = G.zip

-- | zip together three vectors into a vector of triples
zip3 :: Vector a -> Vector b -> Vector c -> Vector (a, b, c)
{-# INLINE zip3 #-}
zip3 = G.zip3

zip4 :: Vector a -> Vector b -> Vector c -> Vector d
     -> Vector (a, b, c, d)
{-# INLINE zip4 #-}
zip4 = G.zip4

zip5 :: Vector a -> Vector b -> Vector c -> Vector d -> Vector e
     -> Vector (a, b, c, d, e)
{-# INLINE zip5 #-}
zip5 = G.zip5

zip6 :: Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector f
     -> Vector (a, b, c, d, e, f)
{-# INLINE zip6 #-}
zip6 = G.zip6

-- | Elementwise unpairing of array elements.
unzip :: Vector (a, b) -> (Vector a, Vector b)
{-# INLINE unzip #-}
unzip = G.unzip

unzip3 :: Vector (a, b, c) -> (Vector a, Vector b, Vector c)
{-# INLINE unzip3 #-}
unzip3 = G.unzip3

unzip4 :: Vector (a, b, c, d) -> (Vector a, Vector b, Vector c, Vector d)
{-# INLINE unzip4 #-}
unzip4 = G.unzip4

unzip5 :: Vector (a, b, c, d, e)
       -> (Vector a, Vector b, Vector c, Vector d, Vector e)
{-# INLINE unzip5 #-}
unzip5 = G.unzip5

unzip6 :: Vector (a, b, c, d, e, f)
       -> (Vector a, Vector b, Vector c, Vector d, Vector e, Vector f)
{-# INLINE unzip6 #-}
unzip6 = G.unzip6

-- Filtering
-- ---------

-- |/O(n)/, Remove elements from the vector which do not satisfy the predicate
filter :: (a -> Bool) -> Vector a -> Vector a
{-# INLINE filter #-}
filter = G.filter

-- |/O(n)/, Drop elements that do not satisfy the predicate (applied to values and
-- their indices)
ifilter :: (Int -> a -> Bool) -> Vector a -> Vector a
{-# INLINE ifilter #-}
ifilter = G.ifilter

-- |/O(n)/, Yield the longest prefix of elements satisfying the predicate.
takeWhile :: (a -> Bool) -> Vector a -> Vector a
{-# INLINE takeWhile #-}
takeWhile = G.takeWhile

-- |/O(n)/, Drop the longest prefix of elements that satisfy the predicate.
dropWhile :: (a -> Bool) -> Vector a -> Vector a
{-# INLINE dropWhile #-}
dropWhile = G.dropWhile

-- | Split the vector in two parts, the first one containing those elements
-- that satisfy the predicate and the second one those that don't. The
-- relative order of the elements is preserved at the cost of a (sometimes)
-- reduced performance compared to 'unstablePartition'.
partition :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE partition #-}
partition = G.partition

-- |/O(n)/, Split the vector in two parts, the first one containing those elements
-- that satisfy the predicate and the second one those that don't. The order
-- of the elements is not preserved.
unstablePartition :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE unstablePartition #-}
unstablePartition = G.unstablePartition

-- |/O(n)/, Split the vector into the longest prefix of elements that satisfy the
-- predicate and the rest.
span :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE span #-}
span = G.span

-- | Split the vector into the longest prefix of elements that do not satisfy
-- the predicate and the rest.
break :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE break #-}
break = G.break

-- Searching
-- ---------

infix 4 `elem`
-- | Check whether the vector contains an element
elem :: Eq a => a -> Vector a -> Bool
{-# INLINE elem #-}
elem = G.elem

infix 4 `notElem`
-- | Inverse of `elem`
notElem :: Eq a => a -> Vector a -> Bool
{-# INLINE notElem #-}
notElem = G.notElem

-- | Yield 'Just' the first element matching the predicate or 'Nothing' if no
-- such element exists.
find :: (a -> Bool) -> Vector a -> Maybe a
{-# INLINE find #-}
find = G.find

-- | Yield 'Just' the index of the first element matching the predicate or
-- 'Nothing' if no such element exists.
findIndex :: (a -> Bool) -> Vector a -> Maybe Int
{-# INLINE findIndex #-}
findIndex = G.findIndex

-- | Yield the indices of elements satisfying the predicate
findIndices :: (a -> Bool) -> Vector a -> Vector Int
{-# INLINE findIndices #-}
findIndices = G.findIndices

-- | Yield 'Just' the index of the first occurence of the given element or
-- 'Nothing' if the vector does not contain the element
elemIndex :: Eq a => a -> Vector a -> Maybe Int
{-# INLINE elemIndex #-}
elemIndex = G.elemIndex

-- | Yield the indices of all occurences of the given element
elemIndices :: Eq a => a -> Vector a -> Vector Int
{-# INLINE elemIndices #-}
elemIndices = G.elemIndices

-- Folding
-- -------

-- | Left fold
foldl :: (a -> b -> a) -> a -> Vector b -> a
{-# INLINE foldl #-}
foldl = G.foldl

-- | Left fold on non-empty vectors
foldl1 :: (a -> a -> a) -> Vector a -> a
{-# INLINE foldl1 #-}
foldl1 = G.foldl1

-- | Left fold with strict accumulator
foldl' :: (a -> b -> a) -> a -> Vector b -> a
{-# INLINE foldl' #-}
foldl' = G.foldl'

-- | Left fold on non-empty vectors with strict accumulator
foldl1' :: (a -> a -> a) -> Vector a -> a
{-# INLINE foldl1' #-}
foldl1' = G.foldl1'

-- | Right fold
foldr :: (a -> b -> b) -> b -> Vector a -> b
{-# INLINE foldr #-}
foldr = G.foldr

-- | Right fold on non-empty vectors
foldr1 :: (a -> a -> a) -> Vector a -> a
{-# INLINE foldr1 #-}
foldr1 = G.foldr1

-- | Right fold with a strict accumulator
foldr' :: (a -> b -> b) -> b -> Vector a -> b
{-# INLINE foldr' #-}
foldr' = G.foldr'

-- | Right fold on non-empty vectors with strict accumulator
foldr1' :: (a -> a -> a) -> Vector a -> a
{-# INLINE foldr1' #-}
foldr1' = G.foldr1'

-- | Left fold (function applied to each element and its index)
ifoldl :: (a -> Int -> b -> a) -> a -> Vector b -> a
{-# INLINE ifoldl #-}
ifoldl = G.ifoldl

-- | Left fold with strict accumulator (function applied to each element and
-- its index)
ifoldl' :: (a -> Int -> b -> a) -> a -> Vector b -> a
{-# INLINE ifoldl' #-}
ifoldl' = G.ifoldl'

-- | Right fold (function applied to each element and its index)
ifoldr :: (Int -> a -> b -> b) -> b -> Vector a -> b
{-# INLINE ifoldr #-}
ifoldr = G.ifoldr

-- | Right fold with strict accumulator (function applied to each element and
-- its index)
ifoldr' :: (Int -> a -> b -> b) -> b -> Vector a -> b
{-# INLINE ifoldr' #-}
ifoldr' = G.ifoldr'

-- Specialised folds
-- -----------------

-- |/O(n)/. @'all' p u@ determines whether all elements in array @u@ satisfy 
-- predicate @p@.
all :: (a -> Bool) -> Vector a -> Bool
{-# INLINE all #-}
all = G.all

-- |/O(n)/. @'any' p u@ determines whether any element in array @u@ satisfies
-- predicate @p@.
any :: (a -> Bool) -> Vector a -> Bool
{-# INLINE any #-}
any = G.any

-- |/O(n)/. 'and' yields the conjunction of a boolean array.
and :: Vector Bool -> Bool
{-# INLINE and #-}
and = G.and

-- |/O(n)/. 'or' yields the disjunction of a boolean array.
or :: Vector Bool -> Bool
{-# INLINE or #-}
or = G.or

-- |/O(n)/. 'sum' computes the sum (with @(+)@) of an array of elements.
sum :: Num a => Vector a -> a
{-# INLINE sum #-}
sum = G.sum

-- |/O(n)/. 'sum' computes the product (with @(*)@) of an array of elements.
product :: Num a => Vector a -> a
{-# INLINE product #-}
product = G.product

-- |/O(n)/. 'maximum' finds the maximum element in an array of orderable elements.
maximum :: Ord a => Vector a -> a
{-# INLINE maximum #-}
maximum = G.maximum

-- |/O(n)/. 'maximumBy' finds the maximum element in an array under the given ordering.
maximumBy :: (a -> a -> Ordering) -> Vector a -> a
{-# INLINE maximumBy #-}
maximumBy = G.maximumBy

-- |/O(n)/. 'minimum' finds the minimum element in an array of orderable elements.
minimum :: Ord a => Vector a -> a
{-# INLINE minimum #-}
minimum = G.minimum

-- |/O(n)/. 'minimumBy' finds the minimum element in an array under the given ordering.
minimumBy :: (a -> a -> Ordering) -> Vector a -> a
{-# INLINE minimumBy #-}
minimumBy = G.minimumBy

-- | TODO maxIndex
maxIndex :: Ord a => Vector a -> Int
{-# INLINE maxIndex #-}
maxIndex = G.maxIndex

-- | TODO maxIndexBy
maxIndexBy :: (a -> a -> Ordering) -> Vector a -> Int
{-# INLINE maxIndexBy #-}
maxIndexBy = G.maxIndexBy

-- | TODO minIndex
minIndex :: Ord a => Vector a -> Int
{-# INLINE minIndex #-}
minIndex = G.minIndex

-- | TODO minIndexBy
minIndexBy :: (a -> a -> Ordering) -> Vector a -> Int
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
unfoldr :: (b -> Maybe (a, b)) -> b -> Vector a
{-# INLINE unfoldr #-}
unfoldr = G.unfoldr

-- | Unfold at most @n@ elements
unfoldrN :: Int -> (b -> Maybe (a, b)) -> b -> Vector a
{-# INLINE unfoldrN #-}
unfoldrN = G.unfoldrN

-- Scans
-- -----

-- | Prefix scan
prescanl :: (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE prescanl #-}
prescanl = G.prescanl

-- | Prefix scan with strict accumulator
prescanl' :: (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE prescanl' #-}
prescanl' = G.prescanl'

-- | Suffix scan
postscanl :: (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE postscanl #-}
postscanl = G.postscanl

-- | Suffix scan with strict accumulator
postscanl' :: (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE postscanl' #-}
postscanl' = G.postscanl'

-- | Haskell-style scan function.
scanl :: (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE scanl #-}
scanl = G.scanl

-- | Haskell-style scan with strict accumulator
scanl' :: (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE scanl' #-}
scanl' = G.scanl'

-- | Scan over a non-empty 'Vector'
scanl1 :: (a -> a -> a) -> Vector a -> Vector a
{-# INLINE scanl1 #-}
scanl1 = G.scanl1

-- | Scan over a non-empty 'Vector' with a strict accumulator
scanl1' :: (a -> a -> a) -> Vector a -> Vector a
{-# INLINE scanl1' #-}
scanl1' = G.scanl1'

-- | Prefix right-to-left scan
prescanr :: (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE prescanr #-}
prescanr = G.prescanr

-- | Prefix right-to-left scan with strict accumulator
prescanr' :: (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE prescanr' #-}
prescanr' = G.prescanr'

-- | Suffix right-to-left scan
postscanr :: (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE postscanr #-}
postscanr = G.postscanr

-- | Suffix right-to-left scan with strict accumulator
postscanr' :: (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE postscanr' #-}
postscanr' = G.postscanr'

-- | Haskell-style right-to-left scan
scanr :: (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE scanr #-}
scanr = G.scanr

-- | Haskell-style right-to-left scan with strict accumulator
scanr' :: (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE scanr' #-}
scanr' = G.scanr'

-- | Right-to-left scan over a non-empty vector
scanr1 :: (a -> a -> a) -> Vector a -> Vector a
{-# INLINE scanr1 #-}
scanr1 = G.scanr1

-- | Right-to-left scan over a non-empty vector with a strict accumulator
scanr1' :: (a -> a -> a) -> Vector a -> Vector a
{-# INLINE scanr1' #-}
scanr1' = G.scanr1'

-- Enumeration
-- -----------

-- | Yield a vector of the given length containing the values @x@, @x+1@ etc.
-- This operation is usually more efficient than 'enumFromTo'.
enumFromN :: Num a => a -> Int -> Vector a
{-# INLINE enumFromN #-}
enumFromN = G.enumFromN

-- | Yield a vector of the given length containing the values @x@, @x+y@,
-- @x+y+y@ etc. This operations is usually more efficient than
-- 'enumFromThenTo'.
enumFromStepN :: Num a => a -> a -> Int -> Vector a
{-# INLINE enumFromStepN #-}
enumFromStepN = G.enumFromStepN

-- | Enumerate values from @x@ to @y@.
--
-- /WARNING:/ This operation can be very inefficient. If at all possible, use
-- 'enumFromN' instead.
enumFromTo :: Enum a => a -> a -> Vector a
{-# INLINE enumFromTo #-}
enumFromTo = G.enumFromTo

-- | Enumerate values from @x@ to @y@ with a specific step @z@.
--
-- /WARNING:/ This operation can be very inefficient. If at all possible, use
-- 'enumFromStepN' instead.
enumFromThenTo :: Enum a => a -> a -> a -> Vector a
{-# INLINE enumFromThenTo #-}
enumFromThenTo = G.enumFromThenTo

-- Conversion to/from lists
-- ------------------------

-- | Convert a vector to a list
toList :: Vector a -> [a]
{-# INLINE toList #-}
toList = G.toList

-- | Convert a list to a vector
fromList :: [a] -> Vector a
{-# INLINE fromList #-}
fromList = G.fromList

