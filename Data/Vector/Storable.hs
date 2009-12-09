{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}

-- |
-- Module      : Data.Vector.Storable
-- Copyright   : (c) Roman Leshchinskiy 2009
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
-- 
-- 'Storable'-based vectors.
--

module Data.Vector.Storable (
  Vector, MVector(..), Storable,

  -- * Length information
  length, null,

  -- * Construction
  empty, singleton, cons, snoc, replicate, generate, (++), copy,

  -- * Accessing individual elements
  (!), head, last,

  -- * Subvectors
  slice, init, tail, take, drop,
  unsafeSlice,

  -- * Permutations
  accum, accumulate_, (//), update_, backpermute, reverse,

  -- * Mapping
  map, imap, concatMap,

  -- * Zipping and unzipping
  zipWith, zipWith3, zipWith4, zipWith5, zipWith6,
  izipWith, izipWith3, izipWith4, izipWith5, izipWith6,

  -- * Filtering
  filter, ifilter, takeWhile, dropWhile,
  unstablePartition, span, break,

  -- * Searching
  elem, notElem, find, findIndex, findIndices, elemIndex, elemIndices,

  -- * Folding
  foldl, foldl1, foldl', foldl1', foldr, foldr1,
  ifoldl, ifoldl', ifoldr,

  -- * Specialised folds
  all, any, and, or,
  sum, product,
  maximum, maximumBy, minimum, minimumBy,
  minIndex, minIndexBy, maxIndex, maxIndexBy,

  -- * Unfolding
  unfoldr,

  -- * Scans
  prescanl, prescanl',
  postscanl, postscanl',
  scanl, scanl', scanl1, scanl1',

  -- * Enumeration
  enumFromTo, enumFromThenTo,

  -- * Conversion to/from lists
  toList, fromList,

  -- * Unsafe operations
  unsafeIndex,
  unsafeAccum, unsafeAccumulate_,
  unsafeUpd, unsafeUpdate_
) where

import qualified Data.Vector.Generic          as G
import           Data.Vector.Storable.Mutable ( MVector(..) )
import Data.Vector.Storable.Internal

import Foreign.Storable
import Foreign.ForeignPtr
-- import Foreign.Ptr
-- import Foreign.C.Types

import Control.Monad.ST ( ST, runST )

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
                        scanl, scanl1,
                        enumFromTo, enumFromThenTo )

import qualified Prelude

#include "vector.h"

-- | 'Storable'-based vectors
data Vector a = Vector {-# UNPACK #-} !Int
                       {-# UNPACK #-} !Int
                       {-# UNPACK #-} !(ForeignPtr a)

instance (Show a, Storable a) => Show (Vector a) where
  show = (Prelude.++ " :: Data.Vector.Storable.Vector")
       . ("fromList " Prelude.++)
       . show
       . toList

type instance G.Mutable Vector = MVector

instance Storable a => G.Vector Vector a where
  {-# INLINE unsafeFreeze #-}
  unsafeFreeze (MVector i n p) = return $ Vector i n p

  {-# INLINE basicLength #-}
  basicLength (Vector _ n _) = n

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice (Vector i _ p) j n = Vector (i+j) n p

  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (Vector i _ p) j = return
                                     . inlinePerformIO
                                     $ withForeignPtr p (`peekElemOff` (i+j))

  {-# INLINE elemseq #-}
  elemseq _ = seq

instance (Storable a, Eq a) => Eq (Vector a) where
  {-# INLINE (==) #-}
  (==) = G.eq

{-
eq_memcmp :: forall a. Storable a => Vector a -> Vector a -> Bool
{-# INLINE_STREAM eq_memcmp #-}
eq_memcmp (Vector i m p) (Vector j n q)
  = m == n && inlinePerformIO
              (withForeignPtr p $ \p' ->
               withForeignPtr q $ \q' ->
               return $
               memcmp (p' `plusPtr` i) (q' `plusPtr` j)
                      (fromIntegral $ sizeOf (undefined :: a) * m) == 0)

foreign import ccall unsafe "string.h memcmp" memcmp
        :: Ptr a -> Ptr a -> CSize -> CInt

{-# RULES

"(==) [Vector.Storable Int]"
  G.eq = eq_memcmp :: Vector Int -> Vector Int -> Bool
 #-}
-}

instance (Storable a, Ord a) => Ord (Vector a) where
  {-# INLINE compare #-}
  compare = G.cmp

-- Length
-- ------

length :: Storable a => Vector a -> Int
{-# INLINE length #-}
length = G.length

null :: Storable a => Vector a -> Bool
{-# INLINE null #-}
null = G.null

-- Construction
-- ------------

-- | Empty vector
empty :: Storable a => Vector a
{-# INLINE empty #-}
empty = G.empty

-- | Vector with exaclty one element
singleton :: Storable a => a -> Vector a
{-# INLINE singleton #-}
singleton = G.singleton

-- | Vector of the given length with the given value in each position
replicate :: Storable a => Int -> a -> Vector a
{-# INLINE replicate #-}
replicate = G.replicate

-- | Generate a vector of the given length by applying the function to each
-- index
generate :: Storable a => Int -> (Int -> a) -> Vector a
{-# INLINE generate #-}
generate = G.generate

-- | Prepend an element
cons :: Storable a => a -> Vector a -> Vector a
{-# INLINE cons #-}
cons = G.cons

-- | Append an element
snoc :: Storable a => Vector a -> a -> Vector a
{-# INLINE snoc #-}
snoc = G.snoc

infixr 5 ++
-- | Concatenate two vectors
(++) :: Storable a => Vector a -> Vector a -> Vector a
{-# INLINE (++) #-}
(++) = (G.++)

-- | Create a copy of a vector. Useful when dealing with slices.
copy :: Storable a => Vector a -> Vector a
{-# INLINE copy #-}
copy = G.copy

-- Accessing individual elements
-- -----------------------------

-- | Indexing
(!) :: Storable a => Vector a -> Int -> a
{-# INLINE (!) #-}
(!) = (G.!)

-- | Unsafe indexing without bounds checks
unsafeIndex :: Storable a => Vector a -> Int -> a
{-# INLINE unsafeIndex #-}
unsafeIndex = G.unsafeIndex

-- | First element
head :: Storable a => Vector a -> a
{-# INLINE head #-}
head = G.head

-- | Last element
last :: Storable a => Vector a -> a
{-# INLINE last #-}
last = G.last

-- Subarrays
-- ---------

-- | Yield a part of the vector without copying it. Safer version of
-- 'basicUnsafeSlice'.
slice :: Storable a => Vector a -> Int   -- ^ starting index
                                -> Int   -- ^ length
                                -> Vector a
{-# INLINE slice #-}
slice = G.slice

-- | Unsafely yield a part of the vector without copying it and without
-- performing bounds checks.
unsafeSlice :: Storable a => Vector a -> Int   -- ^ starting index
                                      -> Int   -- ^ length
                                      -> Vector a
{-# INLINE unsafeSlice #-}
unsafeSlice = G.unsafeSlice

-- | Yield all but the last element without copying.
init :: Storable a => Vector a -> Vector a
{-# INLINE init #-}
init = G.init

-- | All but the first element (without copying).
tail :: Storable a => Vector a -> Vector a
{-# INLINE tail #-}
tail = G.tail

-- | Yield the first @n@ elements without copying.
take :: Storable a => Int -> Vector a -> Vector a
{-# INLINE take #-}
take = G.take

-- | Yield all but the first @n@ elements without copying.
drop :: Storable a => Int -> Vector a -> Vector a
{-# INLINE drop #-}
drop = G.drop

-- Permutations
-- ------------

unsafeAccum :: Storable a => (a -> b -> a) -> Vector a -> [(Int,b)] -> Vector a
{-# INLINE unsafeAccum #-}
unsafeAccum = G.unsafeAccum

unsafeAccumulate_ :: (Storable a, Storable b) =>
               (a -> b -> a) -> Vector a -> Vector Int -> Vector b -> Vector a
{-# INLINE unsafeAccumulate_ #-}
unsafeAccumulate_ = G.unsafeAccumulate_

accum :: Storable a => (a -> b -> a) -> Vector a -> [(Int,b)] -> Vector a
{-# INLINE accum #-}
accum = G.accum

accumulate_ :: (Storable a, Storable b) =>
               (a -> b -> a) -> Vector a -> Vector Int -> Vector b -> Vector a
{-# INLINE accumulate_ #-}
accumulate_ = G.accumulate_

unsafeUpd :: Storable a => Vector a -> [(Int, a)] -> Vector a
{-# INLINE unsafeUpd #-}
unsafeUpd = G.unsafeUpd

unsafeUpdate_ :: Storable a => Vector a -> Vector Int -> Vector a -> Vector a
{-# INLINE unsafeUpdate_ #-}
unsafeUpdate_ = G.unsafeUpdate_

(//) :: Storable a => Vector a -> [(Int, a)] -> Vector a
{-# INLINE (//) #-}
(//) = (G.//)

update_ :: Storable a => Vector a -> Vector Int -> Vector a -> Vector a
{-# INLINE update_ #-}
update_ = G.update_

backpermute :: Storable a => Vector a -> Vector Int -> Vector a
{-# INLINE backpermute #-}
backpermute = G.backpermute

reverse :: Storable a => Vector a -> Vector a
{-# INLINE reverse #-}
reverse = G.reverse

-- Mapping
-- -------

-- | Map a function over a vector
map :: (Storable a, Storable b) => (a -> b) -> Vector a -> Vector b
{-# INLINE map #-}
map = G.map

-- | Apply a function to every index/value pair
imap :: (Storable a, Storable b) => (Int -> a -> b) -> Vector a -> Vector b
{-# INLINE imap #-}
imap = G.imap

concatMap :: (Storable a, Storable b) => (a -> Vector b) -> Vector a -> Vector b
{-# INLINE concatMap #-}
concatMap = G.concatMap

-- Zipping/unzipping
-- -----------------

-- | Zip two vectors with the given function.
zipWith :: (Storable a, Storable b, Storable c)
        => (a -> b -> c) -> Vector a -> Vector b -> Vector c
{-# INLINE zipWith #-}
zipWith = G.zipWith

-- | Zip three vectors with the given function.
zipWith3 :: (Storable a, Storable b, Storable c, Storable d)
         => (a -> b -> c -> d) -> Vector a -> Vector b -> Vector c -> Vector d
{-# INLINE zipWith3 #-}
zipWith3 = G.zipWith3

zipWith4 :: (Storable a, Storable b, Storable c, Storable d, Storable e)
         => (a -> b -> c -> d -> e)
         -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
{-# INLINE zipWith4 #-}
zipWith4 = G.zipWith4

zipWith5 :: (Storable a, Storable b, Storable c, Storable d, Storable e,
             Storable f)
         => (a -> b -> c -> d -> e -> f)
         -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
         -> Vector f
{-# INLINE zipWith5 #-}
zipWith5 = G.zipWith5

zipWith6 :: (Storable a, Storable b, Storable c, Storable d, Storable e,
             Storable f, Storable g)
         => (a -> b -> c -> d -> e -> f -> g)
         -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
         -> Vector f -> Vector g
{-# INLINE zipWith6 #-}
zipWith6 = G.zipWith6

-- | Zip two vectors and their indices with the given function.
izipWith :: (Storable a, Storable b, Storable c)
         => (Int -> a -> b -> c) -> Vector a -> Vector b -> Vector c
{-# INLINE izipWith #-}
izipWith = G.izipWith

-- | Zip three vectors and their indices with the given function.
izipWith3 :: (Storable a, Storable b, Storable c, Storable d)
          => (Int -> a -> b -> c -> d)
          -> Vector a -> Vector b -> Vector c -> Vector d
{-# INLINE izipWith3 #-}
izipWith3 = G.izipWith3

izipWith4 :: (Storable a, Storable b, Storable c, Storable d, Storable e)
          => (Int -> a -> b -> c -> d -> e)
          -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
{-# INLINE izipWith4 #-}
izipWith4 = G.izipWith4

izipWith5 :: (Storable a, Storable b, Storable c, Storable d, Storable e,
              Storable f)
          => (Int -> a -> b -> c -> d -> e -> f)
          -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
          -> Vector f
{-# INLINE izipWith5 #-}
izipWith5 = G.izipWith5

izipWith6 :: (Storable a, Storable b, Storable c, Storable d, Storable e,
              Storable f, Storable g)
          => (Int -> a -> b -> c -> d -> e -> f -> g)
          -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
          -> Vector f -> Vector g
{-# INLINE izipWith6 #-}
izipWith6 = G.izipWith6

-- Filtering
-- ---------

-- | Drop elements which do not satisfy the predicate
filter :: Storable a => (a -> Bool) -> Vector a -> Vector a
{-# INLINE filter #-}
filter = G.filter

-- | Drop elements that do not satisfy the predicate (applied to values and
-- their indices)
ifilter :: Storable a => (Int -> a -> Bool) -> Vector a -> Vector a
{-# INLINE ifilter #-}
ifilter = G.ifilter

-- | Yield the longest prefix of elements satisfying the predicate.
takeWhile :: Storable a => (a -> Bool) -> Vector a -> Vector a
{-# INLINE takeWhile #-}
takeWhile = G.takeWhile

-- | Drop the longest prefix of elements that satisfy the predicate.
dropWhile :: Storable a => (a -> Bool) -> Vector a -> Vector a
{-# INLINE dropWhile #-}
dropWhile = G.dropWhile

-- | Split the vector in two parts, the first one containing those elements
-- that satisfy the predicate and the second one those that don't. The order
-- of the elements is not preserved.
unstablePartition
        :: Storable a => (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE unstablePartition #-}
unstablePartition = G.unstablePartition

-- | Split the vector into the longest prefix of elements that satisfy the
-- predicate and the rest.
span :: Storable a => (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE span #-}
span = G.span

-- | Split the vector into the longest prefix of elements that do not satisfy
-- the predicate and the rest.
break :: Storable a => (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE break #-}
break = G.break

-- Searching
-- ---------

infix 4 `elem`
-- | Check whether the vector contains an element
elem :: (Storable a, Eq a) => a -> Vector a -> Bool
{-# INLINE elem #-}
elem = G.elem

infix 4 `notElem`
-- | Inverse of `elem`
notElem :: (Storable a, Eq a) => a -> Vector a -> Bool
{-# INLINE notElem #-}
notElem = G.notElem

-- | Yield 'Just' the first element matching the predicate or 'Nothing' if no
-- such element exists.
find :: Storable a => (a -> Bool) -> Vector a -> Maybe a
{-# INLINE find #-}
find = G.find

-- | Yield 'Just' the index of the first element matching the predicate or
-- 'Nothing' if no such element exists.
findIndex :: Storable a => (a -> Bool) -> Vector a -> Maybe Int
{-# INLINE findIndex #-}
findIndex = G.findIndex

-- | Yield the indices of elements satisfying the predicate
findIndices :: Storable a => (a -> Bool) -> Vector a -> Vector Int
{-# INLINE findIndices #-}
findIndices = G.findIndices

-- | Yield 'Just' the index of the first occurence of the given element or
-- 'Nothing' if the vector does not contain the element
elemIndex :: (Storable a, Eq a) => a -> Vector a -> Maybe Int
{-# INLINE elemIndex #-}
elemIndex = G.elemIndex

-- | Yield the indices of all occurences of the given element
elemIndices :: (Storable a, Eq a) => a -> Vector a -> Vector Int
{-# INLINE elemIndices #-}
elemIndices = G.elemIndices

-- Folding
-- -------

-- | Left fold
foldl :: Storable b => (a -> b -> a) -> a -> Vector b -> a
{-# INLINE foldl #-}
foldl = G.foldl

-- | Lefgt fold on non-empty vectors
foldl1 :: Storable a => (a -> a -> a) -> Vector a -> a
{-# INLINE foldl1 #-}
foldl1 = G.foldl1

-- | Left fold with strict accumulator
foldl' :: Storable b => (a -> b -> a) -> a -> Vector b -> a
{-# INLINE foldl' #-}
foldl' = G.foldl'

-- | Left fold on non-empty vectors with strict accumulator
foldl1' :: Storable a => (a -> a -> a) -> Vector a -> a
{-# INLINE foldl1' #-}
foldl1' = G.foldl1'

-- | Right fold
foldr :: Storable a => (a -> b -> b) -> b -> Vector a -> b
{-# INLINE foldr #-}
foldr = G.foldr

-- | Right fold on non-empty vectors
foldr1 :: Storable a => (a -> a -> a) -> Vector a -> a
{-# INLINE foldr1 #-}
foldr1 = G.foldr1

-- | Left fold (function applied to each element and its index)
ifoldl :: Storable b => (a -> Int -> b -> a) -> a -> Vector b -> a
{-# INLINE ifoldl #-}
ifoldl = G.ifoldl

-- | Left fold with strict accumulator (function applied to each element and
-- its index)
ifoldl' :: Storable b => (a -> Int -> b -> a) -> a -> Vector b -> a
{-# INLINE ifoldl' #-}
ifoldl' = G.ifoldl'

-- | Right fold (function applied to each element and its index)
ifoldr :: Storable a => (Int -> a -> b -> b) -> b -> Vector a -> b
{-# INLINE ifoldr #-}
ifoldr = G.ifoldr

-- Specialised folds
-- -----------------

all :: Storable a => (a -> Bool) -> Vector a -> Bool
{-# INLINE all #-}
all = G.all

any :: Storable a => (a -> Bool) -> Vector a -> Bool
{-# INLINE any #-}
any = G.any

and :: Vector Bool -> Bool
{-# INLINE and #-}
and = G.and

or :: Vector Bool -> Bool
{-# INLINE or #-}
or = G.or

sum :: (Storable a, Num a) => Vector a -> a
{-# INLINE sum #-}
sum = G.sum

product :: (Storable a, Num a) => Vector a -> a
{-# INLINE product #-}
product = G.product

maximum :: (Storable a, Ord a) => Vector a -> a
{-# INLINE maximum #-}
maximum = G.maximum

maximumBy :: Storable a => (a -> a -> Ordering) -> Vector a -> a
{-# INLINE maximumBy #-}
maximumBy = G.maximumBy

minimum :: (Storable a, Ord a) => Vector a -> a
{-# INLINE minimum #-}
minimum = G.minimum

minimumBy :: Storable a => (a -> a -> Ordering) -> Vector a -> a
{-# INLINE minimumBy #-}
minimumBy = G.minimumBy

maxIndex :: (Storable a, Ord a) => Vector a -> Int
{-# INLINE maxIndex #-}
maxIndex = G.maxIndex

maxIndexBy :: Storable a => (a -> a -> Ordering) -> Vector a -> Int
{-# INLINE maxIndexBy #-}
maxIndexBy = G.maxIndexBy

minIndex :: (Storable a, Ord a) => Vector a -> Int
{-# INLINE minIndex #-}
minIndex = G.minIndex

minIndexBy :: Storable a => (a -> a -> Ordering) -> Vector a -> Int
{-# INLINE minIndexBy #-}
minIndexBy = G.minIndexBy

-- Unfolding
-- ---------

unfoldr :: Storable a => (b -> Maybe (a, b)) -> b -> Vector a
{-# INLINE unfoldr #-}
unfoldr = G.unfoldr

-- Scans
-- -----

-- | Prefix scan
prescanl :: (Storable a, Storable b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE prescanl #-}
prescanl = G.prescanl

-- | Prefix scan with strict accumulator
prescanl' :: (Storable a, Storable b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE prescanl' #-}
prescanl' = G.prescanl'

-- | Suffix scan
postscanl :: (Storable a, Storable b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE postscanl #-}
postscanl = G.postscanl

-- | Suffix scan with strict accumulator
postscanl' :: (Storable a, Storable b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE postscanl' #-}
postscanl' = G.postscanl'

-- | Haskell-style scan
scanl :: (Storable a, Storable b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE scanl #-}
scanl = G.scanl

-- | Haskell-style scan with strict accumulator
scanl' :: (Storable a, Storable b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE scanl' #-}
scanl' = G.scanl'

-- | Scan over a non-empty 'Vector'
scanl1 :: Storable a => (a -> a -> a) -> Vector a -> Vector a
{-# INLINE scanl1 #-}
scanl1 = G.scanl1

-- | Scan over a non-empty 'Vector' with a strict accumulator
scanl1' :: Storable a => (a -> a -> a) -> Vector a -> Vector a
{-# INLINE scanl1' #-}
scanl1' = G.scanl1'

-- Enumeration
-- -----------

enumFromTo :: (Storable a, Enum a) => a -> a -> Vector a
{-# INLINE enumFromTo #-}
enumFromTo = G.enumFromTo

enumFromThenTo :: (Storable a, Enum a) => a -> a -> a -> Vector a
{-# INLINE enumFromThenTo #-}
enumFromThenTo = G.enumFromThenTo

-- Conversion to/from lists
-- ------------------------

-- | Convert a vector to a list
toList :: Storable a => Vector a -> [a]
{-# INLINE toList #-}
toList = G.toList

-- | Convert a list to a vector
fromList :: Storable a => [a] -> Vector a
{-# INLINE fromList #-}
fromList = G.fromList

