{-# LANGUAGE MagicHash, UnboxedTuples, FlexibleInstances, MultiParamTypeClasses #-}

-- |
-- Module      : Data.Vector.Primitive
-- Copyright   : (c) Roman Leshchinskiy 2008
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
-- 
-- Unboxed vectors of primitive types.
--

module Data.Vector.Primitive (
  Vector,

  -- * Length information
  length, null,

  -- * Construction
  empty, singleton, cons, snoc, replicate, (++), copy,

  -- * Accessing individual elements
  (!), head, last,

  -- * Subvectors
  slice, init, tail, take, drop,

  -- * Permutations
  accum, (//), backpermute, reverse,

  -- * Mapping
  map, concatMap,

  -- * Zipping and unzipping
  zipWith, zipWith3,

  -- * Filtering
  filter, takeWhile, dropWhile,

  -- * Searching
  elem, notElem, find, findIndex,

  -- * Folding
  foldl, foldl1, foldl', foldl1', foldr, foldr1,

  -- * Specialised folds
  {-and, or,-} sum, product, maximum, minimum,

  -- * Unfolding
  unfoldr,

  -- * Scans
  prescanl, prescanl',
  postscanl, postscanl',
  scanl, scanl', scanl1, scanl1',

  -- * Enumeration
  enumFromTo, enumFromThenTo,

  -- * Conversion to/from lists
  toList, fromList
) where

import           Data.Vector.IVector ( IVector(..) )
import qualified Data.Vector.IVector           as IV
import qualified Data.Vector.Primitive.Mutable as Mut
import           Data.Primitive.ByteArray
import           Data.Primitive ( Prim )

import Control.Monad.ST ( runST )

import GHC.ST   ( ST(..) )
import GHC.Prim ( ByteArray#, unsafeFreezeByteArray#, (+#) )
import GHC.Base ( Int(..) )

import Prelude hiding ( length, null,
                        replicate, (++),
                        head, last,
                        init, tail, take, drop, reverse,
                        map, concatMap,
                        zipWith, zipWith3, zip, zip3, unzip, unzip3,
                        filter, takeWhile, dropWhile,
                        elem, notElem,
                        foldl, foldl1, foldr, foldr1,
                        and, or, sum, product, minimum, maximum,
                        scanl, scanl1,
                        enumFromTo, enumFromThenTo )

import qualified Prelude

-- | Unboxed vectors of primitive types
data Vector a = Vector {-# UNPACK #-} !Int
                       {-# UNPACK #-} !Int
                       {-# UNPACK #-} !ByteArray

instance (Show a, Prim a) => Show (Vector a) where
    show = (Prelude.++ " :: Data.Vector.Primitive.Vector") . ("fromList " Prelude.++) . show . toList

instance Prim a => IVector Vector a where
  {-# INLINE vnew #-}
  vnew init = runST (do
                       Mut.Vector i n marr <- init
                       arr <- unsafeFreezeByteArray marr
                       return (Vector i n arr))

  {-# INLINE vlength #-}
  vlength (Vector _ n _) = n

  {-# INLINE unsafeSlice #-}
  unsafeSlice (Vector i _ arr) j n = Vector (i+j) n arr

  {-# INLINE unsafeIndexM #-}
  unsafeIndexM (Vector i _ arr) j = return (indexByteArray arr (i+j))

instance (Prim a, Eq a) => Eq (Vector a) where
  {-# INLINE (==) #-}
  (==) = IV.eq

instance (Prim a, Ord a) => Ord (Vector a) where
  {-# INLINE compare #-}
  compare = IV.cmp

-- Length
-- ------

length :: Prim a => Vector a -> Int
{-# INLINE length #-}
length = IV.length

null :: Prim a => Vector a -> Bool
{-# INLINE null #-}
null = IV.null

-- Construction
-- ------------

-- | Empty vector
empty :: Prim a => Vector a
{-# INLINE empty #-}
empty = IV.empty

-- | Vector with exaclty one element
singleton :: Prim a => a -> Vector a
{-# INLINE singleton #-}
singleton = IV.singleton

-- | Vector of the given length with the given value in each position
replicate :: Prim a => Int -> a -> Vector a
{-# INLINE replicate #-}
replicate = IV.replicate

-- | Prepend an element
cons :: Prim a => a -> Vector a -> Vector a
{-# INLINE cons #-}
cons = IV.cons

-- | Append an element
snoc :: Prim a => Vector a -> a -> Vector a
{-# INLINE snoc #-}
snoc = IV.snoc

infixr 5 ++
-- | Concatenate two vectors
(++) :: Prim a => Vector a -> Vector a -> Vector a
{-# INLINE (++) #-}
(++) = (IV.++)

-- | Create a copy of a vector. Useful when dealing with slices.
copy :: Prim a => Vector a -> Vector a
{-# INLINE copy #-}
copy = IV.copy

-- Accessing individual elements
-- -----------------------------

-- | Indexing
(!) :: Prim a => Vector a -> Int -> a
{-# INLINE (!) #-}
(!) = (IV.!)

-- | First element
head :: Prim a => Vector a -> a
{-# INLINE head #-}
head = IV.head

-- | Last element
last :: Prim a => Vector a -> a
{-# INLINE last #-}
last = IV.last

-- Subarrays
-- ---------

-- | Yield a part of the vector without copying it. Safer version of
-- 'unsafeSlice'.
slice :: Prim a => Vector a -> Int   -- ^ starting index
                             -> Int   -- ^ length
                             -> Vector a
{-# INLINE slice #-}
slice = IV.slice

-- | Yield all but the last element without copying.
init :: Prim a => Vector a -> Vector a
{-# INLINE init #-}
init = IV.init

-- | All but the first element (without copying).
tail :: Prim a => Vector a -> Vector a
{-# INLINE tail #-}
tail = IV.tail

-- | Yield the first @n@ elements without copying.
take :: Prim a => Int -> Vector a -> Vector a
{-# INLINE take #-}
take = IV.take

-- | Yield all but the first @n@ elements without copying.
drop :: Prim a => Int -> Vector a -> Vector a
{-# INLINE drop #-}
drop = IV.drop

-- Permutations
-- ------------

accum :: Prim a => (a -> b -> a) -> Vector a -> [(Int,b)] -> Vector a
{-# INLINE accum #-}
accum = IV.accum

(//) :: Prim a => Vector a -> [(Int, a)] -> Vector a
{-# INLINE (//) #-}
(//) = (IV.//)

backpermute :: Prim a => Vector a -> Vector Int -> Vector a
{-# INLINE backpermute #-}
backpermute = IV.backpermute

reverse :: Prim a => Vector a -> Vector a
{-# INLINE reverse #-}
reverse = IV.reverse

-- Mapping
-- -------

-- | Map a function over a vector
map :: (Prim a, Prim b) => (a -> b) -> Vector a -> Vector b
{-# INLINE map #-}
map = IV.map

concatMap :: (Prim a, Prim b) => (a -> Vector b) -> Vector a -> Vector b
{-# INLINE concatMap #-}
concatMap = IV.concatMap

-- Zipping/unzipping
-- -----------------

-- | Zip two vectors with the given function.
zipWith :: (Prim a, Prim b, Prim c)
        => (a -> b -> c) -> Vector a -> Vector b -> Vector c
{-# INLINE zipWith #-}
zipWith = IV.zipWith

-- | Zip three vectors with the given function.
zipWith3 :: (Prim a, Prim b, Prim c, Prim d)
         => (a -> b -> c -> d) -> Vector a -> Vector b -> Vector c -> Vector d
{-# INLINE zipWith3 #-}
zipWith3 = IV.zipWith3

-- Filtering
-- ---------

-- | Drop elements which do not satisfy the predicate
filter :: Prim a => (a -> Bool) -> Vector a -> Vector a
{-# INLINE filter #-}
filter = IV.filter

-- | Yield the longest prefix of elements satisfying the predicate.
takeWhile :: Prim a => (a -> Bool) -> Vector a -> Vector a
{-# INLINE takeWhile #-}
takeWhile = IV.takeWhile

-- | Drop the longest prefix of elements that satisfy the predicate.
dropWhile :: Prim a => (a -> Bool) -> Vector a -> Vector a
{-# INLINE dropWhile #-}
dropWhile = IV.dropWhile

-- Searching
-- ---------

infix 4 `elem`
-- | Check whether the vector contains an element
elem :: (Prim a, Eq a) => a -> Vector a -> Bool
{-# INLINE elem #-}
elem = IV.elem

infix 4 `notElem`
-- | Inverse of `elem`
notElem :: (Prim a, Eq a) => a -> Vector a -> Bool
{-# INLINE notElem #-}
notElem = IV.notElem

-- | Yield 'Just' the first element matching the predicate or 'Nothing' if no
-- such element exists.
find :: Prim a => (a -> Bool) -> Vector a -> Maybe a
{-# INLINE find #-}
find = IV.find

-- | Yield 'Just' the index of the first element matching the predicate or
-- 'Nothing' if no such element exists.
findIndex :: Prim a => (a -> Bool) -> Vector a -> Maybe Int
{-# INLINE findIndex #-}
findIndex = IV.findIndex

-- Folding
-- -------

-- | Left fold
foldl :: Prim b => (a -> b -> a) -> a -> Vector b -> a
{-# INLINE foldl #-}
foldl = IV.foldl

-- | Lefgt fold on non-empty vectors
foldl1 :: Prim a => (a -> a -> a) -> Vector a -> a
{-# INLINE foldl1 #-}
foldl1 = IV.foldl1

-- | Left fold with strict accumulator
foldl' :: Prim b => (a -> b -> a) -> a -> Vector b -> a
{-# INLINE foldl' #-}
foldl' = IV.foldl'

-- | Left fold on non-empty vectors with strict accumulator
foldl1' :: Prim a => (a -> a -> a) -> Vector a -> a
{-# INLINE foldl1' #-}
foldl1' = IV.foldl1'

-- | Right fold
foldr :: Prim a => (a -> b -> b) -> b -> Vector a -> b
{-# INLINE foldr #-}
foldr = IV.foldr

-- | Right fold on non-empty vectors
foldr1 :: Prim a => (a -> a -> a) -> Vector a -> a
{-# INLINE foldr1 #-}
foldr1 = IV.foldr1

-- Specialised folds
-- -----------------

{-
and :: Vector Bool -> Bool
{-# INLINE and #-}
and = IV.and

or :: Vector Bool -> Bool
{-# INLINE or #-}
or = IV.or
-}

sum :: (Prim a, Num a) => Vector a -> a
{-# INLINE sum #-}
sum = IV.sum

product :: (Prim a, Num a) => Vector a -> a
{-# INLINE product #-}
product = IV.product

maximum :: (Prim a, Ord a) => Vector a -> a
{-# INLINE maximum #-}
maximum = IV.maximum

minimum :: (Prim a, Ord a) => Vector a -> a
{-# INLINE minimum #-}
minimum = IV.minimum

-- Unfolding
-- ---------

unfoldr :: Prim a => (b -> Maybe (a, b)) -> b -> Vector a
{-# INLINE unfoldr #-}
unfoldr = IV.unfoldr

-- Scans
-- -----

-- | Prefix scan
prescanl :: (Prim a, Prim b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE prescanl #-}
prescanl = IV.prescanl

-- | Prefix scan with strict accumulator
prescanl' :: (Prim a, Prim b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE prescanl' #-}
prescanl' = IV.prescanl'

-- | Suffix scan
postscanl :: (Prim a, Prim b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE postscanl #-}
postscanl = IV.postscanl

-- | Suffix scan with strict accumulator
postscanl' :: (Prim a, Prim b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE postscanl' #-}
postscanl' = IV.postscanl'

-- | Haskell-style scan
scanl :: (Prim a, Prim b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE scanl #-}
scanl = IV.scanl

-- | Haskell-style scan with strict accumulator
scanl' :: (Prim a, Prim b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE scanl' #-}
scanl' = IV.scanl'

-- | Scan over a non-empty 'Vector'
scanl1 :: Prim a => (a -> a -> a) -> Vector a -> Vector a
{-# INLINE scanl1 #-}
scanl1 = IV.scanl1

-- | Scan over a non-empty 'Vector' with a strict accumulator
scanl1' :: Prim a => (a -> a -> a) -> Vector a -> Vector a
{-# INLINE scanl1' #-}
scanl1' = IV.scanl1'

-- Enumeration
-- -----------

enumFromTo :: (Prim a, Enum a) => a -> a -> Vector a
{-# INLINE enumFromTo #-}
enumFromTo = IV.enumFromTo

enumFromThenTo :: (Prim a, Enum a) => a -> a -> a -> Vector a
{-# INLINE enumFromThenTo #-}
enumFromThenTo = IV.enumFromThenTo

-- Conversion to/from lists
-- ------------------------

-- | Convert a vector to a list
toList :: Prim a => Vector a -> [a]
{-# INLINE toList #-}
toList = IV.toList

-- | Convert a list to a vector
fromList :: Prim a => [a] -> Vector a
{-# INLINE fromList #-}
fromList = IV.fromList

