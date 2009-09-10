{-# LANGUAGE MagicHash, UnboxedTuples, FlexibleInstances, MultiParamTypeClasses #-}

-- |
-- Module      : Data.Vector.Unboxed
-- Copyright   : (c) Roman Leshchinskiy 2008
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
-- 
-- Unboxed vectors based on 'Unbox'.
--

module Data.Vector.Unboxed (
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

  -- * Enumeration
  enumFromTo, enumFromThenTo,

  -- * Conversion to/from lists
  toList, fromList
) where

import           Data.Vector.IVector ( IVector(..) )
import qualified Data.Vector.IVector            as IV
import qualified Data.Vector.Unboxed.Mutable.ST as Mut
import           Data.Vector.Unboxed.Unbox

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
                        enumFromTo, enumFromThenTo )

import qualified Prelude

-- | Unboxed vectors
data Vector a = Vector {-# UNPACK #-} !Int
                       {-# UNPACK #-} !Int
                                      ByteArray#

instance (Show a, Unbox a) => Show (Vector a) where
    show = (Prelude.++ " :: Data.Vector.Unboxed.Vector") . ("fromList " Prelude.++) . show . toList

instance Unbox a => IVector Vector a where
  {-# INLINE vnew #-}
  vnew init = runST (do
                       Mut.Vector i n marr# <- init
                       ST (\s# -> case unsafeFreezeByteArray# marr# s# of
                            (# s2#, arr# #) -> (# s2#, Vector i n arr# #)))

  {-# INLINE vlength #-}
  vlength (Vector _ n _) = n

  {-# INLINE unsafeSlice #-}
  unsafeSlice (Vector i _ arr#) j n = Vector (i+j) n arr#

  {-# INLINE unsafeIndexM #-}
  unsafeIndexM (Vector (I# i#) _ arr#) (I# j#) = return (at# arr# (i# +# j#))

instance (Unbox a, Eq a) => Eq (Vector a) where
  {-# INLINE (==) #-}
  (==) = IV.eq

instance (Unbox a, Ord a) => Ord (Vector a) where
  {-# INLINE compare #-}
  compare = IV.cmp

-- Length
-- ------

length :: Unbox a => Vector a -> Int
{-# INLINE length #-}
length = IV.length

null :: Unbox a => Vector a -> Bool
{-# INLINE null #-}
null = IV.null

-- Construction
-- ------------

-- | Empty vector
empty :: Unbox a => Vector a
{-# INLINE empty #-}
empty = IV.empty

-- | Vector with exaclty one element
singleton :: Unbox a => a -> Vector a
{-# INLINE singleton #-}
singleton = IV.singleton

-- | Vector of the given length with the given value in each position
replicate :: Unbox a => Int -> a -> Vector a
{-# INLINE replicate #-}
replicate = IV.replicate

-- | Prepend an element
cons :: Unbox a => a -> Vector a -> Vector a
{-# INLINE cons #-}
cons = IV.cons

-- | Append an element
snoc :: Unbox a => Vector a -> a -> Vector a
{-# INLINE snoc #-}
snoc = IV.snoc

infixr 5 ++
-- | Concatenate two vectors
(++) :: Unbox a => Vector a -> Vector a -> Vector a
{-# INLINE (++) #-}
(++) = (IV.++)

-- | Create a copy of a vector. Useful when dealing with slices.
copy :: Unbox a => Vector a -> Vector a
{-# INLINE copy #-}
copy = IV.copy

-- Accessing individual elements
-- -----------------------------

-- | Indexing
(!) :: Unbox a => Vector a -> Int -> a
{-# INLINE (!) #-}
(!) = (IV.!)

-- | First element
head :: Unbox a => Vector a -> a
{-# INLINE head #-}
head = IV.head

-- | Last element
last :: Unbox a => Vector a -> a
{-# INLINE last #-}
last = IV.last

-- Subarrays
-- ---------

-- | Yield a part of the vector without copying it. Safer version of
-- 'unsafeSlice'.
slice :: Unbox a => Vector a -> Int   -- ^ starting index
                             -> Int   -- ^ length
                             -> Vector a
{-# INLINE slice #-}
slice = IV.slice

-- | Yield all but the last element without copying.
init :: Unbox a => Vector a -> Vector a
{-# INLINE init #-}
init = IV.init

-- | All but the first element (without copying).
tail :: Unbox a => Vector a -> Vector a
{-# INLINE tail #-}
tail = IV.tail

-- | Yield the first @n@ elements without copying.
take :: Unbox a => Int -> Vector a -> Vector a
{-# INLINE take #-}
take = IV.take

-- | Yield all but the first @n@ elements without copying.
drop :: Unbox a => Int -> Vector a -> Vector a
{-# INLINE drop #-}
drop = IV.drop

-- Permutations
-- ------------

accum :: Unbox a => (a -> b -> a) -> Vector a -> [(Int,b)] -> Vector a
{-# INLINE accum #-}
accum = IV.accum

(//) :: Unbox a => Vector a -> [(Int, a)] -> Vector a
{-# INLINE (//) #-}
(//) = (IV.//)

backpermute :: Unbox a => Vector a -> Vector Int -> Vector a
{-# INLINE backpermute #-}
backpermute = IV.backpermute

reverse :: Unbox a => Vector a -> Vector a
{-# INLINE reverse #-}
reverse = IV.reverse

-- Mapping
-- -------

-- | Map a function over a vector
map :: (Unbox a, Unbox b) => (a -> b) -> Vector a -> Vector b
{-# INLINE map #-}
map = IV.map

concatMap :: (Unbox a, Unbox b) => (a -> Vector b) -> Vector a -> Vector b
{-# INLINE concatMap #-}
concatMap = IV.concatMap

-- Zipping/unzipping
-- -----------------

-- | Zip two vectors with the given function.
zipWith :: (Unbox a, Unbox b, Unbox c)
        => (a -> b -> c) -> Vector a -> Vector b -> Vector c
{-# INLINE zipWith #-}
zipWith = IV.zipWith

-- | Zip three vectors with the given function.
zipWith3 :: (Unbox a, Unbox b, Unbox c, Unbox d)
         => (a -> b -> c -> d) -> Vector a -> Vector b -> Vector c -> Vector d
{-# INLINE zipWith3 #-}
zipWith3 = IV.zipWith3

-- Filtering
-- ---------

-- | Drop elements which do not satisfy the predicate
filter :: Unbox a => (a -> Bool) -> Vector a -> Vector a
{-# INLINE filter #-}
filter = IV.filter

-- | Yield the longest prefix of elements satisfying the predicate.
takeWhile :: Unbox a => (a -> Bool) -> Vector a -> Vector a
{-# INLINE takeWhile #-}
takeWhile = IV.takeWhile

-- | Drop the longest prefix of elements that satisfy the predicate.
dropWhile :: Unbox a => (a -> Bool) -> Vector a -> Vector a
{-# INLINE dropWhile #-}
dropWhile = IV.dropWhile

-- Searching
-- ---------

infix 4 `elem`
-- | Check whether the vector contains an element
elem :: (Unbox a, Eq a) => a -> Vector a -> Bool
{-# INLINE elem #-}
elem = IV.elem

infix 4 `notElem`
-- | Inverse of `elem`
notElem :: (Unbox a, Eq a) => a -> Vector a -> Bool
{-# INLINE notElem #-}
notElem = IV.notElem

-- | Yield 'Just' the first element matching the predicate or 'Nothing' if no
-- such element exists.
find :: Unbox a => (a -> Bool) -> Vector a -> Maybe a
{-# INLINE find #-}
find = IV.find

-- | Yield 'Just' the index of the first element matching the predicate or
-- 'Nothing' if no such element exists.
findIndex :: Unbox a => (a -> Bool) -> Vector a -> Maybe Int
{-# INLINE findIndex #-}
findIndex = IV.findIndex

-- Folding
-- -------

-- | Left fold
foldl :: Unbox b => (a -> b -> a) -> a -> Vector b -> a
{-# INLINE foldl #-}
foldl = IV.foldl

-- | Lefgt fold on non-empty vectors
foldl1 :: Unbox a => (a -> a -> a) -> Vector a -> a
{-# INLINE foldl1 #-}
foldl1 = IV.foldl1

-- | Left fold with strict accumulator
foldl' :: Unbox b => (a -> b -> a) -> a -> Vector b -> a
{-# INLINE foldl' #-}
foldl' = IV.foldl'

-- | Left fold on non-empty vectors with strict accumulator
foldl1' :: Unbox a => (a -> a -> a) -> Vector a -> a
{-# INLINE foldl1' #-}
foldl1' = IV.foldl1'

-- | Right fold
foldr :: Unbox a => (a -> b -> b) -> b -> Vector a -> b
{-# INLINE foldr #-}
foldr = IV.foldr

-- | Right fold on non-empty vectors
foldr1 :: Unbox a => (a -> a -> a) -> Vector a -> a
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

sum :: (Unbox a, Num a) => Vector a -> a
{-# INLINE sum #-}
sum = IV.sum

product :: (Unbox a, Num a) => Vector a -> a
{-# INLINE product #-}
product = IV.product

maximum :: (Unbox a, Ord a) => Vector a -> a
{-# INLINE maximum #-}
maximum = IV.maximum

minimum :: (Unbox a, Ord a) => Vector a -> a
{-# INLINE minimum #-}
minimum = IV.minimum

-- Unfolding
-- ---------

unfoldr :: Unbox a => (b -> Maybe (a, b)) -> b -> Vector a
{-# INLINE unfoldr #-}
unfoldr = IV.unfoldr

-- Scans
-- -----

-- | Prefix scan
prescanl :: (Unbox a, Unbox b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE prescanl #-}
prescanl = IV.prescanl

-- | Prefix scan with strict accumulator
prescanl' :: (Unbox a, Unbox b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE prescanl' #-}
prescanl' = IV.prescanl'

-- Enumeration
-- -----------

enumFromTo :: (Unbox a, Enum a) => a -> a -> Vector a
{-# INLINE enumFromTo #-}
enumFromTo = IV.enumFromTo

enumFromThenTo :: (Unbox a, Enum a) => a -> a -> a -> Vector a
{-# INLINE enumFromThenTo #-}
enumFromThenTo = IV.enumFromThenTo

-- Conversion to/from lists
-- ------------------------

-- | Convert a vector to a list
toList :: Unbox a => Vector a -> [a]
{-# INLINE toList #-}
toList = IV.toList

-- | Convert a list to a vector
fromList :: Unbox a => [a] -> Vector a
{-# INLINE fromList #-}
fromList = IV.fromList

