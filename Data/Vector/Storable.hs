{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

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
  and, or, sum, product, maximum, minimum,

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

import Data.Vector.IVector ( IVector(..) )
import qualified Data.Vector.IVector          as IV
import qualified Data.Vector.Storable.Mutable as Mut
import Data.Vector.Storable.Internal

import Foreign.Storable
import Foreign.ForeignPtr

import System.IO.Unsafe ( unsafePerformIO )

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

-- | 'Storable'-based vectors
data Vector a = Vector {-# UNPACK #-} !Int
                       {-# UNPACK #-} !Int
                       {-# UNPACK #-} !(ForeignPtr a)

instance (Show a, Storable a) => Show (Vector a) where
  show = (Prelude.++ " :: Data.Vector.Storable.Vector")
       . ("fromList " Prelude.++)
       . show
       . toList

instance Storable a => IVector Vector a where
  {-# INLINE vnew #-}
  vnew init = unsafePerformIO (do
                                 Mut.Vector i n p <- init
                                 return (Vector i n p))

  {-# INLINE vlength #-}
  vlength (Vector _ n _) = n

  {-# INLINE unsafeSlice #-}
  unsafeSlice (Vector i _ p) j n = Vector (i+j) n p

  {-# INLINE unsafeIndexM #-}
  unsafeIndexM (Vector i _ p) j = return
                                . inlinePerformIO
                                $ withForeignPtr p (`peekElemOff` (i+j))

instance (Storable a, Eq a) => Eq (Vector a) where
  {-# INLINE (==) #-}
  (==) = IV.eq

instance (Storable a, Ord a) => Ord (Vector a) where
  {-# INLINE compare #-}
  compare = IV.cmp

-- Length
-- ------

length :: Storable a => Vector a -> Int
{-# INLINE length #-}
length = IV.length

null :: Storable a => Vector a -> Bool
{-# INLINE null #-}
null = IV.null

-- Construction
-- ------------

-- | Empty vector
empty :: Storable a => Vector a
{-# INLINE empty #-}
empty = IV.empty

-- | Vector with exaclty one element
singleton :: Storable a => a -> Vector a
{-# INLINE singleton #-}
singleton = IV.singleton

-- | Vector of the given length with the given value in each position
replicate :: Storable a => Int -> a -> Vector a
{-# INLINE replicate #-}
replicate = IV.replicate

-- | Prepend an element
cons :: Storable a => a -> Vector a -> Vector a
{-# INLINE cons #-}
cons = IV.cons

-- | Append an element
snoc :: Storable a => Vector a -> a -> Vector a
{-# INLINE snoc #-}
snoc = IV.snoc

infixr 5 ++
-- | Concatenate two vectors
(++) :: Storable a => Vector a -> Vector a -> Vector a
{-# INLINE (++) #-}
(++) = (IV.++)

-- | Create a copy of a vector. Useful when dealing with slices.
copy :: Storable a => Vector a -> Vector a
{-# INLINE copy #-}
copy = IV.copy

-- Accessing individual elements
-- -----------------------------

-- | Indexing
(!) :: Storable a => Vector a -> Int -> a
{-# INLINE (!) #-}
(!) = (IV.!)

-- | First element
head :: Storable a => Vector a -> a
{-# INLINE head #-}
head = IV.head

-- | Last element
last :: Storable a => Vector a -> a
{-# INLINE last #-}
last = IV.last

-- Subarrays
-- ---------

-- | Yield a part of the vector without copying it. Safer version of
-- 'unsafeSlice'.
slice :: Storable a => Vector a -> Int   -- ^ starting index
                             -> Int   -- ^ length
                             -> Vector a
{-# INLINE slice #-}
slice = IV.slice

-- | Yield all but the last element without copying.
init :: Storable a => Vector a -> Vector a
{-# INLINE init #-}
init = IV.init

-- | All but the first element (without copying).
tail :: Storable a => Vector a -> Vector a
{-# INLINE tail #-}
tail = IV.tail

-- | Yield the first @n@ elements without copying.
take :: Storable a => Int -> Vector a -> Vector a
{-# INLINE take #-}
take = IV.take

-- | Yield all but the first @n@ elements without copying.
drop :: Storable a => Int -> Vector a -> Vector a
{-# INLINE drop #-}
drop = IV.drop

-- Permutations
-- ------------

accum :: Storable a => (a -> b -> a) -> Vector a -> [(Int,b)] -> Vector a
{-# INLINE accum #-}
accum = IV.accum

(//) :: Storable a => Vector a -> [(Int, a)] -> Vector a
{-# INLINE (//) #-}
(//) = (IV.//)

backpermute :: Storable a => Vector a -> Vector Int -> Vector a
{-# INLINE backpermute #-}
backpermute = IV.backpermute

reverse :: Storable a => Vector a -> Vector a
{-# INLINE reverse #-}
reverse = IV.reverse

-- Mapping
-- -------

-- | Map a function over a vector
map :: (Storable a, Storable b) => (a -> b) -> Vector a -> Vector b
{-# INLINE map #-}
map = IV.map

concatMap :: (Storable a, Storable b) => (a -> Vector b) -> Vector a -> Vector b
{-# INLINE concatMap #-}
concatMap = IV.concatMap

-- Zipping/unzipping
-- -----------------

-- | Zip two vectors with the given function.
zipWith :: (Storable a, Storable b, Storable c)
        => (a -> b -> c) -> Vector a -> Vector b -> Vector c
{-# INLINE zipWith #-}
zipWith = IV.zipWith

-- | Zip three vectors with the given function.
zipWith3 :: (Storable a, Storable b, Storable c, Storable d)
         => (a -> b -> c -> d) -> Vector a -> Vector b -> Vector c -> Vector d
{-# INLINE zipWith3 #-}
zipWith3 = IV.zipWith3

-- Filtering
-- ---------

-- | Drop elements which do not satisfy the predicate
filter :: Storable a => (a -> Bool) -> Vector a -> Vector a
{-# INLINE filter #-}
filter = IV.filter

-- | Yield the longest prefix of elements satisfying the predicate.
takeWhile :: Storable a => (a -> Bool) -> Vector a -> Vector a
{-# INLINE takeWhile #-}
takeWhile = IV.takeWhile

-- | Drop the longest prefix of elements that satisfy the predicate.
dropWhile :: Storable a => (a -> Bool) -> Vector a -> Vector a
{-# INLINE dropWhile #-}
dropWhile = IV.dropWhile

-- Searching
-- ---------

infix 4 `elem`
-- | Check whether the vector contains an element
elem :: (Storable a, Eq a) => a -> Vector a -> Bool
{-# INLINE elem #-}
elem = IV.elem

infix 4 `notElem`
-- | Inverse of `elem`
notElem :: (Storable a, Eq a) => a -> Vector a -> Bool
{-# INLINE notElem #-}
notElem = IV.notElem

-- | Yield 'Just' the first element matching the predicate or 'Nothing' if no
-- such element exists.
find :: Storable a => (a -> Bool) -> Vector a -> Maybe a
{-# INLINE find #-}
find = IV.find

-- | Yield 'Just' the index of the first element matching the predicate or
-- 'Nothing' if no such element exists.
findIndex :: Storable a => (a -> Bool) -> Vector a -> Maybe Int
{-# INLINE findIndex #-}
findIndex = IV.findIndex

-- Folding
-- -------

-- | Left fold
foldl :: Storable b => (a -> b -> a) -> a -> Vector b -> a
{-# INLINE foldl #-}
foldl = IV.foldl

-- | Lefgt fold on non-empty vectors
foldl1 :: Storable a => (a -> a -> a) -> Vector a -> a
{-# INLINE foldl1 #-}
foldl1 = IV.foldl1

-- | Left fold with strict accumulator
foldl' :: Storable b => (a -> b -> a) -> a -> Vector b -> a
{-# INLINE foldl' #-}
foldl' = IV.foldl'

-- | Left fold on non-empty vectors with strict accumulator
foldl1' :: Storable a => (a -> a -> a) -> Vector a -> a
{-# INLINE foldl1' #-}
foldl1' = IV.foldl1'

-- | Right fold
foldr :: Storable a => (a -> b -> b) -> b -> Vector a -> b
{-# INLINE foldr #-}
foldr = IV.foldr

-- | Right fold on non-empty vectors
foldr1 :: Storable a => (a -> a -> a) -> Vector a -> a
{-# INLINE foldr1 #-}
foldr1 = IV.foldr1

-- Specialised folds
-- -----------------

and :: Vector Bool -> Bool
{-# INLINE and #-}
and = IV.and

or :: Vector Bool -> Bool
{-# INLINE or #-}
or = IV.or

sum :: (Storable a, Num a) => Vector a -> a
{-# INLINE sum #-}
sum = IV.sum

product :: (Storable a, Num a) => Vector a -> a
{-# INLINE product #-}
product = IV.product

maximum :: (Storable a, Ord a) => Vector a -> a
{-# INLINE maximum #-}
maximum = IV.maximum

minimum :: (Storable a, Ord a) => Vector a -> a
{-# INLINE minimum #-}
minimum = IV.minimum

-- Unfolding
-- ---------

unfoldr :: Storable a => (b -> Maybe (a, b)) -> b -> Vector a
{-# INLINE unfoldr #-}
unfoldr = IV.unfoldr

-- Scans
-- -----

-- | Prefix scan
prescanl :: (Storable a, Storable b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE prescanl #-}
prescanl = IV.prescanl

-- | Prefix scan with strict accumulator
prescanl' :: (Storable a, Storable b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE prescanl' #-}
prescanl' = IV.prescanl'

-- | Suffix scan
postscanl :: (Storable a, Storable b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE postscanl #-}
postscanl = IV.postscanl

-- | Suffix scan with strict accumulator
postscanl' :: (Storable a, Storable b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE postscanl' #-}
postscanl' = IV.postscanl'

-- | Haskell-style scan
scanl :: (Storable a, Storable b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE scanl #-}
scanl = IV.scanl

-- | Haskell-style scan with strict accumulator
scanl' :: (Storable a, Storable b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE scanl' #-}
scanl' = IV.scanl'

-- | Scan over a non-empty 'Vector'
scanl1 :: Storable a => (a -> a -> a) -> Vector a -> Vector a
{-# INLINE scanl1 #-}
scanl1 = IV.scanl1

-- | Scan over a non-empty 'Vector' with a strict accumulator
scanl1' :: Storable a => (a -> a -> a) -> Vector a -> Vector a
{-# INLINE scanl1' #-}
scanl1' = IV.scanl1'

-- Enumeration
-- -----------

enumFromTo :: (Storable a, Enum a) => a -> a -> Vector a
{-# INLINE enumFromTo #-}
enumFromTo = IV.enumFromTo

enumFromThenTo :: (Storable a, Enum a) => a -> a -> a -> Vector a
{-# INLINE enumFromThenTo #-}
enumFromThenTo = IV.enumFromThenTo

-- Conversion to/from lists
-- ------------------------

-- | Convert a vector to a list
toList :: Storable a => Vector a -> [a]
{-# INLINE toList #-}
toList = IV.toList

-- | Convert a list to a vector
fromList :: Storable a => [a] -> Vector a
{-# INLINE fromList #-}
fromList = IV.fromList

