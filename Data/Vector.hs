{-# LANGUAGE MagicHash, UnboxedTuples, FlexibleInstances, MultiParamTypeClasses #-}

-- |
-- Module      : Data.Vector
-- Copyright   : (c) Roman Leshchinskiy 2008
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
-- 
-- Boxed vectors
--

module Data.Vector (
  Vector,

  -- * Length information
  length, null,

  -- * Construction
  empty, singleton, cons, snoc, replicate, (++), copy,

  -- * Accessing individual elements
  (!), head, last, indexM, headM, lastM,

  -- * Subvectors
  slice, init, tail, take, drop,

  -- * Permutations
  accum, (//), update, backpermute, reverse,

  -- * Mapping
  map, concatMap,

  -- * Zipping and unzipping
  zipWith, zipWith3, zip, zip3, unzip, unzip3,

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
  scanl, scanl',

  -- * Enumeration
  enumFromTo, enumFromThenTo,

  -- * Conversion to/from lists
  toList, fromList
) where

import           Data.Vector.IVector ( IVector(..) )
import qualified Data.Vector.IVector    as IV
import qualified Data.Vector.Mutable.ST as Mut

import Control.Monad.ST ( runST )

import GHC.ST   ( ST(..) )
import GHC.Prim ( Array#, unsafeFreezeArray#, indexArray#, (+#) )
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
                        scanl,
                        enumFromTo, enumFromThenTo )

import qualified Prelude

data Vector a = Vector {-# UNPACK #-} !Int
                       {-# UNPACK #-} !Int
                                      (Array# a)

instance Show a => Show (Vector a) where
    show = (Prelude.++ " :: Data.Vector.Vector") . ("fromList " Prelude.++) . show . toList

instance IVector Vector a where
  {-# INLINE vnew #-}
  vnew init = runST (do
                       Mut.Vector i n marr# <- init
                       ST (\s# -> case unsafeFreezeArray# marr# s# of
                               (# s2#, arr# #) -> (# s2#, Vector i n arr# #)))

  {-# INLINE vlength #-}
  vlength (Vector _ n _) = n

  {-# INLINE unsafeSlice #-}
  unsafeSlice (Vector i _ arr#) j n = Vector (i+j) n arr#

  {-# INLINE unsafeIndexM #-}
  unsafeIndexM (Vector (I# i#) _ arr#) (I# j#)
    = case indexArray# arr# (i# +# j#) of (# x #) -> return x

instance Eq a => Eq (Vector a) where
  {-# INLINE (==) #-}
  (==) = IV.eq

instance Ord a => Ord (Vector a) where
  {-# INLINE compare #-}
  compare = IV.cmp

-- Length
-- ------

length :: Vector a -> Int
{-# INLINE length #-}
length = IV.length

null :: Vector a -> Bool
{-# INLINE null #-}
null = IV.null

-- Construction
-- ------------

-- | Empty vector
empty :: Vector a
{-# INLINE empty #-}
empty = IV.empty

-- | Vector with exaclty one element
singleton :: a -> Vector a
{-# INLINE singleton #-}
singleton = IV.singleton

-- | Vector of the given length with the given value in each position
replicate :: Int -> a -> Vector a
{-# INLINE replicate #-}
replicate = IV.replicate

-- | Prepend an element
cons :: a -> Vector a -> Vector a
{-# INLINE cons #-}
cons = IV.cons

-- | Append an element
snoc :: Vector a -> a -> Vector a
{-# INLINE snoc #-}
snoc = IV.snoc

infixr 5 ++
-- | Concatenate two vectors
(++) :: Vector a -> Vector a -> Vector a
{-# INLINE (++) #-}
(++) = (IV.++)

-- | Create a copy of a vector. Useful when dealing with slices.
copy :: Vector a -> Vector a
{-# INLINE copy #-}
copy = IV.copy

-- Accessing individual elements
-- -----------------------------

-- | Indexing
(!) :: Vector a -> Int -> a
{-# INLINE (!) #-}
(!) = (IV.!)

-- | First element
head :: Vector a -> a
{-# INLINE head #-}
head = IV.head

-- | Last element
last :: Vector a -> a
{-# INLINE last #-}
last = IV.last

-- | Monadic indexing which can be strict in the vector while remaining lazy in
-- the element
indexM :: Monad m => Vector a -> Int -> m a
{-# INLINE indexM #-}
indexM = IV.indexM

headM :: Monad m => Vector a -> m a
{-# INLINE headM #-}
headM = IV.headM

lastM :: Monad m => Vector a -> m a
{-# INLINE lastM #-}
lastM = IV.lastM

-- Subarrays
-- ---------

-- | Yield a part of the vector without copying it. Safer version of
-- 'unsafeSlice'.
slice :: Vector a -> Int   -- ^ starting index
                  -> Int   -- ^ length
                  -> Vector a
{-# INLINE slice #-}
slice = IV.slice

-- | Yield all but the last element without copying.
init :: Vector a -> Vector a
{-# INLINE init #-}
init = IV.init

-- | All but the first element (without copying).
tail :: Vector a -> Vector a
{-# INLINE tail #-}
tail = IV.tail

-- | Yield the first @n@ elements without copying.
take :: Int -> Vector a -> Vector a
{-# INLINE take #-}
take = IV.take

-- | Yield all but the first @n@ elements without copying.
drop :: Int -> Vector a -> Vector a
{-# INLINE drop #-}
drop = IV.drop

-- Permutations
-- ------------

accum :: (a -> b -> a) -> Vector a -> [(Int,b)] -> Vector a
{-# INLINE accum #-}
accum = IV.accum

(//) :: Vector a -> [(Int, a)] -> Vector a
{-# INLINE (//) #-}
(//) = (IV.//)

update :: Vector a -> Vector (Int, a) -> Vector a
{-# INLINE update #-}
update = IV.update

backpermute :: Vector a -> Vector Int -> Vector a
{-# INLINE backpermute #-}
backpermute = IV.backpermute

reverse :: Vector a -> Vector a
{-# INLINE reverse #-}
reverse = IV.reverse

-- Mapping
-- -------

-- | Map a function over a vector
map :: (a -> b) -> Vector a -> Vector b
{-# INLINE map #-}
map = IV.map

concatMap :: (a -> Vector b) -> Vector a -> Vector b
{-# INLINE concatMap #-}
concatMap = IV.concatMap

-- Zipping/unzipping
-- -----------------

-- | Zip two vectors with the given function.
zipWith :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
{-# INLINE zipWith #-}
zipWith = IV.zipWith

-- | Zip three vectors with the given function.
zipWith3 :: (a -> b -> c -> d) -> Vector a -> Vector b -> Vector c -> Vector d
{-# INLINE zipWith3 #-}
zipWith3 = IV.zipWith3

zip :: Vector a -> Vector b -> Vector (a, b)
{-# INLINE zip #-}
zip = IV.zip

zip3 :: Vector a -> Vector b -> Vector c -> Vector (a, b, c)
{-# INLINE zip3 #-}
zip3 = IV.zip3

unzip :: Vector (a, b) -> (Vector a, Vector b)
{-# INLINE unzip #-}
unzip = IV.unzip

unzip3 :: Vector (a, b, c) -> (Vector a, Vector b, Vector c)
{-# INLINE unzip3 #-}
unzip3 = IV.unzip3

-- Filtering
-- ---------

-- | Drop elements which do not satisfy the predicate
filter :: (a -> Bool) -> Vector a -> Vector a
{-# INLINE filter #-}
filter = IV.filter

-- | Yield the longest prefix of elements satisfying the predicate.
takeWhile :: (a -> Bool) -> Vector a -> Vector a
{-# INLINE takeWhile #-}
takeWhile = IV.takeWhile

-- | Drop the longest prefix of elements that satisfy the predicate.
dropWhile :: (a -> Bool) -> Vector a -> Vector a
{-# INLINE dropWhile #-}
dropWhile = IV.dropWhile

-- Searching
-- ---------

infix 4 `elem`
-- | Check whether the vector contains an element
elem :: Eq a => a -> Vector a -> Bool
{-# INLINE elem #-}
elem = IV.elem

infix 4 `notElem`
-- | Inverse of `elem`
notElem :: Eq a => a -> Vector a -> Bool
{-# INLINE notElem #-}
notElem = IV.notElem

-- | Yield 'Just' the first element matching the predicate or 'Nothing' if no
-- such element exists.
find :: (a -> Bool) -> Vector a -> Maybe a
{-# INLINE find #-}
find = IV.find

-- | Yield 'Just' the index of the first element matching the predicate or
-- 'Nothing' if no such element exists.
findIndex :: (a -> Bool) -> Vector a -> Maybe Int
{-# INLINE findIndex #-}
findIndex = IV.findIndex

-- Folding
-- -------

-- | Left fold
foldl :: (a -> b -> a) -> a -> Vector b -> a
{-# INLINE foldl #-}
foldl = IV.foldl

-- | Lefgt fold on non-empty vectors
foldl1 :: (a -> a -> a) -> Vector a -> a
{-# INLINE foldl1 #-}
foldl1 = IV.foldl1

-- | Left fold with strict accumulator
foldl' :: (a -> b -> a) -> a -> Vector b -> a
{-# INLINE foldl' #-}
foldl' = IV.foldl'

-- | Left fold on non-empty vectors with strict accumulator
foldl1' :: (a -> a -> a) -> Vector a -> a
{-# INLINE foldl1' #-}
foldl1' = IV.foldl1'

-- | Right fold
foldr :: (a -> b -> b) -> b -> Vector a -> b
{-# INLINE foldr #-}
foldr = IV.foldr

-- | Right fold on non-empty vectors
foldr1 :: (a -> a -> a) -> Vector a -> a
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

sum :: Num a => Vector a -> a
{-# INLINE sum #-}
sum = IV.sum

product :: Num a => Vector a -> a
{-# INLINE product #-}
product = IV.product

maximum :: Ord a => Vector a -> a
{-# INLINE maximum #-}
maximum = IV.maximum

minimum :: Ord a => Vector a -> a
{-# INLINE minimum #-}
minimum = IV.minimum

-- Unfolding
-- ---------

unfoldr :: (b -> Maybe (a, b)) -> b -> Vector a
{-# INLINE unfoldr #-}
unfoldr = IV.unfoldr

-- Scans
-- -----

-- | Prefix scan
prescanl :: (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE prescanl #-}
prescanl = IV.prescanl

-- | Prefix scan with strict accumulator
prescanl' :: (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE prescanl' #-}
prescanl' = IV.prescanl'

-- | Suffix scan
postscanl :: (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE postscanl #-}
postscanl = IV.postscanl

-- | Suffix scan with strict accumulator
postscanl' :: (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE postscanl' #-}
postscanl' = IV.postscanl'

-- | Haskell-style scan
scanl :: (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE scanl #-}
scanl = IV.scanl

-- | Haskell-style scan with strict accumulator
scanl' :: (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE scanl' #-}
scanl' = IV.scanl'

-- Enumeration
-- -----------

enumFromTo :: Enum a => a -> a -> Vector a
{-# INLINE enumFromTo #-}
enumFromTo = IV.enumFromTo

enumFromThenTo :: Enum a => a -> a -> a -> Vector a
{-# INLINE enumFromThenTo #-}
enumFromThenTo = IV.enumFromThenTo

-- Conversion to/from lists
-- ------------------------

-- | Convert a vector to a list
toList :: Vector a -> [a]
{-# INLINE toList #-}
toList = IV.toList

-- | Convert a list to a vector
fromList :: [a] -> Vector a
{-# INLINE fromList #-}
fromList = IV.fromList

