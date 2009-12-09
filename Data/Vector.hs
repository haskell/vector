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
-- Boxed vectors
--

module Data.Vector (
  Vector, MVector,

  -- * Length information
  length, null,

  -- * Construction
  empty, singleton, cons, snoc, replicate, generate, (++), copy,

  -- * Accessing individual elements
  (!), head, last, indexM, headM, lastM,
  unsafeIndex, unsafeHead, unsafeLast,
  unsafeIndexM, unsafeHeadM, unsafeLastM,

  -- * Subvectors
  slice, init, tail, take, drop,
  unsafeSlice, unsafeInit, unsafeTail,

  -- * Permutations
  accum, accumulate, accumulate_,
  (//), update, update_,
  backpermute, reverse,

  -- * Mapping
  map, imap, concatMap,

  -- * Zipping and unzipping
  zipWith, zipWith3, zipWith4, zipWith5, zipWith6,
  izipWith, izipWith3, izipWith4, izipWith5, izipWith6,
  zip, zip3, zip4, zip5, zip6,
  unzip, unzip3, unzip4, unzip5, unzip6,

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
  unsafeAccum, unsafeAccumulate, unsafeAccumulate_,
  unsafeUpd, unsafeUpdate, unsafeUpdate_
) where

import qualified Data.Vector.Generic as G
import           Data.Vector.Mutable  ( MVector(..) )
import           Data.Primitive.Array

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
                        scanl, scanl1,
                        enumFromTo, enumFromThenTo )

import qualified Prelude

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

instance Eq a => Eq (Vector a) where
  {-# INLINE (==) #-}
  (==) = G.eq

instance Ord a => Ord (Vector a) where
  {-# INLINE compare #-}
  compare = G.cmp

-- Length
-- ------

length :: Vector a -> Int
{-# INLINE length #-}
length = G.length

null :: Vector a -> Bool
{-# INLINE null #-}
null = G.null

-- Construction
-- ------------

-- | Empty vector
empty :: Vector a
{-# INLINE empty #-}
empty = G.empty

-- | Vector with exaclty one element
singleton :: a -> Vector a
{-# INLINE singleton #-}
singleton = G.singleton

-- | Vector of the given length with the given value in each position
replicate :: Int -> a -> Vector a
{-# INLINE replicate #-}
replicate = G.replicate

-- | Generate a vector of the given length by applying the function to each
-- index
generate :: Int -> (Int -> a) -> Vector a
{-# INLINE generate #-}
generate = G.generate

-- | Prepend an element
cons :: a -> Vector a -> Vector a
{-# INLINE cons #-}
cons = G.cons

-- | Append an element
snoc :: Vector a -> a -> Vector a
{-# INLINE snoc #-}
snoc = G.snoc

infixr 5 ++
-- | Concatenate two vectors
(++) :: Vector a -> Vector a -> Vector a
{-# INLINE (++) #-}
(++) = (G.++)

-- | Create a copy of a vector. Useful when dealing with slices.
copy :: Vector a -> Vector a
{-# INLINE copy #-}
copy = G.copy

-- Accessing individual elements
-- -----------------------------

-- | Indexing
(!) :: Vector a -> Int -> a
{-# INLINE (!) #-}
(!) = (G.!)

-- | First element
head :: Vector a -> a
{-# INLINE head #-}
head = G.head

-- | Last element
last :: Vector a -> a
{-# INLINE last #-}
last = G.last

-- | Unsafe indexing without bounds checking
unsafeIndex :: Vector a -> Int -> a
{-# INLINE unsafeIndex #-}
unsafeIndex = G.unsafeIndex

-- | Yield the first element of a vector without checking if the vector is
-- empty
unsafeHead :: Vector a -> a
{-# INLINE unsafeHead #-}
unsafeHead = G.unsafeHead

-- | Yield the last element of a vector without checking if the vector is
-- empty
unsafeLast :: Vector a -> a
{-# INLINE unsafeLast #-}
unsafeLast = G.unsafeLast

-- | Monadic indexing which can be strict in the vector while remaining lazy in
-- the element
indexM :: Monad m => Vector a -> Int -> m a
{-# INLINE indexM #-}
indexM = G.indexM

headM :: Monad m => Vector a -> m a
{-# INLINE headM #-}
headM = G.headM

lastM :: Monad m => Vector a -> m a
{-# INLINE lastM #-}
lastM = G.lastM

-- | Unsafe monadic indexing without bounds checks
unsafeIndexM :: Monad m => Vector a -> Int -> m a
{-# INLINE unsafeIndexM #-}
unsafeIndexM = G.unsafeIndexM

unsafeHeadM :: Monad m => Vector a -> m a
{-# INLINE unsafeHeadM #-}
unsafeHeadM = G.unsafeHeadM

unsafeLastM :: Monad m => Vector a -> m a
{-# INLINE unsafeLastM #-}
unsafeLastM = G.unsafeLastM

-- Subarrays
-- ---------

-- | Yield a part of the vector without copying it. Safer version of
-- 'basicUnsafeSlice'.
slice :: Int   -- ^ starting index
      -> Int   -- ^ length
      -> Vector a
      -> Vector a
{-# INLINE slice #-}
slice = G.slice

-- | Yield all but the last element without copying.
init :: Vector a -> Vector a
{-# INLINE init #-}
init = G.init

-- | All but the first element (without copying).
tail :: Vector a -> Vector a
{-# INLINE tail #-}
tail = G.tail

-- | Yield the first @n@ elements without copying.
take :: Int -> Vector a -> Vector a
{-# INLINE take #-}
take = G.take

-- | Yield all but the first @n@ elements without copying.
drop :: Int -> Vector a -> Vector a
{-# INLINE drop #-}
drop = G.drop

unsafeInit :: Vector a -> Vector a
{-# INLINE unsafeInit #-}
unsafeInit = G.unsafeInit

unsafeTail :: Vector a -> Vector a
{-# INLINE unsafeTail #-}
unsafeTail = G.unsafeTail

-- | Unsafely yield a part of the vector without copying it and without
-- performing bounds checks.
unsafeSlice :: Int   -- ^ starting index
            -> Int   -- ^ length
            -> Vector a
            -> Vector a
{-# INLINE unsafeSlice #-}
unsafeSlice = G.unsafeSlice

-- Permutations
-- ------------

unsafeAccum :: (a -> b -> a) -> Vector a -> [(Int,b)] -> Vector a
{-# INLINE unsafeAccum #-}
unsafeAccum = G.unsafeAccum

unsafeAccumulate :: (a -> b -> a) -> Vector a -> Vector (Int,b) -> Vector a
{-# INLINE unsafeAccumulate #-}
unsafeAccumulate = G.unsafeAccumulate

unsafeAccumulate_
  :: (a -> b -> a) -> Vector a -> Vector Int -> Vector b -> Vector a
{-# INLINE unsafeAccumulate_ #-}
unsafeAccumulate_ = G.unsafeAccumulate_

accum :: (a -> b -> a) -> Vector a -> [(Int,b)] -> Vector a
{-# INLINE accum #-}
accum = G.accum

accumulate :: (a -> b -> a) -> Vector a -> Vector (Int,b) -> Vector a
{-# INLINE accumulate #-}
accumulate = G.accumulate

accumulate_ :: (a -> b -> a) -> Vector a -> Vector Int -> Vector b -> Vector a
{-# INLINE accumulate_ #-}
accumulate_ = G.accumulate_

unsafeUpd :: Vector a -> [(Int, a)] -> Vector a
{-# INLINE unsafeUpd #-}
unsafeUpd = G.unsafeUpd

unsafeUpdate :: Vector a -> Vector (Int, a) -> Vector a
{-# INLINE unsafeUpdate #-}
unsafeUpdate = G.unsafeUpdate

unsafeUpdate_ :: Vector a -> Vector Int -> Vector a -> Vector a
{-# INLINE unsafeUpdate_ #-}
unsafeUpdate_ = G.unsafeUpdate_

(//) :: Vector a -> [(Int, a)] -> Vector a
{-# INLINE (//) #-}
(//) = (G.//)

update :: Vector a -> Vector (Int, a) -> Vector a
{-# INLINE update #-}
update = G.update

update_ :: Vector a -> Vector Int -> Vector a -> Vector a
{-# INLINE update_ #-}
update_ = G.update_

backpermute :: Vector a -> Vector Int -> Vector a
{-# INLINE backpermute #-}
backpermute = G.backpermute

reverse :: Vector a -> Vector a
{-# INLINE reverse #-}
reverse = G.reverse

-- Mapping
-- -------

-- | Map a function over a vector
map :: (a -> b) -> Vector a -> Vector b
{-# INLINE map #-}
map = G.map

-- | Apply a function to every index/value pair
imap :: (Int -> a -> b) -> Vector a -> Vector b
{-# INLINE imap #-}
imap = G.imap

concatMap :: (a -> Vector b) -> Vector a -> Vector b
{-# INLINE concatMap #-}
concatMap = G.concatMap

-- Zipping/unzipping
-- -----------------

-- | Zip two vectors with the given function.
zipWith :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
{-# INLINE zipWith #-}
zipWith = G.zipWith

-- | Zip three vectors with the given function.
zipWith3 :: (a -> b -> c -> d) -> Vector a -> Vector b -> Vector c -> Vector d
{-# INLINE zipWith3 #-}
zipWith3 = G.zipWith3

zipWith4 :: (a -> b -> c -> d -> e)
          -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
{-# INLINE zipWith4 #-}
zipWith4 = G.zipWith4

zipWith5 :: (a -> b -> c -> d -> e -> f)
          -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
          -> Vector f
{-# INLINE zipWith5 #-}
zipWith5 = G.zipWith5

zipWith6 :: (a -> b -> c -> d -> e -> f -> g)
          -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
          -> Vector f -> Vector g
{-# INLINE zipWith6 #-}
zipWith6 = G.zipWith6

-- | Zip two vectors and their indices with the given function.
izipWith :: (Int -> a -> b -> c) -> Vector a -> Vector b -> Vector c
{-# INLINE izipWith #-}
izipWith = G.izipWith

-- | Zip three vectors and their indices with the given function.
izipWith3 :: (Int -> a -> b -> c -> d)
          -> Vector a -> Vector b -> Vector c -> Vector d
{-# INLINE izipWith3 #-}
izipWith3 = G.izipWith3

izipWith4 :: (Int -> a -> b -> c -> d -> e)
          -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
{-# INLINE izipWith4 #-}
izipWith4 = G.izipWith4

izipWith5 :: (Int -> a -> b -> c -> d -> e -> f)
          -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
          -> Vector f
{-# INLINE izipWith5 #-}
izipWith5 = G.izipWith5

izipWith6 :: (Int -> a -> b -> c -> d -> e -> f -> g)
          -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
          -> Vector f -> Vector g
{-# INLINE izipWith6 #-}
izipWith6 = G.izipWith6

zip :: Vector a -> Vector b -> Vector (a, b)
{-# INLINE zip #-}
zip = G.zip

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

-- | Drop elements which do not satisfy the predicate
filter :: (a -> Bool) -> Vector a -> Vector a
{-# INLINE filter #-}
filter = G.filter

-- | Drop elements that do not satisfy the predicate (applied to values and
-- their indices)
ifilter :: (Int -> a -> Bool) -> Vector a -> Vector a
{-# INLINE ifilter #-}
ifilter = G.ifilter

-- | Yield the longest prefix of elements satisfying the predicate.
takeWhile :: (a -> Bool) -> Vector a -> Vector a
{-# INLINE takeWhile #-}
takeWhile = G.takeWhile

-- | Drop the longest prefix of elements that satisfy the predicate.
dropWhile :: (a -> Bool) -> Vector a -> Vector a
{-# INLINE dropWhile #-}
dropWhile = G.dropWhile

-- | Split the vector in two parts, the first one containing those elements
-- that satisfy the predicate and the second one those that don't. The order
-- of the elements is not preserved.
unstablePartition :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE unstablePartition #-}
unstablePartition = G.unstablePartition

-- | Split the vector into the longest prefix of elements that satisfy the
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

-- | Lefgt fold on non-empty vectors
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

-- Specialised folds
-- -----------------

all :: (a -> Bool) -> Vector a -> Bool
{-# INLINE all #-}
all = G.all

any :: (a -> Bool) -> Vector a -> Bool
{-# INLINE any #-}
any = G.any

and :: Vector Bool -> Bool
{-# INLINE and #-}
and = G.and

or :: Vector Bool -> Bool
{-# INLINE or #-}
or = G.or

sum :: Num a => Vector a -> a
{-# INLINE sum #-}
sum = G.sum

product :: Num a => Vector a -> a
{-# INLINE product #-}
product = G.product

maximum :: Ord a => Vector a -> a
{-# INLINE maximum #-}
maximum = G.maximum

maximumBy :: (a -> a -> Ordering) -> Vector a -> a
{-# INLINE maximumBy #-}
maximumBy = G.maximumBy

minimum :: Ord a => Vector a -> a
{-# INLINE minimum #-}
minimum = G.minimum

minimumBy :: (a -> a -> Ordering) -> Vector a -> a
{-# INLINE minimumBy #-}
minimumBy = G.minimumBy

maxIndex :: Ord a => Vector a -> Int
{-# INLINE maxIndex #-}
maxIndex = G.maxIndex

maxIndexBy :: (a -> a -> Ordering) -> Vector a -> Int
{-# INLINE maxIndexBy #-}
maxIndexBy = G.maxIndexBy

minIndex :: Ord a => Vector a -> Int
{-# INLINE minIndex #-}
minIndex = G.minIndex

minIndexBy :: (a -> a -> Ordering) -> Vector a -> Int
{-# INLINE minIndexBy #-}
minIndexBy = G.minIndexBy

-- Unfolding
-- ---------

unfoldr :: (b -> Maybe (a, b)) -> b -> Vector a
{-# INLINE unfoldr #-}
unfoldr = G.unfoldr

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

-- | Haskell-style scan
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

-- Enumeration
-- -----------

enumFromTo :: Enum a => a -> a -> Vector a
{-# INLINE enumFromTo #-}
enumFromTo = G.enumFromTo

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

