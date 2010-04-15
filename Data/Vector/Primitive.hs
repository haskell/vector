{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies, ScopedTypeVariables #-}

-- |
-- Module      : Data.Vector.Primitive
-- Copyright   : (c) Roman Leshchinskiy 2008-2010
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
-- 
-- Unboxed vectors of primitive types.
--

module Data.Vector.Primitive (
  Vector, MVector(..), Prim,

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
  accum, accumulate_, (//), update_, backpermute, reverse,
  unsafeAccum, unsafeAccumulate_,
  unsafeUpd, unsafeUpdate_,
  unsafeBackpermute,

  -- * Mapping
  map, imap, concatMap,

  -- * Zipping and unzipping
  zipWith, zipWith3, zipWith4, zipWith5, zipWith6,
  izipWith, izipWith3, izipWith4, izipWith5, izipWith6,

  -- * Filtering
  filter, ifilter, takeWhile, dropWhile,
  partition, unstablePartition, span, break,

  -- * Searching
  elem, notElem, find, findIndex, findIndices, elemIndex, elemIndices,

  -- * Folding
  foldl, foldl1, foldl', foldl1', foldr, foldr1, foldr', foldr1',
  ifoldl, ifoldl', ifoldr, ifoldr',

  -- * Specialised folds
  all, any,
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
  toList, fromList, fromListN
) where

import qualified Data.Vector.Generic           as G
import           Data.Vector.Primitive.Mutable ( MVector(..) )
import qualified Data.Vector.Fusion.Stream as Stream
import           Data.Primitive.ByteArray
import           Data.Primitive ( Prim, sizeOf )

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
                        all, any, sum, product, minimum, maximum,
                        scanl, scanl1, scanr, scanr1,
                        enumFromTo, enumFromThenTo )

import qualified Prelude

import Data.Typeable ( Typeable )
import Data.Data     ( Data(..) )

-- | Unboxed vectors of primitive types
data Vector a = Vector {-# UNPACK #-} !Int
                       {-# UNPACK #-} !Int
                       {-# UNPACK #-} !ByteArray
  deriving ( Typeable )

instance (Show a, Prim a) => Show (Vector a) where
    show = (Prelude.++ " :: Data.Vector.Primitive.Vector") . ("fromList " Prelude.++) . show . toList

instance (Data a, Prim a) => Data (Vector a) where
  gfoldl       = G.gfoldl
  toConstr _   = error "toConstr"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = G.mkType "Data.Vector.Primitive.Vector"
  dataCast1    = G.dataCast


type instance G.Mutable Vector = MVector

instance Prim a => G.Vector Vector a where
  {-# INLINE unsafeFreeze #-}
  unsafeFreeze (MVector i n marr)
    = Vector i n `liftM` unsafeFreezeByteArray marr

  {-# INLINE basicLength #-}
  basicLength (Vector _ n _) = n

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice j n (Vector i _ arr) = Vector (i+j) n arr

  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (Vector i _ arr) j = return (indexByteArray arr (i+j))

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MVector i n dst) (Vector j _ src)
    = memcpyByteArray' dst (i * sz) src (j * sz) (n * sz)
    where
      sz = sizeOf (undefined :: a)

  {-# INLINE elemseq #-}
  elemseq _ = seq

-- See [HACKS:Eq and Ord instances]
instance (Prim a, Eq a) => Eq (Vector a) where
  {-# INLINE (==) #-}
  xs == ys = Stream.eq (G.stream xs) (G.stream ys)

  {-# INLINE (/=) #-}
  xs /= ys = not (Stream.eq (G.stream xs) (G.stream ys))

-- See [HACKS:Eq and Ord instances]
instance (Prim a, Ord a) => Ord (Vector a) where
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

length :: Prim a => Vector a -> Int
{-# INLINE length #-}
length = G.length

null :: Prim a => Vector a -> Bool
{-# INLINE null #-}
null = G.null

-- Construction
-- ------------

-- | Empty vector
empty :: Prim a => Vector a
{-# INLINE empty #-}
empty = G.empty

-- | Vector with exaclty one element
singleton :: Prim a => a -> Vector a
{-# INLINE singleton #-}
singleton = G.singleton

-- | Vector of the given length with the given value in each position
replicate :: Prim a => Int -> a -> Vector a
{-# INLINE replicate #-}
replicate = G.replicate

-- | Generate a vector of the given length by applying the function to each
-- index
generate :: Prim a => Int -> (Int -> a) -> Vector a
{-# INLINE generate #-}
generate = G.generate

-- | Prepend an element
cons :: Prim a => a -> Vector a -> Vector a
{-# INLINE cons #-}
cons = G.cons

-- | Append an element
snoc :: Prim a => Vector a -> a -> Vector a
{-# INLINE snoc #-}
snoc = G.snoc

infixr 5 ++
-- | Concatenate two vectors
(++) :: Prim a => Vector a -> Vector a -> Vector a
{-# INLINE (++) #-}
(++) = (G.++)

-- | Create a copy of a vector. Useful when dealing with slices.
force :: Prim a => Vector a -> Vector a
{-# INLINE force #-}
force = G.force

-- Accessing individual elements
-- -----------------------------

-- | Indexing
(!) :: Prim a => Vector a -> Int -> a
{-# INLINE (!) #-}
(!) = (G.!)

-- | First element
head :: Prim a => Vector a -> a
{-# INLINE head #-}
head = G.head

-- | Last element
last :: Prim a => Vector a -> a
{-# INLINE last #-}
last = G.last

-- | Unsafe indexing without bounds checking
unsafeIndex :: Prim a => Vector a -> Int -> a
{-# INLINE unsafeIndex #-}
unsafeIndex = G.unsafeIndex

-- | Yield the first element of a vector without checking if the vector is
-- empty
unsafeHead :: Prim a => Vector a -> a
{-# INLINE unsafeHead #-}
unsafeHead = G.unsafeHead

-- | Yield the last element of a vector without checking if the vector is
-- empty
unsafeLast :: Prim a => Vector a -> a
{-# INLINE unsafeLast #-}
unsafeLast = G.unsafeLast

-- | Monadic indexing which can be strict in the vector while remaining lazy in
-- the element
indexM :: (Prim a, Monad m) => Vector a -> Int -> m a
{-# INLINE indexM #-}
indexM = G.indexM

headM :: (Prim a, Monad m) => Vector a -> m a
{-# INLINE headM #-}
headM = G.headM

lastM :: (Prim a, Monad m) => Vector a -> m a
{-# INLINE lastM #-}
lastM = G.lastM

-- | Unsafe monadic indexing without bounds checks
unsafeIndexM :: (Prim a, Monad m) => Vector a -> Int -> m a
{-# INLINE unsafeIndexM #-}
unsafeIndexM = G.unsafeIndexM

unsafeHeadM :: (Prim a, Monad m) => Vector a -> m a
{-# INLINE unsafeHeadM #-}
unsafeHeadM = G.unsafeHeadM

unsafeLastM :: (Prim a, Monad m) => Vector a -> m a
{-# INLINE unsafeLastM #-}
unsafeLastM = G.unsafeLastM

-- Subarrays
-- ---------

-- | Yield a part of the vector without copying it. Safer version of
-- 'basicUnsafeSlice'.
slice :: Prim a => Int   -- ^ starting index
                -> Int   -- ^ length
                -> Vector a
                -> Vector a
{-# INLINE slice #-}
slice = G.slice

-- | Yield all but the last element without copying.
init :: Prim a => Vector a -> Vector a
{-# INLINE init #-}
init = G.init

-- | All but the first element (without copying).
tail :: Prim a => Vector a -> Vector a
{-# INLINE tail #-}
tail = G.tail

-- | Yield the first @n@ elements without copying.
take :: Prim a => Int -> Vector a -> Vector a
{-# INLINE take #-}
take = G.take

-- | Yield all but the first @n@ elements without copying.
drop :: Prim a => Int -> Vector a -> Vector a
{-# INLINE drop #-}
drop = G.drop

-- | Unsafely yield a part of the vector without copying it and without
-- performing bounds checks.
unsafeSlice :: Prim a => Int   -- ^ starting index
                      -> Int   -- ^ length
                      -> Vector a
                      -> Vector a
{-# INLINE unsafeSlice #-}
unsafeSlice = G.unsafeSlice

unsafeInit :: Prim a => Vector a -> Vector a
{-# INLINE unsafeInit #-}
unsafeInit = G.unsafeInit

unsafeTail :: Prim a => Vector a -> Vector a
{-# INLINE unsafeTail #-}
unsafeTail = G.unsafeTail

unsafeTake :: Prim a => Int -> Vector a -> Vector a
{-# INLINE unsafeTake #-}
unsafeTake = G.unsafeTake

unsafeDrop :: Prim a => Int -> Vector a -> Vector a
{-# INLINE unsafeDrop #-}
unsafeDrop = G.unsafeDrop

-- Permutations
-- ------------

unsafeAccum :: Prim a => (a -> b -> a) -> Vector a -> [(Int,b)] -> Vector a
{-# INLINE unsafeAccum #-}
unsafeAccum = G.unsafeAccum

unsafeAccumulate_ :: (Prim a, Prim b) =>
               (a -> b -> a) -> Vector a -> Vector Int -> Vector b -> Vector a
{-# INLINE unsafeAccumulate_ #-}
unsafeAccumulate_ = G.unsafeAccumulate_

accum :: Prim a => (a -> b -> a) -> Vector a -> [(Int,b)] -> Vector a
{-# INLINE accum #-}
accum = G.accum

accumulate_ :: (Prim a, Prim b) =>
               (a -> b -> a) -> Vector a -> Vector Int -> Vector b -> Vector a
{-# INLINE accumulate_ #-}
accumulate_ = G.accumulate_

unsafeUpd :: Prim a => Vector a -> [(Int, a)] -> Vector a
{-# INLINE unsafeUpd #-}
unsafeUpd = G.unsafeUpd

unsafeUpdate_ :: Prim a => Vector a -> Vector Int -> Vector a -> Vector a
{-# INLINE unsafeUpdate_ #-}
unsafeUpdate_ = G.unsafeUpdate_

(//) :: Prim a => Vector a -> [(Int, a)] -> Vector a
{-# INLINE (//) #-}
(//) = (G.//)

update_ :: Prim a => Vector a -> Vector Int -> Vector a -> Vector a
{-# INLINE update_ #-}
update_ = G.update_

backpermute :: Prim a => Vector a -> Vector Int -> Vector a
{-# INLINE backpermute #-}
backpermute = G.backpermute

unsafeBackpermute :: Prim a => Vector a -> Vector Int -> Vector a
{-# INLINE unsafeBackpermute #-}
unsafeBackpermute = G.unsafeBackpermute

reverse :: Prim a => Vector a -> Vector a
{-# INLINE reverse #-}
reverse = G.reverse

-- Mapping
-- -------

-- | Map a function over a vector
map :: (Prim a, Prim b) => (a -> b) -> Vector a -> Vector b
{-# INLINE map #-}
map = G.map

-- | Apply a function to every index/value pair
imap :: (Prim a, Prim b) => (Int -> a -> b) -> Vector a -> Vector b
{-# INLINE imap #-}
imap = G.imap

concatMap :: (Prim a, Prim b) => (a -> Vector b) -> Vector a -> Vector b
{-# INLINE concatMap #-}
concatMap = G.concatMap

-- Zipping/unzipping
-- -----------------

-- | Zip two vectors with the given function.
zipWith :: (Prim a, Prim b, Prim c)
        => (a -> b -> c) -> Vector a -> Vector b -> Vector c
{-# INLINE zipWith #-}
zipWith = G.zipWith

-- | Zip three vectors with the given function.
zipWith3 :: (Prim a, Prim b, Prim c, Prim d)
         => (a -> b -> c -> d) -> Vector a -> Vector b -> Vector c -> Vector d
{-# INLINE zipWith3 #-}
zipWith3 = G.zipWith3

zipWith4 :: (Prim a, Prim b, Prim c, Prim d, Prim e)
         => (a -> b -> c -> d -> e)
         -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
{-# INLINE zipWith4 #-}
zipWith4 = G.zipWith4

zipWith5 :: (Prim a, Prim b, Prim c, Prim d, Prim e, Prim f)
         => (a -> b -> c -> d -> e -> f)
         -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
         -> Vector f
{-# INLINE zipWith5 #-}
zipWith5 = G.zipWith5

zipWith6 :: (Prim a, Prim b, Prim c, Prim d, Prim e, Prim f, Prim g)
         => (a -> b -> c -> d -> e -> f -> g)
         -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
         -> Vector f -> Vector g
{-# INLINE zipWith6 #-}
zipWith6 = G.zipWith6

-- | Zip two vectors and their indices with the given function.
izipWith :: (Prim a, Prim b, Prim c)
         => (Int -> a -> b -> c) -> Vector a -> Vector b -> Vector c
{-# INLINE izipWith #-}
izipWith = G.izipWith

-- | Zip three vectors and their indices with the given function.
izipWith3 :: (Prim a, Prim b, Prim c, Prim d)
          => (Int -> a -> b -> c -> d)
          -> Vector a -> Vector b -> Vector c -> Vector d
{-# INLINE izipWith3 #-}
izipWith3 = G.izipWith3

izipWith4 :: (Prim a, Prim b, Prim c, Prim d, Prim e)
          => (Int -> a -> b -> c -> d -> e)
          -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
{-# INLINE izipWith4 #-}
izipWith4 = G.izipWith4

izipWith5 :: (Prim a, Prim b, Prim c, Prim d, Prim e, Prim f)
          => (Int -> a -> b -> c -> d -> e -> f)
          -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
          -> Vector f
{-# INLINE izipWith5 #-}
izipWith5 = G.izipWith5

izipWith6 :: (Prim a, Prim b, Prim c, Prim d, Prim e, Prim f, Prim g)
          => (Int -> a -> b -> c -> d -> e -> f -> g)
          -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
          -> Vector f -> Vector g
{-# INLINE izipWith6 #-}
izipWith6 = G.izipWith6

-- Filtering
-- ---------

-- | Drop elements which do not satisfy the predicate
filter :: Prim a => (a -> Bool) -> Vector a -> Vector a
{-# INLINE filter #-}
filter = G.filter

-- | Drop elements that do not satisfy the predicate (applied to values and
-- their indices)
ifilter :: Prim a => (Int -> a -> Bool) -> Vector a -> Vector a
{-# INLINE ifilter #-}
ifilter = G.ifilter

-- | Yield the longest prefix of elements satisfying the predicate.
takeWhile :: Prim a => (a -> Bool) -> Vector a -> Vector a
{-# INLINE takeWhile #-}
takeWhile = G.takeWhile

-- | Drop the longest prefix of elements that satisfy the predicate.
dropWhile :: Prim a => (a -> Bool) -> Vector a -> Vector a
{-# INLINE dropWhile #-}
dropWhile = G.dropWhile

-- | Split the vector in two parts, the first one containing those elements
-- that satisfy the predicate and the second one those that don't. The
-- relative order of the elements is preserved at the cost of a (sometimes)
-- reduced performance compared to 'unstablePartition'.
partition :: Prim a => (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE partition #-}
partition = G.partition

-- | Split the vector in two parts, the first one containing those elements
-- that satisfy the predicate and the second one those that don't. The order
-- of the elements is not preserved.
unstablePartition :: Prim a => (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE unstablePartition #-}
unstablePartition = G.unstablePartition

-- | Split the vector into the longest prefix of elements that satisfy the
-- predicate and the rest.
span :: Prim a => (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE span #-}
span = G.span

-- | Split the vector into the longest prefix of elements that do not satisfy
-- the predicate and the rest.
break :: Prim a => (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE break #-}
break = G.break

-- Searching
-- ---------

infix 4 `elem`
-- | Check whether the vector contains an element
elem :: (Prim a, Eq a) => a -> Vector a -> Bool
{-# INLINE elem #-}
elem = G.elem

infix 4 `notElem`
-- | Inverse of `elem`
notElem :: (Prim a, Eq a) => a -> Vector a -> Bool
{-# INLINE notElem #-}
notElem = G.notElem

-- | Yield 'Just' the first element matching the predicate or 'Nothing' if no
-- such element exists.
find :: Prim a => (a -> Bool) -> Vector a -> Maybe a
{-# INLINE find #-}
find = G.find

-- | Yield 'Just' the index of the first element matching the predicate or
-- 'Nothing' if no such element exists.
findIndex :: Prim a => (a -> Bool) -> Vector a -> Maybe Int
{-# INLINE findIndex #-}
findIndex = G.findIndex

-- | Yield the indices of elements satisfying the predicate
findIndices :: Prim a => (a -> Bool) -> Vector a -> Vector Int
{-# INLINE findIndices #-}
findIndices = G.findIndices

-- | Yield 'Just' the index of the first occurence of the given element or
-- 'Nothing' if the vector does not contain the element
elemIndex :: (Prim a, Eq a) => a -> Vector a -> Maybe Int
{-# INLINE elemIndex #-}
elemIndex = G.elemIndex

-- | Yield the indices of all occurences of the given element
elemIndices :: (Prim a, Eq a) => a -> Vector a -> Vector Int
{-# INLINE elemIndices #-}
elemIndices = G.elemIndices

-- Folding
-- -------

-- | Left fold
foldl :: Prim b => (a -> b -> a) -> a -> Vector b -> a
{-# INLINE foldl #-}
foldl = G.foldl

-- | Lefgt fold on non-empty vectors
foldl1 :: Prim a => (a -> a -> a) -> Vector a -> a
{-# INLINE foldl1 #-}
foldl1 = G.foldl1

-- | Left fold with strict accumulator
foldl' :: Prim b => (a -> b -> a) -> a -> Vector b -> a
{-# INLINE foldl' #-}
foldl' = G.foldl'

-- | Left fold on non-empty vectors with strict accumulator
foldl1' :: Prim a => (a -> a -> a) -> Vector a -> a
{-# INLINE foldl1' #-}
foldl1' = G.foldl1'

-- | Right fold
foldr :: Prim a => (a -> b -> b) -> b -> Vector a -> b
{-# INLINE foldr #-}
foldr = G.foldr

-- | Right fold on non-empty vectors
foldr1 :: Prim a => (a -> a -> a) -> Vector a -> a
{-# INLINE foldr1 #-}
foldr1 = G.foldr1

-- | Right fold with a strict accumulator
foldr' :: Prim a => (a -> b -> b) -> b -> Vector a -> b
{-# INLINE foldr' #-}
foldr' = G.foldr'

-- | Right fold on non-empty vectors with strict accumulator
foldr1' :: Prim a => (a -> a -> a) -> Vector a -> a
{-# INLINE foldr1' #-}
foldr1' = G.foldr1'

-- | Left fold (function applied to each element and its index)
ifoldl :: Prim b => (a -> Int -> b -> a) -> a -> Vector b -> a
{-# INLINE ifoldl #-}
ifoldl = G.ifoldl

-- | Left fold with strict accumulator (function applied to each element and
-- its index)
ifoldl' :: Prim b => (a -> Int -> b -> a) -> a -> Vector b -> a
{-# INLINE ifoldl' #-}
ifoldl' = G.ifoldl'

-- | Right fold (function applied to each element and its index)
ifoldr :: Prim a => (Int -> a -> b -> b) -> b -> Vector a -> b
{-# INLINE ifoldr #-}
ifoldr = G.ifoldr

-- | Right fold with strict accumulator (function applied to each element and
-- its index)
ifoldr' :: Prim a => (Int -> a -> b -> b) -> b -> Vector a -> b
{-# INLINE ifoldr' #-}
ifoldr' = G.ifoldr'

-- Specialised folds
-- -----------------

all :: Prim a => (a -> Bool) -> Vector a -> Bool
{-# INLINE all #-}
all = G.all

any :: Prim a => (a -> Bool) -> Vector a -> Bool
{-# INLINE any #-}
any = G.any

sum :: (Prim a, Num a) => Vector a -> a
{-# INLINE sum #-}
sum = G.sum

product :: (Prim a, Num a) => Vector a -> a
{-# INLINE product #-}
product = G.product

maximum :: (Prim a, Ord a) => Vector a -> a
{-# INLINE maximum #-}
maximum = G.maximum

maximumBy :: Prim a => (a -> a -> Ordering) -> Vector a -> a
{-# INLINE maximumBy #-}
maximumBy = G.maximumBy

minimum :: (Prim a, Ord a) => Vector a -> a
{-# INLINE minimum #-}
minimum = G.minimum

minimumBy :: Prim a => (a -> a -> Ordering) -> Vector a -> a
{-# INLINE minimumBy #-}
minimumBy = G.minimumBy

maxIndex :: (Prim a, Ord a) => Vector a -> Int
{-# INLINE maxIndex #-}
maxIndex = G.maxIndex

maxIndexBy :: Prim a => (a -> a -> Ordering) -> Vector a -> Int
{-# INLINE maxIndexBy #-}
maxIndexBy = G.maxIndexBy

minIndex :: (Prim a, Ord a) => Vector a -> Int
{-# INLINE minIndex #-}
minIndex = G.minIndex

minIndexBy :: Prim a => (a -> a -> Ordering) -> Vector a -> Int
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
unfoldr :: Prim a => (b -> Maybe (a, b)) -> b -> Vector a
{-# INLINE unfoldr #-}
unfoldr = G.unfoldr

-- | Unfold at most @n@ elements
unfoldrN :: Prim a => Int -> (b -> Maybe (a, b)) -> b -> Vector a
{-# INLINE unfoldrN #-}
unfoldrN = G.unfoldrN

-- Scans
-- -----

-- | Prefix scan
prescanl :: (Prim a, Prim b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE prescanl #-}
prescanl = G.prescanl

-- | Prefix scan with strict accumulator
prescanl' :: (Prim a, Prim b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE prescanl' #-}
prescanl' = G.prescanl'

-- | Suffix scan
postscanl :: (Prim a, Prim b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE postscanl #-}
postscanl = G.postscanl

-- | Suffix scan with strict accumulator
postscanl' :: (Prim a, Prim b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE postscanl' #-}
postscanl' = G.postscanl'

-- | Haskell-style scan
scanl :: (Prim a, Prim b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE scanl #-}
scanl = G.scanl

-- | Haskell-style scan with strict accumulator
scanl' :: (Prim a, Prim b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE scanl' #-}
scanl' = G.scanl'

-- | Scan over a non-empty 'Vector'
scanl1 :: Prim a => (a -> a -> a) -> Vector a -> Vector a
{-# INLINE scanl1 #-}
scanl1 = G.scanl1

-- | Scan over a non-empty 'Vector' with a strict accumulator
scanl1' :: Prim a => (a -> a -> a) -> Vector a -> Vector a
{-# INLINE scanl1' #-}
scanl1' = G.scanl1'


-- | Prefix right-to-left scan
prescanr :: (Prim a, Prim b) => (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE prescanr #-}
prescanr = G.prescanr

-- | Prefix right-to-left scan with strict accumulator
prescanr' :: (Prim a, Prim b) => (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE prescanr' #-}
prescanr' = G.prescanr'

-- | Suffix right-to-left scan
postscanr :: (Prim a, Prim b) => (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE postscanr #-}
postscanr = G.postscanr

-- | Suffix right-to-left scan with strict accumulator
postscanr' :: (Prim a, Prim b) => (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE postscanr' #-}
postscanr' = G.postscanr'

-- | Haskell-style right-to-left scan
scanr :: (Prim a, Prim b) => (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE scanr #-}
scanr = G.scanr

-- | Haskell-style right-to-left scan with strict accumulator
scanr' :: (Prim a, Prim b) => (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE scanr' #-}
scanr' = G.scanr'

-- | Right-to-left scan over a non-empty vector
scanr1 :: Prim a => (a -> a -> a) -> Vector a -> Vector a
{-# INLINE scanr1 #-}
scanr1 = G.scanr1

-- | Right-to-left scan over a non-empty vector with a strict accumulator
scanr1' :: Prim a => (a -> a -> a) -> Vector a -> Vector a
{-# INLINE scanr1' #-}
scanr1' = G.scanr1'

-- Enumeration
-- -----------

-- | Yield a vector of the given length containing the values @x@, @x+1@ etc.
-- This operation is usually more efficient than 'enumFromTo'.
enumFromN :: (Prim a, Num a) => a -> Int -> Vector a
{-# INLINE enumFromN #-}
enumFromN = G.enumFromN

-- | Yield a vector of the given length containing the values @x@, @x+y@,
-- @x+y+y@ etc. This operations is usually more efficient than
-- 'enumFromThenTo'.
enumFromStepN :: (Prim a, Num a) => a -> a -> Int -> Vector a
{-# INLINE enumFromStepN #-}
enumFromStepN = G.enumFromStepN

-- | Enumerate values from @x@ to @y@.
--
-- /WARNING:/ This operation can be very inefficient. If at all possible, use
-- 'enumFromN' instead.
enumFromTo :: (Prim a, Enum a) => a -> a -> Vector a
{-# INLINE enumFromTo #-}
enumFromTo = G.enumFromTo

-- | Enumerate values from @x@ to @y@ with a specific step @z@.
--
-- /WARNING:/ This operation can be very inefficient. If at all possible, use
-- 'enumFromStepN' instead.
enumFromThenTo :: (Prim a, Enum a) => a -> a -> a -> Vector a
{-# INLINE enumFromThenTo #-}
enumFromThenTo = G.enumFromThenTo

-- Conversion to/from lists
-- ------------------------

-- | Convert a vector to a list
toList :: Prim a => Vector a -> [a]
{-# INLINE toList #-}
toList = G.toList

-- | Convert a list to a vector
fromList :: Prim a => [a] -> Vector a
{-# INLINE fromList #-}
fromList = G.fromList

-- | Convert the first @n@ elements of a list to a vector
--
-- > fromListN n xs = fromList (take n xs)
fromListN :: Prim a => Int -> [a] -> Vector a
{-# INLINE fromListN #-}
fromListN = G.fromListN

