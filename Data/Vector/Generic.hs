{-# LANGUAGE Rank2Types, MultiParamTypeClasses, FlexibleContexts,
             TypeFamilies, ScopedTypeVariables #-}
-- |
-- Module      : Data.Vector.Generic
-- Copyright   : (c) Roman Leshchinskiy 2008-2010
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
-- 
-- Generic interface to pure vectors
--

module Data.Vector.Generic (
  -- * Immutable vectors
  Vector(..), Mutable,

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

  -- * Comparisons
  eq, cmp,

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

  -- * Conversion to/from Streams
  stream, unstream, streamR, unstreamR,

  -- * MVector-based initialisation
  new, copy, unsafeCopy,

  -- * Utilities for defining Data instances
  gfoldl, dataCast, mkType
) where

import           Data.Vector.Generic.Base

import           Data.Vector.Generic.Mutable ( MVector )
import qualified Data.Vector.Generic.Mutable as M

import qualified Data.Vector.Generic.New as New
import           Data.Vector.Generic.New ( New )

import qualified Data.Vector.Fusion.Stream as Stream
import           Data.Vector.Fusion.Stream ( Stream, MStream, inplace )
import qualified Data.Vector.Fusion.Stream.Monadic as MStream
import           Data.Vector.Fusion.Stream.Size
import           Data.Vector.Fusion.Util

import Control.Monad.ST ( ST, runST )
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
                        all, any, and, or, sum, product, maximum, minimum,
                        scanl, scanl1, scanr, scanr1,
                        enumFromTo, enumFromThenTo )

import Data.Typeable ( Typeable1, gcast1 )
import Data.Data ( Data, DataType, mkNorepType )

#include "vector.h"

-- Fusion
-- ------

-- | Construct a pure vector from a monadic initialiser 
new :: Vector v a => New v a -> v a
{-# INLINE_STREAM new #-}
new m = m `seq` runST (unsafeFreeze =<< New.run m)

-- | Convert a vector to a 'Stream'
stream :: Vector v a => v a -> Stream a
{-# INLINE_STREAM stream #-}
stream v = v `seq` (Stream.unfoldr get 0 `Stream.sized` Exact n)
  where
    n = length v

    -- NOTE: the False case comes first in Core so making it the recursive one
    -- makes the code easier to read
    {-# INLINE get #-}
    get i | i >= n    = Nothing
          | otherwise = case basicUnsafeIndexM v i of Box x -> Just (x, i+1)

-- | Create a vector from a 'Stream'
unstream :: Vector v a => Stream a -> v a
{-# INLINE unstream #-}
unstream s = new (New.unstream s)

{-# RULES

"stream/unstream [Vector]" forall s.
  stream (new (New.unstream s)) = s

"New.unstream/stream/new [Vector]" forall p.
  New.unstream (stream (new p)) = p

 #-}

{-# RULES

"inplace [Vector]"
  forall (f :: forall m. Monad m => MStream m a -> MStream m a) m.
  New.unstream (inplace f (stream (new m))) = New.transform f m

"uninplace [Vector]"
  forall (f :: forall m. Monad m => MStream m a -> MStream m a) m.
  stream (new (New.transform f m)) = inplace f (stream (new m))

 #-}

-- | Convert a vector to a 'Stream'
streamR :: Vector v a => v a -> Stream a
{-# INLINE_STREAM streamR #-}
streamR v = v `seq` (Stream.unfoldr get n `Stream.sized` Exact n)
  where
    n = length v

    {-# INLINE get #-}
    get 0 = Nothing
    get i = let i' = i-1
            in
            case basicUnsafeIndexM v i' of Box x -> Just (x, i')

-- | Create a vector from a 'Stream'
unstreamR :: Vector v a => Stream a -> v a
{-# INLINE unstreamR #-}
unstreamR s = new (New.unstreamR s)

{-# RULES

"streamR/unstreamR [Vector]" forall s.
  streamR (new (New.unstreamR s)) = s

"New.unstreamR/streamR/new [Vector]" forall p.
  New.unstreamR (streamR (new p)) = p

 #-}

{-# RULES

"inplace [Vector]"
  forall (f :: forall m. Monad m => MStream m a -> MStream m a) m.
  New.unstreamR (inplace f (streamR (new m))) = New.transformR f m

"uninplace [Vector]"
  forall (f :: forall m. Monad m => MStream m a -> MStream m a) m.
  streamR (new (New.transformR f m)) = inplace f (streamR (new m))

 #-}

-- | Copy an immutable vector into a mutable one. The two vectors must have
-- the same length. This is not checked.
unsafeCopy
  :: (PrimMonad m, Vector v a) => Mutable v (PrimState m) a -> v a -> m ()
{-# INLINE unsafeCopy #-}
unsafeCopy dst src = UNSAFE_CHECK(check) "unsafeCopy" "length mismatch"
                                         (M.length dst == length src)
                   $ (dst `seq` src `seq` basicUnsafeCopy dst src)
           
-- | Copy an immutable vector into a mutale one. The two vectors must have the
-- same length.
copy
  :: (PrimMonad m, Vector v a) => Mutable v (PrimState m) a -> v a -> m ()
{-# INLINE copy #-}
copy dst src = BOUNDS_CHECK(check) "copy" "length mismatch"
                                          (M.length dst == length src)
             $ unsafeCopy dst src

modify :: Vector v a => (forall mv s. MVector mv a => mv s a -> ST s ())
                                                                -> v a -> v a
{-# INLINE_STREAM modify #-}
modify p v = runST (
             do
               mv <- M.unsafeNew (length v)
               unsafeCopy mv v
               p mv
               unsafeFreeze mv)

{-# RULES

"modify/new [Vector]"
    forall (f :: forall mv s. MVector mv a => mv s a -> ST s ()) m.
  modify f (new m) = new (New.modify f m)

 #-}

-- Length
-- ------

length :: Vector v a => v a -> Int
{-# INLINE_STREAM length #-}
length v = basicLength v

{-# RULES

"length/unstream [Vector]" forall s.
  length (new (New.unstream s)) = Stream.length s

  #-}

null :: Vector v a => v a -> Bool
{-# INLINE_STREAM null #-}
null v = basicLength v == 0

{-# RULES

"null/unstream [Vector]" forall s.
  null (new (New.unstream s)) = Stream.null s

  #-}

-- Construction
-- ------------

-- | Empty vector
empty :: Vector v a => v a
{-# INLINE empty #-}
empty = unstream Stream.empty

-- | Vector with exaclty one element
singleton :: forall v a. Vector v a => a -> v a
{-# INLINE singleton #-}
singleton x = elemseq (undefined :: v a) x
            $ unstream (Stream.singleton x)

-- | Vector of the given length with the given value in each position
replicate :: forall v a. Vector v a => Int -> a -> v a
{-# INLINE replicate #-}
replicate n x = elemseq (undefined :: v a) x
              $ unstream
              $ Stream.replicate n x

-- | Generate a vector of the given length by applying the function to each
-- index
generate :: Vector v a => Int -> (Int -> a) -> v a
{-# INLINE generate #-}
generate n f = unstream (Stream.generate n f)

-- | Prepend an element
cons :: forall v a. Vector v a => a -> v a -> v a
{-# INLINE cons #-}
cons x v = elemseq (undefined :: v a) x
         $ unstream
         $ Stream.cons x
         $ stream v

-- | Append an element
snoc :: forall v a. Vector v a => v a -> a -> v a
{-# INLINE snoc #-}
snoc v x = elemseq (undefined :: v a) x
         $ unstream
         $ Stream.snoc (stream v) x

infixr 5 ++
-- | Concatenate two vectors
(++) :: Vector v a => v a -> v a -> v a
{-# INLINE (++) #-}
v ++ w = unstream (stream v Stream.++ stream w)

-- | Create a copy of a vector. Useful when dealing with slices.
force :: Vector v a => v a -> v a
{-# INLINE_STREAM force #-}
force = unstream . stream

{-# RULES

"force/unstream [Vector]" forall s.
  force (new (New.unstream s)) = new (New.unstream s)

 #-}

-- Accessing individual elements
-- -----------------------------

-- | Indexing
(!) :: Vector v a => v a -> Int -> a
{-# INLINE_STREAM (!) #-}
v ! i = BOUNDS_CHECK(checkIndex) "(!)" i (length v)
      $ unId (basicUnsafeIndexM v i)

-- | First element
head :: Vector v a => v a -> a
{-# INLINE_STREAM head #-}
head v = v ! 0

-- | Last element
last :: Vector v a => v a -> a
{-# INLINE_STREAM last #-}
last v = v ! (length v - 1)

-- | Unsafe indexing without bounds checking
unsafeIndex :: Vector v a => v a -> Int -> a
{-# INLINE_STREAM unsafeIndex #-}
unsafeIndex v i = UNSAFE_CHECK(checkIndex) "unsafeIndex" i (length v)
                $ unId (basicUnsafeIndexM v i)

-- | Yield the first element of a vector without checking if the vector is
-- empty
unsafeHead :: Vector v a => v a -> a
{-# INLINE_STREAM unsafeHead #-}
unsafeHead v = unsafeIndex v 0

-- | Yield the last element of a vector without checking if the vector is
-- empty
unsafeLast :: Vector v a => v a -> a
{-# INLINE_STREAM unsafeLast #-}
unsafeLast v = unsafeIndex v (length v - 1)

{-# RULES

"(!)/unstream [Vector]" forall i s.
  new (New.unstream s) ! i = s Stream.!! i

"head/unstream [Vector]" forall s.
  head (new (New.unstream s)) = Stream.head s

"last/unstream [Vector]" forall s.
  last (new (New.unstream s)) = Stream.last s

"unsafeIndex/unstream [Vector]" forall i s.
  unsafeIndex (new (New.unstream s)) i = s Stream.!! i

"unsafeHead/unstream [Vector]" forall s.
  unsafeHead (new (New.unstream s)) = Stream.head s

"unsafeLast/unstream [Vector]" forall s.
  unsafeLast (new (New.unstream s)) = Stream.last s

 #-}

-- | Monadic indexing which can be strict in the vector while remaining lazy in
-- the element.
indexM :: (Vector v a, Monad m) => v a -> Int -> m a
{-# INLINE_STREAM indexM #-}
indexM v i = BOUNDS_CHECK(checkIndex) "indexM" i (length v)
           $ basicUnsafeIndexM v i

headM :: (Vector v a, Monad m) => v a -> m a
{-# INLINE_STREAM headM #-}
headM v = indexM v 0

lastM :: (Vector v a, Monad m) => v a -> m a
{-# INLINE_STREAM lastM #-}
lastM v = indexM v (length v - 1)

-- | Unsafe monadic indexing without bounds checks
unsafeIndexM :: (Vector v a, Monad m) => v a -> Int -> m a
{-# INLINE_STREAM unsafeIndexM #-}
unsafeIndexM v i = UNSAFE_CHECK(checkIndex) "unsafeIndexM" i (length v)
                 $ basicUnsafeIndexM v i

unsafeHeadM :: (Vector v a, Monad m) => v a -> m a
{-# INLINE_STREAM unsafeHeadM #-}
unsafeHeadM v = unsafeIndexM v 0

unsafeLastM :: (Vector v a, Monad m) => v a -> m a
{-# INLINE_STREAM unsafeLastM #-}
unsafeLastM v = unsafeIndexM v (length v - 1)

-- FIXME: the rhs of these rules are lazy in the stream which is WRONG
{- RULES

"indexM/unstream [Vector]" forall v i s.
  indexM (new' v (New.unstream s)) i = return (s Stream.!! i)

"headM/unstream [Vector]" forall v s.
  headM (new' v (New.unstream s)) = return (Stream.head s)

"lastM/unstream [Vector]" forall v s.
  lastM (new' v (New.unstream s)) = return (Stream.last s)

 -}

-- Subarrays
-- ---------

-- FIXME: slicing doesn't work with the inplace stuff at the moment

-- | Yield a part of the vector without copying it.
slice :: Vector v a => Int   -- ^ starting index
                    -> Int   -- ^ length
                    -> v a
                    -> v a
{-# INLINE_STREAM slice #-}
slice i n v = BOUNDS_CHECK(checkSlice) "slice" i n (length v)
            $ basicUnsafeSlice i n v

-- | Yield all but the last element without copying.
init :: Vector v a => v a -> v a
{-# INLINE_STREAM init #-}
init v = slice 0 (length v - 1) v

-- | All but the first element (without copying).
tail :: Vector v a => v a -> v a
{-# INLINE_STREAM tail #-}
tail v = slice 1 (length v - 1) v

-- | Yield the first @n@ elements without copying.
take :: Vector v a => Int -> v a -> v a
{-# INLINE_STREAM take #-}
take n v = unsafeSlice 0 (delay_inline min n' (length v)) v
  where n' = max n 0

-- | Yield all but the first @n@ elements without copying.
drop :: Vector v a => Int -> v a -> v a
{-# INLINE_STREAM drop #-}
drop n v = unsafeSlice (delay_inline min n' len)
                       (delay_inline max 0 (len - n')) v
  where n' = max n 0
        len = length v

-- | Unsafely yield a part of the vector without copying it and without
-- performing bounds checks.
unsafeSlice :: Vector v a => Int   -- ^ starting index
                          -> Int   -- ^ length
                          -> v a
                          -> v a
{-# INLINE_STREAM unsafeSlice #-}
unsafeSlice i n v = UNSAFE_CHECK(checkSlice) "unsafeSlice" i n (length v)
                  $ basicUnsafeSlice i n v

unsafeInit :: Vector v a => v a -> v a
{-# INLINE_STREAM unsafeInit #-}
unsafeInit v = unsafeSlice 0 (length v - 1) v

unsafeTail :: Vector v a => v a -> v a
{-# INLINE_STREAM unsafeTail #-}
unsafeTail v = unsafeSlice 1 (length v - 1) v

unsafeTake :: Vector v a => Int -> v a -> v a
{-# INLINE unsafeTake #-}
unsafeTake n v = unsafeSlice 0 n v

unsafeDrop :: Vector v a => Int -> v a -> v a
{-# INLINE unsafeDrop #-}
unsafeDrop n v = unsafeSlice n (length v - n) v

{-# RULES

"slice/new [Vector]" forall i n p.
  slice i n (new p) = new (New.slice i n p)

"init/new [Vector]" forall p.
  init (new p) = new (New.init p)

"tail/new [Vector]" forall p.
  tail (new p) = new (New.tail p)

"take/new [Vector]" forall n p.
  take n (new p) = new (New.take n p)

"drop/new [Vector]" forall n p.
  drop n (new p) = new (New.drop n p)

"unsafeSlice/new [Vector]" forall i n p.
  unsafeSlice i n (new p) = new (New.unsafeSlice i n p)

"unsafeInit/new [Vector]" forall p.
  unsafeInit (new p) = new (New.unsafeInit p)

"unsafeTail/new [Vector]" forall p.
  unsafeTail (new p) = new (New.unsafeTail p)

  #-}

-- Permutations
-- ------------

unsafeAccum_stream
  :: Vector v a => (a -> b -> a) -> v a -> Stream (Int,b) -> v a
{-# INLINE unsafeAccum_stream #-}
unsafeAccum_stream f v s = new (New.accum f (New.unstream (stream v)) s)

unsafeAccum :: Vector v a => (a -> b -> a) -> v a -> [(Int,b)] -> v a
{-# INLINE unsafeAccum #-}
unsafeAccum f v us = unsafeAccum_stream f v (Stream.fromList us)

unsafeAccumulate :: (Vector v a, Vector v (Int, b))
                => (a -> b -> a) -> v a -> v (Int,b) -> v a
{-# INLINE unsafeAccumulate #-}
unsafeAccumulate f v us = unsafeAccum_stream f v (stream us)

unsafeAccumulate_ :: (Vector v a, Vector v Int, Vector v b)
                => (a -> b -> a) -> v a -> v Int -> v b -> v a
{-# INLINE unsafeAccumulate_ #-}
unsafeAccumulate_ f v is xs
  = unsafeAccum_stream f v (Stream.zipWith (,) (stream is) (stream xs))

accum_stream :: Vector v a => (a -> b -> a) -> v a -> Stream (Int,b) -> v a
{-# INLINE accum_stream #-}
accum_stream f v s = new (New.accum f (New.unstream (stream v)) s)

accum :: Vector v a => (a -> b -> a) -> v a -> [(Int,b)] -> v a
{-# INLINE accum #-}
accum f v us = accum_stream f v (Stream.fromList us)

accumulate :: (Vector v a, Vector v (Int, b))
                => (a -> b -> a) -> v a -> v (Int,b) -> v a
{-# INLINE accumulate #-}
accumulate f v us = accum_stream f v (stream us)

accumulate_ :: (Vector v a, Vector v Int, Vector v b)
                => (a -> b -> a) -> v a -> v Int -> v b -> v a
{-# INLINE accumulate_ #-}
accumulate_ f v is xs = accum_stream f v (Stream.zipWith (,) (stream is)
                                                             (stream xs))
                                        

unsafeUpdate_stream :: Vector v a => v a -> Stream (Int,a) -> v a
{-# INLINE unsafeUpdate_stream #-}
unsafeUpdate_stream v s = new (New.unsafeUpdate (New.unstream (stream v)) s)

unsafeUpd :: Vector v a => v a -> [(Int, a)] -> v a
{-# INLINE unsafeUpd #-}
unsafeUpd v us = unsafeUpdate_stream v (Stream.fromList us)

unsafeUpdate :: (Vector v a, Vector v (Int, a)) => v a -> v (Int, a) -> v a
{-# INLINE unsafeUpdate #-}
unsafeUpdate v w = unsafeUpdate_stream v (stream w)

unsafeUpdate_ :: (Vector v a, Vector v Int) => v a -> v Int -> v a -> v a
{-# INLINE unsafeUpdate_ #-}
unsafeUpdate_ v is w
  = unsafeUpdate_stream v (Stream.zipWith (,) (stream is) (stream w))

update_stream :: Vector v a => v a -> Stream (Int,a) -> v a
{-# INLINE update_stream #-}
update_stream v s = new (New.update (New.unstream (stream v)) s)

(//) :: Vector v a => v a -> [(Int, a)] -> v a
{-# INLINE (//) #-}
v // us = update_stream v (Stream.fromList us)

update :: (Vector v a, Vector v (Int, a)) => v a -> v (Int, a) -> v a
{-# INLINE update #-}
update v w = update_stream v (stream w)

update_ :: (Vector v a, Vector v Int) => v a -> v Int -> v a -> v a
{-# INLINE update_ #-}
update_ v is w = update_stream v (Stream.zipWith (,) (stream is) (stream w))

-- This somewhat non-intuitive definition ensures that the resulting vector
-- does not retain references to the original one even if it is lazy in its
-- elements. This would not be the case if we simply used
--
-- backpermute v is = map (v!) is
backpermute :: (Vector v a, Vector v Int) => v a -> v Int -> v a
{-# INLINE backpermute #-}
backpermute v is = seq v
                 $ unstream
                 $ Stream.unbox
                 $ Stream.map (indexM v)
                 $ stream is

unsafeBackpermute :: (Vector v a, Vector v Int) => v a -> v Int -> v a
{-# INLINE unsafeBackpermute #-}
unsafeBackpermute v is = seq v
                       $ unstream
                       $ Stream.unbox
                       $ Stream.map (unsafeIndexM v)
                       $ stream is

-- FIXME: make this fuse better, add support for recycling
reverse :: (Vector v a) => v a -> v a
{-# INLINE reverse #-}
reverse = unstream . streamR

-- Mapping
-- -------

-- | Map a function over a vector
map :: (Vector v a, Vector v b) => (a -> b) -> v a -> v b
{-# INLINE map #-}
map f = unstream . inplace (MStream.map f) . stream

-- | Apply a function to every index/value pair
imap :: (Vector v a, Vector v b) => (Int -> a -> b) -> v a -> v b
{-# INLINE imap #-}
imap f = unstream . inplace (MStream.map (uncurry f) . MStream.indexed)
                  . stream

concatMap :: (Vector v a, Vector v b) => (a -> v b) -> v a -> v b
{-# INLINE concatMap #-}
concatMap f = unstream . Stream.concatMap (stream . f) . stream

-- Zipping/unzipping
-- -----------------

-- | Zip two vectors with the given function.
zipWith :: (Vector v a, Vector v b, Vector v c)
        => (a -> b -> c) -> v a -> v b -> v c
{-# INLINE zipWith #-}
zipWith f xs ys = unstream (Stream.zipWith f (stream xs) (stream ys))

-- | Zip three vectors with the given function.
zipWith3 :: (Vector v a, Vector v b, Vector v c, Vector v d)
         => (a -> b -> c -> d) -> v a -> v b -> v c -> v d
{-# INLINE zipWith3 #-}
zipWith3 f as bs cs = unstream (Stream.zipWith3 f (stream as)
                                                  (stream bs)
                                                  (stream cs))

zipWith4 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e)
         => (a -> b -> c -> d -> e) -> v a -> v b -> v c -> v d -> v e
{-# INLINE zipWith4 #-}
zipWith4 f as bs cs ds
  = unstream (Stream.zipWith4 f (stream as)
                                (stream bs)
                                (stream cs)
                                (stream ds))

zipWith5 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e,
             Vector v f)
         => (a -> b -> c -> d -> e -> f) -> v a -> v b -> v c -> v d -> v e
                                         -> v f
{-# INLINE zipWith5 #-}
zipWith5 f as bs cs ds es
  = unstream (Stream.zipWith5 f (stream as)
                                (stream bs)
                                (stream cs)
                                (stream ds)
                                (stream es))

zipWith6 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e,
             Vector v f, Vector v g)
         => (a -> b -> c -> d -> e -> f -> g)
         -> v a -> v b -> v c -> v d -> v e -> v f -> v g
{-# INLINE zipWith6 #-}
zipWith6 f as bs cs ds es fs
  = unstream (Stream.zipWith6 f (stream as)
                                (stream bs)
                                (stream cs)
                                (stream ds)
                                (stream es)
                                (stream fs))

-- | Zip two vectors and their indices with the given function.
izipWith :: (Vector v a, Vector v b, Vector v c)
        => (Int -> a -> b -> c) -> v a -> v b -> v c
{-# INLINE izipWith #-}
izipWith f xs ys = unstream
                  (Stream.zipWith (uncurry f) (Stream.indexed (stream xs))
                                                              (stream ys))

-- | Zip three vectors and their indices with the given function.
izipWith3 :: (Vector v a, Vector v b, Vector v c, Vector v d)
         => (Int -> a -> b -> c -> d) -> v a -> v b -> v c -> v d
{-# INLINE izipWith3 #-}
izipWith3 f as bs cs
  = unstream (Stream.zipWith3 (uncurry f) (Stream.indexed (stream as))
                                                          (stream bs)
                                                          (stream cs))

izipWith4 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e)
         => (Int -> a -> b -> c -> d -> e) -> v a -> v b -> v c -> v d -> v e
{-# INLINE izipWith4 #-}
izipWith4 f as bs cs ds
  = unstream (Stream.zipWith4 (uncurry f) (Stream.indexed (stream as))
                                                          (stream bs)
                                                          (stream cs)
                                                          (stream ds))

izipWith5 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e,
             Vector v f)
         => (Int -> a -> b -> c -> d -> e -> f) -> v a -> v b -> v c -> v d
                                                -> v e -> v f
{-# INLINE izipWith5 #-}
izipWith5 f as bs cs ds es
  = unstream (Stream.zipWith5 (uncurry f) (Stream.indexed (stream as))
                                                          (stream bs)
                                                          (stream cs)
                                                          (stream ds)
                                                          (stream es))

izipWith6 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e,
             Vector v f, Vector v g)
         => (Int -> a -> b -> c -> d -> e -> f -> g)
         -> v a -> v b -> v c -> v d -> v e -> v f -> v g
{-# INLINE izipWith6 #-}
izipWith6 f as bs cs ds es fs
  = unstream (Stream.zipWith6 (uncurry f) (Stream.indexed (stream as))
                                                          (stream bs)
                                                          (stream cs)
                                                          (stream ds)
                                                          (stream es)
                                                          (stream fs))

zip :: (Vector v a, Vector v b, Vector v (a,b)) => v a -> v b -> v (a, b)
{-# INLINE zip #-}
zip = zipWith (,)

zip3 :: (Vector v a, Vector v b, Vector v c, Vector v (a, b, c))
     => v a -> v b -> v c -> v (a, b, c)
{-# INLINE zip3 #-}
zip3 = zipWith3 (,,)

zip4 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v (a, b, c, d))
     => v a -> v b -> v c -> v d -> v (a, b, c, d)
{-# INLINE zip4 #-}
zip4 = zipWith4 (,,,)

zip5 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e,
         Vector v (a, b, c, d, e))
     => v a -> v b -> v c -> v d -> v e -> v (a, b, c, d, e)
{-# INLINE zip5 #-}
zip5 = zipWith5 (,,,,)

zip6 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e,
         Vector v f, Vector v (a, b, c, d, e, f))
     => v a -> v b -> v c -> v d -> v e -> v f -> v (a, b, c, d, e, f)
{-# INLINE zip6 #-}
zip6 = zipWith6 (,,,,,)

unzip :: (Vector v a, Vector v b, Vector v (a,b)) => v (a, b) -> (v a, v b)
{-# INLINE unzip #-}
unzip xs = (map fst xs, map snd xs)

unzip3 :: (Vector v a, Vector v b, Vector v c, Vector v (a, b, c))
       => v (a, b, c) -> (v a, v b, v c)
{-# INLINE unzip3 #-}
unzip3 xs = (map (\(a, b, c) -> a) xs,
             map (\(a, b, c) -> b) xs,
             map (\(a, b, c) -> c) xs)

unzip4 :: (Vector v a, Vector v b, Vector v c, Vector v d,
           Vector v (a, b, c, d))
       => v (a, b, c, d) -> (v a, v b, v c, v d)
{-# INLINE unzip4 #-}
unzip4 xs = (map (\(a, b, c, d) -> a) xs,
             map (\(a, b, c, d) -> b) xs,
             map (\(a, b, c, d) -> c) xs,
             map (\(a, b, c, d) -> d) xs)

unzip5 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e,
           Vector v (a, b, c, d, e))
       => v (a, b, c, d, e) -> (v a, v b, v c, v d, v e)
{-# INLINE unzip5 #-}
unzip5 xs = (map (\(a, b, c, d, e) -> a) xs,
             map (\(a, b, c, d, e) -> b) xs,
             map (\(a, b, c, d, e) -> c) xs,
             map (\(a, b, c, d, e) -> d) xs,
             map (\(a, b, c, d, e) -> e) xs)

unzip6 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e,
           Vector v f, Vector v (a, b, c, d, e, f))
       => v (a, b, c, d, e, f) -> (v a, v b, v c, v d, v e, v f)
{-# INLINE unzip6 #-}
unzip6 xs = (map (\(a, b, c, d, e, f) -> a) xs,
             map (\(a, b, c, d, e, f) -> b) xs,
             map (\(a, b, c, d, e, f) -> c) xs,
             map (\(a, b, c, d, e, f) -> d) xs,
             map (\(a, b, c, d, e, f) -> e) xs,
             map (\(a, b, c, d, e, f) -> f) xs)

-- Comparisons
-- -----------

eq :: (Vector v a, Eq a) => v a -> v a -> Bool
{-# INLINE eq #-}
xs `eq` ys = stream xs == stream ys

cmp :: (Vector v a, Ord a) => v a -> v a -> Ordering
{-# INLINE cmp #-}
cmp xs ys = compare (stream xs) (stream ys)

-- Filtering
-- ---------

-- | Drop elements that do not satisfy the predicate
filter :: Vector v a => (a -> Bool) -> v a -> v a
{-# INLINE filter #-}
filter f = unstream . inplace (MStream.filter f) . stream

-- | Drop elements that do not satisfy the predicate (applied to values and
-- their indices)
ifilter :: Vector v a => (Int -> a -> Bool) -> v a -> v a
{-# INLINE ifilter #-}
ifilter f = unstream
          . inplace (MStream.map snd . MStream.filter (uncurry f)
                                     . MStream.indexed)
          . stream

-- | Yield the longest prefix of elements satisfying the predicate.
takeWhile :: Vector v a => (a -> Bool) -> v a -> v a
{-# INLINE takeWhile #-}
takeWhile f = unstream . Stream.takeWhile f . stream

-- | Drop the longest prefix of elements that satisfy the predicate.
dropWhile :: Vector v a => (a -> Bool) -> v a -> v a
{-# INLINE dropWhile #-}
dropWhile f = unstream . Stream.dropWhile f . stream

-- | Split the vector in two parts, the first one containing those elements
-- that satisfy the predicate and the second one those that don't. The
-- relative order of the elements is preserved at the cost of a (sometimes)
-- reduced performance compared to 'unstablePartition'.
partition :: Vector v a => (a -> Bool) -> v a -> (v a, v a)
{-# INLINE partition #-}
partition f = partition_stream f . stream

-- FIXME: Make this inplace-fusible (look at how stable_partition is
-- implemented in C++)

partition_stream :: Vector v a => (a -> Bool) -> Stream a -> (v a, v a)
{-# INLINE_STREAM partition_stream #-}
partition_stream f s = s `seq` runST (
  do
    (mv1,mv2) <- M.partitionStream f s
    v1 <- unsafeFreeze mv1
    v2 <- unsafeFreeze mv2
    return (v1,v2))

-- | Split the vector in two parts, the first one containing those elements
-- that satisfy the predicate and the second one those that don't. The order
-- of the elements is not preserved but the operation is often faster than
-- 'partition'.
unstablePartition :: Vector v a => (a -> Bool) -> v a -> (v a, v a)
{-# INLINE unstablePartition #-}
unstablePartition f = unstablePartition_stream f . stream

unstablePartition_stream
  :: Vector v a => (a -> Bool) -> Stream a -> (v a, v a)
{-# INLINE_STREAM unstablePartition_stream #-}
unstablePartition_stream f s = s `seq` runST (
  do
    (mv1,mv2) <- M.unstablePartitionStream f s
    v1 <- unsafeFreeze mv1
    v2 <- unsafeFreeze mv2
    return (v1,v2))

unstablePartition_new :: Vector v a => (a -> Bool) -> New v a -> (v a, v a)
{-# INLINE_STREAM unstablePartition_new #-}
unstablePartition_new f (New.New p) = runST (
  do
    mv <- p
    i <- M.unstablePartition f mv
    v <- unsafeFreeze mv
    return (unsafeTake i v, unsafeDrop i v))

{-# RULES

"unstablePartition" forall f p.
  unstablePartition_stream f (stream (new p))
    = unstablePartition_new f p

  #-}


-- FIXME: make span and break fusible

-- | Split the vector into the longest prefix of elements that satisfy the
-- predicate and the rest.
span :: Vector v a => (a -> Bool) -> v a -> (v a, v a)
{-# INLINE span #-}
span f = break (not . f)

-- | Split the vector into the longest prefix of elements that do not satisfy
-- the predicate and the rest.
break :: Vector v a => (a -> Bool) -> v a -> (v a, v a)
{-# INLINE break #-}
break f xs = case findIndex f xs of
               Just i  -> (unsafeSlice 0 i xs, unsafeSlice i (length xs - i) xs)
               Nothing -> (xs, empty)
    

-- Searching
-- ---------

infix 4 `elem`
-- | Check whether the vector contains an element
elem :: (Vector v a, Eq a) => a -> v a -> Bool
{-# INLINE elem #-}
elem x = Stream.elem x . stream

infix 4 `notElem`
-- | Inverse of `elem`
notElem :: (Vector v a, Eq a) => a -> v a -> Bool
{-# INLINE notElem #-}
notElem x = Stream.notElem x . stream

-- | Yield 'Just' the first element matching the predicate or 'Nothing' if no
-- such element exists.
find :: Vector v a => (a -> Bool) -> v a -> Maybe a
{-# INLINE find #-}
find f = Stream.find f . stream

-- | Yield 'Just' the index of the first element matching the predicate or
-- 'Nothing' if no such element exists.
findIndex :: Vector v a => (a -> Bool) -> v a -> Maybe Int
{-# INLINE findIndex #-}
findIndex f = Stream.findIndex f . stream

-- | Yield the indices of elements satisfying the predicate
findIndices :: (Vector v a, Vector v Int) => (a -> Bool) -> v a -> v Int
{-# INLINE findIndices #-}
findIndices f = unstream
              . inplace (MStream.map fst . MStream.filter (f . snd)
                                         . MStream.indexed)
              . stream

-- | Yield 'Just' the index of the first occurence of the given element or
-- 'Nothing' if the vector does not contain the element
elemIndex :: (Vector v a, Eq a) => a -> v a -> Maybe Int
{-# INLINE elemIndex #-}
elemIndex x = findIndex (x==)

-- | Yield the indices of all occurences of the given element
elemIndices :: (Vector v a, Vector v Int, Eq a) => a -> v a -> v Int
{-# INLINE elemIndices #-}
elemIndices x = findIndices (x==)

-- Folding
-- -------

-- | Left fold
foldl :: Vector v b => (a -> b -> a) -> a -> v b -> a
{-# INLINE foldl #-}
foldl f z = Stream.foldl f z . stream

-- | Left fold on non-empty vectors
foldl1 :: Vector v a => (a -> a -> a) -> v a -> a
{-# INLINE foldl1 #-}
foldl1 f = Stream.foldl1 f . stream

-- | Left fold with strict accumulator
foldl' :: Vector v b => (a -> b -> a) -> a -> v b -> a
{-# INLINE foldl' #-}
foldl' f z = Stream.foldl' f z . stream

-- | Left fold on non-empty vectors with strict accumulator
foldl1' :: Vector v a => (a -> a -> a) -> v a -> a
{-# INLINE foldl1' #-}
foldl1' f = Stream.foldl1' f . stream

-- | Right fold
foldr :: Vector v a => (a -> b -> b) -> b -> v a -> b
{-# INLINE foldr #-}
foldr f z = Stream.foldr f z . stream

-- | Right fold on non-empty vectors
foldr1 :: Vector v a => (a -> a -> a) -> v a -> a
{-# INLINE foldr1 #-}
foldr1 f = Stream.foldr1 f . stream

-- | Right fold with a strict accumulator
foldr' :: Vector v a => (a -> b -> b) -> b -> v a -> b
{-# INLINE foldr' #-}
foldr' f z = Stream.foldl' (flip f) z . streamR

-- | Right fold on non-empty vectors with strict accumulator
foldr1' :: Vector v a => (a -> a -> a) -> v a -> a
{-# INLINE foldr1' #-}
foldr1' f = Stream.foldl1' (flip f) . streamR

-- | Left fold (function applied to each element and its index)
ifoldl :: Vector v b => (a -> Int -> b -> a) -> a -> v b -> a
{-# INLINE ifoldl #-}
ifoldl f z = Stream.foldl (uncurry . f) z . Stream.indexed . stream

-- | Left fold with strict accumulator (function applied to each element and
-- its index)
ifoldl' :: Vector v b => (a -> Int -> b -> a) -> a -> v b -> a
{-# INLINE ifoldl' #-}
ifoldl' f z = Stream.foldl' (uncurry . f) z . Stream.indexed . stream

-- | Right fold (function applied to each element and its index)
ifoldr :: Vector v a => (Int -> a -> b -> b) -> b -> v a -> b
{-# INLINE ifoldr #-}
ifoldr f z = Stream.foldr (uncurry f) z . Stream.indexed . stream

-- | Right fold with strict accumulator (function applied to each element and
-- its index)
ifoldr' :: Vector v a => (Int -> a -> b -> b) -> b -> v a -> b
{-# INLINE ifoldr' #-}
ifoldr' f z xs = Stream.foldl' (flip (uncurry f)) z
               $ Stream.indexedR (length xs) $ streamR xs

-- Specialised folds
-- -----------------

all :: Vector v a => (a -> Bool) -> v a -> Bool
{-# INLINE all #-}
all f = Stream.and . Stream.map f . stream

any :: Vector v a => (a -> Bool) -> v a -> Bool
{-# INLINE any #-}
any f = Stream.or . Stream.map f . stream

and :: Vector v Bool => v Bool -> Bool
{-# INLINE and #-}
and = Stream.and . stream

or :: Vector v Bool => v Bool -> Bool
{-# INLINE or #-}
or = Stream.or . stream

sum :: (Vector v a, Num a) => v a -> a
{-# INLINE sum #-}
sum = Stream.foldl' (+) 0 . stream

product :: (Vector v a, Num a) => v a -> a
{-# INLINE product #-}
product = Stream.foldl' (*) 1 . stream

maximum :: (Vector v a, Ord a) => v a -> a
{-# INLINE maximum #-}
maximum = Stream.foldl1' max . stream

maximumBy :: Vector v a => (a -> a -> Ordering) -> v a -> a
{-# INLINE maximumBy #-}
maximumBy cmp = Stream.foldl1' maxBy . stream
  where
    {-# INLINE maxBy #-}
    maxBy x y = case cmp x y of
                  LT -> y
                  _  -> x

minimum :: (Vector v a, Ord a) => v a -> a
{-# INLINE minimum #-}
minimum = Stream.foldl1' min . stream

minimumBy :: Vector v a => (a -> a -> Ordering) -> v a -> a
{-# INLINE minimumBy #-}
minimumBy cmp = Stream.foldl1' minBy . stream
  where
    {-# INLINE minBy #-}
    minBy x y = case cmp x y of
                  GT -> y
                  _  -> x

maxIndex :: (Vector v a, Ord a) => v a -> Int
{-# INLINE maxIndex #-}
maxIndex = maxIndexBy compare

maxIndexBy :: Vector v a => (a -> a -> Ordering) -> v a -> Int
{-# INLINE maxIndexBy #-}
maxIndexBy cmp = fst . Stream.foldl1' imax . Stream.indexed . stream
  where
    imax (i,x) (j,y) = case cmp x y of
                         LT -> (j,y)
                         _  -> (i,x)

minIndex :: (Vector v a, Ord a) => v a -> Int
{-# INLINE minIndex #-}
minIndex = minIndexBy compare

minIndexBy :: Vector v a => (a -> a -> Ordering) -> v a -> Int
{-# INLINE minIndexBy #-}
minIndexBy cmp = fst . Stream.foldl1' imin . Stream.indexed . stream
  where
    imin (i,x) (j,y) = case cmp x y of
                         GT -> (j,y)
                         _  -> (i,x)


-- Unfolding
-- ---------

-- | Unfold
unfoldr :: Vector v a => (b -> Maybe (a, b)) -> b -> v a
{-# INLINE unfoldr #-}
unfoldr f = unstream . Stream.unfoldr f

-- | Unfoldr at most @n@ elements.
unfoldrN  :: Vector v a => Int -> (b -> Maybe (a, b)) -> b -> v a
{-# INLINE unfoldrN #-}
unfoldrN n f = unstream . Stream.unfoldrN n f

-- Scans
-- -----

-- | Prefix scan
prescanl :: (Vector v a, Vector v b) => (a -> b -> a) -> a -> v b -> v a
{-# INLINE prescanl #-}
prescanl f z = unstream . inplace (MStream.prescanl f z) . stream

-- | Prefix scan with strict accumulator
prescanl' :: (Vector v a, Vector v b) => (a -> b -> a) -> a -> v b -> v a
{-# INLINE prescanl' #-}
prescanl' f z = unstream . inplace (MStream.prescanl' f z) . stream

-- | Suffix scan
postscanl :: (Vector v a, Vector v b) => (a -> b -> a) -> a -> v b -> v a
{-# INLINE postscanl #-}
postscanl f z = unstream . inplace (MStream.postscanl f z) . stream

-- | Suffix scan with strict accumulator
postscanl' :: (Vector v a, Vector v b) => (a -> b -> a) -> a -> v b -> v a
{-# INLINE postscanl' #-}
postscanl' f z = unstream . inplace (MStream.postscanl' f z) . stream

-- | Haskell-style scan
scanl :: (Vector v a, Vector v b) => (a -> b -> a) -> a -> v b -> v a
{-# INLINE scanl #-}
scanl f z = unstream . Stream.scanl f z . stream

-- | Haskell-style scan with strict accumulator
scanl' :: (Vector v a, Vector v b) => (a -> b -> a) -> a -> v b -> v a
{-# INLINE scanl' #-}
scanl' f z = unstream . Stream.scanl' f z . stream

-- | Scan over a non-empty vector
scanl1 :: Vector v a => (a -> a -> a) -> v a -> v a
{-# INLINE scanl1 #-}
scanl1 f = unstream . inplace (MStream.scanl1 f) . stream

-- | Scan over a non-empty vector with a strict accumulator
scanl1' :: Vector v a => (a -> a -> a) -> v a -> v a
{-# INLINE scanl1' #-}
scanl1' f = unstream . inplace (MStream.scanl1' f) . stream


-- | Prefix right-to-left scan
prescanr :: (Vector v a, Vector v b) => (a -> b -> b) -> b -> v a -> v b
{-# INLINE prescanr #-}
prescanr f z = unstreamR . inplace (MStream.prescanl (flip f) z) . streamR

-- | Prefix right-to-left scan with strict accumulator
prescanr' :: (Vector v a, Vector v b) => (a -> b -> b) -> b -> v a -> v b
{-# INLINE prescanr' #-}
prescanr' f z = unstreamR . inplace (MStream.prescanl' (flip f) z) . streamR

-- | Suffix right-to-left scan
postscanr :: (Vector v a, Vector v b) => (a -> b -> b) -> b -> v a -> v b
{-# INLINE postscanr #-}
postscanr f z = unstreamR . inplace (MStream.postscanl (flip f) z) . streamR

-- | Suffix right-to-left scan with strict accumulator
postscanr' :: (Vector v a, Vector v b) => (a -> b -> b) -> b -> v a -> v b
{-# INLINE postscanr' #-}
postscanr' f z = unstreamR . inplace (MStream.postscanl' (flip f) z) . streamR

-- | Haskell-style right-to-left scan
scanr :: (Vector v a, Vector v b) => (a -> b -> b) -> b -> v a -> v b
{-# INLINE scanr #-}
scanr f z = unstreamR . Stream.scanl (flip f) z . streamR

-- | Haskell-style right-to-left scan with strict accumulator
scanr' :: (Vector v a, Vector v b) => (a -> b -> b) -> b -> v a -> v b
{-# INLINE scanr' #-}
scanr' f z = unstreamR . Stream.scanl' (flip f) z . streamR

-- | Right-to-left scan over a non-empty vector
scanr1 :: Vector v a => (a -> a -> a) -> v a -> v a
{-# INLINE scanr1 #-}
scanr1 f = unstreamR . inplace (MStream.scanl1 (flip f)) . streamR

-- | Right-to-left scan over a non-empty vector with a strict accumulator
scanr1' :: Vector v a => (a -> a -> a) -> v a -> v a
{-# INLINE scanr1' #-}
scanr1' f = unstreamR . inplace (MStream.scanl1' (flip f)) . streamR

-- Enumeration
-- -----------

-- | Yield a vector of the given length containing the values @x@, @x+1@ etc.
-- This operation is usually more efficient than 'enumFromTo'.
enumFromN :: (Vector v a, Num a) => a -> Int -> v a
{-# INLINE enumFromN #-}
enumFromN x n = enumFromStepN x 1 n

-- | Yield a vector of the given length containing the values @x@, @x+y@,
-- @x+y+y@ etc. This operations is usually more efficient than
-- 'enumFromThenTo'.
enumFromStepN :: forall v a. (Vector v a, Num a) => a -> a -> Int -> v a
{-# INLINE enumFromStepN #-}
enumFromStepN x y n = elemseq (undefined :: v a) x
                    $ elemseq (undefined :: v a) y
                    $ unstream
                    $ Stream.enumFromStepN  x y n

-- | Enumerate values from @x@ to @y@.
--
-- /WARNING:/ This operation can be very inefficient. If at all possible, use
-- 'enumFromN' instead.
enumFromTo :: (Vector v a, Enum a) => a -> a -> v a
{-# INLINE enumFromTo #-}
enumFromTo x y = unstream (Stream.enumFromTo x y)

-- | Enumerate values from @x@ to @y@ with a specific step @z@.
--
-- /WARNING:/ This operation can be very inefficient. If at all possible, use
-- 'enumFromStepN' instead.
enumFromThenTo :: (Vector v a, Enum a) => a -> a -> a -> v a
{-# INLINE enumFromThenTo #-}
enumFromThenTo x y z = unstream (Stream.enumFromThenTo x y z)

-- | Convert a vector to a list
toList :: Vector v a => v a -> [a]
{-# INLINE toList #-}
toList = Stream.toList . stream

-- | Convert a list to a vector
fromList :: Vector v a => [a] -> v a
{-# INLINE fromList #-}
fromList = unstream . Stream.fromList

-- | Convert the first @n@ elements of a list to a vector
--
-- > fromListN n xs = fromList (take n xs)
fromListN :: Vector v a => Int -> [a] -> v a
{-# INLINE fromListN #-}
fromListN n = unstream . Stream.fromListN n

-- Utilities for defining Data instances
-- -------------------------------------

-- | Generic definion of 'Data.Data.gfoldl' that views a 'Vector' as a
-- list.
gfoldl :: (Vector v a, Data a)
       => (forall d b. Data d => c (d -> b) -> d -> c b)
       -> (forall g. g -> c g)
       -> v a
       -> c (v a)
{-# INLINE gfoldl #-}
gfoldl f z v = z fromList `f` toList v

mkType :: String -> DataType
{-# INLINE mkType #-}
mkType = mkNorepType

dataCast :: (Vector v a, Data a, Typeable1 v, Typeable1 t)
         => (forall d. Data  d => c (t d)) -> Maybe  (c (v a))
{-# INLINE dataCast #-}
dataCast f = gcast1 f

