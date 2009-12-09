{-# LANGUAGE Rank2Types, MultiParamTypeClasses, FlexibleContexts,
             TypeFamilies, ScopedTypeVariables #-}
-- |
-- Module      : Data.Vector.Generic
-- Copyright   : (c) Roman Leshchinskiy 2008-2009
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
  empty, singleton, cons, snoc, replicate, generate, (++), copy,

  -- * Accessing individual elements
  (!), head, last, indexM, headM, lastM,
  unsafeIndex, unsafeHead, unsafeLast,
  unsafeIndexM, unsafeHeadM, unsafeLastM,

  -- * Subvectors
  slice, init, tail, take, drop,
  unsafeSlice,

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

  -- * Comparisons
  eq, cmp,

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

  -- * Conversion to/from Streams
  stream, unstream,

  -- * MVector-based initialisation
  new,

  -- * Unsafe operations
  unsafeAccum, unsafeAccumulate, unsafeAccumulate_,
  unsafeUpd, unsafeUpdate, unsafeUpdate_
) where

import           Data.Vector.Generic.Mutable ( MVector )
import qualified Data.Vector.Generic.Mutable as M

import qualified Data.Vector.Generic.New as New
import           Data.Vector.Generic.New ( New )

import qualified Data.Vector.Fusion.Stream as Stream
import           Data.Vector.Fusion.Stream ( Stream, MStream, inplace )
import qualified Data.Vector.Fusion.Stream.Monadic as MStream
import           Data.Vector.Fusion.Stream.Size
import           Data.Vector.Fusion.Util

import Control.Monad.ST ( runST )
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
                        scanl, scanl1,
                        enumFromTo, enumFromThenTo )

#include "vector.h"

type family Mutable (v :: * -> *) :: * -> * -> *

-- | Class of immutable vectors.
--
class MVector (Mutable v) a => Vector v a where
  -- | Unsafely convert a mutable vector to its immutable version
  -- without copying. The mutable vector may not be used after
  -- this operation.
  unsafeFreeze :: PrimMonad m => Mutable v (PrimState m) a -> m (v a)

  -- | Length of the vector (not fusible!)
  basicLength      :: v a -> Int

  -- | Yield a part of the vector without copying it. No range checks!
  basicUnsafeSlice  :: v a -> Int -> Int -> v a

  -- | Yield the element at the given position in a monad. The monad allows us
  -- to be strict in the vector if we want. Suppose we had
  --
  -- > unsafeIndex :: v a -> Int -> a
  --
  -- instead. Now, if we wanted to copy a vector, we'd do something like
  --
  -- > copy mv v ... = ... unsafeWrite mv i (unsafeIndex v i) ...
  --
  -- For lazy vectors, the indexing would not be evaluated which means that we
  -- would retain a reference to the original vector in each element we write.
  -- This is not what we want!
  --
  -- With 'basicUnsafeIndexM', we can do
  --
  -- > copy mv v ... = ... case basicUnsafeIndexM v i of
  -- >                       Box x -> unsafeWrite mv i x ...
  --
  -- which does not have this problem because indexing (but not the returned
  -- element!) is evaluated immediately.
  --
  basicUnsafeIndexM  :: Monad m => v a -> Int -> m a

  elemseq :: v a -> a -> b -> b

  {-# INLINE elemseq #-}
  elemseq _ = \_ x -> x

-- Fusion
-- ------

-- | Construct a pure vector from a monadic initialiser 
new :: Vector v a => New a -> v a
{-# INLINE new #-}
new m = new' undefined m

-- | Same as 'new' but with a dummy argument necessary for correctly typing
-- the rule @uninplace@.
--
-- See http://hackage.haskell.org/trac/ghc/ticket/2600
new' :: Vector v a => v a -> New a -> v a
{-# INLINE_STREAM new' #-}
new' _ m = m `seq` runST (do
                            mv <- New.run m
                            unsafeFreeze mv)

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

"stream/unstream [Vector]" forall v s.
  stream (new' v (New.unstream s)) = s

"New.unstream/stream/new [Vector]" forall v p.
  New.unstream (stream (new' v p)) = p

 #-}

{-# RULES

"inplace [Vector]"
  forall (f :: forall m. Monad m => MStream m a -> MStream m a) v m.
  New.unstream (inplace f (stream (new' v m))) = New.transform f m

"uninplace [Vector]"
  forall (f :: forall m. Monad m => MStream m a -> MStream m a) v m.
  stream (new' v (New.transform f m)) = inplace f (stream (new' v m))

 #-}

-- Length
-- ------

length :: Vector v a => v a -> Int
{-# INLINE_STREAM length #-}
length v = basicLength v

{-# RULES

"length/unstream [Vector]" forall v s.
  length (new' v (New.unstream s)) = Stream.length s

  #-}

null :: Vector v a => v a -> Bool
{-# INLINE null #-}
null v = length v == 0

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
copy :: Vector v a => v a -> v a
{-# INLINE_STREAM copy #-}
copy = unstream . stream

{-# RULES

"copy/unstream [Vector]" forall v s.
  copy (new' v (New.unstream s)) = new' v (New.unstream s)

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

"(!)/unstream [Vector]" forall v i s.
  new' v (New.unstream s) ! i = s Stream.!! i

"head/unstream [Vector]" forall v s.
  head (new' v (New.unstream s)) = Stream.head s

"last/unstream [Vector]" forall v s.
  last (new' v (New.unstream s)) = Stream.last s

"unsafeIndex/unstream [Vector]" forall v i s.
  unsafeIndex (new' v (New.unstream s)) i = s Stream.!! i

"unsafeHead/unstream [Vector]" forall v s.
  unsafeHead (new' v (New.unstream s)) = Stream.head s

"unsafeLast/unstream [Vector]" forall v s.
  unsafeLast (new' v (New.unstream s)) = Stream.last s

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
slice :: Vector v a => v a -> Int   -- ^ starting index
                           -> Int   -- ^ length
                           -> v a
{-# INLINE_STREAM slice #-}
slice v i n = BOUNDS_CHECK(checkSlice) "slice" i n (length v)
            $ basicUnsafeSlice v i n

-- | Unsafely yield a part of the vector without copying it and without
-- performing bounds checks.
unsafeSlice :: Vector v a => v a -> Int   -- ^ starting index
                                 -> Int   -- ^ length
                                 -> v a
{-# INLINE_STREAM unsafeSlice #-}
unsafeSlice v i n = UNSAFE_CHECK(checkSlice) "unsafeSlice" i n (length v)
                  $ basicUnsafeSlice v i n

-- | Yield all but the last element without copying.
init :: Vector v a => v a -> v a
{-# INLINE_STREAM init #-}
init v = slice v 0 (length v - 1)

-- | All but the first element (without copying).
tail :: Vector v a => v a -> v a
{-# INLINE_STREAM tail #-}
tail v = slice v 1 (length v - 1)

-- | Yield the first @n@ elements without copying.
take :: Vector v a => Int -> v a -> v a
{-# INLINE_STREAM take #-}
take n v = slice v 0 (min n' (length v))
  where n' = max n 0

-- | Yield all but the first @n@ elements without copying.
drop :: Vector v a => Int -> v a -> v a
{-# INLINE_STREAM drop #-}
drop n v = slice v (min n' len) (max 0 (len - n'))
  where n' = max n 0
        len = length v

{-# RULES

"slice/new [Vector]" forall v p i n.
  slice (new' v p) i n = new' v (New.slice p i n)

"unsafeSlice/new [Vector]" forall v p i n.
  unsafeSlice (new' v p) i n = new' v (New.unsafeSlice p i n)

"init/new [Vector]" forall v p.
  init (new' v p) = new' v (New.init p)

"tail/new [Vector]" forall v p.
  tail (new' v p) = new' v (New.tail p)

"take/new [Vector]" forall n v p.
  take n (new' v p) = new' v (New.take n p)

"drop/new [Vector]" forall n v p.
  drop n (new' v p) = new' v (New.drop n p)

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

reverse :: (Vector v a) => v a -> v a
{-# INLINE reverse #-}
reverse = new . New.reverse . New.unstream . stream

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
-- that satisfy the predicate and the second one those that don't. The order
-- of the elements is not preserved.
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

unstablePartition_new :: Vector v a => (a -> Bool) -> New a -> (v a, v a)
{-# INLINE_STREAM unstablePartition_new #-}
unstablePartition_new f (New.New p) = runST (
  do
    mv <- p
    i <- M.unstablePartition f mv
    v <- unsafeFreeze mv
    return (take i v, drop i v))

{-# RULES

"unstablePartition" forall f v p.
  unstablePartition_stream f (stream (new' v p))
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
               Just i  -> (unsafeSlice xs 0 i, unsafeSlice xs i (length xs - i))
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
              . Stream.map fst
              . Stream.filter (f . snd)
              . Stream.indexed
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

unfoldr :: Vector v a => (b -> Maybe (a, b)) -> b -> v a
{-# INLINE unfoldr #-}
unfoldr f = unstream . Stream.unfoldr f

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

-- Enumeration
-- -----------

enumFromTo :: (Vector v a, Enum a) => a -> a -> v a
{-# INLINE enumFromTo #-}
enumFromTo x y = unstream (Stream.enumFromTo x y)

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

