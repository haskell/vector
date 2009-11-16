{-# LANGUAGE Rank2Types, MultiParamTypeClasses, FlexibleContexts,
             ScopedTypeVariables #-}
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

#include "phases.h"

module Data.Vector.Generic (
  -- * Immutable vectors
  Vector(..),

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

  -- * Comparisons
  eq, cmp,

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
  toList, fromList,

  -- * Conversion to/from Streams
  stream, unstream,

  -- * MVector-based initialisation
  new
) where

import qualified Data.Vector.Generic.Mutable as Mut
import           Data.Vector.Generic.Mutable ( MVector )

import qualified Data.Vector.Generic.New as New
import           Data.Vector.Generic.New ( New )

import qualified Data.Vector.Fusion.Stream as Stream
import           Data.Vector.Fusion.Stream ( Stream, MStream )
import qualified Data.Vector.Fusion.Stream.Monadic as MStream
import           Data.Vector.Fusion.Stream.Size
import           Data.Vector.Fusion.Util

import Control.Exception ( assert )

import Prelude hiding ( length, null,
                        replicate, (++),
                        head, last,
                        init, tail, take, drop, reverse,
                        map, concatMap,
                        zipWith, zipWith3, zip, zip3, unzip, unzip3,
                        filter, takeWhile, dropWhile,
                        elem, notElem,
                        foldl, foldl1, foldr, foldr1,
                        and, or, sum, product, maximum, minimum,
                        scanl, scanl1,
                        enumFromTo, enumFromThenTo )

-- | Class of immutable vectors.
--
class Vector v a where
  -- | Construct a pure vector from a monadic initialiser (not fusible!)
  vnew         :: (forall mv m. MVector mv m a => m (mv a)) -> v a

  -- | Length of the vector (not fusible!)
  vlength      :: v a -> Int

  -- | Yield a part of the vector without copying it. No range checks!
  unsafeSlice  :: v a -> Int -> Int -> v a

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
  -- With 'unsafeIndexM', we can do
  --
  -- > copy mv v ... = ... case unsafeIndexM v i of
  -- >                       Box x -> unsafeWrite mv i x ...
  --
  -- which does not have this problem because indexing (but not the returned
  -- element!) is evaluated immediately.
  --
  unsafeIndexM  :: Monad m => v a -> Int -> m a

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
new' _ m = vnew (New.run m)

-- | Convert a vector to a 'Stream'
stream :: Vector v a => v a -> Stream a
{-# INLINE_STREAM stream #-}
stream v = v `seq` (Stream.unfoldr get 0 `Stream.sized` Exact n)
  where
    n = length v

    {-# INLINE get #-}
    get i | i < n     = case unsafeIndexM v i of Box x -> Just (x, i+1)
          | otherwise = Nothing

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

inplace :: (forall m. Monad m => MStream m a -> MStream m a)
        -> Stream a -> Stream a
{-# INLINE_STREAM inplace #-}
inplace f s = f s

{-# RULES

"inplace [Vector]"
  forall (f :: forall m. Monad m => MStream m a -> MStream m a) v m.
  New.unstream (inplace f (stream (new' v m))) = New.transform f m

"uninplace [Vector]"
  forall (f :: forall m. Monad m => MStream m a -> MStream m a) v m.
  stream (new' v (New.transform f m)) = inplace f (stream (new' v m))

"inplace/inplace [Vector]"
  forall (f :: forall m. Monad m => MStream m a -> MStream m a)
         (g :: forall m. Monad m => MStream m a -> MStream m a)
         s.
  inplace f (inplace g s) = inplace (f . g) s

 #-}

-- Length
-- ------

length :: Vector v a => v a -> Int
{-# INLINE_STREAM length #-}
length v = vlength v

{-# RULES

"length/unstream [Vector]" forall v s.
  length (new' v (New.unstream s)) = Stream.length s

  #-}

null :: Vector v a => v a -> Bool
{-# INLINE_STREAM null #-}
null v = vlength v == 0

{-# RULES

"null/unstream [Vector]" forall v s.
  null (new' v (New.unstream s)) = Stream.null s

  #-}

-- Construction
-- ------------

-- | Empty vector
empty :: Vector v a => v a
{-# INLINE empty #-}
empty = unstream Stream.empty

-- | Vector with exaclty one element
singleton :: Vector v a => a -> v a
{-# INLINE singleton #-}
singleton x = unstream (Stream.singleton x)

-- | Vector of the given length with the given value in each position
replicate :: Vector v a => Int -> a -> v a
{-# INLINE replicate #-}
replicate n = unstream . Stream.replicate n

-- | Prepend an element
cons :: Vector v a => a -> v a -> v a
{-# INLINE cons #-}
cons x = unstream . Stream.cons x . stream

-- | Append an element
snoc :: Vector v a => v a -> a -> v a
{-# INLINE snoc #-}
snoc v = unstream . Stream.snoc (stream v)

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
v ! i = assert (i >= 0 && i < length v)
      $ unId (unsafeIndexM v i)

-- | First element
head :: Vector v a => v a -> a
{-# INLINE_STREAM head #-}
head v = v ! 0

-- | Last element
last :: Vector v a => v a -> a
{-# INLINE_STREAM last #-}
last v = v ! (length v - 1)

{-# RULES

"(!)/unstream [Vector]" forall v i s.
  new' v (New.unstream s) ! i = s Stream.!! i

"head/unstream [Vector]" forall v s.
  head (new' v (New.unstream s)) = Stream.head s

"last/unstream [Vector]" forall v s.
  last (new' v (New.unstream s)) = Stream.last s

 #-}

-- | Monadic indexing which can be strict in the vector while remaining lazy in
-- the element.
indexM :: (Vector v a, Monad m) => v a -> Int -> m a
{-# INLINE_STREAM indexM #-}
indexM v i = assert (i >= 0 && i < length v)
           $ unsafeIndexM v i

headM :: (Vector v a, Monad m) => v a -> m a
{-# INLINE_STREAM headM #-}
headM v = indexM v 0

lastM :: (Vector v a, Monad m) => v a -> m a
{-# INLINE_STREAM lastM #-}
lastM v = indexM v (length v - 1)

{-# RULES

"indexM/unstream [Vector]" forall v i s.
  indexM (new' v (New.unstream s)) i = return (s Stream.!! i)

"headM/unstream [Vector]" forall v s.
  headM (new' v (New.unstream s)) = return (Stream.head s)

"lastM/unstream [Vector]" forall v s.
  lastM (new' v (New.unstream s)) = return (Stream.last s)

 #-}

-- Subarrays
-- ---------

-- FIXME: slicing doesn't work with the inplace stuff at the moment

-- | Yield a part of the vector without copying it. Safer version of
-- 'unsafeSlice'.
slice :: Vector v a => v a -> Int   -- ^ starting index
                            -> Int   -- ^ length
                            -> v a
{-# INLINE_STREAM slice #-}
slice v i n = assert (i >= 0 && n >= 0  && i+n <= length v)
            $ unsafeSlice v i n

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

accum :: Vector v a => (a -> b -> a) -> v a -> [(Int,b)] -> v a
{-# INLINE accum #-}
accum f v us = new (New.accum f (New.unstream (stream v))
                                (Stream.fromList us))

(//) :: Vector v a => v a -> [(Int, a)] -> v a
{-# INLINE (//) #-}
v // us = new (New.update (New.unstream (stream v))
                          (Stream.fromList us))

update :: (Vector v a, Vector v (Int, a)) => v a -> v (Int, a) -> v a
{-# INLINE update #-}
update v w = new (New.update (New.unstream (stream v)) (stream w))

-- This somewhat non-intuitive definition ensures that the resulting vector
-- does not retain references to the original one even if it is lazy in its
-- elements. This would not be the case if we simply used
--
-- backpermute v is = map (v!) is
backpermute :: (Vector v a, Vector v Int) => v a -> v Int -> v a
{-# INLINE backpermute #-}
backpermute v is = seq v
                 $ unstream
                 $ MStream.trans (Id . unBox)
                 $ MStream.mapM (indexM v)
                 $ MStream.trans (Box . unId)
                 $ stream is

reverse :: (Vector v a) => v a -> v a
{-# INLINE reverse #-}
reverse = new . New.reverse . New.unstream . stream

-- Mapping
-- -------

-- | Map a function over a vector
map :: (Vector v a, Vector v b) => (a -> b) -> v a -> v b
{-# INLINE map #-}
map f = unstream . Stream.map f . stream

inplace_map :: Vector v a => (a -> a) -> v a -> v a
{-# INLINE inplace_map #-}
inplace_map f = unstream . inplace (MStream.map f) . stream

{-# RULES

"map->inplace_map [Vector]" map = inplace_map

 #-}

concatMap :: (Vector v a, Vector v b) => (a -> v b) -> v a -> v b
{-# INLINE concatMap #-}
concatMap f = unstream . Stream.concatMap (stream . f) . stream

-- Zipping/unzipping
-- -----------------

-- | Zip two vectors with the given function.
zipWith :: (Vector v a, Vector v b, Vector v c) => (a -> b -> c) -> v a -> v b -> v c
{-# INLINE zipWith #-}
zipWith f xs ys = unstream (Stream.zipWith f (stream xs) (stream ys))

-- | Zip three vectors with the given function.
zipWith3 :: (Vector v a, Vector v b, Vector v c, Vector v d) => (a -> b -> c -> d) -> v a -> v b -> v c -> v d
{-# INLINE zipWith3 #-}
zipWith3 f xs ys zs = unstream (Stream.zipWith3 f (stream xs) (stream ys) (stream zs))

zip :: (Vector v a, Vector v b, Vector v (a,b)) => v a -> v b -> v (a, b)
{-# INLINE zip #-}
zip = zipWith (,)

zip3 :: (Vector v a, Vector v b, Vector v c, Vector v (a, b, c)) => v a -> v b -> v c -> v (a, b, c)
{-# INLINE zip3 #-}
zip3 = zipWith3 (,,)

unzip :: (Vector v a, Vector v b, Vector v (a,b)) => v (a, b) -> (v a, v b)
{-# INLINE unzip #-}
unzip xs = (map fst xs, map snd xs)

unzip3 :: (Vector v a, Vector v b, Vector v c, Vector v (a, b, c)) => v (a, b, c) -> (v a, v b, v c)
{-# INLINE unzip3 #-}
unzip3 xs = (map (\(a, b, c) -> a) xs, map (\(a, b, c) -> b) xs, map (\(a, b, c) -> c) xs)

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

-- | Drop elements which do not satisfy the predicate
filter :: Vector v a => (a -> Bool) -> v a -> v a
{-# INLINE filter #-}
filter f = unstream . inplace (MStream.filter f) . stream

-- | Yield the longest prefix of elements satisfying the predicate.
takeWhile :: Vector v a => (a -> Bool) -> v a -> v a
{-# INLINE takeWhile #-}
takeWhile f = unstream . Stream.takeWhile f . stream

-- | Drop the longest prefix of elements that satisfy the predicate.
dropWhile :: Vector v a => (a -> Bool) -> v a -> v a
{-# INLINE dropWhile #-}
dropWhile f = unstream . Stream.dropWhile f . stream

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

-- Folding
-- -------

-- | Left fold
foldl :: Vector v b => (a -> b -> a) -> a -> v b -> a
{-# INLINE foldl #-}
foldl f z = Stream.foldl f z . stream

-- | Lefgt fold on non-empty vectors
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

-- Specialised folds
-- -----------------

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

minimum :: (Vector v a, Ord a) => v a -> a
{-# INLINE minimum #-}
minimum = Stream.foldl1' min . stream

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
prescanl f z = unstream . Stream.prescanl f z . stream

inplace_prescanl :: Vector v a => (a -> a -> a) -> a -> v a -> v a
{-# INLINE inplace_prescanl #-}
inplace_prescanl f z = unstream . inplace (MStream.prescanl f z) . stream

{-# RULES

"prescanl -> inplace_prescanl [Vector]" prescanl = inplace_prescanl

 #-}

-- | Prefix scan with strict accumulator
prescanl' :: (Vector v a, Vector v b) => (a -> b -> a) -> a -> v b -> v a
{-# INLINE prescanl' #-}
prescanl' f z = unstream . Stream.prescanl' f z . stream

inplace_prescanl' :: Vector v a => (a -> a -> a) -> a -> v a -> v a
{-# INLINE inplace_prescanl' #-}
inplace_prescanl' f z = unstream . inplace (MStream.prescanl' f z) . stream

{-# RULES

"prescanl' -> inplace_prescanl' [Vector]" prescanl' = inplace_prescanl'

 #-}

-- | Suffix scan
postscanl :: (Vector v a, Vector v b) => (a -> b -> a) -> a -> v b -> v a
{-# INLINE postscanl #-}
postscanl f z = unstream . Stream.postscanl f z . stream

inplace_postscanl :: Vector v a => (a -> a -> a) -> a -> v a -> v a
{-# INLINE inplace_postscanl #-}
inplace_postscanl f z = unstream . inplace (MStream.postscanl f z) . stream

{-# RULES

"postscanl -> inplace_postscanl [Vector]" postscanl = inplace_postscanl

 #-}

-- | Suffix scan with strict accumulator
postscanl' :: (Vector v a, Vector v b) => (a -> b -> a) -> a -> v b -> v a
{-# INLINE postscanl' #-}
postscanl' f z = unstream . Stream.postscanl' f z . stream

inplace_postscanl' :: Vector v a => (a -> a -> a) -> a -> v a -> v a
{-# INLINE inplace_postscanl' #-}
inplace_postscanl' f z = unstream . inplace (MStream.postscanl' f z) . stream

{-# RULES

"postscanl' -> inplace_postscanl' [Vector]" postscanl' = inplace_postscanl'

 #-}

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
enumFromTo from to = from `seq` to `seq` unfoldr enumFromTo_go (fromEnum from)
  where
    to_i = fromEnum to
    enumFromTo_go i | i <= to_i = Just (toEnum i, i + 1)
                    | otherwise = Nothing

enumFromThenTo :: (Vector v a, Enum a) => a -> a -> a -> v a
{-# INLINE enumFromThenTo #-}
enumFromThenTo from next to = from `seq` next `seq` to `seq` unfoldr enumFromThenTo_go from_i
  where
    from_i = fromEnum from
    to_i = fromEnum to
    step_i = fromEnum next - from_i
    enumFromThenTo_go i | i <= to_i = Just (toEnum i, i + step_i)
                        | otherwise = Nothing

-- | Convert a vector to a list
toList :: Vector v a => v a -> [a]
{-# INLINE toList #-}
toList = Stream.toList . stream

-- | Convert a list to a vector
fromList :: Vector v a => [a] -> v a
{-# INLINE fromList #-}
fromList = unstream . Stream.fromList

