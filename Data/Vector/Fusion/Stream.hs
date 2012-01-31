{-# LANGUAGE FlexibleInstances, Rank2Types, BangPatterns #-}

-- |
-- Module      : Data.Vector.Fusion.Stream
-- Copyright   : (c) Roman Leshchinskiy 2008-2010
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
-- 
-- Streams for stream fusion
--

module Data.Vector.Fusion.Stream (
  -- * Types
  Step(..), Chunk(..), Facets, MFacets,

  -- * In-place markers
  inplace,

  -- * Size hints
  size, sized,

  -- * Length information
  length, null,

  -- * Construction
  empty, singleton, cons, snoc, replicate, generate, (++),

  -- * Accessing individual elements
  head, last, (!!), (!?),

  -- * Substreams
  slice, init, tail, take, drop,

  -- * Mapping
  map, concatMap, flatten, unbox,
  
  -- * Zipping
  indexed, indexedR,
  zipWith, zipWith3, zipWith4, zipWith5, zipWith6,
  zip, zip3, zip4, zip5, zip6,

  -- * Filtering
  filter, takeWhile, dropWhile,

  -- * Searching
  elem, notElem, find, findIndex,

  -- * Folding
  foldl, foldl1, foldl', foldl1', foldr, foldr1,

  -- * Specialised folds
  and, or,

  -- * Unfolding
  unfoldr, unfoldrN, iterateN,

  -- * Scans
  prescanl, prescanl',
  postscanl, postscanl',
  scanl, scanl',
  scanl1, scanl1',

  -- * Enumerations
  enumFromStepN, enumFromTo, enumFromThenTo,

  -- * Conversions
  toList, fromList, fromListN, unsafeFromList, liftStream,
  fromVector, reVector, fromVectors, concatVectors,

  -- * Monadic combinators
  mapM, mapM_, zipWithM, zipWithM_, filterM, foldM, fold1M, foldM', fold1M',

  eq, cmp
) where

import Data.Vector.Generic.Base ( Vector )
import Data.Vector.Fusion.Stream.Size
import Data.Vector.Fusion.Util
import Data.Vector.Fusion.Stream.Monadic ( Step(..), Chunk(..), SPEC(..) )
import qualified Data.Vector.Fusion.Stream.Monadic as M

import Prelude hiding ( length, null,
                        replicate, (++),
                        head, last, (!!),
                        init, tail, take, drop,
                        map, concatMap,
                        zipWith, zipWith3, zip, zip3,
                        filter, takeWhile, dropWhile,
                        elem, notElem,
                        foldl, foldl1, foldr, foldr1,
                        and, or,
                        scanl, scanl1,
                        enumFromTo, enumFromThenTo,
                        mapM, mapM_ )

import GHC.Base ( build )

#include "vector.h"

-- | The type of pure streams 
type Facets = M.Facets Id

-- | Alternative name for monadic streams
type MFacets = M.Facets

inplace :: (forall m. Monad m => M.Facets m v a -> M.Facets m v b)
        -> Facets v a -> Facets v b
{-# INLINE_STREAM inplace #-}
inplace f s = s `seq` f s

{-# RULES

"inplace/inplace [Vector]"
  forall (f :: forall m. Monad m => MFacets m v a -> MFacets m v a)
         (g :: forall m. Monad m => MFacets m v a -> MFacets m v a)
         s.
  inplace f (inplace g s) = inplace (f . g) s

  #-}

-- | Convert a pure stream to a monadic stream
liftStream :: Monad m => Facets v a -> M.Facets m v a
{-# INLINE_STREAM liftStream #-}
liftStream (M.Facets (M.Unf step s) (M.Unf vstep t) v sz)
    = M.Facets (M.Unf (return . unId . step) s)
               (M.Unf (return . unId . vstep) t) v sz

-- | 'Size' hint of a 'Facets'
size :: Facets v a -> Size
{-# INLINE size #-}
size = M.size

-- | Attach a 'Size' hint to a 'Facets'
sized :: Facets v a -> Size -> Facets v a
{-# INLINE sized #-}
sized = M.sized

-- Length
-- ------

-- | Length of a 'Facets'
length :: Facets v a -> Int
{-# INLINE length #-}
length = unId . M.length

-- | Check if a 'Facets' is empty
null :: Facets v a -> Bool
{-# INLINE null #-}
null = unId . M.null

-- Construction
-- ------------

-- | Empty 'Facets'
empty :: Facets v a
{-# INLINE empty #-}
empty = M.empty

-- | Singleton 'Facets'
singleton :: a -> Facets v a
{-# INLINE singleton #-}
singleton = M.singleton

-- | Replicate a value to a given length
replicate :: Int -> a -> Facets v a
{-# INLINE replicate #-}
replicate = M.replicate

-- | Generate a stream from its indices
generate :: Int -> (Int -> a) -> Facets v a
{-# INLINE generate #-}
generate = M.generate

-- | Prepend an element
cons :: a -> Facets v a -> Facets v a
{-# INLINE cons #-}
cons = M.cons

-- | Append an element
snoc :: Facets v a -> a -> Facets v a
{-# INLINE snoc #-}
snoc = M.snoc

infixr 5 ++
-- | Concatenate two 'Facets's
(++) :: Facets v a -> Facets v a -> Facets v a
{-# INLINE (++) #-}
(++) = (M.++)

-- Accessing elements
-- ------------------

-- | First element of the 'Facets' or error if empty
head :: Facets v a -> a
{-# INLINE head #-}
head = unId . M.head

-- | Last element of the 'Facets' or error if empty
last :: Facets v a -> a
{-# INLINE last #-}
last = unId . M.last

infixl 9 !!
-- | Element at the given position
(!!) :: Facets v a -> Int -> a
{-# INLINE (!!) #-}
s !! i = unId (s M.!! i)

infixl 9 !?
-- | Element at the given position or 'Nothing' if out of bounds
(!?) :: Facets v a -> Int -> Maybe a
{-# INLINE (!?) #-}
s !? i = unId (s M.!? i)

-- Substreams
-- ----------

-- | Extract a substream of the given length starting at the given position.
slice :: Int   -- ^ starting index
      -> Int   -- ^ length
      -> Facets v a
      -> Facets v a
{-# INLINE slice #-}
slice = M.slice

-- | All but the last element
init :: Facets v a -> Facets v a
{-# INLINE init #-}
init = M.init

-- | All but the first element
tail :: Facets v a -> Facets v a
{-# INLINE tail #-}
tail = M.tail

-- | The first @n@ elements
take :: Int -> Facets v a -> Facets v a
{-# INLINE take #-}
take = M.take

-- | All but the first @n@ elements
drop :: Int -> Facets v a -> Facets v a
{-# INLINE drop #-}
drop = M.drop

-- Mapping
-- ---------------

-- | Map a function over a 'Facets'
map :: (a -> b) -> Facets v a -> Facets v b
{-# INLINE map #-}
map = M.map

unbox :: Facets v (Box a) -> Facets v a
{-# INLINE unbox #-}
unbox = M.unbox

concatMap :: (a -> Facets v b) -> Facets v a -> Facets v b
{-# INLINE concatMap #-}
concatMap = M.concatMap

-- Zipping
-- -------

-- | Pair each element in a 'Facets' with its index
indexed :: Facets v a -> Facets v (Int,a)
{-# INLINE indexed #-}
indexed = M.indexed

-- | Pair each element in a 'Facets' with its index, starting from the right
-- and counting down
indexedR :: Int -> Facets v a -> Facets v (Int,a)
{-# INLINE_STREAM indexedR #-}
indexedR = M.indexedR

-- | Zip two 'Facets's with the given function
zipWith :: (a -> b -> c) -> Facets v a -> Facets v b -> Facets v c
{-# INLINE zipWith #-}
zipWith = M.zipWith

-- | Zip three 'Facets's with the given function
zipWith3 :: (a -> b -> c -> d) -> Facets v a -> Facets v b -> Facets v c -> Facets v d
{-# INLINE zipWith3 #-}
zipWith3 = M.zipWith3

zipWith4 :: (a -> b -> c -> d -> e)
                    -> Facets v a -> Facets v b -> Facets v c -> Facets v d
                    -> Facets v e
{-# INLINE zipWith4 #-}
zipWith4 = M.zipWith4

zipWith5 :: (a -> b -> c -> d -> e -> f)
                    -> Facets v a -> Facets v b -> Facets v c -> Facets v d
                    -> Facets v e -> Facets v f
{-# INLINE zipWith5 #-}
zipWith5 = M.zipWith5

zipWith6 :: (a -> b -> c -> d -> e -> f -> g)
                    -> Facets v a -> Facets v b -> Facets v c -> Facets v d
                    -> Facets v e -> Facets v f -> Facets v g
{-# INLINE zipWith6 #-}
zipWith6 = M.zipWith6

zip :: Facets v a -> Facets v b -> Facets v (a,b)
{-# INLINE zip #-}
zip = M.zip

zip3 :: Facets v a -> Facets v b -> Facets v c -> Facets v (a,b,c)
{-# INLINE zip3 #-}
zip3 = M.zip3

zip4 :: Facets v a -> Facets v b -> Facets v c -> Facets v d
                -> Facets v (a,b,c,d)
{-# INLINE zip4 #-}
zip4 = M.zip4

zip5 :: Facets v a -> Facets v b -> Facets v c -> Facets v d
                -> Facets v e -> Facets v (a,b,c,d,e)
{-# INLINE zip5 #-}
zip5 = M.zip5

zip6 :: Facets v a -> Facets v b -> Facets v c -> Facets v d
                -> Facets v e -> Facets v f -> Facets v (a,b,c,d,e,f)
{-# INLINE zip6 #-}
zip6 = M.zip6

-- Filtering
-- ---------

-- | Drop elements which do not satisfy the predicate
filter :: (a -> Bool) -> Facets v a -> Facets v a
{-# INLINE filter #-}
filter = M.filter

-- | Longest prefix of elements that satisfy the predicate
takeWhile :: (a -> Bool) -> Facets v a -> Facets v a
{-# INLINE takeWhile #-}
takeWhile = M.takeWhile

-- | Drop the longest prefix of elements that satisfy the predicate
dropWhile :: (a -> Bool) -> Facets v a -> Facets v a
{-# INLINE dropWhile #-}
dropWhile = M.dropWhile

-- Searching
-- ---------

infix 4 `elem`
-- | Check whether the 'Facets' contains an element
elem :: Eq a => a -> Facets v a -> Bool
{-# INLINE elem #-}
elem x = unId . M.elem x

infix 4 `notElem`
-- | Inverse of `elem`
notElem :: Eq a => a -> Facets v a -> Bool
{-# INLINE notElem #-}
notElem x = unId . M.notElem x

-- | Yield 'Just' the first element matching the predicate or 'Nothing' if no
-- such element exists.
find :: (a -> Bool) -> Facets v a -> Maybe a
{-# INLINE find #-}
find f = unId . M.find f

-- | Yield 'Just' the index of the first element matching the predicate or
-- 'Nothing' if no such element exists.
findIndex :: (a -> Bool) -> Facets v a -> Maybe Int
{-# INLINE findIndex #-}
findIndex f = unId . M.findIndex f

-- Folding
-- -------

-- | Left fold
foldl :: (a -> b -> a) -> a -> Facets v b -> a
{-# INLINE foldl #-}
foldl f z = unId . M.foldl f z

-- | Left fold on non-empty 'Facets's
foldl1 :: (a -> a -> a) -> Facets v a -> a
{-# INLINE foldl1 #-}
foldl1 f = unId . M.foldl1 f

-- | Left fold with strict accumulator
foldl' :: (a -> b -> a) -> a -> Facets v b -> a
{-# INLINE foldl' #-}
foldl' f z = unId . M.foldl' f z

-- | Left fold on non-empty 'Facets's with strict accumulator
foldl1' :: (a -> a -> a) -> Facets v a -> a
{-# INLINE foldl1' #-}
foldl1' f = unId . M.foldl1' f

-- | Right fold
foldr :: (a -> b -> b) -> b -> Facets v a -> b
{-# INLINE foldr #-}
foldr f z = unId . M.foldr f z

-- | Right fold on non-empty 'Facets's
foldr1 :: (a -> a -> a) -> Facets v a -> a
{-# INLINE foldr1 #-}
foldr1 f = unId . M.foldr1 f

-- Specialised folds
-- -----------------

and :: Facets v Bool -> Bool
{-# INLINE and #-}
and = unId . M.and

or :: Facets v Bool -> Bool
{-# INLINE or #-}
or = unId . M.or

-- Unfolding
-- ---------

-- | Unfold
unfoldr :: (s -> Maybe (a, s)) -> s -> Facets v a
{-# INLINE unfoldr #-}
unfoldr = M.unfoldr

-- | Unfold at most @n@ elements
unfoldrN :: Int -> (s -> Maybe (a, s)) -> s -> Facets v a
{-# INLINE unfoldrN #-}
unfoldrN = M.unfoldrN

-- | Apply function n-1 times to value. Zeroth element is original value.
iterateN :: Int -> (a -> a) -> a -> Facets v a
{-# INLINE iterateN #-}
iterateN = M.iterateN

-- Scans
-- -----

-- | Prefix scan
prescanl :: (a -> b -> a) -> a -> Facets v b -> Facets v a
{-# INLINE prescanl #-}
prescanl = M.prescanl

-- | Prefix scan with strict accumulator
prescanl' :: (a -> b -> a) -> a -> Facets v b -> Facets v a
{-# INLINE prescanl' #-}
prescanl' = M.prescanl'

-- | Suffix scan
postscanl :: (a -> b -> a) -> a -> Facets v b -> Facets v a
{-# INLINE postscanl #-}
postscanl = M.postscanl

-- | Suffix scan with strict accumulator
postscanl' :: (a -> b -> a) -> a -> Facets v b -> Facets v a
{-# INLINE postscanl' #-}
postscanl' = M.postscanl'

-- | Haskell-style scan
scanl :: (a -> b -> a) -> a -> Facets v b -> Facets v a
{-# INLINE scanl #-}
scanl = M.scanl

-- | Haskell-style scan with strict accumulator
scanl' :: (a -> b -> a) -> a -> Facets v b -> Facets v a
{-# INLINE scanl' #-}
scanl' = M.scanl'

-- | Scan over a non-empty 'Facets'
scanl1 :: (a -> a -> a) -> Facets v a -> Facets v a
{-# INLINE scanl1 #-}
scanl1 = M.scanl1

-- | Scan over a non-empty 'Facets' with a strict accumulator
scanl1' :: (a -> a -> a) -> Facets v a -> Facets v a
{-# INLINE scanl1' #-}
scanl1' = M.scanl1'


-- Comparisons
-- -----------

-- | Check if two 'Facets's are equal
eq :: Eq a => Facets v a -> Facets v a -> Bool
{-# INLINE eq #-}
eq x y = unId (M.eq x y)

-- | Lexicographically compare two 'Facets's
cmp :: Ord a => Facets v a -> Facets v a -> Ordering
{-# INLINE cmp #-}
cmp x y = unId (M.cmp x y)

instance Eq a => Eq (M.Facets Id v a) where
  {-# INLINE (==) #-}
  (==) = eq

instance Ord a => Ord (M.Facets Id v a) where
  {-# INLINE compare #-}
  compare = cmp

-- Monadic combinators
-- -------------------

-- | Apply a monadic action to each element of the stream, producing a monadic
-- stream of results
mapM :: Monad m => (a -> m b) -> Facets v a -> M.Facets m v b
{-# INLINE mapM #-}
mapM f = M.mapM f . liftStream

-- | Apply a monadic action to each element of the stream
mapM_ :: Monad m => (a -> m b) -> Facets v a -> m ()
{-# INLINE mapM_ #-}
mapM_ f = M.mapM_ f . liftStream

zipWithM :: Monad m => (a -> b -> m c) -> Facets v a -> Facets v b -> M.Facets m v c
{-# INLINE zipWithM #-}
zipWithM f as bs = M.zipWithM f (liftStream as) (liftStream bs)

zipWithM_ :: Monad m => (a -> b -> m c) -> Facets v a -> Facets v b -> m ()
{-# INLINE zipWithM_ #-}
zipWithM_ f as bs = M.zipWithM_ f (liftStream as) (liftStream bs)

-- | Yield a monadic stream of elements that satisfy the monadic predicate
filterM :: Monad m => (a -> m Bool) -> Facets v a -> M.Facets m v a
{-# INLINE filterM #-}
filterM f = M.filterM f . liftStream

-- | Monadic fold
foldM :: Monad m => (a -> b -> m a) -> a -> Facets v b -> m a
{-# INLINE foldM #-}
foldM m z = M.foldM m z . liftStream

-- | Monadic fold over non-empty stream
fold1M :: Monad m => (a -> a -> m a) -> Facets v a -> m a
{-# INLINE fold1M #-}
fold1M m = M.fold1M m . liftStream

-- | Monadic fold with strict accumulator
foldM' :: Monad m => (a -> b -> m a) -> a -> Facets v b -> m a
{-# INLINE foldM' #-}
foldM' m z = M.foldM' m z . liftStream

-- | Monad fold over non-empty stream with strict accumulator
fold1M' :: Monad m => (a -> a -> m a) -> Facets v a -> m a
{-# INLINE fold1M' #-}
fold1M' m = M.fold1M' m . liftStream

-- Enumerations
-- ------------

-- | Yield a 'Facets' of the given length containing the values @x@, @x+y@,
-- @x+y+y@ etc.
enumFromStepN :: Num a => a -> a -> Int -> Facets v a
{-# INLINE enumFromStepN #-}
enumFromStepN = M.enumFromStepN

-- | Enumerate values
--
-- /WARNING:/ This operations can be very inefficient. If at all possible, use
-- 'enumFromStepN' instead.
enumFromTo :: Enum a => a -> a -> Facets v a
{-# INLINE enumFromTo #-}
enumFromTo = M.enumFromTo

-- | Enumerate values with a given step.
--
-- /WARNING:/ This operations is very inefficient. If at all possible, use
-- 'enumFromStepN' instead.
enumFromThenTo :: Enum a => a -> a -> a -> Facets v a
{-# INLINE enumFromThenTo #-}
enumFromThenTo = M.enumFromThenTo

-- Conversions
-- -----------

-- | Convert a 'Facets' to a list
toList :: Facets v a -> [a]
{-# INLINE toList #-}
-- toList s = unId (M.toList s)
toList s = build (\c n -> toListFB c n s)

-- This supports foldr/build list fusion that GHC implements
toListFB :: (a -> b -> b) -> b -> Facets v a -> b
{-# INLINE [0] toListFB #-}
toListFB c n M.Facets{M.sElems = M.Unf step s} = go s
  where
    go s = case unId (step s) of
             Yield x s' -> x `c` go s'
             Skip    s' -> go s'
             Done       -> n

-- | Create a 'Facets' from a list
fromList :: [a] -> Facets v a
{-# INLINE fromList #-}
fromList = M.fromList

-- | Create a 'Facets' from the first @n@ elements of a list
--
-- > fromListN n xs = fromList (take n xs)
fromListN :: Int -> [a] -> Facets v a
{-# INLINE fromListN #-}
fromListN = M.fromListN

unsafeFromList :: Size -> [a] -> Facets v a
{-# INLINE unsafeFromList #-}
unsafeFromList = M.unsafeFromList

fromVector :: Vector v a => v a -> Facets v a
{-# INLINE fromVector #-}
fromVector = M.fromVector

reVector :: Facets u a -> Facets v a
{-# INLINE reVector #-}
reVector = M.reVector

fromVectors :: Vector v a => [v a] -> Facets v a
{-# INLINE fromVectors #-}
fromVectors = M.fromVectors

concatVectors :: Vector v a => Facets u (v a) -> Facets v a
{-# INLINE concatVectors #-}
concatVectors = M.concatVectors

-- | Create a 'Facets' of values from a 'Facets' of streamable things
flatten :: (a -> s) -> (s -> Step s b) -> Size -> Facets v a -> Facets v b
{-# INLINE_STREAM flatten #-}
flatten mk istep sz = M.flatten (return . mk) (return . istep) sz . liftStream

