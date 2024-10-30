{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Module      : Data.Vector.Strict
-- Copyright   : (c) Roman Leshchinskiy 2008-2010
--                   Alexey Kuleshevich 2020-2022
--                   Aleksey Khudyakov 2020-2022
--                   Andrew Lelechenko 2020-2022
-- License     : BSD-style
--
-- Maintainer  : Haskell Libraries Team <libraries@haskell.org>
-- Stability   : experimental
-- Portability : non-portable
--
-- Immutable strict boxed vectors (that is, polymorphic arrays capable
-- of holding any Haskell value). Vectors created using API for
-- immutable vector will have all elements evaluated to WHNF. Note
-- it's possible to create vector containing bottoms using mutable API
-- ('Data.Vector.Strict.Mutable.new' initialize vector with ⊥) fill
-- but all subsequent writes will be evauated to WHNF.
--
-- For unboxed arrays, use "Data.Vector.Unboxed".
module Data.Vector.Strict (
  -- * Boxed vectors
  Vector, MVector,

  -- * Accessors

  -- ** Length information
  length, null,

  -- ** Indexing
  (!), (!?), head, last,
  unsafeIndex, unsafeHead, unsafeLast,

  -- ** Monadic indexing
  indexM, headM, lastM,
  unsafeIndexM, unsafeHeadM, unsafeLastM,

  -- ** Extracting subvectors (slicing)
  slice, init, tail, take, drop, splitAt, uncons, unsnoc,
  unsafeSlice, unsafeInit, unsafeTail, unsafeTake, unsafeDrop,

  -- * Construction

  -- ** Initialisation
  empty, singleton, replicate, generate, iterateN,

  -- ** Monadic initialisation
  replicateM, generateM, iterateNM, create, createT,

  -- ** Unfolding
  unfoldr, unfoldrN, unfoldrExactN,
  unfoldrM, unfoldrNM, unfoldrExactNM,
  constructN, constructrN,

  -- ** Enumeration
  enumFromN, enumFromStepN, enumFromTo, enumFromThenTo,

  -- ** Concatenation
  cons, snoc, (++), concat,

  -- ** Restricting memory usage
  force,

  -- * Modifying vectors

  -- ** Bulk updates
  (//), update, update_,
  unsafeUpd, unsafeUpdate, unsafeUpdate_,

  -- ** Accumulations
  accum, accumulate, accumulate_,
  unsafeAccum, unsafeAccumulate, unsafeAccumulate_,

  -- ** Permutations
  reverse, backpermute, unsafeBackpermute,

  -- ** Safe destructive updates
  modify,

  -- * Elementwise operations

  -- ** Indexing
  indexed,

  -- ** Mapping
  map, imap, concatMap,

  -- ** Monadic mapping
  mapM, imapM, mapM_, imapM_, forM, forM_,
  iforM, iforM_,

  -- ** Zipping
  zipWith, zipWith3, zipWith4, zipWith5, zipWith6,
  izipWith, izipWith3, izipWith4, izipWith5, izipWith6,
  zip, zip3, zip4, zip5, zip6,

  -- ** Monadic zipping
  zipWithM, izipWithM, zipWithM_, izipWithM_,

  -- ** Unzipping
  unzip, unzip3, unzip4, unzip5, unzip6,

  -- * Working with predicates

  -- ** Filtering
  filter, ifilter, filterM, uniq,
  mapMaybe, imapMaybe,
  mapMaybeM, imapMaybeM,
  catMaybes,
  takeWhile, dropWhile,

  -- ** Partitioning
  partition, unstablePartition, partitionWith, span, break, spanR, breakR, groupBy, group,

  -- ** Searching
  elem, notElem, find, findIndex, findIndexR, findIndices, elemIndex, elemIndices,

  -- * Folding
  foldl, foldl1, foldl', foldl1', foldr, foldr1, foldr', foldr1',
  ifoldl, ifoldl', ifoldr, ifoldr',
  foldMap, foldMap',

  -- ** Specialised folds
  all, any, and, or,
  sum, product,
  maximum, maximumBy, maximumOn,
  minimum, minimumBy, minimumOn,
  minIndex, minIndexBy, maxIndex, maxIndexBy,

  -- ** Monadic folds
  foldM, ifoldM, foldM', ifoldM',
  fold1M, fold1M',foldM_, ifoldM_,
  foldM'_, ifoldM'_, fold1M_, fold1M'_,

  -- ** Monadic sequencing
  sequence, sequence_,

  -- * Scans
  prescanl, prescanl',
  postscanl, postscanl',
  scanl, scanl', scanl1, scanl1',
  iscanl, iscanl',
  prescanr, prescanr',
  postscanr, postscanr',
  scanr, scanr', scanr1, scanr1',
  iscanr, iscanr',

  -- ** Comparisons
  eqBy, cmpBy,

  -- * Conversions

  -- ** Lists
  toList, Data.Vector.Strict.fromList, Data.Vector.Strict.fromListN,
  -- ** Lazy vectors
  toLazy, fromLazy,
  -- ** Arrays
  toArray, fromArray, toArraySlice, unsafeFromArraySlice,

  -- ** Other vector types
  G.convert,

  -- ** Mutable vectors
  freeze, thaw, copy, unsafeFreeze, unsafeThaw, unsafeCopy
) where

import Data.Coerce
import Data.Vector.Strict.Mutable  ( MVector(..) )
import Data.Primitive.Array
import qualified Data.Vector.Fusion.Bundle as Bundle
import qualified Data.Vector.Generic as G
import qualified Data.Vector as V

import Control.DeepSeq ( NFData(rnf)
#if MIN_VERSION_deepseq(1,4,3)
                       , NFData1(liftRnf)
#endif
                       )

import Control.Monad ( MonadPlus(..), ap )
#if !MIN_VERSION_base(4,13,0)
import Control.Monad (fail)
#endif
import Control.Monad.ST ( ST, runST )
import Control.Monad.Primitive
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix ( MonadFix (mfix) )
import Control.Monad.Zip
import Data.Function ( fix )

import Prelude
  ( Eq(..), Ord(..), Num, Enum, Monoid, Functor, Monad, Show, Bool, Ordering(..), Int, Maybe, Either
  , return, showsPrec, fmap, otherwise, id, flip, const
  , (>>=), (+), (-), (.), ($), seq)

import Data.Functor.Classes (Eq1 (..), Ord1 (..), Read1 (..), Show1 (..))
import Data.Typeable  ( Typeable )
import Data.Data      ( Data(..) )
import Text.Read      ( Read(..), readListPrecDefault )
import Data.Semigroup ( Semigroup(..) )

import qualified Control.Applicative as Applicative
import qualified Data.Foldable as Foldable
import qualified Data.Traversable as Traversable

import qualified GHC.Exts as Exts (IsList(..))


-- | Strict boxed vectors, supporting efficient slicing.
newtype Vector a = Vector (V.Vector a)
  deriving (Typeable, Foldable.Foldable, Semigroup, Monoid)

-- NOTE: [GND for strict vector]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Strict boxed vectors (both mutable an immutable) are newtypes over
-- lazy ones. This makes it possible to use GND to derive instances.
-- However one must take care to preserve strictness since Vector
-- instance for lazy vectors would be used.
--
-- In general it's OK to derive instances where vectors are passed as
-- parameters (e.g. Eq, Ord) and not OK to derive ones where new
-- vector is created (e.g. Read, Functor)

liftRnfV :: (a -> ()) -> Vector a -> ()
liftRnfV elemRnf = foldl' (\_ -> elemRnf) ()

instance NFData a => NFData (Vector a) where
  rnf = liftRnfV rnf
  {-# INLINEABLE rnf #-}

#if MIN_VERSION_deepseq(1,4,3)
-- | @since 0.13.2.0
instance NFData1 Vector where
  liftRnf = liftRnfV
  {-# INLINEABLE liftRnf #-}
#endif

instance Show a => Show (Vector a) where
  showsPrec = G.showsPrec

instance Read a => Read (Vector a) where
  readPrec = G.readPrec
  readListPrec = readListPrecDefault

instance Show1 Vector where
  liftShowsPrec = G.liftShowsPrec

instance Read1 Vector where
  liftReadsPrec = G.liftReadsPrec

instance Exts.IsList (Vector a) where
  type Item (Vector a) = a
  fromList = Data.Vector.Strict.fromList
  fromListN = Data.Vector.Strict.fromListN
  toList = toList

instance Data a => Data (Vector a) where
  gfoldl       = G.gfoldl
  toConstr _   = G.mkVecConstr "Data.Vector.Strict.Vector"
  gunfold      = G.gunfold
  dataTypeOf _ = G.mkVecType "Data.Vector.Strict.Vector"
  dataCast1    = G.dataCast

type instance G.Mutable Vector = MVector

instance G.Vector Vector a where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze = coerce (G.basicUnsafeFreeze @V.Vector @a)
  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw = coerce (G.basicUnsafeThaw @V.Vector @a)
  {-# INLINE basicLength #-}
  basicLength = coerce (G.basicLength @V.Vector @a)
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice = coerce (G.basicUnsafeSlice @V.Vector @a)
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM = coerce (G.basicUnsafeIndexM @V.Vector @a)
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy = coerce (G.basicUnsafeCopy @V.Vector @a)
  {-# INLINE elemseq #-}
  elemseq _ = seq

-- See NOTE: [GND for strict vector]
--
-- Deriving strategies are only available since 8.2. So we can't use
-- deriving newtype until we drop support for 8.0
instance Eq a => Eq (Vector a) where
  {-# INLINE (==) #-}
  (==) = coerce ((==) @(V.Vector a))

-- See NOTE: [GND for strict vector]
instance Ord a => Ord (Vector a) where
  {-# INLINE compare #-}
  compare = coerce (compare @(V.Vector a))
  {-# INLINE (<) #-}
  (<)  = coerce ((<)  @(V.Vector a))
  {-# INLINE (<=) #-}
  (<=) = coerce ((<=) @(V.Vector a))
  {-# INLINE (>) #-}
  (>)  = coerce ((>)  @(V.Vector a))
  {-# INLINE (>=) #-}
  (>=) = coerce ((>=) @(V.Vector a))

instance Eq1 Vector where
  liftEq eq xs ys = Bundle.eqBy eq (G.stream xs) (G.stream ys)

instance Ord1 Vector where
  liftCompare cmp xs ys = Bundle.cmpBy cmp (G.stream xs) (G.stream ys)

instance Functor Vector where
  {-# INLINE fmap #-}
  fmap = map

  {-# INLINE (<$) #-}
  (<$) = map . const

instance Monad Vector where
  {-# INLINE return #-}
  return = Applicative.pure

  {-# INLINE (>>=) #-}
  (>>=) = flip concatMap

#if !(MIN_VERSION_base(4,13,0))
  {-# INLINE fail #-}
  fail = Fail.fail -- == \ _str -> empty
#endif

-- | @since 0.13.2.0
instance Fail.MonadFail Vector where
  {-# INLINE fail #-}
  fail _ = empty

instance MonadPlus Vector where
  {-# INLINE mzero #-}
  mzero = empty

  {-# INLINE mplus #-}
  mplus = (++)

instance MonadZip Vector where
  {-# INLINE mzip #-}
  mzip = zip

  {-# INLINE mzipWith #-}
  mzipWith = zipWith

  {-# INLINE munzip #-}
  munzip = unzip

-- | This instance has the same semantics as the one for lists.
--
--  @since 0.13.2.0
instance MonadFix Vector where
  -- We take care to dispose of v0 as soon as possible (see headM docs).
  --
  -- It's perfectly safe to use non-monadic indexing within generate
  -- call since intermediate vector won't be created until result's
  -- value is demanded.
  {-# INLINE mfix #-}
  mfix f
    | null v0 = empty
    -- We take first element of resulting vector from v0 and create
    -- rest using generate. Note that cons should fuse with generate
    | otherwise = runST $ do
        h <- headM v0
        return $ cons h $
          generate (lv0 - 1) $
            \i -> fix (\a -> f a ! (i + 1))
    where
      -- Used to calculate size of resulting vector
      v0 = fix (f . head)
      !lv0 = length v0

instance Applicative.Applicative Vector where
  {-# INLINE pure #-}
  pure = singleton

  {-# INLINE (<*>) #-}
  (<*>) = ap

instance Applicative.Alternative Vector where
  {-# INLINE empty #-}
  empty = empty

  {-# INLINE (<|>) #-}
  (<|>) = (++)

instance Traversable.Traversable Vector where
  {-# INLINE traverse #-}
  traverse f xs =
      -- Get the length of the vector in /O(1)/ time
      let !n = G.length xs
      -- Use fromListN to be more efficient in construction of resulting vector
      -- Also behaves better with compact regions, preventing runtime exceptions
      in  Data.Vector.Strict.fromListN n Applicative.<$> Traversable.traverse f (toList xs)

  {-# INLINE mapM #-}
  mapM = mapM

  {-# INLINE sequence #-}
  sequence = sequence

-- Length information
-- ------------------

-- | /O(1)/ Yield the length of the vector.
--
-- @since 0.13.2.0
length :: Vector a -> Int
{-# INLINE length #-}
length = G.length

-- | /O(1)/ Test whether a vector is empty.
--
-- @since 0.13.2.0
null :: Vector a -> Bool
{-# INLINE null #-}
null = G.null

-- Indexing
-- --------

-- | O(1) Indexing.
--
-- @since 0.13.2.0
(!) :: Vector a -> Int -> a
{-# INLINE (!) #-}
(!) = (G.!)

-- | O(1) Safe indexing.
--
-- @since 0.13.2.0
(!?) :: Vector a -> Int -> Maybe a
{-# INLINE (!?) #-}
(!?) = (G.!?)

-- | /O(1)/ First element.
--
-- @since 0.13.2.0
head :: Vector a -> a
{-# INLINE head #-}
head = G.head

-- | /O(1)/ Last element.
--
-- @since 0.13.2.0
last :: Vector a -> a
{-# INLINE last #-}
last = G.last

-- | /O(1)/ Unsafe indexing without bounds checking.
--
-- @since 0.13.2.0
unsafeIndex :: Vector a -> Int -> a
{-# INLINE unsafeIndex #-}
unsafeIndex = G.unsafeIndex

-- | /O(1)/ First element, without checking if the vector is empty.
--
-- @since 0.13.2.0
unsafeHead :: Vector a -> a
{-# INLINE unsafeHead #-}
unsafeHead = G.unsafeHead

-- | /O(1)/ Last element, without checking if the vector is empty.
--
-- @since 0.13.2.0
unsafeLast :: Vector a -> a
{-# INLINE unsafeLast #-}
unsafeLast = G.unsafeLast

-- Monadic indexing
-- ----------------

-- | /O(1)/ Indexing in a monad.
--
-- The monad allows operations to be strict in the vector when necessary.
-- Suppose vector copying is implemented like this:
--
-- > copy mv v = ... write mv i (v ! i) ...
--
-- For lazy vectors, @v ! i@ would not be evaluated which means that @mv@
-- would unnecessarily retain a reference to @v@ in each element written.
--
-- With 'indexM', copying can be implemented like this instead:
--
-- > copy mv v = ... do
-- >                   x <- indexM v i
-- >                   write mv i x
--
-- Here, no references to @v@ are retained because indexing (but /not/ the
-- element) is evaluated eagerly.
--
-- @since 0.13.2.0
indexM :: Monad m => Vector a -> Int -> m a
{-# INLINE indexM #-}
indexM = G.indexM

-- | /O(1)/ First element of a vector in a monad. See 'indexM' for an
-- explanation of why this is useful.
--
-- @since 0.13.2.0
headM :: Monad m => Vector a -> m a
{-# INLINE headM #-}
headM = G.headM

-- | /O(1)/ Last element of a vector in a monad. See 'indexM' for an
-- explanation of why this is useful.
--
-- @since 0.13.2.0
lastM :: Monad m => Vector a -> m a
{-# INLINE lastM #-}
lastM = G.lastM

-- | /O(1)/ Indexing in a monad, without bounds checks. See 'indexM' for an
-- explanation of why this is useful.
--
-- @since 0.13.2.0
unsafeIndexM :: Monad m => Vector a -> Int -> m a
{-# INLINE unsafeIndexM #-}
unsafeIndexM = G.unsafeIndexM

-- | /O(1)/ First element in a monad, without checking for empty vectors.
-- See 'indexM' for an explanation of why this is useful.
--
-- @since 0.13.2.0
unsafeHeadM :: Monad m => Vector a -> m a
{-# INLINE unsafeHeadM #-}
unsafeHeadM = G.unsafeHeadM

-- | /O(1)/ Last element in a monad, without checking for empty vectors.
-- See 'indexM' for an explanation of why this is useful.
--
-- @since 0.13.2.0
unsafeLastM :: Monad m => Vector a -> m a
{-# INLINE unsafeLastM #-}
unsafeLastM = G.unsafeLastM

-- Extracting subvectors (slicing)
-- -------------------------------

-- | /O(1)/ Yield a slice of the vector without copying it. The vector must
-- contain at least @i+n@ elements.
--
-- @since 0.13.2.0
slice :: Int   -- ^ @i@ starting index
                 -> Int   -- ^ @n@ length
                 -> Vector a
                 -> Vector a
{-# INLINE slice #-}
slice = G.slice

-- | /O(1)/ Yield all but the last element without copying. The vector may not
-- be empty.
--
-- @since 0.13.2.0
init :: Vector a -> Vector a
{-# INLINE init #-}
init = G.init

-- | /O(1)/ Yield all but the first element without copying. The vector may not
-- be empty.
--
-- @since 0.13.2.0
tail :: Vector a -> Vector a
{-# INLINE tail #-}
tail = G.tail

-- | /O(1)/ Yield at the first @n@ elements without copying. The vector may
-- contain less than @n@ elements, in which case it is returned unchanged.
--
-- @since 0.13.2.0
take :: Int -> Vector a -> Vector a
{-# INLINE take #-}
take = G.take

-- | /O(1)/ Yield all but the first @n@ elements without copying. The vector may
-- contain less than @n@ elements, in which case an empty vector is returned.
--
-- @since 0.13.2.0
drop :: Int -> Vector a -> Vector a
{-# INLINE drop #-}
drop = G.drop

-- | /O(1)/ Yield the first @n@ elements paired with the remainder, without copying.
--
-- Note that @'splitAt' n v@ is equivalent to @('take' n v, 'drop' n v)@,
-- but slightly more efficient.
--
-- @since 0.13.2.0
splitAt :: Int -> Vector a -> (Vector a, Vector a)
{-# INLINE splitAt #-}
splitAt = G.splitAt

-- | /O(1)/ Yield the 'head' and 'tail' of the vector, or 'Nothing' if
-- the vector is empty.
--
-- @since 0.13.2.0
uncons :: Vector a -> Maybe (a, Vector a)
{-# INLINE uncons #-}
uncons = G.uncons

-- | /O(1)/ Yield the 'last' and 'init' of the vector, or 'Nothing' if
-- the vector is empty.
--
-- @since 0.13.2.0
unsnoc :: Vector a -> Maybe (Vector a, a)
{-# INLINE unsnoc #-}
unsnoc = G.unsnoc

-- | /O(1)/ Yield a slice of the vector without copying. The vector must
-- contain at least @i+n@ elements, but this is not checked.
--
-- @since 0.13.2.0
unsafeSlice :: Int   -- ^ @i@ starting index
                       -> Int   -- ^ @n@ length
                       -> Vector a
                       -> Vector a
{-# INLINE unsafeSlice #-}
unsafeSlice = G.unsafeSlice

-- | /O(1)/ Yield all but the last element without copying. The vector may not
-- be empty, but this is not checked.
--
-- @since 0.13.2.0
unsafeInit :: Vector a -> Vector a
{-# INLINE unsafeInit #-}
unsafeInit = G.unsafeInit

-- | /O(1)/ Yield all but the first element without copying. The vector may not
-- be empty, but this is not checked.
--
-- @since 0.13.2.0
unsafeTail :: Vector a -> Vector a
{-# INLINE unsafeTail #-}
unsafeTail = G.unsafeTail

-- | /O(1)/ Yield the first @n@ elements without copying. The vector must
-- contain at least @n@ elements, but this is not checked.
--
-- @since 0.13.2.0
unsafeTake :: Int -> Vector a -> Vector a
{-# INLINE unsafeTake #-}
unsafeTake = G.unsafeTake

-- | /O(1)/ Yield all but the first @n@ elements without copying. The vector
-- must contain at least @n@ elements, but this is not checked.
--
-- @since 0.13.2.0
unsafeDrop :: Int -> Vector a -> Vector a
{-# INLINE unsafeDrop #-}
unsafeDrop = G.unsafeDrop

-- Initialisation
-- --------------

-- | /O(1)/ The empty vector.
--
-- @since 0.13.2.0
empty :: Vector a
{-# INLINE empty #-}
empty = G.empty

-- | /O(1)/ A vector with exactly one element.
--
-- @since 0.13.2.0
singleton :: a -> Vector a
{-# INLINE singleton #-}
singleton = G.singleton

-- | /O(n)/ A vector of the given length with the same value in each position.
--
-- @since 0.13.2.0
replicate :: Int -> a -> Vector a
{-# INLINE replicate #-}
replicate = G.replicate

-- | /O(n)/ Construct a vector of the given length by applying the function to
-- each index.
--
-- @since 0.13.2.0
generate :: Int -> (Int -> a) -> Vector a
{-# INLINE generate #-}
generate = G.generate

-- | /O(n)/ Apply the function \(\max(n - 1, 0)\) times to an initial value, producing a vector
-- of length \(\max(n, 0)\). The 0th element will contain the initial value, which is why there
-- is one less function application than the number of elements in the produced vector.
--
-- \( \underbrace{x, f (x), f (f (x)), \ldots}_{\max(0,n)\rm{~elements}} \)
--
-- ===__Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.iterateN 0 undefined undefined :: V.Vector String
-- []
-- >>> V.iterateN 4 (\x -> x <> x) "Hi"
-- ["Hi","HiHi","HiHiHiHi","HiHiHiHiHiHiHiHi"]
--
-- @since 0.13.2.0
iterateN :: Int -> (a -> a) -> a -> Vector a
{-# INLINE iterateN #-}
iterateN = G.iterateN

-- Unfolding
-- ---------

-- | /O(n)/ Construct a vector by repeatedly applying the generator function
-- to a seed. The generator function yields 'Just' the next element and the
-- new seed or 'Nothing' if there are no more elements.
--
-- > unfoldr (\n -> if n == 0 then Nothing else Just (n,n-1)) 10
-- >  = <10,9,8,7,6,5,4,3,2,1>
--
-- @since 0.13.2.0
unfoldr :: (b -> Maybe (a, b)) -> b -> Vector a
{-# INLINE unfoldr #-}
unfoldr = G.unfoldr

-- | /O(n)/ Construct a vector with at most @n@ elements by repeatedly applying
-- the generator function to a seed. The generator function yields 'Just' the
-- next element and the new seed or 'Nothing' if there are no more elements.
--
-- > unfoldrN 3 (\n -> Just (n,n-1)) 10 = <10,9,8>
--
-- @since 0.13.2.0
unfoldrN :: Int -> (b -> Maybe (a, b)) -> b -> Vector a
{-# INLINE unfoldrN #-}
unfoldrN = G.unfoldrN

-- | /O(n)/ Construct a vector with exactly @n@ elements by repeatedly applying
-- the generator function to a seed. The generator function yields the
-- next element and the new seed.
--
-- > unfoldrExactN 3 (\n -> (n,n-1)) 10 = <10,9,8>
--
-- @since 0.13.2.0
unfoldrExactN  :: Int -> (b -> (a, b)) -> b -> Vector a
{-# INLINE unfoldrExactN #-}
unfoldrExactN = G.unfoldrExactN

-- | /O(n)/ Construct a vector by repeatedly applying the monadic
-- generator function to a seed. The generator function yields 'Just'
-- the next element and the new seed or 'Nothing' if there are no more
-- elements.
--
-- @since 0.13.2.0
unfoldrM :: (Monad m) => (b -> m (Maybe (a, b))) -> b -> m (Vector a)
{-# INLINE unfoldrM #-}
unfoldrM = G.unfoldrM

-- | /O(n)/ Construct a vector by repeatedly applying the monadic
-- generator function to a seed. The generator function yields 'Just'
-- the next element and the new seed or 'Nothing' if there are no more
-- elements.
--
-- @since 0.13.2.0
unfoldrNM :: (Monad m) => Int -> (b -> m (Maybe (a, b))) -> b -> m (Vector a)
{-# INLINE unfoldrNM #-}
unfoldrNM = G.unfoldrNM

-- | /O(n)/ Construct a vector with exactly @n@ elements by repeatedly
-- applying the monadic generator function to a seed. The generator
-- function yields the next element and the new seed.
--
-- @since 0.13.2.0
unfoldrExactNM :: (Monad m) => Int -> (b -> m (a, b)) -> b -> m (Vector a)
{-# INLINE unfoldrExactNM #-}
unfoldrExactNM = G.unfoldrExactNM

-- | /O(n)/ Construct a vector with @n@ elements by repeatedly applying the
-- generator function to the already constructed part of the vector.
--
-- > constructN 3 f = let a = f <> ; b = f <a> ; c = f <a,b> in <a,b,c>
--
-- @since 0.13.2.0
constructN :: Int -> (Vector a -> a) -> Vector a
{-# INLINE constructN #-}
constructN = G.constructN

-- | /O(n)/ Construct a vector with @n@ elements from right to left by
-- repeatedly applying the generator function to the already constructed part
-- of the vector.
--
-- > constructrN 3 f = let a = f <> ; b = f<a> ; c = f <b,a> in <c,b,a>
--
-- @since 0.13.2.0
constructrN :: Int -> (Vector a -> a) -> Vector a
{-# INLINE constructrN #-}
constructrN = G.constructrN

-- Enumeration
-- -----------

-- | /O(n)/ Yield a vector of the given length, containing the values @x@, @x+1@
-- etc. This operation is usually more efficient than 'enumFromTo'.
--
-- > enumFromN 5 3 = <5,6,7>
--
-- @since 0.13.2.0
enumFromN :: Num a => a -> Int -> Vector a
{-# INLINE enumFromN #-}
enumFromN = G.enumFromN

-- | /O(n)/ Yield a vector of the given length, containing the values @x@, @x+y@,
-- @x+y+y@ etc. This operations is usually more efficient than 'enumFromThenTo'.
--
-- > enumFromStepN 1 2 5 = <1,3,5,7,9>
--
-- @since 0.13.2.0
enumFromStepN :: Num a => a -> a -> Int -> Vector a
{-# INLINE enumFromStepN #-}
enumFromStepN = G.enumFromStepN

-- | /O(n)/ Enumerate values from @x@ to @y@.
--
-- /WARNING:/ This operation can be very inefficient. If possible, use
-- 'enumFromN' instead.
--
-- @since 0.13.2.0
enumFromTo :: Enum a => a -> a -> Vector a
{-# INLINE enumFromTo #-}
enumFromTo = G.enumFromTo

-- | /O(n)/ Enumerate values from @x@ to @y@ with a specific step @z@.
--
-- /WARNING:/ This operation can be very inefficient. If possible, use
-- 'enumFromStepN' instead.
--
-- @since 0.13.2.0
enumFromThenTo :: Enum a => a -> a -> a -> Vector a
{-# INLINE enumFromThenTo #-}
enumFromThenTo = G.enumFromThenTo

-- Concatenation
-- -------------

-- | /O(n)/ Prepend an element.
--
-- @since 0.13.2.0
cons :: a -> Vector a -> Vector a
{-# INLINE cons #-}
cons = G.cons

-- | /O(n)/ Append an element.
--
-- @since 0.13.2.0
snoc :: Vector a -> a -> Vector a
{-# INLINE snoc #-}
snoc = G.snoc

infixr 5 ++
-- | /O(m+n)/ Concatenate two vectors.
--
-- @since 0.13.2.0
(++) :: Vector a -> Vector a -> Vector a
{-# INLINE (++) #-}
(++) = (G.++)

-- | /O(n)/ Concatenate all vectors in the list.
--
-- @since 0.13.2.0
concat :: [Vector a] -> Vector a
{-# INLINE concat #-}
concat = G.concat

-- Monadic initialisation
-- ----------------------

-- | /O(n)/ Execute the monadic action the given number of times and store the
-- results in a vector.
--
-- @since 0.13.2.0
replicateM :: Monad m => Int -> m a -> m (Vector a)
{-# INLINE replicateM #-}
replicateM = G.replicateM

-- | /O(n)/ Construct a vector of the given length by applying the monadic
-- action to each index.
--
-- @since 0.13.2.0
generateM :: Monad m => Int -> (Int -> m a) -> m (Vector a)
{-# INLINE generateM #-}
generateM = G.generateM

-- | /O(n)/ Apply the monadic function \(\max(n - 1, 0)\) times to an initial value, producing a vector
-- of length \(\max(n, 0)\). The 0th element will contain the initial value, which is why there
-- is one less function application than the number of elements in the produced vector.
--
-- For a non-monadic version, see `iterateN`.
--
-- @since 0.13.2.0
iterateNM :: Monad m => Int -> (a -> m a) -> a -> m (Vector a)
{-# INLINE iterateNM #-}
iterateNM = G.iterateNM

-- | Execute the monadic action and freeze the resulting vector.
--
-- @
-- create (do { v \<- new 2; write v 0 \'a\'; write v 1 \'b\'; return v }) = \<'a','b'\>
-- @
--
-- @since 0.13.2.0
create :: (forall s. ST s (MVector s a)) -> Vector a
{-# INLINE create #-}
-- NOTE: eta-expanded due to http://hackage.haskell.org/trac/ghc/ticket/4120
create p = G.create p

-- | Execute the monadic action and freeze the resulting vectors.
--
-- @since 0.13.2.0
createT :: Traversable.Traversable f => (forall s. ST s (f (MVector s a))) -> f (Vector a)
{-# INLINE createT #-}
createT p = G.createT p



-- Restricting memory usage
-- ------------------------

-- | /O(n)/ Yield the argument, but force it not to retain any extra memory,
-- by copying it.
--
-- This is especially useful when dealing with slices. For example:
--
-- > force (slice 0 2 <huge vector>)
--
-- Here, the slice retains a reference to the huge vector. Forcing it creates
-- a copy of just the elements that belong to the slice and allows the huge
-- vector to be garbage collected.
--
-- @since 0.13.2.0
force :: Vector a -> Vector a
{-# INLINE force #-}
force = G.force

-- Bulk updates
-- ------------

-- | /O(m+n)/ For each pair @(i,a)@ from the list of index/value pairs,
-- replace the vector element at position @i@ by @a@.
--
-- > <5,9,2,7> // [(2,1),(0,3),(2,8)] = <3,9,8,7>
--
-- @since 0.13.2.0
(//) :: Vector a   -- ^ initial vector (of length @m@)
                -> [(Int, a)] -- ^ list of index/value pairs (of length @n@)
                -> Vector a
{-# INLINE (//) #-}
(//) = (G.//)

-- | /O(m+n)/ For each pair @(i,a)@ from the vector of index/value pairs,
-- replace the vector element at position @i@ by @a@.
--
-- > update <5,9,2,7> <(2,1),(0,3),(2,8)> = <3,9,8,7>
--
-- @since 0.13.2.0
update :: Vector a        -- ^ initial vector (of length @m@)
       -> Vector (Int, a) -- ^ vector of index/value pairs (of length @n@)
       -> Vector a
{-# INLINE update #-}
update = G.update

-- | /O(m+min(n1,n2))/ For each index @i@ from the index vector and the
-- corresponding value @a@ from the value vector, replace the element of the
-- initial vector at position @i@ by @a@.
--
-- > update_ <5,9,2,7>  <2,0,2> <1,3,8> = <3,9,8,7>
--
-- The function 'update' provides the same functionality and is usually more
-- convenient.
--
-- @
-- update_ xs is ys = 'update' xs ('zip' is ys)
-- @
--
-- @since 0.13.2.0
update_ :: Vector a   -- ^ initial vector (of length @m@)
        -> Vector Int -- ^ index vector (of length @n1@)
        -> Vector a   -- ^ value vector (of length @n2@)
        -> Vector a
{-# INLINE update_ #-}
update_ = G.update_

-- | Same as ('//'), but without bounds checking.
--
-- @since 0.13.2.0
unsafeUpd :: Vector a -> [(Int, a)] -> Vector a
{-# INLINE unsafeUpd #-}
unsafeUpd = G.unsafeUpd

-- | Same as 'update', but without bounds checking.
--
-- @since 0.13.2.0
unsafeUpdate :: Vector a -> Vector (Int, a) -> Vector a
{-# INLINE unsafeUpdate #-}
unsafeUpdate = G.unsafeUpdate

-- | Same as 'update_', but without bounds checking.
--
-- @since 0.13.2.0
unsafeUpdate_ :: Vector a -> Vector Int -> Vector a -> Vector a
{-# INLINE unsafeUpdate_ #-}
unsafeUpdate_ = G.unsafeUpdate_

-- Accumulations
-- -------------

-- | /O(m+n)/ For each pair @(i,b)@ from the list, replace the vector element
-- @a@ at position @i@ by @f a b@.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.accum (+) (V.fromList [1000,2000,3000]) [(2,4),(1,6),(0,3),(1,10)]
-- [1003,2016,3004]
--
-- @since 0.13.2.0
accum :: (a -> b -> a) -- ^ accumulating function @f@
      -> Vector a      -- ^ initial vector (of length @m@)
      -> [(Int,b)]     -- ^ list of index/value pairs (of length @n@)
      -> Vector a
{-# INLINE accum #-}
accum = G.accum

-- | /O(m+n)/ For each pair @(i,b)@ from the vector of pairs, replace the vector
-- element @a@ at position @i@ by @f a b@.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.accumulate (+) (V.fromList [1000,2000,3000]) (V.fromList [(2,4),(1,6),(0,3),(1,10)])
-- [1003,2016,3004]
--
-- @since 0.13.2.0
accumulate :: (a -> b -> a)  -- ^ accumulating function @f@
            -> Vector a       -- ^ initial vector (of length @m@)
            -> Vector (Int,b) -- ^ vector of index/value pairs (of length @n@)
            -> Vector a
{-# INLINE accumulate #-}
accumulate = G.accumulate

-- | /O(m+min(n1,n2))/ For each index @i@ from the index vector and the
-- corresponding value @b@ from the value vector,
-- replace the element of the initial vector at
-- position @i@ by @f a b@.
--
-- > accumulate_ (+) <5,9,2> <2,1,0,1> <4,6,3,7> = <5+3, 9+6+7, 2+4>
--
-- The function 'accumulate' provides the same functionality and is usually more
-- convenient.
--
-- @
-- accumulate_ f as is bs = 'accumulate' f as ('zip' is bs)
-- @
--
-- @since 0.13.2.0
accumulate_ :: (a -> b -> a) -- ^ accumulating function @f@
            -> Vector a      -- ^ initial vector (of length @m@)
            -> Vector Int    -- ^ index vector (of length @n1@)
            -> Vector b      -- ^ value vector (of length @n2@)
            -> Vector a
{-# INLINE accumulate_ #-}
accumulate_ = G.accumulate_

-- | Same as 'accum', but without bounds checking.
--
-- @since 0.13.2.0
unsafeAccum :: (a -> b -> a) -> Vector a -> [(Int,b)] -> Vector a
{-# INLINE unsafeAccum #-}
unsafeAccum = G.unsafeAccum

-- | Same as 'accumulate', but without bounds checking.
--
-- @since 0.13.2.0
unsafeAccumulate :: (a -> b -> a) -> Vector a -> Vector (Int,b) -> Vector a
{-# INLINE unsafeAccumulate #-}
unsafeAccumulate = G.unsafeAccumulate

-- | Same as 'accumulate_', but without bounds checking.
--
-- @since 0.13.2.0
unsafeAccumulate_
  :: (a -> b -> a) -> Vector a -> Vector Int -> Vector b -> Vector a
{-# INLINE unsafeAccumulate_ #-}
unsafeAccumulate_ = G.unsafeAccumulate_

-- Permutations
-- ------------

-- | /O(n)/ Reverse a vector.
--
-- @since 0.13.2.0
reverse :: Vector a -> Vector a
{-# INLINE reverse #-}
reverse = G.reverse

-- | /O(n)/ Yield the vector obtained by replacing each element @i@ of the
-- index vector by @xs'!'i@. This is equivalent to @'map' (xs'!') is@, but is
-- often much more efficient.
--
-- > backpermute <a,b,c,d> <0,3,2,3,1,0> = <a,d,c,d,b,a>
--
-- @since 0.13.2.0
backpermute :: Vector a -> Vector Int -> Vector a
{-# INLINE backpermute #-}
backpermute = G.backpermute

-- | Same as 'backpermute', but without bounds checking.
--
-- @since 0.13.2.0
unsafeBackpermute :: Vector a -> Vector Int -> Vector a
{-# INLINE unsafeBackpermute #-}
unsafeBackpermute = G.unsafeBackpermute

-- Safe destructive updates
-- ------------------------

-- | Apply a destructive operation to a vector. The operation may be
-- performed in place if it is safe to do so and will modify a copy of the
-- vector otherwise (see 'Data.Vector.Generic.New.New' for details).
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> import qualified Data.Vector.Mutable as MV
-- >>> V.modify (\v -> MV.write v 0 'x') $ V.replicate 4 'a'
-- "xaaa"
--
-- @since 0.13.2.0
modify :: (forall s. MVector s a -> ST s ()) -> Vector a -> Vector a
{-# INLINE modify #-}
modify p = G.modify p

-- Indexing
-- --------

-- | /O(n)/ Pair each element in a vector with its index.
--
-- @since 0.13.2.0
indexed :: Vector a -> Vector (Int,a)
{-# INLINE indexed #-}
indexed = G.indexed

-- Mapping
-- -------

-- | /O(n)/ Map a function over a vector.
--
-- @since 0.13.2.0
map :: (a -> b) -> Vector a -> Vector b
{-# INLINE map #-}
map = G.map

-- | /O(n)/ Apply a function to every element of a vector and its index.
--
-- @since 0.13.2.0
imap :: (Int -> a -> b) -> Vector a -> Vector b
{-# INLINE imap #-}
imap = G.imap

-- | Map a function over a vector and concatenate the results.
--
-- @since 0.13.2.0
concatMap :: (a -> Vector b) -> Vector a -> Vector b
{-# INLINE concatMap #-}
concatMap = G.concatMap

-- Monadic mapping
-- ---------------

-- | /O(n)/ Apply the monadic action to all elements of the vector, yielding a
-- vector of results.
--
-- @since 0.13.2.0
mapM :: Monad m => (a -> m b) -> Vector a -> m (Vector b)
{-# INLINE mapM #-}
mapM = G.mapM

-- | /O(n)/ Apply the monadic action to every element of a vector and its
-- index, yielding a vector of results.
--
-- @since 0.13.2.0
imapM :: Monad m => (Int -> a -> m b) -> Vector a -> m (Vector b)
{-# INLINE imapM #-}
imapM = G.imapM

-- | /O(n)/ Apply the monadic action to all elements of a vector and ignore the
-- results.
--
-- @since 0.13.2.0
mapM_ :: Monad m => (a -> m b) -> Vector a -> m ()
{-# INLINE mapM_ #-}
mapM_ = G.mapM_

-- | /O(n)/ Apply the monadic action to every element of a vector and its
-- index, ignoring the results.
--
-- @since 0.13.2.0
imapM_ :: Monad m => (Int -> a -> m b) -> Vector a -> m ()
{-# INLINE imapM_ #-}
imapM_ = G.imapM_

-- | /O(n)/ Apply the monadic action to all elements of the vector, yielding a
-- vector of results. Equivalent to @flip 'mapM'@.
--
-- @since 0.13.2.0
forM :: Monad m => Vector a -> (a -> m b) -> m (Vector b)
{-# INLINE forM #-}
forM = G.forM

-- | /O(n)/ Apply the monadic action to all elements of a vector and ignore the
-- results. Equivalent to @flip 'mapM_'@.
--
-- @since 0.13.2.0
forM_ :: Monad m => Vector a -> (a -> m b) -> m ()
{-# INLINE forM_ #-}
forM_ = G.forM_

-- | /O(n)/ Apply the monadic action to all elements of the vector and their indices, yielding a
-- vector of results. Equivalent to @'flip' 'imapM'@.
--
-- @since 0.13.2.0
iforM :: Monad m => Vector a -> (Int -> a -> m b) -> m (Vector b)
{-# INLINE iforM #-}
iforM = G.iforM

-- | /O(n)/ Apply the monadic action to all elements of the vector and their indices
-- and ignore the results. Equivalent to @'flip' 'imapM_'@.
--
-- @since 0.13.2.0
iforM_ :: Monad m => Vector a -> (Int -> a -> m b) -> m ()
{-# INLINE iforM_ #-}
iforM_ = G.iforM_

-- Zipping
-- -------

-- | /O(min(m,n))/ Zip two vectors with the given function.
--
-- @since 0.13.2.0
zipWith :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
{-# INLINE zipWith #-}
zipWith = G.zipWith

-- | Zip three vectors with the given function.
--
-- @since 0.13.2.0
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

-- | /O(min(m,n))/ Zip two vectors with a function that also takes the
-- elements' indices.
--
-- @since 0.13.2.0
izipWith :: (Int -> a -> b -> c) -> Vector a -> Vector b -> Vector c
{-# INLINE izipWith #-}
izipWith = G.izipWith

-- | Zip three vectors and their indices with the given function.
--
-- @since 0.13.2.0
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

-- | /O(min(m,n))/ Zip two vectors.
--
-- @since 0.13.2.0
zip :: Vector a -> Vector b -> Vector (a, b)
{-# INLINE zip #-}
zip = G.zip

-- | Zip together three vectors into a vector of triples.
--
-- @since 0.13.2.0
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

-- Unzipping
-- ---------

-- | /O(min(m,n))/ Unzip a vector of pairs.
--
-- @since 0.13.2.0
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

-- Monadic zipping
-- ---------------

-- | /O(min(m,n))/ Zip the two vectors with the monadic action and yield a
-- vector of results.
--
-- @since 0.13.2.0
zipWithM :: Monad m => (a -> b -> m c) -> Vector a -> Vector b -> m (Vector c)
{-# INLINE zipWithM #-}
zipWithM = G.zipWithM

-- | /O(min(m,n))/ Zip the two vectors with a monadic action that also takes
-- the element index and yield a vector of results.
--
-- @since 0.13.2.0
izipWithM :: Monad m => (Int -> a -> b -> m c) -> Vector a -> Vector b -> m (Vector c)
{-# INLINE izipWithM #-}
izipWithM = G.izipWithM

-- | /O(min(m,n))/ Zip the two vectors with the monadic action and ignore the
-- results.
--
-- @since 0.13.2.0
zipWithM_ :: Monad m => (a -> b -> m c) -> Vector a -> Vector b -> m ()
{-# INLINE zipWithM_ #-}
zipWithM_ = G.zipWithM_

-- | /O(min(m,n))/ Zip the two vectors with a monadic action that also takes
-- the element index and ignore the results.
--
-- @since 0.13.2.0
izipWithM_ :: Monad m => (Int -> a -> b -> m c) -> Vector a -> Vector b -> m ()
{-# INLINE izipWithM_ #-}
izipWithM_ = G.izipWithM_

-- Filtering
-- ---------

-- | /O(n)/ Drop all elements that do not satisfy the predicate.
--
-- @since 0.13.2.0
filter :: (a -> Bool) -> Vector a -> Vector a
{-# INLINE filter #-}
filter = G.filter

-- | /O(n)/ Drop all elements that do not satisfy the predicate which is applied to
-- the values and their indices.
--
-- @since 0.13.2.0
ifilter :: (Int -> a -> Bool) -> Vector a -> Vector a
{-# INLINE ifilter #-}
ifilter = G.ifilter

-- | /O(n)/ Drop repeated adjacent elements. The first element in each group is returned.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.uniq $ V.fromList [1,3,3,200,3]
-- [1,3,200,3]
-- >>> import Data.Semigroup
-- >>> V.uniq $ V.fromList [ Arg 1 'a', Arg 1 'b', Arg 1 'c']
-- [Arg 1 'a']
--
-- @since 0.13.2.0
uniq :: (Eq a) => Vector a -> Vector a
{-# INLINE uniq #-}
uniq = G.uniq

-- | /O(n)/ Map the values and collect the 'Just' results.
--
-- @since 0.13.2.0
mapMaybe :: (a -> Maybe b) -> Vector a -> Vector b
{-# INLINE mapMaybe #-}
mapMaybe = G.mapMaybe

-- | /O(n)/ Map the indices/values and collect the 'Just' results.
--
-- @since 0.13.2.0
imapMaybe :: (Int -> a -> Maybe b) -> Vector a -> Vector b
{-# INLINE imapMaybe #-}
imapMaybe = G.imapMaybe

-- | /O(n)/ Return a Vector of all the 'Just' values.
--
-- @since 0.13.2.0
catMaybes :: Vector (Maybe a) -> Vector a
{-# INLINE catMaybes #-}
catMaybes = mapMaybe id

-- | /O(n)/ Drop all elements that do not satisfy the monadic predicate.
--
-- @since 0.13.2.0
filterM :: Monad m => (a -> m Bool) -> Vector a -> m (Vector a)
{-# INLINE filterM #-}
filterM = G.filterM

-- | /O(n)/ Apply the monadic function to each element of the vector and
-- discard elements returning 'Nothing'.
--
-- @since 0.13.2.0
mapMaybeM :: Monad m => (a -> m (Maybe b)) -> Vector a -> m (Vector b)
{-# INLINE mapMaybeM #-}
mapMaybeM = G.mapMaybeM

-- | /O(n)/ Apply the monadic function to each element of the vector and its index.
-- Discard elements returning 'Nothing'.
--
-- @since 0.13.2.0
imapMaybeM :: Monad m => (Int -> a -> m (Maybe b)) -> Vector a -> m (Vector b)
{-# INLINE imapMaybeM #-}
imapMaybeM = G.imapMaybeM

-- | /O(n)/ Yield the longest prefix of elements satisfying the predicate.
-- The current implementation is not copy-free, unless the result vector is
-- fused away.
--
-- @since 0.13.2.0
takeWhile :: (a -> Bool) -> Vector a -> Vector a
{-# INLINE takeWhile #-}
takeWhile = G.takeWhile

-- | /O(n)/ Drop the longest prefix of elements that satisfy the predicate
-- without copying.
--
-- @since 0.13.2.0
dropWhile :: (a -> Bool) -> Vector a -> Vector a
{-# INLINE dropWhile #-}
dropWhile = G.dropWhile

-- Parititioning
-- -------------

-- | /O(n)/ Split the vector in two parts, the first one containing those
-- elements that satisfy the predicate and the second one those that don't. The
-- relative order of the elements is preserved at the cost of a sometimes
-- reduced performance compared to 'unstablePartition'.
--
-- @since 0.13.2.0
partition :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE partition #-}
partition = G.partition

-- | /O(n)/ Split the vector into two parts, the first one containing the
-- @`Left`@ elements and the second containing the @`Right`@ elements.
-- The relative order of the elements is preserved.
--
-- @since 0.13.2.0
partitionWith :: (a -> Either b c) -> Vector a -> (Vector b, Vector c)
{-# INLINE partitionWith #-}
partitionWith = G.partitionWith

-- | /O(n)/ Split the vector in two parts, the first one containing those
-- elements that satisfy the predicate and the second one those that don't.
-- The order of the elements is not preserved, but the operation is often
-- faster than 'partition'.
--
-- @since 0.13.2.0
unstablePartition :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE unstablePartition #-}
unstablePartition = G.unstablePartition

-- | /O(n)/ Split the vector into the longest prefix of elements that satisfy
-- the predicate and the rest without copying.
--
-- Does not fuse.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.span (<4) $ V.generate 10 id
-- ([0,1,2,3],[4,5,6,7,8,9])
--
-- @since 0.13.2.0
span :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE span #-}
span = G.span

-- | /O(n)/ Split the vector into the longest prefix of elements that do not
-- satisfy the predicate and the rest without copying.
--
-- Does not fuse.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.break (>4) $ V.generate 10 id
-- ([0,1,2,3,4],[5,6,7,8,9])
--
-- @since 0.13.2.0
break :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE break #-}
break = G.break

-- | /O(n)/ Split the vector into the longest prefix of elements that satisfy
-- the predicate and the rest without copying.
--
-- Does not fuse.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.spanR (>4) $ V.generate 10 id
-- ([5,6,7,8,9],[0,1,2,3,4])
--
-- @since 0.13.2.0
spanR :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE spanR #-}
spanR = G.spanR

-- | /O(n)/ Split the vector into the longest prefix of elements that do not
-- satisfy the predicate and the rest without copying.
--
-- Does not fuse.
--
-- @since 0.13.2.0
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.breakR (<5) $ V.generate 10 id
-- ([5,6,7,8,9],[0,1,2,3,4])
--
-- @since 0.13.2.0
breakR :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE breakR #-}
breakR = G.breakR

-- | /O(n)/ Split a vector into a list of slices, using a predicate function.
--
-- The concatenation of this list of slices is equal to the argument vector,
-- and each slice contains only equal elements, as determined by the equality
-- predicate function.
--
-- Does not fuse.
--
-- >>> import qualified Data.Vector as V
-- >>> import           Data.Char (isUpper)
-- >>> V.groupBy (\a b -> isUpper a == isUpper b) (V.fromList "Mississippi River")
-- ["M","ississippi ","R","iver"]
--
-- See also 'Data.List.groupBy', 'group'.
--
-- @since 0.13.2.0
groupBy :: (a -> a -> Bool) -> Vector a -> [Vector a]
{-# INLINE groupBy #-}
groupBy = G.groupBy

-- | /O(n)/ Split a vector into a list of slices of the input vector.
--
-- The concatenation of this list of slices is equal to the argument vector,
-- and each slice contains only equal elements.
--
-- Does not fuse.
--
-- This is the equivalent of 'groupBy (==)'.
--
-- >>> import qualified Data.Vector as V
-- >>> V.group (V.fromList "Mississippi")
-- ["M","i","ss","i","ss","i","pp","i"]
--
-- See also 'Data.List.group'.
--
-- @since 0.13.2.0
group :: Eq a => Vector a -> [Vector a]
{-# INLINE group #-}
group = G.groupBy (==)

-- Searching
-- ---------

infix 4 `elem`
-- | /O(n)/ Check if the vector contains an element.
--
-- @since 0.13.2.0
elem :: Eq a => a -> Vector a -> Bool
{-# INLINE elem #-}
elem = G.elem

infix 4 `notElem`
-- | /O(n)/ Check if the vector does not contain an element (inverse of 'elem').
--
-- @since 0.13.2.0
notElem :: Eq a => a -> Vector a -> Bool
{-# INLINE notElem #-}
notElem = G.notElem

-- | /O(n)/ Yield 'Just' the first element matching the predicate or 'Nothing'
-- if no such element exists.
--
-- @since 0.13.2.0
find :: (a -> Bool) -> Vector a -> Maybe a
{-# INLINE find #-}
find = G.find

-- | /O(n)/ Yield 'Just' the index of the first element matching the predicate
-- or 'Nothing' if no such element exists.
--
-- @since 0.13.2.0
findIndex :: (a -> Bool) -> Vector a -> Maybe Int
{-# INLINE findIndex #-}
findIndex = G.findIndex

-- | /O(n)/ Yield 'Just' the index of the /last/ element matching the predicate
-- or 'Nothing' if no such element exists.
--
-- Does not fuse.
--
-- @since 0.13.2.0
findIndexR :: (a -> Bool) -> Vector a -> Maybe Int
{-# INLINE findIndexR #-}
findIndexR = G.findIndexR

-- | /O(n)/ Yield the indices of elements satisfying the predicate in ascending
-- order.
--
-- @since 0.13.2.0
findIndices :: (a -> Bool) -> Vector a -> Vector Int
{-# INLINE findIndices #-}
findIndices = G.findIndices

-- | /O(n)/ Yield 'Just' the index of the first occurrence of the given element or
-- 'Nothing' if the vector does not contain the element. This is a specialised
-- version of 'findIndex'.
--
-- @since 0.13.2.0
elemIndex :: Eq a => a -> Vector a -> Maybe Int
{-# INLINE elemIndex #-}
elemIndex = G.elemIndex

-- | /O(n)/ Yield the indices of all occurrences of the given element in
-- ascending order. This is a specialised version of 'findIndices'.
--
-- @since 0.13.2.0
elemIndices :: Eq a => a -> Vector a -> Vector Int
{-# INLINE elemIndices #-}
elemIndices = G.elemIndices

-- Folding
-- -------

-- | /O(n)/ Left fold.
--
-- @since 0.13.2.0
foldl :: (a -> b -> a) -> a -> Vector b -> a
{-# INLINE foldl #-}
foldl = G.foldl

-- | /O(n)/ Left fold on non-empty vectors.
--
-- @since 0.13.2.0
foldl1 :: (a -> a -> a) -> Vector a -> a
{-# INLINE foldl1 #-}
foldl1 = G.foldl1

-- | /O(n)/ Left fold with strict accumulator.
--
-- @since 0.13.2.0
foldl' :: (a -> b -> a) -> a -> Vector b -> a
{-# INLINE foldl' #-}
foldl' = G.foldl'

-- | /O(n)/ Left fold on non-empty vectors with strict accumulator.
--
-- @since 0.13.2.0
foldl1' :: (a -> a -> a) -> Vector a -> a
{-# INLINE foldl1' #-}
foldl1' = G.foldl1'

-- | /O(n)/ Right fold.
--
-- @since 0.13.2.0
foldr :: (a -> b -> b) -> b -> Vector a -> b
{-# INLINE foldr #-}
foldr = G.foldr

-- | /O(n)/ Right fold on non-empty vectors.
--
-- @since 0.13.2.0
foldr1 :: (a -> a -> a) -> Vector a -> a
{-# INLINE foldr1 #-}
foldr1 = G.foldr1

-- | /O(n)/ Right fold with a strict accumulator.
--
-- @since 0.13.2.0
foldr' :: (a -> b -> b) -> b -> Vector a -> b
{-# INLINE foldr' #-}
foldr' = G.foldr'

-- | /O(n)/ Right fold on non-empty vectors with strict accumulator.
--
-- @since 0.13.2.0
foldr1' :: (a -> a -> a) -> Vector a -> a
{-# INLINE foldr1' #-}
foldr1' = G.foldr1'

-- | /O(n)/ Left fold using a function applied to each element and its index.
--
-- @since 0.13.2.0
ifoldl :: (a -> Int -> b -> a) -> a -> Vector b -> a
{-# INLINE ifoldl #-}
ifoldl = G.ifoldl

-- | /O(n)/ Left fold with strict accumulator using a function applied to each element
-- and its index.
--
-- @since 0.13.2.0
ifoldl' :: (a -> Int -> b -> a) -> a -> Vector b -> a
{-# INLINE ifoldl' #-}
ifoldl' = G.ifoldl'

-- | /O(n)/ Right fold using a function applied to each element and its index.
--
-- @since 0.13.2.0
ifoldr :: (Int -> a -> b -> b) -> b -> Vector a -> b
{-# INLINE ifoldr #-}
ifoldr = G.ifoldr

-- | /O(n)/ Right fold with strict accumulator using a function applied to each
-- element and its index.
--
-- @since 0.13.2.0
ifoldr' :: (Int -> a -> b -> b) -> b -> Vector a -> b
{-# INLINE ifoldr' #-}
ifoldr' = G.ifoldr'

-- | /O(n)/ Map each element of the structure to a monoid and combine
-- the results. It uses the same implementation as the corresponding method
-- of the 'Foldable' type class. Note that it's implemented in terms of 'foldr'
-- and won't fuse with functions that traverse the vector from left to
-- right ('map', 'generate', etc.).
--
-- @since 0.13.2.0
foldMap :: (Monoid m) => (a -> m) -> Vector a -> m
{-# INLINE foldMap #-}
foldMap = G.foldMap

-- | /O(n)/ Like 'foldMap', but strict in the accumulator. It uses the same
-- implementation as the corresponding method of the 'Foldable' type class.
-- Note that it's implemented in terms of 'foldl'', so it fuses in most
-- contexts.
--
-- @since 0.13.2.0
foldMap' :: (Monoid m) => (a -> m) -> Vector a -> m
{-# INLINE foldMap' #-}
foldMap' = G.foldMap'


-- Specialised folds
-- -----------------

-- | /O(n)/ Check if all elements satisfy the predicate.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.all even $ V.fromList [2, 4, 12]
-- True
-- >>> V.all even $ V.fromList [2, 4, 13]
-- False
-- >>> V.all even (V.empty :: V.Vector Int)
-- True
--
-- @since 0.13.2.0
all :: (a -> Bool) -> Vector a -> Bool
{-# INLINE all #-}
all = G.all

-- | /O(n)/ Check if any element satisfies the predicate.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.any even $ V.fromList [1, 3, 7]
-- False
-- >>> V.any even $ V.fromList [3, 2, 13]
-- True
-- >>> V.any even (V.empty :: V.Vector Int)
-- False
--
-- @since 0.13.2.0
any :: (a -> Bool) -> Vector a -> Bool
{-# INLINE any #-}
any = G.any

-- | /O(n)/ Check if all elements are 'True'.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.and $ V.fromList [True, False]
-- False
-- >>> V.and V.empty
-- True
--
-- @since 0.13.2.0
and :: Vector Bool -> Bool
{-# INLINE and #-}
and = G.and

-- | /O(n)/ Check if any element is 'True'.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.or $ V.fromList [True, False]
-- True
-- >>> V.or V.empty
-- False
--
-- @since 0.13.2.0
or :: Vector Bool -> Bool
{-# INLINE or #-}
or = G.or

-- | /O(n)/ Compute the sum of the elements.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.sum $ V.fromList [300,20,1]
-- 321
-- >>> V.sum (V.empty :: V.Vector Int)
-- 0
--
-- @since 0.13.2.0
sum :: Num a => Vector a -> a
{-# INLINE sum #-}
sum = G.sum

-- | /O(n)/ Compute the product of the elements.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.product $ V.fromList [1,2,3,4]
-- 24
-- >>> V.product (V.empty :: V.Vector Int)
-- 1
--
-- @since 0.13.2.0
product :: Num a => Vector a -> a
{-# INLINE product #-}
product = G.product

-- | /O(n)/ Yield the maximum element of the vector. The vector may not be
-- empty. In case of a tie, the first occurrence wins.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.maximum $ V.fromList [2, 1]
-- 2
-- >>> import Data.Semigroup
-- >>> V.maximum $ V.fromList [Arg 1 'a', Arg 2 'b']
-- Arg 2 'b'
-- >>> V.maximum $ V.fromList [Arg 1 'a', Arg 1 'b']
-- Arg 1 'a'
--
-- @since 0.13.2.0
maximum :: Ord a => Vector a -> a
{-# INLINE maximum #-}
maximum = G.maximum

-- | /O(n)/ Yield the maximum element of the vector according to the
-- given comparison function. The vector may not be empty. In case of
-- a tie, the first occurrence wins. This behavior is different from
-- 'Data.List.maximumBy' which returns the last tie.
--
-- ==== __Examples__
--
-- >>> import Data.Ord
-- >>> import qualified Data.Vector as V
-- >>> V.maximumBy (comparing fst) $ V.fromList [(2,'a'), (1,'b')]
-- (2,'a')
-- >>> V.maximumBy (comparing fst) $ V.fromList [(1,'a'), (1,'b')]
-- (1,'a')
--
-- @since 0.13.2.0
maximumBy :: (a -> a -> Ordering) -> Vector a -> a
{-# INLINE maximumBy #-}
maximumBy = G.maximumBy

-- | /O(n)/ Yield the maximum element of the vector by comparing the results
-- of a key function on each element. In case of a tie, the first occurrence
-- wins. The vector may not be empty.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.maximumOn fst $ V.fromList [(2,'a'), (1,'b')]
-- (2,'a')
-- >>> V.maximumOn fst $ V.fromList [(1,'a'), (1,'b')]
-- (1,'a')
--
-- @since 0.13.2.0
maximumOn :: Ord b => (a -> b) -> Vector a -> a
{-# INLINE maximumOn #-}
maximumOn = G.maximumOn

-- | /O(n)/ Yield the minimum element of the vector. The vector may not be
-- empty. In case of a tie, the first occurrence wins.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.minimum $ V.fromList [2, 1]
-- 1
-- >>> import Data.Semigroup
-- >>> V.minimum $ V.fromList [Arg 2 'a', Arg 1 'b']
-- Arg 1 'b'
-- >>> V.minimum $ V.fromList [Arg 1 'a', Arg 1 'b']
-- Arg 1 'a'
--
-- @since 0.13.2.0
minimum :: Ord a => Vector a -> a
{-# INLINE minimum #-}
minimum = G.minimum

-- | /O(n)/ Yield the minimum element of the vector according to the
-- given comparison function. The vector may not be empty. In case of
-- a tie, the first occurrence wins.
--
-- ==== __Examples__
--
-- >>> import Data.Ord
-- >>> import qualified Data.Vector as V
-- >>> V.minimumBy (comparing fst) $ V.fromList [(2,'a'), (1,'b')]
-- (1,'b')
-- >>> V.minimumBy (comparing fst) $ V.fromList [(1,'a'), (1,'b')]
-- (1,'a')
--
-- @since 0.13.2.0
minimumBy :: (a -> a -> Ordering) -> Vector a -> a
{-# INLINE minimumBy #-}
minimumBy = G.minimumBy

-- | /O(n)/ Yield the minimum element of the vector by comparing the results
-- of a key function on each element. In case of a tie, the first occurrence
-- wins. The vector may not be empty.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.minimumOn fst $ V.fromList [(2,'a'), (1,'b')]
-- (1,'b')
-- >>> V.minimumOn fst $ V.fromList [(1,'a'), (1,'b')]
-- (1,'a')
--
-- @since 0.13.2.0
minimumOn :: Ord b => (a -> b) -> Vector a -> a
{-# INLINE minimumOn #-}
minimumOn = G.minimumOn

-- | /O(n)/ Yield the index of the maximum element of the vector. The vector
-- may not be empty.
--
-- @since 0.13.2.0
maxIndex :: Ord a => Vector a -> Int
{-# INLINE maxIndex #-}
maxIndex = G.maxIndex

-- | /O(n)/ Yield the index of the maximum element of the vector
-- according to the given comparison function. The vector may not be
-- empty. In case of a tie, the first occurrence wins.
--
-- ==== __Examples__
--
-- >>> import Data.Ord
-- >>> import qualified Data.Vector as V
-- >>> V.maxIndexBy (comparing fst) $ V.fromList [(2,'a'), (1,'b')]
-- 0
-- >>> V.maxIndexBy (comparing fst) $ V.fromList [(1,'a'), (1,'b')]
-- 0
--
-- @since 0.13.2.0
maxIndexBy :: (a -> a -> Ordering) -> Vector a -> Int
{-# INLINE maxIndexBy #-}
maxIndexBy = G.maxIndexBy

-- | /O(n)/ Yield the index of the minimum element of the vector. The vector
-- may not be empty.
--
-- @since 0.13.2.0
minIndex :: Ord a => Vector a -> Int
{-# INLINE minIndex #-}
minIndex = G.minIndex

-- | /O(n)/ Yield the index of the minimum element of the vector according to
-- the given comparison function. The vector may not be empty.
--
-- ==== __Examples__
--
-- >>> import Data.Ord
-- >>> import qualified Data.Vector as V
-- >>> V.minIndexBy (comparing fst) $ V.fromList [(2,'a'), (1,'b')]
-- 1
-- >>> V.minIndexBy (comparing fst) $ V.fromList [(1,'a'), (1,'b')]
-- 0
--
-- @since 0.13.2.0
minIndexBy :: (a -> a -> Ordering) -> Vector a -> Int
{-# INLINE minIndexBy #-}
minIndexBy = G.minIndexBy

-- Monadic folds
-- -------------

-- | /O(n)/ Monadic fold.
--
-- @since 0.13.2.0
foldM :: Monad m => (a -> b -> m a) -> a -> Vector b -> m a
{-# INLINE foldM #-}
foldM = G.foldM

-- | /O(n)/ Monadic fold using a function applied to each element and its index.
--
-- @since 0.13.2.0
ifoldM :: Monad m => (a -> Int -> b -> m a) -> a -> Vector b -> m a
{-# INLINE ifoldM #-}
ifoldM = G.ifoldM

-- | /O(n)/ Monadic fold over non-empty vectors.
--
-- @since 0.13.2.0
fold1M :: Monad m => (a -> a -> m a) -> Vector a -> m a
{-# INLINE fold1M #-}
fold1M = G.fold1M

-- | /O(n)/ Monadic fold with strict accumulator.
--
-- @since 0.13.2.0
foldM' :: Monad m => (a -> b -> m a) -> a -> Vector b -> m a
{-# INLINE foldM' #-}
foldM' = G.foldM'

-- | /O(n)/ Monadic fold with strict accumulator using a function applied to each
-- element and its index.
--
-- @since 0.13.2.0
ifoldM' :: Monad m => (a -> Int -> b -> m a) -> a -> Vector b -> m a
{-# INLINE ifoldM' #-}
ifoldM' = G.ifoldM'

-- | /O(n)/ Monadic fold over non-empty vectors with strict accumulator.
--
-- @since 0.13.2.0
fold1M' :: Monad m => (a -> a -> m a) -> Vector a -> m a
{-# INLINE fold1M' #-}
fold1M' = G.fold1M'

-- | /O(n)/ Monadic fold that discards the result.
--
-- @since 0.13.2.0
foldM_ :: Monad m => (a -> b -> m a) -> a -> Vector b -> m ()
{-# INLINE foldM_ #-}
foldM_ = G.foldM_

-- | /O(n)/ Monadic fold that discards the result using a function applied to
-- each element and its index.
--
-- @since 0.13.2.0
ifoldM_ :: Monad m => (a -> Int -> b -> m a) -> a -> Vector b -> m ()
{-# INLINE ifoldM_ #-}
ifoldM_ = G.ifoldM_

-- | /O(n)/ Monadic fold over non-empty vectors that discards the result.
--
-- @since 0.13.2.0
fold1M_ :: Monad m => (a -> a -> m a) -> Vector a -> m ()
{-# INLINE fold1M_ #-}
fold1M_ = G.fold1M_

-- | /O(n)/ Monadic fold with strict accumulator that discards the result.
--
-- @since 0.13.2.0
foldM'_ :: Monad m => (a -> b -> m a) -> a -> Vector b -> m ()
{-# INLINE foldM'_ #-}
foldM'_ = G.foldM'_

-- | /O(n)/ Monadic fold with strict accumulator that discards the result
-- using a function applied to each element and its index.
--
-- @since 0.13.2.0
ifoldM'_ :: Monad m => (a -> Int -> b -> m a) -> a -> Vector b -> m ()
{-# INLINE ifoldM'_ #-}
ifoldM'_ = G.ifoldM'_

-- | /O(n)/ Monadic fold over non-empty vectors with strict accumulator
-- that discards the result.
--
-- @since 0.13.2.0
fold1M'_ :: Monad m => (a -> a -> m a) -> Vector a -> m ()
{-# INLINE fold1M'_ #-}
fold1M'_ = G.fold1M'_

-- Monadic sequencing
-- ------------------

-- | Evaluate each action and collect the results.
--
-- @since 0.13.2.0
sequence :: Monad m => Vector (m a) -> m (Vector a)
{-# INLINE sequence #-}
sequence = G.sequence

-- | Evaluate each action and discard the results.
--
-- @since 0.13.2.0
sequence_ :: Monad m => Vector (m a) -> m ()
{-# INLINE sequence_ #-}
sequence_ = G.sequence_

-- Scans
-- -----

-- | /O(n)/ Left-to-right prescan.
--
-- @
-- prescanl f z = 'init' . 'scanl' f z
-- @
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.prescanl (+) 0 (V.fromList [1,2,3,4])
-- [0,1,3,6]
--
-- @since 0.13.2.0
prescanl :: (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE prescanl #-}
prescanl = G.prescanl

-- | /O(n)/ Left-to-right prescan with strict accumulator.
--
-- @since 0.13.2.0
prescanl' :: (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE prescanl' #-}
prescanl' = G.prescanl'

-- | /O(n)/ Left-to-right postscan.
--
-- @
-- postscanl f z = 'tail' . 'scanl' f z
-- @
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.postscanl (+) 0 (V.fromList [1,2,3,4])
-- [1,3,6,10]
--
-- @since 0.13.2.0
postscanl :: (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE postscanl #-}
postscanl = G.postscanl

-- | /O(n)/ Left-to-right postscan with strict accumulator.
--
-- @since 0.13.2.0
postscanl' :: (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE postscanl' #-}
postscanl' = G.postscanl'

-- | /O(n)/ Left-to-right scan.
--
-- > scanl f z <x1,...,xn> = <y1,...,y(n+1)>
-- >   where y1 = z
-- >         yi = f y(i-1) x(i-1)
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.scanl (+) 0 (V.fromList [1,2,3,4])
-- [0,1,3,6,10]
--
-- @since 0.13.2.0
scanl :: (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE scanl #-}
scanl = G.scanl

-- | /O(n)/ Left-to-right scan with strict accumulator.
--
-- @since 0.13.2.0
scanl' :: (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE scanl' #-}
scanl' = G.scanl'

-- | /O(n)/ Left-to-right scan over a vector with its index.
--
-- @since 0.13.2.0
iscanl :: (Int -> a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE iscanl #-}
iscanl = G.iscanl

-- | /O(n)/ Left-to-right scan over a vector (strictly) with its index.
--
-- @since 0.13.2.0
iscanl' :: (Int -> a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE iscanl' #-}
iscanl' = G.iscanl'

-- | /O(n)/ Initial-value free left-to-right scan over a vector.
--
-- > scanl f <x1,...,xn> = <y1,...,yn>
-- >   where y1 = x1
-- >         yi = f y(i-1) xi
--
-- Note: Since 0.13, application of this to an empty vector no longer
-- results in an error; instead it produces an empty vector.
--
-- ==== __Examples__
-- >>> import qualified Data.Vector as V
-- >>> V.scanl1 min $ V.fromListN 5 [4,2,4,1,3]
-- [4,2,2,1,1]
-- >>> V.scanl1 max $ V.fromListN 5 [1,3,2,5,4]
-- [1,3,3,5,5]
-- >>> V.scanl1 min (V.empty :: V.Vector Int)
-- []
--
-- @since 0.13.2.0
scanl1 :: (a -> a -> a) -> Vector a -> Vector a
{-# INLINE scanl1 #-}
scanl1 = G.scanl1

-- | /O(n)/ Initial-value free left-to-right scan over a vector with a strict accumulator.
--
-- Note: Since 0.13, application of this to an empty vector no longer
-- results in an error; instead it produces an empty vector.
--
-- ==== __Examples__
-- >>> import qualified Data.Vector as V
-- >>> V.scanl1' min $ V.fromListN 5 [4,2,4,1,3]
-- [4,2,2,1,1]
-- >>> V.scanl1' max $ V.fromListN 5 [1,3,2,5,4]
-- [1,3,3,5,5]
-- >>> V.scanl1' min (V.empty :: V.Vector Int)
-- []
--
-- @since 0.13.2.0
scanl1' :: (a -> a -> a) -> Vector a -> Vector a
{-# INLINE scanl1' #-}
scanl1' = G.scanl1'

-- | /O(n)/ Right-to-left prescan.
--
-- @
-- prescanr f z = 'reverse' . 'prescanl' (flip f) z . 'reverse'
-- @
--
-- @since 0.13.2.0
prescanr :: (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE prescanr #-}
prescanr = G.prescanr

-- | /O(n)/ Right-to-left prescan with strict accumulator.
--
-- @since 0.13.2.0
prescanr' :: (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE prescanr' #-}
prescanr' = G.prescanr'

-- | /O(n)/ Right-to-left postscan.
--
-- @since 0.13.2.0
postscanr :: (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE postscanr #-}
postscanr = G.postscanr

-- | /O(n)/ Right-to-left postscan with strict accumulator.
--
-- @since 0.13.2.0
postscanr' :: (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE postscanr' #-}
postscanr' = G.postscanr'

-- | /O(n)/ Right-to-left scan.
--
-- @since 0.13.2.0
scanr :: (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE scanr #-}
scanr = G.scanr

-- | /O(n)/ Right-to-left scan with strict accumulator.
--
-- @since 0.13.2.0
scanr' :: (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE scanr' #-}
scanr' = G.scanr'

-- | /O(n)/ Right-to-left scan over a vector with its index.
--
-- @since 0.13.2.0
iscanr :: (Int -> a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE iscanr #-}
iscanr = G.iscanr

-- | /O(n)/ Right-to-left scan over a vector (strictly) with its index.
--
-- @since 0.13.2.0
iscanr' :: (Int -> a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE iscanr' #-}
iscanr' = G.iscanr'

-- | /O(n)/ Right-to-left, initial-value free scan over a vector.
--
-- Note: Since 0.13, application of this to an empty vector no longer
-- results in an error; instead it produces an empty vector.
--
-- ==== __Examples__
-- >>> import qualified Data.Vector as V
-- >>> V.scanr1 min $ V.fromListN 5 [3,1,4,2,4]
-- [1,1,2,2,4]
-- >>> V.scanr1 max $ V.fromListN 5 [4,5,2,3,1]
-- [5,5,3,3,1]
-- >>> V.scanr1 min (V.empty :: V.Vector Int)
-- []
--
-- @since 0.13.2.0
scanr1 :: (a -> a -> a) -> Vector a -> Vector a
{-# INLINE scanr1 #-}
scanr1 = G.scanr1

-- | /O(n)/ Right-to-left, initial-value free scan over a vector with a strict
-- accumulator.
--
-- Note: Since 0.13, application of this to an empty vector no longer
-- results in an error; instead it produces an empty vector.
--
-- ==== __Examples__
-- >>> import qualified Data.Vector as V
-- >>> V.scanr1' min $ V.fromListN 5 [3,1,4,2,4]
-- [1,1,2,2,4]
-- >>> V.scanr1' max $ V.fromListN 5 [4,5,2,3,1]
-- [5,5,3,3,1]
-- >>> V.scanr1' min (V.empty :: V.Vector Int)
-- []
--
-- @since 0.13.2.0
scanr1' :: (a -> a -> a) -> Vector a -> Vector a
{-# INLINE scanr1' #-}
scanr1' = G.scanr1'

-- Comparisons
-- ------------------------

-- | /O(n)/ Check if two vectors are equal using the supplied equality
-- predicate.
--
-- @since 0.13.2.0
eqBy :: (a -> b -> Bool) -> Vector a -> Vector b -> Bool
{-# INLINE eqBy #-}
eqBy = G.eqBy

-- | /O(n)/ Compare two vectors using the supplied comparison function for
-- vector elements. Comparison works the same as for lists.
--
-- > cmpBy compare == compare
--
-- @since 0.13.2.0
cmpBy :: (a -> b -> Ordering) -> Vector a -> Vector b -> Ordering
cmpBy = G.cmpBy

-- Conversions - Lists
-- ------------------------

-- | /O(n)/ Convert a vector to a list.
--
-- @since 0.13.2.0
toList :: Vector a -> [a]
{-# INLINE toList #-}
toList = G.toList

-- | /O(n)/ Convert a list to a vector. During the operation, the 
-- vector’s capacity will be doubling until the list's contents are 
-- in the vector. Depending on the list’s size, up to half of the vector’s 
-- capacity might be empty. If you’d rather avoid this, you can use 
-- 'fromListN', which will provide the exact space the list requires but will 
-- prevent list fusion, or @'force' . 'fromList'@, which will create the 
-- vector and then copy it without the superfluous space.
--
-- @since 0.13.2.0
fromList :: [a] -> Vector a
{-# INLINE fromList #-}
fromList = G.fromList

-- | /O(n)/ Convert the first @n@ elements of a list to a vector. It's
-- expected that the supplied list will be exactly @n@ elements long. As
-- an optimization, this function allocates a buffer for @n@ elements, which
-- could be used for DoS-attacks by exhausting the memory if an attacker controls
-- that parameter.
--
-- @
-- fromListN n xs = 'fromList' ('take' n xs)
-- @
--
-- @since 0.13.2.0
fromListN :: Int -> [a] -> Vector a
{-# INLINE fromListN #-}
fromListN = G.fromListN

-- Conversions - Lazy vectors
-- -----------------------------

-- | /O(1)/ Convert strict array to lazy array
toLazy :: Vector a -> V.Vector a
toLazy (Vector v) = v

-- | /O(n)/ Convert lazy array to strict array. This function reduces
-- each element of vector to WHNF.
fromLazy :: V.Vector a -> Vector a
fromLazy vec = liftRnfV (`seq` ()) v `seq` v where v = Vector vec


-- Conversions - Arrays
-- -----------------------------

-- | /O(n)/ Convert an array to a vector and reduce each element to WHNF.
--
-- @since 0.13.2.0
fromArray :: Array a -> Vector a
{-# INLINE fromArray #-}
fromArray arr = liftRnfV (`seq` ()) vec `seq` vec
  where
    vec = Vector $ V.fromArray arr

-- | /O(n)/ Convert a vector to an array.
--
-- @since 0.13.2.0
toArray :: Vector a -> Array a
{-# INLINE toArray #-}
toArray (Vector v) = V.toArray v

-- | /O(1)/ Extract the underlying `Array`, offset where vector starts and the
-- total number of elements in the vector. Below property always holds:
--
-- > let (array, offset, len) = toArraySlice v
-- > v === unsafeFromArraySlice len offset array
--
-- @since 0.13.2.0
toArraySlice :: Vector a -> (Array a, Int, Int)
{-# INLINE toArraySlice #-}
toArraySlice (Vector v) = V.toArraySlice v


-- | /O(n)/ Convert an array slice to a vector and reduce each element to WHNF.
--
-- This function is very unsafe, because constructing an invalid
-- vector can yield almost all other safe functions in this module
-- unsafe. These are equivalent:
--
-- > unsafeFromArraySlice len offset === unsafeTake len . unsafeDrop offset . fromArray
--
-- @since 0.13.2.0
unsafeFromArraySlice ::
     Array a -- ^ Immutable boxed array.
  -> Int -- ^ Offset
  -> Int -- ^ Length
  -> Vector a
{-# INLINE unsafeFromArraySlice #-}
unsafeFromArraySlice arr offset len = liftRnfV (`seq` ()) vec `seq` vec
  where vec = Vector (V.unsafeFromArraySlice arr offset len)



-- Conversions - Mutable vectors
-- -----------------------------

-- | /O(1)/ Unsafely convert a mutable vector to an immutable one without
-- copying. The mutable vector may not be used after this operation.
--
-- @since 0.13.2.0
unsafeFreeze :: PrimMonad m => MVector (PrimState m) a -> m (Vector a)
{-# INLINE unsafeFreeze #-}
unsafeFreeze = G.unsafeFreeze

-- | /O(n)/ Yield an immutable copy of the mutable vector.
--
-- @since 0.13.2.0
freeze :: PrimMonad m => MVector (PrimState m) a -> m (Vector a)
{-# INLINE freeze #-}
freeze = G.freeze

-- | /O(1)/ Unsafely convert an immutable vector to a mutable one
-- without copying. Note that this is a very dangerous function and
-- generally it's only safe to read from the resulting vector. In this
-- case, the immutable vector could be used safely as well.
--
-- Problems with mutation happen because GHC has a lot of freedom to
-- introduce sharing. As a result mutable vectors produced by
-- @unsafeThaw@ may or may not share the same underlying buffer. For
-- example:
--
-- > foo = do
-- >   let vec = V.generate 10 id
-- >   mvec <- V.unsafeThaw vec
-- >   do_something mvec
--
-- Here GHC could lift @vec@ outside of foo which means that all calls to
-- @do_something@ will use same buffer with possibly disastrous
-- results. Whether such aliasing happens or not depends on the program in
-- question, optimization levels, and GHC flags.
--
-- All in all, attempts to modify a vector produced by @unsafeThaw@ fall out of
-- domain of software engineering and into realm of black magic, dark
-- rituals, and unspeakable horrors. The only advice that could be given
-- is: "Don't attempt to mutate a vector produced by @unsafeThaw@ unless you
-- know how to prevent GHC from aliasing buffers accidentally. We don't."
--
-- @since 0.13.2.0
unsafeThaw :: PrimMonad m => Vector a -> m (MVector (PrimState m) a)
{-# INLINE unsafeThaw #-}
unsafeThaw = G.unsafeThaw

-- | /O(n)/ Yield a mutable copy of an immutable vector.
--
-- @since 0.13.2.0
thaw :: PrimMonad m => Vector a -> m (MVector (PrimState m) a)
{-# INLINE thaw #-}
thaw = G.thaw

-- | /O(n)/ Copy an immutable vector into a mutable one. The two vectors must
-- have the same length. This is not checked.
--
-- @since 0.13.2.0
unsafeCopy :: PrimMonad m => MVector (PrimState m) a -> Vector a -> m ()
{-# INLINE unsafeCopy #-}
unsafeCopy = G.unsafeCopy

-- | /O(n)/ Copy an immutable vector into a mutable one. The two vectors must
-- have the same length.
--
-- @since 0.13.2.0
copy :: PrimMonad m => MVector (PrimState m) a -> Vector a -> m ()
{-# INLINE copy #-}
copy = G.copy

-- $setup
-- >>> :set -Wno-type-defaults
-- >>> import Prelude (Char, String, Bool(True, False), min, max, fst, even, undefined, Ord(..))
