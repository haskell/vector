{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Data.Vector.Unboxed
-- Copyright   : (c) Roman Leshchinskiy 2009-2010
--                   Alexey Kuleshevich 2020-2022
--                   Aleksey Khudyakov 2020-2022
--                   Andrew Lelechenko 2020-2022
-- License     : BSD-style
--
-- Maintainer  : Haskell Libraries Team <libraries@haskell.org>
-- Stability   : experimental
-- Portability : non-portable
--
-- Adaptive unboxed vectors. The implementation is based on data families
-- and picks an efficient, specialised representation for every element type.
-- For example, vector of fixed size primitives are backed by
-- 'Data.Vector.Primitive.Vector', unboxed vectors of tuples are represented
-- as tuples of unboxed vectors (see 'zip'\/'unzip'). Note that vector is
-- only adaptive types could pick boxed representation for data type\/field
-- of record. However all library instances are backed by unboxed array(s).
--
-- Defining new instances of unboxed vectors is somewhat complicated since
-- it requires defining two data family and two type class instances. Latter
-- two could be generated using @GeneralizedNewtypeDeriving@ or @DerivingVia@
--
-- >>> :set -XTypeFamilies -XStandaloneDeriving -XMultiParamTypeClasses -XGeneralizedNewtypeDeriving
-- >>>
-- >>> import qualified Data.Vector.Generic         as VG
-- >>> import qualified Data.Vector.Generic.Mutable as VGM
-- >>> import qualified Data.Vector.Unboxed         as VU
-- >>>
-- >>> newtype Foo = Foo Int
-- >>>
-- >>> newtype instance VU.MVector s Foo = MV_Int (VU.MVector s Int)
-- >>> newtype instance VU.Vector    Foo = V_Int  (VU.Vector    Int)
-- >>> deriving instance VGM.MVector VU.MVector Foo
-- >>> deriving instance VG.Vector   VU.Vector  Foo
-- >>> instance VU.Unbox Foo
--
-- For other data types we have several newtype wrappers for use with
-- @DerivingVia@. See documentation of 'As' and 'IsoUnbox' for defining 
-- unboxed vector of product types. 'UnboxViaPrim' could be used to define
-- vector of instances of 'Data.Vector.Primitive.Prim'. Similarly
-- 'DoNotUnboxStrict'/'DoNotUnboxLazy'/'DoNotUnboxNormalForm' could be used
-- to represent polymorphic fields as boxed vectors.
--
-- Or if everything else fails instances could be written by hand.
-- Here is how the library does this for 'Complex' by simply wrapping
-- vectors of pairs.
--
-- @
-- newtype instance 'MVector' s ('Complex' a) = MV_Complex ('MVector' s (a,a))
-- newtype instance 'Vector'    ('Complex' a) = V_Complex  ('Vector'    (a,a))
--
-- instance ('RealFloat' a, 'Unbox' a) => 'Data.Vector.Generic.Mutable.MVector' 'MVector' ('Complex' a) where
--   {-\# INLINE basicLength \#-}
--   basicLength (MV_Complex v) = 'Data.Vector.Generic.Mutable.basicLength' v
--   ...
--
-- instance ('RealFloat' a, 'Unbox' a) => Data.Vector.Generic.Vector 'Vector' ('Complex' a) where
--   {-\# INLINE basicLength \#-}
--   basicLength (V_Complex v) = Data.Vector.Generic.basicLength v
--   ...
--
-- instance ('RealFloat' a, 'Unbox' a) => 'Unbox' ('Complex' a)
-- @
module Data.Vector.Unboxed (
  -- * Unboxed vectors
  Vector(V_UnboxAs, V_UnboxViaPrim, V_UnboxViaStorable,V_DoNotUnboxLazy,V_DoNotUnboxStrict,V_DoNotUnboxNormalForm),
  MVector(..), Unbox,

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
  map, imap, concatMap, iconcatMap,

  -- ** Monadic mapping
  mapM, imapM, mapM_, imapM_, forM, forM_,
  iforM, iforM_,

  -- ** Zipping
  zipWith, zipWith3, zipWith4, zipWith5, zipWith6,
  izipWith, izipWith3, izipWith4, izipWith5, izipWith6,
  -- *** Zipping tuples
  -- $zip
  zip, zip3, zip4, zip5, zip6,

  -- ** Monadic zipping
  zipWithM, izipWithM, zipWithM_, izipWithM_,

  -- ** Unzipping
  -- $unzip
  unzip, unzip3, unzip4, unzip5, unzip6,

  -- * Working with predicates

  -- ** Filtering
  filter, ifilter, filterM, uniq,
  mapMaybe, imapMaybe,
  mapMaybeM, imapMaybeM,
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
  fold1M, fold1M', foldM_, ifoldM_,
  foldM'_, ifoldM'_, fold1M_, fold1M'_,

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
  toList, fromList, fromListN,

  -- ** Other vector types
  G.convert,

  -- ** Mutable vectors
  freeze, thaw, copy, unsafeFreeze, unsafeThaw, unsafeCopy,

  -- ** Deriving via
  UnboxViaPrim(..),
  As(..),
  IsoUnbox(..),
  UnboxViaStorable(..),

  -- *** /Lazy/ boxing
  DoNotUnboxLazy(..),

  -- *** /Strict/ boxing
  DoNotUnboxStrict(..),
  DoNotUnboxNormalForm(..)
) where

import Data.Vector.Unboxed.Base
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Fusion.Bundle as Bundle
import Data.Vector.Fusion.Util ( delayed_min )

import Control.Monad.ST ( ST )
import Control.Monad.Primitive

import Prelude
  ( Eq, Ord, Num, Enum, Monoid, Traversable, Monad, Read, Show, Bool, Ordering(..), Int, Maybe, Either
  , compare, mempty, mappend, mconcat, showsPrec
  , (<), (<=), (>), (>=), (==), (/=) )

import Text.Read      ( Read(..), readListPrecDefault )
import Data.Semigroup ( Semigroup(..) )

import qualified GHC.Exts as Exts (IsList(..))


#define NOT_VECTOR_MODULE
#include "vector.h"

-- See http://trac.haskell.org/vector/ticket/12
instance (Unbox a, Eq a) => Eq (Vector a) where
  {-# INLINE (==) #-}
  xs == ys = Bundle.eq (G.stream xs) (G.stream ys)

-- See http://trac.haskell.org/vector/ticket/12
instance (Unbox a, Ord a) => Ord (Vector a) where
  {-# INLINE compare #-}
  compare xs ys = Bundle.cmp (G.stream xs) (G.stream ys)

  {-# INLINE (<) #-}
  xs < ys = Bundle.cmp (G.stream xs) (G.stream ys) == LT

  {-# INLINE (<=) #-}
  xs <= ys = Bundle.cmp (G.stream xs) (G.stream ys) /= GT

  {-# INLINE (>) #-}
  xs > ys = Bundle.cmp (G.stream xs) (G.stream ys) == GT

  {-# INLINE (>=) #-}
  xs >= ys = Bundle.cmp (G.stream xs) (G.stream ys) /= LT

instance Unbox a => Semigroup (Vector a) where
  {-# INLINE (<>) #-}
  (<>) = (++)

  {-# INLINE sconcat #-}
  sconcat = G.concatNE

instance Unbox a => Monoid (Vector a) where
  {-# INLINE mempty #-}
  mempty = empty

  {-# INLINE mappend #-}
  mappend = (<>)

  {-# INLINE mconcat #-}
  mconcat = concat

instance (Show a, Unbox a) => Show (Vector a) where
  showsPrec = G.showsPrec

instance (Read a, Unbox a) => Read (Vector a) where
  readPrec = G.readPrec
  readListPrec = readListPrecDefault

instance (Unbox e) => Exts.IsList (Vector e) where
  type Item (Vector e) = e
  fromList = fromList
  fromListN = fromListN
  toList = toList

-- Length information
-- ------------------

-- | /O(1)/ Yield the length of the vector.
length :: Unbox a => Vector a -> Int
{-# INLINE length #-}
length = G.length

-- | /O(1)/ Test whether a vector is empty.
null :: Unbox a => Vector a -> Bool
{-# INLINE null #-}
null = G.null

-- Indexing
-- --------

-- | O(1) Indexing.
(!) :: Unbox a => Vector a -> Int -> a
{-# INLINE (!) #-}
(!) = (G.!)

-- | O(1) Safe indexing.
(!?) :: Unbox a => Vector a -> Int -> Maybe a
{-# INLINE (!?) #-}
(!?) = (G.!?)

-- | /O(1)/ First element.
head :: Unbox a => Vector a -> a
{-# INLINE head #-}
head = G.head

-- | /O(1)/ Last element.
last :: Unbox a => Vector a -> a
{-# INLINE last #-}
last = G.last

-- | /O(1)/ Unsafe indexing without bounds checking.
unsafeIndex :: Unbox a => Vector a -> Int -> a
{-# INLINE unsafeIndex #-}
unsafeIndex = G.unsafeIndex

-- | /O(1)/ First element, without checking if the vector is empty.
unsafeHead :: Unbox a => Vector a -> a
{-# INLINE unsafeHead #-}
unsafeHead = G.unsafeHead

-- | /O(1)/ Last element, without checking if the vector is empty.
unsafeLast :: Unbox a => Vector a -> a
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
indexM :: (Unbox a, Monad m) => Vector a -> Int -> m a
{-# INLINE indexM #-}
indexM = G.indexM

-- | /O(1)/ First element of a vector in a monad. See 'indexM' for an
-- explanation of why this is useful.
headM :: (Unbox a, Monad m) => Vector a -> m a
{-# INLINE headM #-}
headM = G.headM

-- | /O(1)/ Last element of a vector in a monad. See 'indexM' for an
-- explanation of why this is useful.
lastM :: (Unbox a, Monad m) => Vector a -> m a
{-# INLINE lastM #-}
lastM = G.lastM

-- | /O(1)/ Indexing in a monad, without bounds checks. See 'indexM' for an
-- explanation of why this is useful.
unsafeIndexM :: (Unbox a, Monad m) => Vector a -> Int -> m a
{-# INLINE unsafeIndexM #-}
unsafeIndexM = G.unsafeIndexM

-- | /O(1)/ First element in a monad, without checking for empty vectors.
-- See 'indexM' for an explanation of why this is useful.
unsafeHeadM :: (Unbox a, Monad m) => Vector a -> m a
{-# INLINE unsafeHeadM #-}
unsafeHeadM = G.unsafeHeadM

-- | /O(1)/ Last element in a monad, without checking for empty vectors.
-- See 'indexM' for an explanation of why this is useful.
unsafeLastM :: (Unbox a, Monad m) => Vector a -> m a
{-# INLINE unsafeLastM #-}
unsafeLastM = G.unsafeLastM

-- Extracting subvectors (slicing)
-- -------------------------------

-- | /O(1)/ Yield a slice of the vector without copying it. The vector must
-- contain at least @i+n@ elements.
slice :: Unbox a => Int   -- ^ @i@ starting index
                 -> Int   -- ^ @n@ length
                 -> Vector a
                 -> Vector a
{-# INLINE slice #-}
slice = G.slice

-- | /O(1)/ Yield all but the last element without copying. The vector may not
-- be empty.
init :: Unbox a => Vector a -> Vector a
{-# INLINE init #-}
init = G.init

-- | /O(1)/ Yield all but the first element without copying. The vector may not
-- be empty.
tail :: Unbox a => Vector a -> Vector a
{-# INLINE tail #-}
tail = G.tail

-- | /O(1)/ Yield at the first @n@ elements without copying. The vector may
-- contain less than @n@ elements, in which case it is returned unchanged.
take :: Unbox a => Int -> Vector a -> Vector a
{-# INLINE take #-}
take = G.take

-- | /O(1)/ Yield all but the first @n@ elements without copying. The vector may
-- contain less than @n@ elements, in which case an empty vector is returned.
drop :: Unbox a => Int -> Vector a -> Vector a
{-# INLINE drop #-}
drop = G.drop

-- | /O(1)/ Yield the first @n@ elements paired with the remainder, without copying.
--
-- Note that @'splitAt' n v@ is equivalent to @('take' n v, 'drop' n v)@,
-- but slightly more efficient.
--
-- @since 0.7.1
splitAt :: Unbox a => Int -> Vector a -> (Vector a, Vector a)
{-# INLINE splitAt #-}
splitAt = G.splitAt

-- | /O(1)/ Yield the 'head' and 'tail' of the vector, or 'Nothing' if
-- the vector is empty.
--
-- @since 0.12.2.0
uncons :: Unbox a => Vector a -> Maybe (a, Vector a)
{-# INLINE uncons #-}
uncons = G.uncons

-- | /O(1)/ Yield the 'last' and 'init' of the vector, or 'Nothing' if
-- the vector is empty.
--
-- @since 0.12.2.0
unsnoc :: Unbox a => Vector a -> Maybe (Vector a, a)
{-# INLINE unsnoc #-}
unsnoc = G.unsnoc

-- | /O(1)/ Yield a slice of the vector without copying. The vector must
-- contain at least @i+n@ elements, but this is not checked.
unsafeSlice :: Unbox a => Int   -- ^ @i@ starting index
                       -> Int   -- ^ @n@ length
                       -> Vector a
                       -> Vector a
{-# INLINE unsafeSlice #-}
unsafeSlice = G.unsafeSlice

-- | /O(1)/ Yield all but the last element without copying. The vector may not
-- be empty, but this is not checked.
unsafeInit :: Unbox a => Vector a -> Vector a
{-# INLINE unsafeInit #-}
unsafeInit = G.unsafeInit

-- | /O(1)/ Yield all but the first element without copying. The vector may not
-- be empty, but this is not checked.
unsafeTail :: Unbox a => Vector a -> Vector a
{-# INLINE unsafeTail #-}
unsafeTail = G.unsafeTail

-- | /O(1)/ Yield the first @n@ elements without copying. The vector must
-- contain at least @n@ elements, but this is not checked.
unsafeTake :: Unbox a => Int -> Vector a -> Vector a
{-# INLINE unsafeTake #-}
unsafeTake = G.unsafeTake

-- | /O(1)/ Yield all but the first @n@ elements without copying. The vector
-- must contain at least @n@ elements, but this is not checked.
unsafeDrop :: Unbox a => Int -> Vector a -> Vector a
{-# INLINE unsafeDrop #-}
unsafeDrop = G.unsafeDrop

-- Initialisation
-- --------------

-- | /O(1)/ The empty vector.
empty :: Unbox a => Vector a
{-# INLINE empty #-}
empty = G.empty

-- | /O(1)/ A vector with exactly one element.
singleton :: Unbox a => a -> Vector a
{-# INLINE singleton #-}
singleton = G.singleton

-- | /O(n)/ A vector of the given length with the same value in each position.
replicate :: Unbox a => Int -> a -> Vector a
{-# INLINE replicate #-}
replicate = G.replicate

-- | /O(n)/ Construct a vector of the given length by applying the function to
-- each index.
generate :: Unbox a => Int -> (Int -> a) -> Vector a
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
-- >>> import qualified Data.Vector.Unboxed as VU
-- >>> VU.iterateN 0 undefined undefined :: VU.Vector Int
-- []
-- >>> VU.iterateN 3 (\(i, c) -> (pred i, succ c)) (0 :: Int, 'a')
-- [(0,'a'),(-1,'b'),(-2,'c')]
--
-- @since 0.7.1
iterateN :: Unbox a => Int -> (a -> a) -> a -> Vector a
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
unfoldr :: Unbox a => (b -> Maybe (a, b)) -> b -> Vector a
{-# INLINE unfoldr #-}
unfoldr = G.unfoldr

-- | /O(n)/ Construct a vector with at most @n@ elements by repeatedly applying
-- the generator function to a seed. The generator function yields 'Just' the
-- next element and the new seed or 'Nothing' if there are no more elements.
--
-- > unfoldrN 3 (\n -> Just (n,n-1)) 10 = <10,9,8>
unfoldrN :: Unbox a => Int -> (b -> Maybe (a, b)) -> b -> Vector a
{-# INLINE unfoldrN #-}
unfoldrN = G.unfoldrN

-- | /O(n)/ Construct a vector with exactly @n@ elements by repeatedly applying
-- the generator function to a seed. The generator function yields the
-- next element and the new seed.
--
-- > unfoldrExactN 3 (\n -> (n,n-1)) 10 = <10,9,8>
--
-- @since 0.12.2.0
unfoldrExactN  :: Unbox a => Int -> (b -> (a, b)) -> b -> Vector a
{-# INLINE unfoldrExactN #-}
unfoldrExactN = G.unfoldrExactN

-- | /O(n)/ Construct a vector by repeatedly applying the monadic
-- generator function to a seed. The generator function yields 'Just'
-- the next element and the new seed or 'Nothing' if there are no more
-- elements.
unfoldrM :: (Monad m, Unbox a) => (b -> m (Maybe (a, b))) -> b -> m (Vector a)
{-# INLINE unfoldrM #-}
unfoldrM = G.unfoldrM

-- | /O(n)/ Construct a vector by repeatedly applying the monadic
-- generator function to a seed. The generator function yields 'Just'
-- the next element and the new seed or 'Nothing' if there are no more
-- elements.
unfoldrNM :: (Monad m, Unbox a) => Int -> (b -> m (Maybe (a, b))) -> b -> m (Vector a)
{-# INLINE unfoldrNM #-}
unfoldrNM = G.unfoldrNM

-- | /O(n)/ Construct a vector with exactly @n@ elements by repeatedly
-- applying the monadic generator function to a seed. The generator
-- function yields the next element and the new seed.
--
-- @since 0.12.2.0
unfoldrExactNM :: (Monad m, Unbox a) => Int -> (b -> m (a, b)) -> b -> m (Vector a)
{-# INLINE unfoldrExactNM #-}
unfoldrExactNM = G.unfoldrExactNM

-- | /O(n)/ Construct a vector with @n@ elements by repeatedly applying the
-- generator function to the already constructed part of the vector.
--
-- > constructN 3 f = let a = f <> ; b = f <a> ; c = f <a,b> in <a,b,c>
constructN :: Unbox a => Int -> (Vector a -> a) -> Vector a
{-# INLINE constructN #-}
constructN = G.constructN

-- | /O(n)/ Construct a vector with @n@ elements from right to left by
-- repeatedly applying the generator function to the already constructed part
-- of the vector.
--
-- > constructrN 3 f = let a = f <> ; b = f<a> ; c = f <b,a> in <c,b,a>
constructrN :: Unbox a => Int -> (Vector a -> a) -> Vector a
{-# INLINE constructrN #-}
constructrN = G.constructrN

-- Enumeration
-- -----------

-- | /O(n)/ Yield a vector of the given length, containing the values @x@, @x+1@
-- etc. This operation is usually more efficient than 'enumFromTo'.
--
-- > enumFromN 5 3 = <5,6,7>
enumFromN :: (Unbox a, Num a) => a -> Int -> Vector a
{-# INLINE enumFromN #-}
enumFromN = G.enumFromN

-- | /O(n)/ Yield a vector of the given length, containing the values @x@, @x+y@,
-- @x+y+y@ etc. This operations is usually more efficient than 'enumFromThenTo'.
--
-- > enumFromStepN 1 2 5 = <1,3,5,7,9>
enumFromStepN :: (Unbox a, Num a) => a -> a -> Int -> Vector a
{-# INLINE enumFromStepN #-}
enumFromStepN = G.enumFromStepN

-- | /O(n)/ Enumerate values from @x@ to @y@.
--
-- /WARNING:/ This operation can be very inefficient. If possible, use
-- 'enumFromN' instead.
enumFromTo :: (Unbox a, Enum a) => a -> a -> Vector a
{-# INLINE enumFromTo #-}
enumFromTo = G.enumFromTo

-- | /O(n)/ Enumerate values from @x@ to @y@ with a specific step @z@.
--
-- /WARNING:/ This operation can be very inefficient. If possible, use
-- 'enumFromStepN' instead.
enumFromThenTo :: (Unbox a, Enum a) => a -> a -> a -> Vector a
{-# INLINE enumFromThenTo #-}
enumFromThenTo = G.enumFromThenTo

-- Concatenation
-- -------------

-- | /O(n)/ Prepend an element.
cons :: Unbox a => a -> Vector a -> Vector a
{-# INLINE cons #-}
cons = G.cons

-- | /O(n)/ Append an element.
snoc :: Unbox a => Vector a -> a -> Vector a
{-# INLINE snoc #-}
snoc = G.snoc

infixr 5 ++
-- | /O(m+n)/ Concatenate two vectors.
(++) :: Unbox a => Vector a -> Vector a -> Vector a
{-# INLINE (++) #-}
(++) = (G.++)

-- | /O(n)/ Concatenate all vectors in the list.
concat :: Unbox a => [Vector a] -> Vector a
{-# INLINE concat #-}
concat = G.concat

-- Monadic initialisation
-- ----------------------

-- | /O(n)/ Execute the monadic action the given number of times and store the
-- results in a vector.
replicateM :: (Monad m, Unbox a) => Int -> m a -> m (Vector a)
{-# INLINE replicateM #-}
replicateM = G.replicateM

-- | /O(n)/ Construct a vector of the given length by applying the monadic
-- action to each index.
generateM :: (Monad m, Unbox a) => Int -> (Int -> m a) -> m (Vector a)
{-# INLINE generateM #-}
generateM = G.generateM

-- | /O(n)/ Apply the monadic function \(\max(n - 1, 0)\) times to an initial value, producing a vector
-- of length \(\max(n, 0)\). The 0th element will contain the initial value, which is why there
-- is one less function application than the number of elements in the produced vector.
--
-- For a non-monadic version, see `iterateN`.
--
-- @since 0.12.0.0
iterateNM :: (Monad m, Unbox a) => Int -> (a -> m a) -> a -> m (Vector a)
{-# INLINE iterateNM #-}
iterateNM = G.iterateNM

-- | Execute the monadic action and freeze the resulting vector.
--
-- @
-- create (do { v \<- new 2; write v 0 \'a\'; write v 1 \'b\'; return v }) = \<'a','b'\>
-- @
create :: Unbox a => (forall s. ST s (MVector s a)) -> Vector a
{-# INLINE create #-}
-- NOTE: eta-expanded due to http://hackage.haskell.org/trac/ghc/ticket/4120
create p = G.create p

-- | Execute the monadic action and freeze the resulting vectors.
createT :: (Traversable f, Unbox a) => (forall s. ST s (f (MVector s a))) -> f (Vector a)
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
force :: Unbox a => Vector a -> Vector a
{-# INLINE force #-}
force = G.force

-- Bulk updates
-- ------------

-- | /O(m+n)/ For each pair @(i,a)@ from the list of idnex/value pairs,
-- replace the vector element at position @i@ by @a@.
--
-- > <5,9,2,7> // [(2,1),(0,3),(2,8)] = <3,9,8,7>
--
(//) :: Unbox a => Vector a   -- ^ initial vector (of length @m@)
                -> [(Int, a)] -- ^ list of index/value pairs (of length @n@)
                -> Vector a
{-# INLINE (//) #-}
(//) = (G.//)

-- | /O(m+n)/ For each pair @(i,a)@ from the vector of index/value pairs,
-- replace the vector element at position @i@ by @a@.
--
-- > update <5,9,2,7> <(2,1),(0,3),(2,8)> = <3,9,8,7>
--
update :: Unbox a
       => Vector a        -- ^ initial vector (of length @m@)
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
update_ :: Unbox a
        => Vector a   -- ^ initial vector (of length @m@)
        -> Vector Int -- ^ index vector (of length @n1@)
        -> Vector a   -- ^ value vector (of length @n2@)
        -> Vector a
{-# INLINE update_ #-}
update_ = G.update_

-- | Same as ('//'), but without bounds checking.
unsafeUpd :: Unbox a => Vector a -> [(Int, a)] -> Vector a
{-# INLINE unsafeUpd #-}
unsafeUpd = G.unsafeUpd

-- | Same as 'update', but without bounds checking.
unsafeUpdate :: Unbox a => Vector a -> Vector (Int, a) -> Vector a
{-# INLINE unsafeUpdate #-}
unsafeUpdate = G.unsafeUpdate

-- | Same as 'update_', but without bounds checking.
unsafeUpdate_ :: Unbox a => Vector a -> Vector Int -> Vector a -> Vector a
{-# INLINE unsafeUpdate_ #-}
unsafeUpdate_ = G.unsafeUpdate_

-- Accumulations
-- -------------

-- | /O(m+n)/ For each pair @(i,b)@ from the list, replace the vector element
-- @a@ at position @i@ by @f a b@.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector.Unboxed as VU
-- >>> VU.accum (+) (VU.fromList [1000,2000,3000 :: Int]) [(2,4),(1,6),(0,3),(1,10)]
-- [1003,2016,3004]
accum :: Unbox a
      => (a -> b -> a) -- ^ accumulating function @f@
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
-- >>> import qualified Data.Vector.Unboxed as VU
-- >>> VU.accumulate (+) (VU.fromList [1000,2000,3000 :: Int]) (VU.fromList [(2,4),(1,6),(0,3),(1,10)])
-- [1003,2016,3004]
accumulate :: (Unbox a, Unbox b)
            => (a -> b -> a)  -- ^ accumulating function @f@
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
accumulate_ :: (Unbox a, Unbox b)
            => (a -> b -> a) -- ^ accumulating function @f@
            -> Vector a      -- ^ initial vector (of length @m@)
            -> Vector Int    -- ^ index vector (of length @n1@)
            -> Vector b      -- ^ value vector (of length @n2@)
            -> Vector a
{-# INLINE accumulate_ #-}
accumulate_ = G.accumulate_

-- | Same as 'accum', but without bounds checking.
unsafeAccum :: Unbox a => (a -> b -> a) -> Vector a -> [(Int,b)] -> Vector a
{-# INLINE unsafeAccum #-}
unsafeAccum = G.unsafeAccum

-- | Same as 'accumulate', but without bounds checking.
unsafeAccumulate :: (Unbox a, Unbox b)
                => (a -> b -> a) -> Vector a -> Vector (Int,b) -> Vector a
{-# INLINE unsafeAccumulate #-}
unsafeAccumulate = G.unsafeAccumulate

-- | Same as 'accumulate_', but without bounds checking.
unsafeAccumulate_ :: (Unbox a, Unbox b) =>
               (a -> b -> a) -> Vector a -> Vector Int -> Vector b -> Vector a
{-# INLINE unsafeAccumulate_ #-}
unsafeAccumulate_ = G.unsafeAccumulate_

-- Permutations
-- ------------

-- | /O(n)/ Reverse a vector.
reverse :: Unbox a => Vector a -> Vector a
{-# INLINE reverse #-}
reverse = G.reverse

-- | /O(n)/ Yield the vector obtained by replacing each element @i@ of the
-- index vector by @xs'!'i@. This is equivalent to @'map' (xs'!') is@, but is
-- often much more efficient.
--
-- > backpermute <a,b,c,d> <0,3,2,3,1,0> = <a,d,c,d,b,a>
backpermute :: Unbox a => Vector a -> Vector Int -> Vector a
{-# INLINE backpermute #-}
backpermute = G.backpermute

-- | Same as 'backpermute', but without bounds checking.
unsafeBackpermute :: Unbox a => Vector a -> Vector Int -> Vector a
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
-- >>> import qualified Data.Vector.Unboxed as VU
-- >>> import qualified Data.Vector.Unboxed.Mutable as MVU
-- >>> VU.modify (\v -> MVU.write v 0 'x') $ VU.replicate 4 'a'
-- "xaaa"
modify :: Unbox a => (forall s. MVector s a -> ST s ()) -> Vector a -> Vector a
{-# INLINE modify #-}
modify p = G.modify p

-- Indexing
-- --------

-- | /O(n)/ Pair each element in a vector with its index.
indexed :: Unbox a => Vector a -> Vector (Int,a)
{-# INLINE indexed #-}
indexed = G.indexed

-- Mapping
-- -------

-- | /O(n)/ Map a function over a vector.
map :: (Unbox a, Unbox b) => (a -> b) -> Vector a -> Vector b
{-# INLINE map #-}
map = G.map

-- | /O(n)/ Apply a function to every element of a vector and its index.
imap :: (Unbox a, Unbox b) => (Int -> a -> b) -> Vector a -> Vector b
{-# INLINE imap #-}
imap = G.imap

-- | Map a function over a vector and concatenate the results.
concatMap :: (Unbox a, Unbox b) => (a -> Vector b) -> Vector a -> Vector b
{-# INLINE concatMap #-}
concatMap = G.concatMap

-- | Apply a function to every element of a vector and its index, and concatenate the results.
--
-- @since 0.13.3.0
iconcatMap :: (Unbox a, Unbox b) => (Int -> a -> Vector b) -> Vector a -> Vector b
{-# INLINE iconcatMap #-}
iconcatMap = G.iconcatMap

-- Monadic mapping
-- ---------------

-- | /O(n)/ Apply the monadic action to all elements of the vector, yielding a
-- vector of results.
mapM :: (Monad m, Unbox a, Unbox b) => (a -> m b) -> Vector a -> m (Vector b)
{-# INLINE mapM #-}
mapM = G.mapM

-- | /O(n)/ Apply the monadic action to every element of a vector and its
-- index, yielding a vector of results.
imapM :: (Monad m, Unbox a, Unbox b)
      => (Int -> a -> m b) -> Vector a -> m (Vector b)
{-# INLINE imapM #-}
imapM = G.imapM

-- | /O(n)/ Apply the monadic action to all elements of a vector and ignore the
-- results.
mapM_ :: (Monad m, Unbox a) => (a -> m b) -> Vector a -> m ()
{-# INLINE mapM_ #-}
mapM_ = G.mapM_

-- | /O(n)/ Apply the monadic action to every element of a vector and its
-- index, ignoring the results.
imapM_ :: (Monad m, Unbox a) => (Int -> a -> m b) -> Vector a -> m ()
{-# INLINE imapM_ #-}
imapM_ = G.imapM_

-- | /O(n)/ Apply the monadic action to all elements of the vector, yielding a
-- vector of results. Equivalent to @flip 'mapM'@.
forM :: (Monad m, Unbox a, Unbox b) => Vector a -> (a -> m b) -> m (Vector b)
{-# INLINE forM #-}
forM = G.forM

-- | /O(n)/ Apply the monadic action to all elements of a vector and ignore the
-- results. Equivalent to @flip 'mapM_'@.
forM_ :: (Monad m, Unbox a) => Vector a -> (a -> m b) -> m ()
{-# INLINE forM_ #-}
forM_ = G.forM_

-- | /O(n)/ Apply the monadic action to all elements of the vector and their indices, yielding a
-- vector of results. Equivalent to @'flip' 'imapM'@.
--
-- @since 0.12.2.0
iforM :: (Monad m, Unbox a, Unbox b) => Vector a -> (Int -> a -> m b) -> m (Vector b)
{-# INLINE iforM #-}
iforM = G.iforM

-- | /O(n)/ Apply the monadic action to all elements of the vector and their indices
-- and ignore the results. Equivalent to @'flip' 'imapM_'@.
--
-- @since 0.12.2.0
iforM_ :: (Monad m, Unbox a) => Vector a -> (Int -> a -> m b) -> m ()
{-# INLINE iforM_ #-}
iforM_ = G.iforM_

-- Zipping
-- -------

-- $zip
--
-- Following functions could be used to construct vector of tuples
-- from tuple of vectors. This operation is done in /O(1)/ time and
-- will share underlying buffers.
--
-- Note that variants from "Data.Vector.Generic" doesn't have this
-- property.

-- $unzip
--
-- Following functions could be used to access underlying
-- representation of array of tuples. They convert array to tuple of
-- arrays. This operation is done in /O(1)/ time and will share
-- underlying buffers.
--
-- Note that variants from "Data.Vector.Generic" doesn't have this
-- property.


-- | /O(min(m,n))/ Zip two vectors with the given function.
zipWith :: (Unbox a, Unbox b, Unbox c)
        => (a -> b -> c) -> Vector a -> Vector b -> Vector c
{-# INLINE zipWith #-}
zipWith = G.zipWith

-- | Zip three vectors with the given function.
zipWith3 :: (Unbox a, Unbox b, Unbox c, Unbox d)
         => (a -> b -> c -> d) -> Vector a -> Vector b -> Vector c -> Vector d
{-# INLINE zipWith3 #-}
zipWith3 = G.zipWith3

zipWith4 :: (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e)
         => (a -> b -> c -> d -> e)
         -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
{-# INLINE zipWith4 #-}
zipWith4 = G.zipWith4

zipWith5 :: (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e, Unbox f)
         => (a -> b -> c -> d -> e -> f)
         -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
         -> Vector f
{-# INLINE zipWith5 #-}
zipWith5 = G.zipWith5

zipWith6 :: (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e, Unbox f, Unbox g)
         => (a -> b -> c -> d -> e -> f -> g)
         -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
         -> Vector f -> Vector g
{-# INLINE zipWith6 #-}
zipWith6 = G.zipWith6

-- | /O(min(m,n))/ Zip two vectors with a function that also takes the
-- elements' indices.
izipWith :: (Unbox a, Unbox b, Unbox c)
         => (Int -> a -> b -> c) -> Vector a -> Vector b -> Vector c
{-# INLINE izipWith #-}
izipWith = G.izipWith

-- | Zip three vectors and their indices with the given function.
izipWith3 :: (Unbox a, Unbox b, Unbox c, Unbox d)
          => (Int -> a -> b -> c -> d)
          -> Vector a -> Vector b -> Vector c -> Vector d
{-# INLINE izipWith3 #-}
izipWith3 = G.izipWith3

izipWith4 :: (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e)
          => (Int -> a -> b -> c -> d -> e)
          -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
{-# INLINE izipWith4 #-}
izipWith4 = G.izipWith4

izipWith5 :: (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e, Unbox f)
          => (Int -> a -> b -> c -> d -> e -> f)
          -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
          -> Vector f
{-# INLINE izipWith5 #-}
izipWith5 = G.izipWith5

izipWith6 :: (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e, Unbox f, Unbox g)
          => (Int -> a -> b -> c -> d -> e -> f -> g)
          -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
          -> Vector f -> Vector g
{-# INLINE izipWith6 #-}
izipWith6 = G.izipWith6

-- Monadic zipping
-- ---------------

-- | /O(min(m,n))/ Zip the two vectors with the monadic action and yield a
-- vector of results.
zipWithM :: (Monad m, Unbox a, Unbox b, Unbox c)
         => (a -> b -> m c) -> Vector a -> Vector b -> m (Vector c)
{-# INLINE zipWithM #-}
zipWithM = G.zipWithM

-- | /O(min(m,n))/ Zip the two vectors with a monadic action that also takes
-- the element index and yield a vector of results.
izipWithM :: (Monad m, Unbox a, Unbox b, Unbox c)
          => (Int -> a -> b -> m c) -> Vector a -> Vector b -> m (Vector c)
{-# INLINE izipWithM #-}
izipWithM = G.izipWithM

-- | /O(min(m,n))/ Zip the two vectors with the monadic action and ignore the
-- results.
zipWithM_ :: (Monad m, Unbox a, Unbox b)
          => (a -> b -> m c) -> Vector a -> Vector b -> m ()
{-# INLINE zipWithM_ #-}
zipWithM_ = G.zipWithM_

-- | /O(min(m,n))/ Zip the two vectors with a monadic action that also takes
-- the element index and ignore the results.
izipWithM_ :: (Monad m, Unbox a, Unbox b)
           => (Int -> a -> b -> m c) -> Vector a -> Vector b -> m ()
{-# INLINE izipWithM_ #-}
izipWithM_ = G.izipWithM_

-- Filtering
-- ---------

-- | /O(n)/ Drop all elements that do not satisfy the predicate.
filter :: Unbox a => (a -> Bool) -> Vector a -> Vector a
{-# INLINE filter #-}
filter = G.filter

-- | /O(n)/ Drop all elements that do not satisfy the predicate which is applied to
-- the values and their indices.
ifilter :: Unbox a => (Int -> a -> Bool) -> Vector a -> Vector a
{-# INLINE ifilter #-}
ifilter = G.ifilter

-- | /O(n)/ Drop repeated adjacent elements. The first element in each group is returned.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector.Unboxed as VU
-- >>> VU.uniq $ VU.fromList [1,3,3,200,3 :: Int]
-- [1,3,200,3]
-- >>> import Data.Semigroup
-- >>> VU.uniq $ VU.fromList [ Arg 1 'a', Arg 1 'b', Arg (1 :: Int) 'c']
-- [Arg 1 'a']
uniq :: (Unbox a, Eq a) => Vector a -> Vector a
{-# INLINE uniq #-}
uniq = G.uniq

-- | /O(n)/ Map the values and collect the 'Just' results.
mapMaybe :: (Unbox a, Unbox b) => (a -> Maybe b) -> Vector a -> Vector b
{-# INLINE mapMaybe #-}
mapMaybe = G.mapMaybe

-- | /O(n)/ Map the indices/values and collect the 'Just' results.
imapMaybe :: (Unbox a, Unbox b) => (Int -> a -> Maybe b) -> Vector a -> Vector b
{-# INLINE imapMaybe #-}
imapMaybe = G.imapMaybe

-- | /O(n)/ Drop all elements that do not satisfy the monadic predicate.
filterM :: (Monad m, Unbox a) => (a -> m Bool) -> Vector a -> m (Vector a)
{-# INLINE filterM #-}
filterM = G.filterM

-- | /O(n)/ Apply the monadic function to each element of the vector and
-- discard elements returning 'Nothing'.
--
-- @since 0.12.2.0
mapMaybeM :: (Monad m, Unbox a, Unbox b) => (a -> m (Maybe b)) -> Vector a -> m (Vector b)
{-# INLINE mapMaybeM #-}
mapMaybeM = G.mapMaybeM

-- | /O(n)/ Apply the monadic function to each element of the vector and its index.
-- Discard elements returning 'Nothing'.
--
-- @since 0.12.2.0
imapMaybeM :: (Monad m, Unbox a, Unbox b) => (Int -> a -> m (Maybe b)) -> Vector a -> m (Vector b)
{-# INLINE imapMaybeM #-}
imapMaybeM = G.imapMaybeM

-- | /O(n)/ Yield the longest prefix of elements satisfying the predicate.
-- The current implementation is not copy-free, unless the result vector is
-- fused away.
takeWhile :: Unbox a => (a -> Bool) -> Vector a -> Vector a
{-# INLINE takeWhile #-}
takeWhile = G.takeWhile

-- | /O(n)/ Drop the longest prefix of elements that satisfy the predicate
-- without copying.
dropWhile :: Unbox a => (a -> Bool) -> Vector a -> Vector a
{-# INLINE dropWhile #-}
dropWhile = G.dropWhile

-- Partitioning
-- -------------

-- | /O(n)/ Split the vector in two parts, the first one containing those
-- elements that satisfy the predicate and the second one those that don't. The
-- relative order of the elements is preserved at the cost of a sometimes
-- reduced performance compared to 'unstablePartition'.
partition :: Unbox a => (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE partition #-}
partition = G.partition

-- | /O(n)/ Split the vector into two parts, the first one containing the
-- @`Left`@ elements and the second containing the @`Right`@ elements.
-- The relative order of the elements is preserved.
--
-- @since 0.12.1.0
partitionWith :: (Unbox a, Unbox b, Unbox c) => (a -> Either b c) -> Vector a -> (Vector b, Vector c)
{-# INLINE partitionWith #-}
partitionWith = G.partitionWith

-- | /O(n)/ Split the vector in two parts, the first one containing those
-- elements that satisfy the predicate and the second one those that don't.
-- The order of the elements is not preserved, but the operation is often
-- faster than 'partition'.
unstablePartition :: Unbox a => (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE unstablePartition #-}
unstablePartition = G.unstablePartition

-- | /O(n)/ Split the vector into the longest prefix of elements that satisfy
-- the predicate and the rest without copying.
--
-- Does not fuse.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector.Unboxed as VU
-- >>> VU.span (<4) $ VU.generate 10 id
-- ([0,1,2,3],[4,5,6,7,8,9])
span :: Unbox a => (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE span #-}
span = G.span

-- | /O(n)/ Split the vector into the longest prefix of elements that do not
-- satisfy the predicate and the rest without copying.
--
-- Does not fuse.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector.Unboxed as VU
-- >>> VU.break (>4) $ VU.generate 10 id
-- ([0,1,2,3,4],[5,6,7,8,9])
break :: Unbox a => (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE break #-}
break = G.break

-- | /O(n)/ Split the vector into the longest prefix of elements that satisfy
-- the predicate and the rest without copying.
--
-- Does not fuse.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector.Unboxed as VU
-- >>> VU.spanR (>4) $ VU.generate 10 id
-- ([5,6,7,8,9],[0,1,2,3,4])
spanR :: Unbox a => (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE spanR #-}
spanR = G.spanR

-- | /O(n)/ Split the vector into the longest prefix of elements that do not
-- satisfy the predicate and the rest without copying.
--
-- Does not fuse.
--
-- @since NEXT_VERSION
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector.Unboxed as VU
-- >>> VU.breakR (<5) $ VU.generate 10 id
-- ([5,6,7,8,9],[0,1,2,3,4])
breakR :: Unbox a => (a -> Bool) -> Vector a -> (Vector a, Vector a)
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
-- >>> import qualified Data.Vector.Unboxed as VU
-- >>> import           Data.Char (isUpper)
-- >>> VU.groupBy (\a b -> isUpper a == isUpper b) (VU.fromList "Mississippi River")
-- ["M","ississippi ","R","iver"]
--
-- See also 'Data.List.groupBy', 'group'.
--
-- @since 0.13.0.1
groupBy :: Unbox a => (a -> a -> Bool) -> Vector a -> [Vector a]
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
-- >>> import qualified Data.Vector.Unboxed as VU
-- >>> VU.group (VU.fromList "Mississippi")
-- ["M","i","ss","i","ss","i","pp","i"]
--
-- See also 'Data.List.group'.
--
-- @since 0.13.0.1
group :: (Unbox a, Eq a) => Vector a -> [Vector a]
group = G.groupBy (==)

-- Searching
-- ---------

infix 4 `elem`
-- | /O(n)/ Check if the vector contains an element.
elem :: (Unbox a, Eq a) => a -> Vector a -> Bool
{-# INLINE elem #-}
elem = G.elem

infix 4 `notElem`
-- | /O(n)/ Check if the vector does not contain an element (inverse of 'elem').
notElem :: (Unbox a, Eq a) => a -> Vector a -> Bool
{-# INLINE notElem #-}
notElem = G.notElem

-- | /O(n)/ Yield 'Just' the first element matching the predicate or 'Nothing'
-- if no such element exists.
find :: Unbox a => (a -> Bool) -> Vector a -> Maybe a
{-# INLINE find #-}
find = G.find

-- | /O(n)/ Yield 'Just' the index of the first element matching the predicate
-- or 'Nothing' if no such element exists.
findIndex :: Unbox a => (a -> Bool) -> Vector a -> Maybe Int
{-# INLINE findIndex #-}
findIndex = G.findIndex

-- | /O(n)/ Yield 'Just' the index of the /last/ element matching the predicate
-- or 'Nothing' if no such element exists.
--
-- Does not fuse.
findIndexR :: Unbox a => (a -> Bool) -> Vector a -> Maybe Int
{-# INLINE findIndexR #-}
findIndexR = G.findIndexR

-- | /O(n)/ Yield the indices of elements satisfying the predicate in ascending
-- order.
findIndices :: Unbox a => (a -> Bool) -> Vector a -> Vector Int
{-# INLINE findIndices #-}
findIndices = G.findIndices

-- | /O(n)/ Yield 'Just' the index of the first occurrence of the given element or
-- 'Nothing' if the vector does not contain the element. This is a specialised
-- version of 'findIndex'.
elemIndex :: (Unbox a, Eq a) => a -> Vector a -> Maybe Int
{-# INLINE elemIndex #-}
elemIndex = G.elemIndex

-- | /O(n)/ Yield the indices of all occurrences of the given element in
-- ascending order. This is a specialised version of 'findIndices'.
elemIndices :: (Unbox a, Eq a) => a -> Vector a -> Vector Int
{-# INLINE elemIndices #-}
elemIndices = G.elemIndices

-- Folding
-- -------

-- | /O(n)/ Left fold.
foldl :: Unbox b => (a -> b -> a) -> a -> Vector b -> a
{-# INLINE foldl #-}
foldl = G.foldl

-- | /O(n)/ Left fold on non-empty vectors.
foldl1 :: Unbox a => (a -> a -> a) -> Vector a -> a
{-# INLINE foldl1 #-}
foldl1 = G.foldl1

-- | /O(n)/ Left fold with strict accumulator.
foldl' :: Unbox b => (a -> b -> a) -> a -> Vector b -> a
{-# INLINE foldl' #-}
foldl' = G.foldl'

-- | /O(n)/ Left fold on non-empty vectors with strict accumulator.
foldl1' :: Unbox a => (a -> a -> a) -> Vector a -> a
{-# INLINE foldl1' #-}
foldl1' = G.foldl1'

-- | /O(n)/ Right fold.
foldr :: Unbox a => (a -> b -> b) -> b -> Vector a -> b
{-# INLINE foldr #-}
foldr = G.foldr

-- | /O(n)/ Right fold on non-empty vectors.
foldr1 :: Unbox a => (a -> a -> a) -> Vector a -> a
{-# INLINE foldr1 #-}
foldr1 = G.foldr1

-- | /O(n)/ Right fold with a strict accumulator.
foldr' :: Unbox a => (a -> b -> b) -> b -> Vector a -> b
{-# INLINE foldr' #-}
foldr' = G.foldr'

-- | /O(n)/ Right fold on non-empty vectors with strict accumulator.
foldr1' :: Unbox a => (a -> a -> a) -> Vector a -> a
{-# INLINE foldr1' #-}
foldr1' = G.foldr1'

-- | /O(n)/ Left fold using a function applied to each element and its index.
ifoldl :: Unbox b => (a -> Int -> b -> a) -> a -> Vector b -> a
{-# INLINE ifoldl #-}
ifoldl = G.ifoldl

-- | /O(n)/ Left fold with strict accumulator using a function applied to each element
-- and its index.
ifoldl' :: Unbox b => (a -> Int -> b -> a) -> a -> Vector b -> a
{-# INLINE ifoldl' #-}
ifoldl' = G.ifoldl'

-- | /O(n)/ Right fold using a function applied to each element and its index.
ifoldr :: Unbox a => (Int -> a -> b -> b) -> b -> Vector a -> b
{-# INLINE ifoldr #-}
ifoldr = G.ifoldr

-- | /O(n)/ Right fold with strict accumulator using a function applied to each
-- element and its index.
ifoldr' :: Unbox a => (Int -> a -> b -> b) -> b -> Vector a -> b
{-# INLINE ifoldr' #-}
ifoldr' = G.ifoldr'

-- | /O(n)/ Map each element of the structure to a monoid and combine
-- the results. It uses the same implementation as the corresponding method
-- of the 'Foldable' type cless. Note that it's implemented in terms of 'foldr'
-- and won't fuse with functions that traverse the vector from left to
-- right ('map', 'generate', etc.).
--
-- @since 0.12.2.0
foldMap :: (Monoid m, Unbox a) => (a -> m) -> Vector a -> m
{-# INLINE foldMap #-}
foldMap = G.foldMap

-- | /O(n)/ Like 'foldMap', but strict in the accumulator. It uses the same
-- implementation as the corresponding method of the 'Foldable' type class.
-- Note that it's implemented in terms of 'foldl'', so it fuses in most
-- contexts.
--
-- @since 0.12.2.0
foldMap' :: (Monoid m, Unbox a) => (a -> m) -> Vector a -> m
{-# INLINE foldMap' #-}
foldMap' = G.foldMap'

-- Specialised folds
-- -----------------

-- | /O(n)/ Check if all elements satisfy the predicate.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector.Unboxed as VU
-- >>> VU.all even $ VU.fromList [2, 4, 12 :: Int]
-- True
-- >>> VU.all even $ VU.fromList [2, 4, 13 :: Int]
-- False
-- >>> VU.all even (VU.empty :: VU.Vector Int)
-- True
all :: Unbox a => (a -> Bool) -> Vector a -> Bool
{-# INLINE all #-}
all = G.all

-- | /O(n)/ Check if any element satisfies the predicate.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector.Unboxed as VU
-- >>> VU.any even $ VU.fromList [1, 3, 7 :: Int]
-- False
-- >>> VU.any even $ VU.fromList [3, 2, 13 :: Int]
-- True
-- >>> VU.any even (VU.empty :: VU.Vector Int)
-- False
any :: Unbox a => (a -> Bool) -> Vector a -> Bool
{-# INLINE any #-}
any = G.any

-- | /O(n)/ Check if all elements are 'True'.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector.Unboxed as VU
-- >>> VU.and $ VU.fromList [True, False]
-- False
-- >>> VU.and VU.empty
-- True
and :: Vector Bool -> Bool
{-# INLINE and #-}
and = G.and

-- | /O(n)/ Check if any element is 'True'.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector.Unboxed as VU
-- >>> VU.or $ VU.fromList [True, False]
-- True
-- >>> VU.or VU.empty
-- False
or :: Vector Bool -> Bool
{-# INLINE or #-}
or = G.or

-- | /O(n)/ Compute the sum of the elements.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector.Unboxed as VU
-- >>> VU.sum $ VU.fromList [300,20,1 :: Int]
-- 321
-- >>> VU.sum (VU.empty :: VU.Vector Int)
-- 0
sum :: (Unbox a, Num a) => Vector a -> a
{-# INLINE sum #-}
sum = G.sum

-- | /O(n)/ Compute the product of the elements.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector.Unboxed as VU
-- >>> VU.product $ VU.fromList [1,2,3,4 :: Int]
-- 24
-- >>> VU.product (VU.empty :: VU.Vector Int)
-- 1
product :: (Unbox a, Num a) => Vector a -> a
{-# INLINE product #-}
product = G.product

-- | /O(n)/ Yield the maximum element of the vector. The vector may not be
-- empty. In case of a tie, the first occurrence wins.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector.Unboxed as VU
-- >>> VU.maximum $ VU.fromList [2, 1 :: Int]
-- 2
-- >>> import Data.Semigroup
-- >>> VU.maximum $ VU.fromList [Arg 1 'a', Arg (2 :: Int) 'b']
-- Arg 2 'b'
-- >>> VU.maximum $ VU.fromList [Arg 1 'a', Arg (1 :: Int) 'b']
-- Arg 1 'a'
maximum :: (Unbox a, Ord a) => Vector a -> a
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
-- >>> import qualified Data.Vector.Unboxed as VU
-- >>> VU.maximumBy (comparing fst) $ VU.fromList [(2,'a'), (1 :: Int,'b')]
-- (2,'a')
-- >>> VU.maximumBy (comparing fst) $ VU.fromList [(1,'a'), (1 :: Int,'b')]
-- (1,'a')
maximumBy :: Unbox a => (a -> a -> Ordering) -> Vector a -> a
{-# INLINE maximumBy #-}
maximumBy = G.maximumBy

-- | /O(n)/ Yield the maximum element of the vector by comparing the results
-- of a key function on each element. In case of a tie, the first occurrence
-- wins. The vector may not be empty.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector.Unboxed as VU
-- >>> VU.maximumOn fst $ VU.fromList [(2,'a'), (1 :: Int,'b')]
-- (2,'a')
-- >>> VU.maximumOn fst $ VU.fromList [(1,'a'), (1 :: Int,'b')]
-- (1,'a')
--
-- @since 0.13.0.0
maximumOn :: (Ord b, Unbox a) => (a -> b) -> Vector a -> a
{-# INLINE maximumOn #-}
maximumOn = G.maximumOn

-- | /O(n)/ Yield the minimum element of the vector. The vector may not be
-- empty. In case of a tie, the first occurrence wins.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector.Unboxed as VU
-- >>> VU.minimum $ VU.fromList [2, 1 :: Int]
-- 1
-- >>> import Data.Semigroup
-- >>> VU.minimum $ VU.fromList [Arg 2 'a', Arg (1 :: Int) 'b']
-- Arg 1 'b'
-- >>> VU.minimum $ VU.fromList [Arg 1 'a', Arg (1 :: Int) 'b']
-- Arg 1 'a'
minimum :: (Unbox a, Ord a) => Vector a -> a
{-# INLINE minimum #-}
minimum = G.minimum

-- | /O(n)/ Yield the minimum element of the vector according to the
-- given comparison function. The vector may not be empty. In case of
-- a tie, the first occurrence wins.
--
-- ==== __Examples__
--
-- >>> import Data.Ord
-- >>> import qualified Data.Vector.Unboxed as VU
-- >>> VU.minimumBy (comparing fst) $ VU.fromList [(2,'a'), (1 :: Int,'b')]
-- (1,'b')
-- >>> VU.minimumBy (comparing fst) $ VU.fromList [(1,'a'), (1 :: Int,'b')]
-- (1,'a')
minimumBy :: Unbox a => (a -> a -> Ordering) -> Vector a -> a
-- | /O(n)/ Yield the minimum element of the vector according to the given
-- comparison function. The vector may not be empty.
{-# INLINE minimumBy #-}
minimumBy = G.minimumBy

-- | /O(n)/ Yield the minimum element of the vector by comparing the results
-- of a key function on each element. In case of a tie, the first occurrence
-- wins. The vector may not be empty.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector.Unboxed as VU
-- >>> VU.minimumOn fst $ VU.fromList [(2,'a'), (1 :: Int,'b')]
-- (1,'b')
-- >>> VU.minimumOn fst $ VU.fromList [(1,'a'), (1 :: Int,'b')]
-- (1,'a')
--
-- @since 0.13.0.0
minimumOn :: (Ord b, Unbox a) => (a -> b) -> Vector a -> a
{-# INLINE minimumOn #-}
minimumOn = G.minimumOn

-- | /O(n)/ Yield the index of the maximum element of the vector. The vector
-- may not be empty.
maxIndex :: (Unbox a, Ord a) => Vector a -> Int
{-# INLINE maxIndex #-}
maxIndex = G.maxIndex

-- | /O(n)/ Yield the index of the maximum element of the vector
-- according to the given comparison function. The vector may not be
-- empty. In case of a tie, the first occurrence wins.
--
-- ==== __Examples__
--
-- >>> import Data.Ord
-- >>> import qualified Data.Vector.Unboxed as VU
-- >>> VU.maxIndexBy (comparing fst) $ VU.fromList [(2,'a'), (1,'b')]
-- 0
-- >>> VU.maxIndexBy (comparing fst) $ VU.fromList [(1,'a'), (1,'b')]
-- 0
maxIndexBy :: Unbox a => (a -> a -> Ordering) -> Vector a -> Int
{-# INLINE maxIndexBy #-}
maxIndexBy = G.maxIndexBy

-- | /O(n)/ Yield the index of the minimum element of the vector. The vector
-- may not be empty.
minIndex :: (Unbox a, Ord a) => Vector a -> Int
{-# INLINE minIndex #-}
minIndex = G.minIndex

-- | /O(n)/ Yield the index of the minimum element of the vector according to
-- the given comparison function. The vector may not be empty.
--
-- ==== __Examples__
--
-- >>> import Data.Ord
-- >>> import qualified Data.Vector.Unboxed as VU
-- >>> VU.minIndexBy (comparing fst) $ VU.fromList [(2,'a'), (1,'b')]
-- 1
-- >>> VU.minIndexBy (comparing fst) $ VU.fromList [(1,'a'), (1,'b')]
-- 0
minIndexBy :: Unbox a => (a -> a -> Ordering) -> Vector a -> Int
{-# INLINE minIndexBy #-}
minIndexBy = G.minIndexBy

-- Monadic folds
-- -------------

-- | /O(n)/ Monadic fold.
foldM :: (Monad m, Unbox b) => (a -> b -> m a) -> a -> Vector b -> m a
{-# INLINE foldM #-}
foldM = G.foldM

-- | /O(n)/ Monadic fold using a function applied to each element and its index.
ifoldM :: (Monad m, Unbox b) => (a -> Int -> b -> m a) -> a -> Vector b -> m a
{-# INLINE ifoldM #-}
ifoldM = G.ifoldM

-- | /O(n)/ Monadic fold over non-empty vectors.
fold1M :: (Monad m, Unbox a) => (a -> a -> m a) -> Vector a -> m a
{-# INLINE fold1M #-}
fold1M = G.fold1M

-- | /O(n)/ Monadic fold with strict accumulator.
foldM' :: (Monad m, Unbox b) => (a -> b -> m a) -> a -> Vector b -> m a
{-# INLINE foldM' #-}
foldM' = G.foldM'

-- | /O(n)/ Monadic fold with strict accumulator using a function applied to each
-- element and its index.
ifoldM' :: (Monad m, Unbox b) => (a -> Int -> b -> m a) -> a -> Vector b -> m a
{-# INLINE ifoldM' #-}
ifoldM' = G.ifoldM'

-- | /O(n)/ Monadic fold over non-empty vectors with strict accumulator.
fold1M' :: (Monad m, Unbox a) => (a -> a -> m a) -> Vector a -> m a
{-# INLINE fold1M' #-}
fold1M' = G.fold1M'

-- | /O(n)/ Monadic fold that discards the result.
foldM_ :: (Monad m, Unbox b) => (a -> b -> m a) -> a -> Vector b -> m ()
{-# INLINE foldM_ #-}
foldM_ = G.foldM_

-- | /O(n)/ Monadic fold that discards the result using a function applied to
-- each element and its index.
ifoldM_ :: (Monad m, Unbox b) => (a -> Int -> b -> m a) -> a -> Vector b -> m ()
{-# INLINE ifoldM_ #-}
ifoldM_ = G.ifoldM_

-- | /O(n)/ Monadic fold over non-empty vectors that discards the result.
fold1M_ :: (Monad m, Unbox a) => (a -> a -> m a) -> Vector a -> m ()
{-# INLINE fold1M_ #-}
fold1M_ = G.fold1M_

-- | /O(n)/ Monadic fold with strict accumulator that discards the result.
foldM'_ :: (Monad m, Unbox b) => (a -> b -> m a) -> a -> Vector b -> m ()
{-# INLINE foldM'_ #-}
foldM'_ = G.foldM'_

-- | /O(n)/ Monadic fold with strict accumulator that discards the result
-- using a function applied to each element and its index.
ifoldM'_ :: (Monad m, Unbox b)
         => (a -> Int -> b -> m a) -> a -> Vector b -> m ()
{-# INLINE ifoldM'_ #-}
ifoldM'_ = G.ifoldM'_

-- | /O(n)/ Monadic fold over non-empty vectors with strict accumulator
-- that discards the result.
fold1M'_ :: (Monad m, Unbox a) => (a -> a -> m a) -> Vector a -> m ()
{-# INLINE fold1M'_ #-}
fold1M'_ = G.fold1M'_

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
-- >>> import qualified Data.Vector as VU
-- >>> VU.prescanl (+) 0 (VU.fromList [1,2,3,4 :: Int])
-- [0,1,3,6]
prescanl :: (Unbox a, Unbox b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE prescanl #-}
prescanl = G.prescanl

-- | /O(n)/ Left-to-right prescan with strict accumulator.
prescanl' :: (Unbox a, Unbox b) => (a -> b -> a) -> a -> Vector b -> Vector a
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
-- >>> import qualified Data.Vector.Unboxed as VU
-- >>> VU.postscanl (+) 0 (VU.fromList [1,2,3,4 :: Int])
-- [1,3,6,10]
postscanl :: (Unbox a, Unbox b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE postscanl #-}
postscanl = G.postscanl

-- | /O(n)/ Left-to-right postscan with strict accumulator.
postscanl' :: (Unbox a, Unbox b) => (a -> b -> a) -> a -> Vector b -> Vector a
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
-- >>> import qualified Data.Vector.Unboxed as VU
-- >>> VU.scanl (+) 0 (VU.fromList [1,2,3,4 :: Int])
-- [0,1,3,6,10]
scanl :: (Unbox a, Unbox b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE scanl #-}
scanl = G.scanl

-- | /O(n)/ Left-to-right scan with strict accumulator.
scanl' :: (Unbox a, Unbox b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE scanl' #-}
scanl' = G.scanl'

-- | /O(n)/ Left-to-right scan over a vector with its index.
--
-- @since 0.12.2.0
iscanl :: (Unbox a, Unbox b) => (Int -> a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE iscanl #-}
iscanl = G.iscanl

-- | /O(n)/ Left-to-right scan over a vector (strictly) with its index.
--
-- @since 0.12.2.0
iscanl' :: (Unbox a, Unbox b) => (Int -> a -> b -> a) -> a -> Vector b -> Vector a
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
-- >>> import qualified Data.Vector.Unboxed as VU
-- >>> VU.scanl1 min $ VU.fromListN 5 [4,2,4,1,3 :: Int]
-- [4,2,2,1,1]
-- >>> VU.scanl1 max $ VU.fromListN 5 [1,3,2,5,4 :: Int]
-- [1,3,3,5,5]
-- >>> VU.scanl1 min (VU.empty :: VU.Vector Int)
-- []
scanl1 :: Unbox a => (a -> a -> a) -> Vector a -> Vector a
{-# INLINE scanl1 #-}
scanl1 = G.scanl1

-- | /O(n)/ Initial-value free left-to-right scan over a vector with a strict accumulator.
--
-- Note: Since 0.13, application of this to an empty vector no longer
-- results in an error; instead it produces an empty vector.
--
-- ==== __Examples__
-- >>> import qualified Data.Vector.Unboxed as VU
-- >>> VU.scanl1' min $ VU.fromListN 5 [4,2,4,1,3 :: Int]
-- [4,2,2,1,1]
-- >>> VU.scanl1' max $ VU.fromListN 5 [1,3,2,5,4 :: Int]
-- [1,3,3,5,5]
-- >>> VU.scanl1' min (VU.empty :: VU.Vector Int)
-- []
scanl1' :: Unbox a => (a -> a -> a) -> Vector a -> Vector a
{-# INLINE scanl1' #-}
scanl1' = G.scanl1'

-- | /O(n)/ Right-to-left prescan.
--
-- @
-- prescanr f z = 'reverse' . 'prescanl' (flip f) z . 'reverse'
-- @
prescanr :: (Unbox a, Unbox b) => (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE prescanr #-}
prescanr = G.prescanr

-- | /O(n)/ Right-to-left prescan with strict accumulator.
prescanr' :: (Unbox a, Unbox b) => (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE prescanr' #-}
prescanr' = G.prescanr'

-- | /O(n)/ Right-to-left postscan.
postscanr :: (Unbox a, Unbox b) => (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE postscanr #-}
postscanr = G.postscanr

-- | /O(n)/ Right-to-left postscan with strict accumulator.
postscanr' :: (Unbox a, Unbox b) => (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE postscanr' #-}
postscanr' = G.postscanr'

-- | /O(n)/ Right-to-left scan.
scanr :: (Unbox a, Unbox b) => (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE scanr #-}
scanr = G.scanr

-- | /O(n)/ Right-to-left scan with strict accumulator.
scanr' :: (Unbox a, Unbox b) => (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE scanr' #-}
scanr' = G.scanr'

-- | /O(n)/ Right-to-left scan over a vector with its index.
--
-- @since 0.12.2.0
iscanr :: (Unbox a, Unbox b) => (Int -> a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE iscanr #-}
iscanr = G.iscanr

-- | /O(n)/ Right-to-left scan over a vector (strictly) with its index.
--
-- @sinqce 0.12.2.0
iscanr' :: (Unbox a, Unbox b) => (Int -> a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE iscanr' #-}
iscanr' = G.iscanr'

-- | /O(n)/ Right-to-left, initial-value free scan over a vector.
--
-- Note: Since 0.13, application of this to an empty vector no longer
-- results in an error; instead it produces an empty vector.
--
-- ==== __Examples__
-- >>> import qualified Data.Vector.Unboxed as VU
-- >>> VU.scanr1 min $ VU.fromListN 5 [3,1,4,2,4 :: Int]
-- [1,1,2,2,4]
-- >>> VU.scanr1 max $ VU.fromListN 5 [4,5,2,3,1 :: Int]
-- [5,5,3,3,1]
-- >>> VU.scanr1 min (VU.empty :: VU.Vector Int)
-- []
scanr1 :: Unbox a => (a -> a -> a) -> Vector a -> Vector a
{-# INLINE scanr1 #-}
scanr1 = G.scanr1

-- | /O(n)/ Right-to-left, initial-value free scan over a vector with a strict
-- accumulator.
--
-- Note: Since 0.13, application of this to an empty vector no longer
-- results in an error; instead it produces an empty vector.
--
-- ==== __Examples__
-- >>> import qualified Data.Vector.Unboxed as VU
-- >>> VU.scanr1' min $ VU.fromListN 5 [3,1,4,2,4 :: Int]
-- [1,1,2,2,4]
-- >>> VU.scanr1' max $ VU.fromListN 5 [4,5,2,3,1 :: Int]
-- [5,5,3,3,1]
-- >>> VU.scanr1' min (VU.empty :: VU.Vector Int)
-- []
scanr1' :: Unbox a => (a -> a -> a) -> Vector a -> Vector a
{-# INLINE scanr1' #-}
scanr1' = G.scanr1'

-- Comparisons
-- ------------------------

-- | /O(n)/ Check if two vectors are equal using the supplied equality
-- predicate.
--
-- @since 0.12.2.0
eqBy :: (Unbox a, Unbox b) => (a -> b -> Bool) -> Vector a -> Vector b -> Bool
{-# INLINE eqBy #-}
eqBy = G.eqBy

-- | /O(n)/ Compare two vectors using the supplied comparison function for
-- vector elements. Comparison works the same as for lists.
--
-- > cmpBy compare == compare
--
-- @since 0.12.2.0
cmpBy :: (Unbox a, Unbox b) => (a -> b -> Ordering) -> Vector a -> Vector b -> Ordering
cmpBy = G.cmpBy

-- Conversions - Lists
-- ------------------------

-- | /O(n)/ Convert a vector to a list.
toList :: Unbox a => Vector a -> [a]
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
-- @since 0.3
fromList :: Unbox a => [a] -> Vector a
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
-- ==== __Examples__
--
-- >>> import qualified Data.Vector.Unboxed as VU
-- >>> VU.fromListN 3 [1,2,3,4,5 :: Int]
-- [1,2,3]
-- >>> VU.fromListN 3 [1 :: Int]
-- [1]
fromListN :: Unbox a => Int -> [a] -> Vector a
{-# INLINE fromListN #-}
fromListN = G.fromListN

-- Conversions - Mutable vectors
-- -----------------------------

-- | /O(1)/ Unsafely convert a mutable vector to an immutable one without
-- copying. The mutable vector may not be used after this operation.
unsafeFreeze :: (Unbox a, PrimMonad m) => MVector (PrimState m) a -> m (Vector a)
{-# INLINE unsafeFreeze #-}
unsafeFreeze = G.unsafeFreeze

-- | /O(n)/ Yield an immutable copy of the mutable vector.
freeze :: (Unbox a, PrimMonad m) => MVector (PrimState m) a -> m (Vector a)
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
unsafeThaw :: (Unbox a, PrimMonad m) => Vector a -> m (MVector (PrimState m) a)
{-# INLINE unsafeThaw #-}
unsafeThaw = G.unsafeThaw

-- | /O(n)/ Yield a mutable copy of an immutable vector.
thaw :: (Unbox a, PrimMonad m) => Vector a -> m (MVector (PrimState m) a)
{-# INLINE thaw #-}
thaw = G.thaw

-- | /O(n)/ Copy an immutable vector into a mutable one. The two vectors must
-- have the same length. This is not checked.
unsafeCopy
  :: (Unbox a, PrimMonad m) => MVector (PrimState m) a -> Vector a -> m ()
{-# INLINE unsafeCopy #-}
unsafeCopy = G.unsafeCopy

-- | /O(n)/ Copy an immutable vector into a mutable one. The two vectors must
-- have the same length.
copy :: (Unbox a, PrimMonad m) => MVector (PrimState m) a -> Vector a -> m ()
{-# INLINE copy #-}
copy = G.copy


#define DEFINE_IMMUTABLE
#include "unbox-tuple-instances"

-- $setup
-- >>> import Prelude (Bool(True, False), ($), (+), min, max, even, fst, pred, id, succ, undefined, Ord(..))
