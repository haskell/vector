{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Vector.Strict.Mutable
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
-- Mutable strict boxed vectors. Strict means that all writes to
-- vector are evaluated to WHNF. However vector may contain bottoms,
-- since all elements of vector allocated using 'new' or 'unsafeNew'
-- are set to ⊥.
module Data.Vector.Strict.Mutable (
  -- * Mutable boxed vectors
  MVector(MVector), IOVector, STVector,

  -- * Accessors

  -- ** Length information
  length, null,

  -- ** Extracting subvectors
  slice, init, tail, take, drop, splitAt,
  unsafeSlice, unsafeInit, unsafeTail, unsafeTake, unsafeDrop,

  -- ** Overlapping
  overlaps,

  -- * Construction

  -- ** Initialisation
  new, unsafeNew, replicate, replicateM, generate, generateM, clone,

  -- ** Growing
  grow, unsafeGrow,

  -- ** Restricting memory usage
  clear,

  -- * Accessing individual elements
  read, readMaybe, write, modify, modifyM, swap, exchange,
  unsafeRead, unsafeWrite, unsafeModify, unsafeModifyM, unsafeSwap, unsafeExchange,

  -- * Folds
  mapM_, imapM_, forM_, iforM_,
  foldl, foldl', foldM, foldM',
  foldr, foldr', foldrM, foldrM',
  ifoldl, ifoldl', ifoldM, ifoldM',
  ifoldr, ifoldr', ifoldrM, ifoldrM',

  -- * Modifying vectors
  nextPermutation, nextPermutationBy,
  prevPermutation, prevPermutationBy,

  -- ** Filling and copying
  set, copy, move, unsafeCopy, unsafeMove,
  -- ** Lazy arrays
  toLazy, fromLazy,
  -- ** Arrays
  fromMutableArray, toMutableArray,

  -- * Re-exports
  PrimMonad, PrimState, RealWorld
) where

import           Data.Coerce
import qualified Data.Vector.Generic.Mutable as G
import qualified Data.Vector.Mutable as MV
import           Data.Primitive.Array
import           Control.Monad.Primitive

import Prelude
  ( Ord, Monad(..), Bool, Int, Maybe, Ordering(..)
  , return, ($), (<$>) )

import Data.Typeable ( Typeable )

#include "vector.h"

type role MVector nominal representational

-- | Mutable boxed vectors keyed on the monad they live in ('IO' or @'ST' s@).
newtype MVector s a = MVector (MV.MVector s a)
        deriving ( Typeable )

type IOVector = MVector RealWorld
type STVector s = MVector s

instance G.MVector MVector a where
  {-# INLINE basicLength #-}
  basicLength = coerce (G.basicLength @MV.MVector @a)
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice = coerce (G.basicUnsafeSlice @MV.MVector @a)
  {-# INLINE basicOverlaps #-}
  basicOverlaps = coerce (G.basicOverlaps @MV.MVector @a)
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew = coerce (G.basicUnsafeNew @MV.MVector @a)
  {-# INLINE basicInitialize #-}
  -- initialization is unnecessary for boxed vectors
  basicInitialize _ = return ()
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeReplicate n !x = coerce (G.basicUnsafeReplicate @MV.MVector @a) n x
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead = coerce (G.basicUnsafeRead @MV.MVector @a)
  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite vec j !x = (coerce (G.basicUnsafeWrite @MV.MVector @a)) vec j x

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy = coerce (G.basicUnsafeCopy @MV.MVector @a)

  {-# INLINE basicUnsafeMove #-}
  basicUnsafeMove = coerce (G.basicUnsafeMove @MV.MVector @a)
  {-# INLINE basicClear #-}
  basicClear = coerce (G.basicClear @MV.MVector @a)


-- Length information
-- ------------------

-- | Length of the mutable vector.
--
-- @since 0.13.2.0
length :: MVector s a -> Int
{-# INLINE length #-}
length = G.length

-- | Check whether the vector is empty.
--
-- @since 0.13.2.0
null :: MVector s a -> Bool
{-# INLINE null #-}
null = G.null

-- Extracting subvectors
-- ---------------------

-- | Yield a part of the mutable vector without copying it. The vector must
-- contain at least @i+n@ elements.
--
-- @since 0.13.2.0
slice :: Int  -- ^ @i@ starting index
      -> Int  -- ^ @n@ length
      -> MVector s a
      -> MVector s a
{-# INLINE slice #-}
slice = G.slice

-- | Take the @n@ first elements of the mutable vector without making a
-- copy. For negative @n@, the empty vector is returned. If @n@ is larger
-- than the vector's length, the vector is returned unchanged.
--
-- @since 0.13.2.0
take :: Int -> MVector s a -> MVector s a
{-# INLINE take #-}
take = G.take

-- | Drop the @n@ first element of the mutable vector without making a
-- copy. For negative @n@, the vector is returned unchanged. If @n@ is
-- larger than the vector's length, the empty vector is returned.
--
-- @since 0.13.2.0
drop :: Int -> MVector s a -> MVector s a
{-# INLINE drop #-}
drop = G.drop

-- | /O(1)/ Split the mutable vector into the first @n@ elements
-- and the remainder, without copying.
--
-- Note that @'splitAt' n v@ is equivalent to @('take' n v, 'drop' n v)@,
-- but slightly more efficient.
--
-- @since 0.13.2.0
splitAt :: Int -> MVector s a -> (MVector s a, MVector s a)
{-# INLINE splitAt #-}
splitAt = G.splitAt

-- | Drop the last element of the mutable vector without making a copy.
-- If the vector is empty, an exception is thrown.
--
-- @since 0.13.2.0
init :: MVector s a -> MVector s a
{-# INLINE init #-}
init = G.init

-- | Drop the first element of the mutable vector without making a copy.
-- If the vector is empty, an exception is thrown.
--
-- @since 0.13.2.0
tail :: MVector s a -> MVector s a
{-# INLINE tail #-}
tail = G.tail

-- | Yield a part of the mutable vector without copying it. No bounds checks
-- are performed.
--
-- @since 0.13.2.0
unsafeSlice :: Int  -- ^ starting index
            -> Int  -- ^ length of the slice
            -> MVector s a
            -> MVector s a
{-# INLINE unsafeSlice #-}
unsafeSlice = G.unsafeSlice

-- | Unsafe variant of 'take'. If @n@ is out of range, it will
-- simply create an invalid slice that likely violate memory safety.
--
-- @since 0.13.2.0
unsafeTake :: Int -> MVector s a -> MVector s a
{-# INLINE unsafeTake #-}
unsafeTake = G.unsafeTake

-- | Unsafe variant of 'drop'. If @n@ is out of range, it will
-- simply create an invalid slice that likely violate memory safety.
--
-- @since 0.13.2.0
unsafeDrop :: Int -> MVector s a -> MVector s a
{-# INLINE unsafeDrop #-}
unsafeDrop = G.unsafeDrop

-- | Same as 'init', but doesn't do range checks.
--
-- @since 0.13.2.0
unsafeInit :: MVector s a -> MVector s a
{-# INLINE unsafeInit #-}
unsafeInit = G.unsafeInit

-- | Same as 'tail', but doesn't do range checks.
--
-- @since 0.13.2.0
unsafeTail :: MVector s a -> MVector s a
{-# INLINE unsafeTail #-}
unsafeTail = G.unsafeTail

-- Overlapping
-- -----------

-- | Check whether two vectors overlap.
--
-- @since 0.13.2.0
overlaps :: MVector s a -> MVector s a -> Bool
{-# INLINE overlaps #-}
overlaps = G.overlaps

-- Initialisation
-- --------------

-- | Create a mutable vector of the given length.
--
-- @since 0.13.2.0
new :: PrimMonad m => Int -> m (MVector (PrimState m) a)
{-# INLINE new #-}
new = G.new

-- | Create a mutable vector of the given length. The vector elements
-- are set to bottom, so accessing them will cause an exception.
--
-- @since 0.13.2.0
unsafeNew :: PrimMonad m => Int -> m (MVector (PrimState m) a)
{-# INLINE unsafeNew #-}
unsafeNew = G.unsafeNew

-- | Create a mutable vector of the given length (0 if the length is negative)
-- and fill it with an initial value.
--
-- @since 0.13.2.0
replicate :: PrimMonad m => Int -> a -> m (MVector (PrimState m) a)
{-# INLINE replicate #-}
replicate = G.replicate

-- | Create a mutable vector of the given length (0 if the length is negative)
-- and fill it with values produced by repeatedly executing the monadic action.
--
-- @since 0.13.2.0
replicateM :: PrimMonad m => Int -> m a -> m (MVector (PrimState m) a)
{-# INLINE replicateM #-}
replicateM = G.replicateM

-- | /O(n)/ Create a mutable vector of the given length (0 if the length is negative)
-- and fill it with the results of applying the function to each index.
-- Iteration starts at index 0.
--
-- @since 0.13.2.0
generate :: (PrimMonad m) => Int -> (Int -> a) -> m (MVector (PrimState m) a)
{-# INLINE generate #-}
generate = G.generate

-- | /O(n)/ Create a mutable vector of the given length (0 if the length is
-- negative) and fill it with the results of applying the monadic function to each
-- index. Iteration starts at index 0.
--
-- @since 0.13.2.0
generateM :: (PrimMonad m) => Int -> (Int -> m a) -> m (MVector (PrimState m) a)
{-# INLINE generateM #-}
generateM = G.generateM

-- | Create a copy of a mutable vector.
--
-- @since 0.13.2.0
clone :: PrimMonad m => MVector (PrimState m) a -> m (MVector (PrimState m) a)
{-# INLINE clone #-}
clone = G.clone

-- Growing
-- -------

-- | Grow a boxed vector by the given number of elements. The number must be
-- non-negative. This has the same semantics as 'G.grow' for generic vectors. It differs
-- from @grow@ functions for unpacked vectors, however, in that only pointers to
-- values are copied over, therefore the values themselves will be shared between the
-- two vectors. This is an important distinction to know about during memory
-- usage analysis and in case the values themselves are of a mutable type, e.g.
-- 'Data.IORef.IORef' or another mutable vector.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> import qualified Data.Vector.Mutable as MV
-- >>> mv <- V.thaw $ V.fromList ([10, 20, 30] :: [Integer])
-- >>> mv' <- MV.grow mv 2
--
-- The two extra elements at the end of the newly allocated vector will be
-- uninitialized and will result in an error if evaluated, so me must overwrite
-- them with new values first:
--
-- >>> MV.write mv' 3 999
-- >>> MV.write mv' 4 777
-- >>> V.freeze mv'
-- [10,20,30,999,777]
--
-- It is important to note that the source mutable vector is not affected when
-- the newly allocated one is mutated.
--
-- >>> MV.write mv' 2 888
-- >>> V.freeze mv'
-- [10,20,888,999,777]
-- >>> V.freeze mv
-- [10,20,30]
--
-- @since 0.13.2.0
grow :: PrimMonad m
     => MVector (PrimState m) a -> Int -> m (MVector (PrimState m) a)
{-# INLINE grow #-}
grow = G.grow

-- | Grow a vector by the given number of elements. The number must be non-negative, but
-- this is not checked. This has the same semantics as 'G.unsafeGrow' for generic vectors.
--
-- @since 0.13.2.0
unsafeGrow :: PrimMonad m
           => MVector (PrimState m) a -> Int -> m (MVector (PrimState m) a)
{-# INLINE unsafeGrow #-}
unsafeGrow = G.unsafeGrow

-- Restricting memory usage
-- ------------------------

-- | Reset all elements of the vector to some undefined value, clearing all
-- references to external objects.
--
-- @since 0.13.2.0
clear :: PrimMonad m => MVector (PrimState m) a -> m ()
{-# INLINE clear #-}
clear = G.clear

-- Accessing individual elements
-- -----------------------------

-- | Yield the element at the given position. Will throw an exception if
-- the index is out of range.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector.Mutable as MV
-- >>> v <- MV.generate 10 (\x -> x*x)
-- >>> MV.read v 3
-- 9
--
-- @since 0.13.2.0
read :: PrimMonad m => MVector (PrimState m) a -> Int -> m a
{-# INLINE read #-}
read = G.read

-- | Yield the element at the given position. Returns 'Nothing' if
-- the index is out of range.
--
-- @since 0.13.2.0
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector.Mutable as MV
-- >>> v <- MV.generate 10 (\x -> x*x)
-- >>> MV.readMaybe v 3
-- Just 9
-- >>> MV.readMaybe v 13
-- Nothing
--
-- @since 0.13.2.0
readMaybe :: (PrimMonad m) => MVector (PrimState m) a -> Int -> m (Maybe a)
{-# INLINE readMaybe #-}
readMaybe = G.readMaybe

-- | Replace the element at the given position.
--
-- @since 0.13.2.0
write :: PrimMonad m => MVector (PrimState m) a -> Int -> a -> m ()
{-# INLINE write #-}
write = G.write

-- | Modify the element at the given position.
--
-- @since 0.13.2.0
modify :: PrimMonad m => MVector (PrimState m) a -> (a -> a) -> Int -> m ()
{-# INLINE modify #-}
modify = G.modify

-- | Modify the element at the given position using a monadic function.
--
-- @since 0.13.2.0
modifyM :: (PrimMonad m) => MVector (PrimState m) a -> (a -> m a) -> Int -> m ()
{-# INLINE modifyM #-}
modifyM = G.modifyM

-- | Swap the elements at the given positions.
--
-- @since 0.13.2.0
swap :: PrimMonad m => MVector (PrimState m) a -> Int -> Int -> m ()
{-# INLINE swap #-}
swap = G.swap

-- | Replace the element at the given position and return the old element.
--
-- @since 0.13.2.0
exchange :: (PrimMonad m) => MVector (PrimState m) a -> Int -> a -> m a
{-# INLINE exchange #-}
exchange = G.exchange

-- | Yield the element at the given position. No bounds checks are performed.
--
-- @since 0.13.2.0
unsafeRead :: PrimMonad m => MVector (PrimState m) a -> Int -> m a
{-# INLINE unsafeRead #-}
unsafeRead = G.unsafeRead

-- | Replace the element at the given position. No bounds checks are performed.
--
-- @since 0.13.2.0
unsafeWrite :: PrimMonad m => MVector (PrimState m) a -> Int -> a -> m ()
{-# INLINE unsafeWrite #-}
unsafeWrite = G.unsafeWrite

-- | Modify the element at the given position. No bounds checks are performed.
--
-- @since 0.13.2.0
unsafeModify :: PrimMonad m => MVector (PrimState m) a -> (a -> a) -> Int -> m ()
{-# INLINE unsafeModify #-}
unsafeModify = G.unsafeModify

-- | Modify the element at the given position using a monadic
-- function. No bounds checks are performed.
--
-- @since 0.13.2.0
unsafeModifyM :: (PrimMonad m) => MVector (PrimState m) a -> (a -> m a) -> Int -> m ()
{-# INLINE unsafeModifyM #-}
unsafeModifyM = G.unsafeModifyM

-- | Swap the elements at the given positions. No bounds checks are performed.
--
-- @since 0.13.2.0
unsafeSwap :: PrimMonad m => MVector (PrimState m) a -> Int -> Int -> m ()
{-# INLINE unsafeSwap #-}
unsafeSwap = G.unsafeSwap

-- | Replace the element at the given position and return the old element. No
-- bounds checks are performed.
--
-- @since 0.13.2.0
unsafeExchange :: (PrimMonad m) => MVector (PrimState m) a -> Int -> a -> m a
{-# INLINE unsafeExchange #-}
unsafeExchange = G.unsafeExchange

-- Filling and copying
-- -------------------

-- | Set all elements of the vector to the given value.
--
-- @since 0.13.2.0
set :: PrimMonad m => MVector (PrimState m) a -> a -> m ()
{-# INLINE set #-}
set = G.set

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap.
--
-- @since 0.13.2.0
copy :: PrimMonad m => MVector (PrimState m) a   -- ^ target
                    -> MVector (PrimState m) a   -- ^ source
                    -> m ()
{-# INLINE copy #-}
copy = G.copy

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap, but this is not checked.
--
-- @since 0.13.2.0
unsafeCopy :: PrimMonad m => MVector (PrimState m) a   -- ^ target
                          -> MVector (PrimState m) a   -- ^ source
                          -> m ()
{-# INLINE unsafeCopy #-}
unsafeCopy = G.unsafeCopy

-- | Move the contents of a vector. The two vectors must have the same
-- length.
--
-- If the vectors do not overlap, then this is equivalent to 'copy'.
-- Otherwise, the copying is performed as if the source vector were
-- copied to a temporary vector and then the temporary vector was copied
-- to the target vector.
--
-- @since 0.13.2.0
move :: PrimMonad m => MVector (PrimState m) a   -- ^ target
                    -> MVector (PrimState m) a   -- ^ source
                    -> m ()
{-# INLINE move #-}
move = G.move

-- | Move the contents of a vector. The two vectors must have the same
-- length, but this is not checked.
--
-- If the vectors do not overlap, then this is equivalent to 'unsafeCopy'.
-- Otherwise, the copying is performed as if the source vector were
-- copied to a temporary vector and then the temporary vector was copied
-- to the target vector.
--
-- @since 0.13.2.0
unsafeMove :: PrimMonad m => MVector (PrimState m) a   -- ^ target
                          -> MVector (PrimState m) a   -- ^ source
                          -> m ()
{-# INLINE unsafeMove #-}
unsafeMove = G.unsafeMove

-- Modifying vectors
-- -----------------

-- | Compute the (lexicographically) next permutation of the given vector in-place.
-- Returns False when the input is the last item in the enumeration, i.e., if it is in
-- weakly descending order. In this case the vector will not get updated,
-- as opposed to the behavior of the C++ function @std::next_permutation@.
--
-- @since 0.13.2.0
nextPermutation :: (PrimMonad m, Ord e) => MVector (PrimState m) e -> m Bool
{-# INLINE nextPermutation #-}
nextPermutation = G.nextPermutation

-- | Compute the (lexicographically) next permutation of the given vector in-place,
-- using the provided comparison function.
-- Returns False when the input is the last item in the enumeration, i.e., if it is in
-- weakly descending order. In this case the vector will not get updated,
-- as opposed to the behavior of the C++ function @std::next_permutation@.
--
-- @since 0.13.2.0
nextPermutationBy :: PrimMonad m => (e -> e -> Ordering) -> MVector (PrimState m) e -> m Bool
{-# INLINE nextPermutationBy #-}
nextPermutationBy = G.nextPermutationBy

-- | Compute the (lexicographically) previous permutation of the given vector in-place.
-- Returns False when the input is the last item in the enumeration, i.e., if it is in
-- weakly ascending order. In this case the vector will not get updated,
-- as opposed to the behavior of the C++ function @std::prev_permutation@.
--
-- @since 0.13.2.0
prevPermutation :: (PrimMonad m, Ord e) => MVector (PrimState m) e -> m Bool
{-# INLINE prevPermutation #-}
prevPermutation = G.prevPermutation

-- | Compute the (lexicographically) previous permutation of the given vector in-place,
-- using the provided comparison function.
-- Returns False when the input is the last item in the enumeration, i.e., if it is in
-- weakly ascending order. In this case the vector will not get updated,
-- as opposed to the behavior of the C++ function @std::prev_permutation@.
--
-- @since 0.13.2.0
prevPermutationBy :: PrimMonad m => (e -> e -> Ordering) -> MVector (PrimState m) e -> m Bool
{-# INLINE prevPermutationBy #-}
prevPermutationBy = G.prevPermutationBy


-- Folds
-- -----

-- | /O(n)/ Apply the monadic action to every element of the vector, discarding the results.
--
-- @since 0.13.2.0
mapM_ :: (PrimMonad m) => (a -> m b) -> MVector (PrimState m) a -> m ()
{-# INLINE mapM_ #-}
mapM_ = G.mapM_

-- | /O(n)/ Apply the monadic action to every element of the vector and its index, discarding the results.
--
-- @since 0.13.2.0
imapM_ :: (PrimMonad m) => (Int -> a -> m b) -> MVector (PrimState m) a -> m ()
{-# INLINE imapM_ #-}
imapM_ = G.imapM_

-- | /O(n)/ Apply the monadic action to every element of the vector,
-- discarding the results. It's the same as @flip mapM_@.
--
-- @since 0.13.2.0
forM_ :: (PrimMonad m) => MVector (PrimState m) a -> (a -> m b) -> m ()
{-# INLINE forM_ #-}
forM_ = G.forM_

-- | /O(n)/ Apply the monadic action to every element of the vector
-- and its index, discarding the results. It's the same as @flip imapM_@.
--
-- @since 0.13.2.0
iforM_ :: (PrimMonad m) => MVector (PrimState m) a -> (Int -> a -> m b) -> m ()
{-# INLINE iforM_ #-}
iforM_ = G.iforM_

-- | /O(n)/ Pure left fold.
--
-- @since 0.13.2.0
foldl :: (PrimMonad m) => (b -> a -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldl #-}
foldl = G.foldl

-- | /O(n)/ Pure left fold with strict accumulator.
--
-- @since 0.13.2.0
foldl' :: (PrimMonad m) => (b -> a -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldl' #-}
foldl' = G.foldl'

-- | /O(n)/ Pure left fold using a function applied to each element and its index.
--
-- @since 0.13.2.0
ifoldl :: (PrimMonad m) => (b -> Int -> a -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldl #-}
ifoldl = G.ifoldl

-- | /O(n)/ Pure left fold with strict accumulator using a function applied to each element and its index.
--
-- @since 0.13.2.0
ifoldl' :: (PrimMonad m) => (b -> Int -> a -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldl' #-}
ifoldl' = G.ifoldl'

-- | /O(n)/ Pure right fold.
--
-- @since 0.13.2.0
foldr :: (PrimMonad m) => (a -> b -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldr #-}
foldr = G.foldr

-- | /O(n)/ Pure right fold with strict accumulator.
--
-- @since 0.13.2.0
foldr' :: (PrimMonad m) => (a -> b -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldr' #-}
foldr' = G.foldr'

-- | /O(n)/ Pure right fold using a function applied to each element and its index.
--
-- @since 0.13.2.0
ifoldr :: (PrimMonad m) => (Int -> a -> b -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldr #-}
ifoldr = G.ifoldr

-- | /O(n)/ Pure right fold with strict accumulator using a function applied
-- to each element and its index.
--
-- @since 0.13.2.0
ifoldr' :: (PrimMonad m) => (Int -> a -> b -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldr' #-}
ifoldr' = G.ifoldr'

-- | /O(n)/ Monadic fold.
--
-- @since 0.13.2.0
foldM :: (PrimMonad m) => (b -> a -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldM #-}
foldM = G.foldM

-- | /O(n)/ Monadic fold with strict accumulator.
--
-- @since 0.13.2.0
foldM' :: (PrimMonad m) => (b -> a -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldM' #-}
foldM' = G.foldM'

-- | /O(n)/ Monadic fold using a function applied to each element and its index.
--
-- @since 0.13.2.0
ifoldM :: (PrimMonad m) => (b -> Int -> a -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldM #-}
ifoldM = G.ifoldM

-- | /O(n)/ Monadic fold with strict accumulator using a function applied to each element and its index.
--
-- @since 0.13.2.0
ifoldM' :: (PrimMonad m) => (b -> Int -> a -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldM' #-}
ifoldM' = G.ifoldM'

-- | /O(n)/ Monadic right fold.
--
-- @since 0.13.2.0
foldrM :: (PrimMonad m) => (a -> b -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldrM #-}
foldrM = G.foldrM

-- | /O(n)/ Monadic right fold with strict accumulator.
--
-- @since 0.13.2.0
foldrM' :: (PrimMonad m) => (a -> b -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldrM' #-}
foldrM' = G.foldrM'

-- | /O(n)/ Monadic right fold using a function applied to each element and its index.
--
-- @since 0.13.2.0
ifoldrM :: (PrimMonad m) => (Int -> a -> b -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldrM #-}
ifoldrM = G.ifoldrM

-- | /O(n)/ Monadic right fold with strict accumulator using a function applied
-- to each element and its index.
--
-- @since 0.13.2.0
ifoldrM' :: (PrimMonad m) => (Int -> a -> b -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldrM' #-}
ifoldrM' = G.ifoldrM'

-- Conversions - Lazy vectors
-- -----------------------------

-- | /O(1)/ Convert strict mutable vector to lazy mutable
-- vector. Vectors will share mutable buffer
toLazy :: MVector s a -> MV.MVector s a
{-# INLINE toLazy #-}
toLazy (MVector vec) = vec

-- | /O(n)/ Convert lazy mutable vector to strict mutable
-- vector. Vectors will share mutable buffer. This function evaluates
-- vector elements to WHNF.
fromLazy :: PrimMonad m => MV.MVector (PrimState m) a -> m (MVector (PrimState m) a)
fromLazy mvec = stToPrim $ do
  G.foldM' (\_ !_ -> return ()) () mvec
  return $ MVector mvec


-- Conversions - Arrays
-- -----------------------------

-- | /O(n)/ Make a copy of a mutable array to a new mutable
-- vector. All elements of a vector are evaluated to WHNF
--
-- @since 0.13.2.0
fromMutableArray :: PrimMonad m => MutableArray (PrimState m) a -> m (MVector (PrimState m) a)
{-# INLINE fromMutableArray #-}
fromMutableArray marr = stToPrim $ do
  mvec <- MVector <$> MV.fromMutableArray marr
  foldM' (\_ !_ -> return ()) () mvec
  return mvec

-- | /O(n)/ Make a copy of a mutable vector into a new mutable array.
--
-- @since 0.13.2.0
toMutableArray :: PrimMonad m => MVector (PrimState m) a -> m (MutableArray (PrimState m) a)
{-# INLINE toMutableArray #-}
toMutableArray (MVector v) = MV.toMutableArray v

-- $setup
-- >>> import Prelude (Integer,Num(..))
