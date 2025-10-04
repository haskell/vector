{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
-- |
-- Module      : Data.Vector.Storable.Mutable
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
-- Mutable vectors based on Storable.

module Data.Vector.Storable.Mutable(
  -- * Mutable vectors of 'Storable' types
  MVector, IOVector, STVector,
  pattern MVector,

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
  mapInPlace, imapInPlace, mapInPlaceM, imapInPlaceM,
  nextPermutation, nextPermutationBy,
  prevPermutation, prevPermutationBy,

  -- ** Filling and copying
  set, copy, move, unsafeCopy, unsafeMove,

  -- * Unsafe conversions
  unsafeCast,
  unsafeCoerceMVector,

  -- * Raw pointers
  unsafeFromForeignPtr, unsafeFromForeignPtr0,
  unsafeToForeignPtr,   unsafeToForeignPtr0,
  unsafeWith,
  -- * Re-exports
  Storable, PrimMonad, PrimState, RealWorld
) where

import qualified Data.Vector.Generic.Mutable as G
import Data.Vector.Storable.Mutable.Unsafe(
  MVector,IOVector,STVector,unsafeCast,unsafeWith,unsafeCoerceMVector,
  unsafeToForeignPtr,unsafeToForeignPtr0,unsafeFromForeignPtr,unsafeFromForeignPtr0)
import qualified Data.Vector.Storable.Mutable.Unsafe as U
import Foreign.Storable

import Control.Monad.Primitive
import Foreign.ForeignPtr (ForeignPtr)

import Prelude (Int, Ord, Bool, Maybe, Ordering(..) )

#include "vector.h"


pattern MVector :: Int -> ForeignPtr a -> MVector s a
pattern MVector i ptr = U.MVector i ptr
{-# COMPLETE MVector #-}
{-# DEPRECATED MVector "Use constructor exported from Data.Vector.Strict.Mutable.Unsafe" #-}


-- Length information
-- ------------------

-- | Length of the mutable vector.
length :: Storable a => MVector s a -> Int
{-# INLINE length #-}
length = G.length

-- | Check whether the vector is empty.
null :: Storable a => MVector s a -> Bool
{-# INLINE null #-}
null = G.null

-- Extracting subvectors
-- ---------------------

-- | Yield a part of the mutable vector without copying it. The vector must
-- contain at least @i+n@ elements.
slice :: Storable a
      => Int  -- ^ @i@ starting index
      -> Int  -- ^ @n@ length
      -> MVector s a
      -> MVector s a
{-# INLINE slice #-}
slice = G.slice

-- | Take the @n@ first elements of the mutable vector without making a
-- copy. For negative @n@, the empty vector is returned. If @n@ is larger
-- than the vector's length, the vector is returned unchanged.
take :: Storable a => Int -> MVector s a -> MVector s a
{-# INLINE take #-}
take = G.take

-- | Drop the @n@ first element of the mutable vector without making a
-- copy. For negative @n@, the vector is returned unchanged. If @n@ is
-- larger than the vector's length, the empty vector is returned.
drop :: Storable a => Int -> MVector s a -> MVector s a
{-# INLINE drop #-}
drop = G.drop

-- | /O(1)/ Split the mutable vector into the first @n@ elements
-- and the remainder, without copying.
--
-- Note that @'splitAt' n v@ is equivalent to @('take' n v, 'drop' n v)@,
-- but slightly more efficient.
splitAt :: Storable a => Int -> MVector s a -> (MVector s a, MVector s a)
{-# INLINE splitAt #-}
splitAt = G.splitAt

-- | Drop the last element of the mutable vector without making a copy.
-- If the vector is empty, an exception is thrown.
init :: Storable a => MVector s a -> MVector s a
{-# INLINE init #-}
init = G.init

-- | Drop the first element of the mutable vector without making a copy.
-- If the vector is empty, an exception is thrown.
tail :: Storable a => MVector s a -> MVector s a
{-# INLINE tail #-}
tail = G.tail

-- | Yield a part of the mutable vector without copying it. No bounds checks
-- are performed.
unsafeSlice :: Storable a
            => Int  -- ^ starting index
            -> Int  -- ^ length of the slice
            -> MVector s a
            -> MVector s a
{-# INLINE unsafeSlice #-}
unsafeSlice = G.unsafeSlice

-- | Unsafe variant of 'take'. If @n@ is out of range, it will
-- simply create an invalid slice that likely violate memory safety.
unsafeTake :: Storable a => Int -> MVector s a -> MVector s a
{-# INLINE unsafeTake #-}
unsafeTake = G.unsafeTake

-- | Unsafe variant of 'drop'. If @n@ is out of range, it will
-- simply create an invalid slice that likely violate memory safety.
unsafeDrop :: Storable a => Int -> MVector s a -> MVector s a
{-# INLINE unsafeDrop #-}
unsafeDrop = G.unsafeDrop

-- | Same as 'init', but doesn't do range checks.
unsafeInit :: Storable a => MVector s a -> MVector s a
{-# INLINE unsafeInit #-}
unsafeInit = G.unsafeInit

-- | Same as 'tail', but doesn't do range checks.
unsafeTail :: Storable a => MVector s a -> MVector s a
{-# INLINE unsafeTail #-}
unsafeTail = G.unsafeTail

-- Overlapping
-- -----------

-- | Check whether two vectors overlap.
overlaps :: Storable a => MVector s a -> MVector s a -> Bool
{-# INLINE overlaps #-}
overlaps = G.overlaps

-- Initialisation
-- --------------

-- | Create a mutable vector of the given length.
new :: (PrimMonad m, Storable a) => Int -> m (MVector (PrimState m) a)
{-# INLINE new #-}
new = G.new

-- | Create a mutable vector of the given length. The vector content
-- is uninitialized, which means it is filled with whatever the
-- underlying memory buffer happens to contain.
--
-- @since 0.5
unsafeNew :: (PrimMonad m, Storable a) => Int -> m (MVector (PrimState m) a)
{-# INLINE unsafeNew #-}
unsafeNew = G.unsafeNew

-- | Create a mutable vector of the given length (0 if the length is negative)
-- and fill it with an initial value.
replicate :: (PrimMonad m, Storable a) => Int -> a -> m (MVector (PrimState m) a)
{-# INLINE replicate #-}
replicate = G.replicate

-- | Create a mutable vector of the given length (0 if the length is negative)
-- and fill it with values produced by repeatedly executing the monadic action.
replicateM :: (PrimMonad m, Storable a) => Int -> m a -> m (MVector (PrimState m) a)
{-# INLINE replicateM #-}
replicateM = G.replicateM

-- | /O(n)/ Create a mutable vector of the given length (0 if the length is negative)
-- and fill it with the results of applying the function to each index.
-- Iteration starts at index 0.
--
-- @since 0.12.3.0
generate :: (PrimMonad m, Storable a) => Int -> (Int -> a) -> m (MVector (PrimState m) a)
{-# INLINE generate #-}
generate = G.generate

-- | /O(n)/ Create a mutable vector of the given length (0 if the length is
-- negative) and fill it with the results of applying the monadic function to each
-- index. Iteration starts at index 0.
--
-- @since 0.12.3.0
generateM :: (PrimMonad m, Storable a) => Int -> (Int -> m a) -> m (MVector (PrimState m) a)
{-# INLINE generateM #-}
generateM = G.generateM

-- | Create a copy of a mutable vector.
clone :: (PrimMonad m, Storable a)
      => MVector (PrimState m) a -> m (MVector (PrimState m) a)
{-# INLINE clone #-}
clone = G.clone

-- Growing
-- -------

-- | Grow a storable vector by the given number of elements. The number must be
-- non-negative. This has the same semantics as 'G.grow' for generic vectors.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector.Storable as VS
-- >>> import qualified Data.Vector.Storable.Mutable as MVS
-- >>> mv <- VS.thaw $ VS.fromList ([10, 20, 30] :: [Int])
-- >>> mv' <- MVS.grow mv 2
--
-- Extra memory at the end of the newly allocated vector is initialized to 0
-- bytes, which for 'Storable' instances will usually correspond to some default
-- value for a particular type, e.g. @0@ for @Int@, @False@ for @Bool@,
-- etc. However, if 'unsafeGrow' was used instead, this would not have been
-- guaranteed and some garbage would be there instead.
--
-- >>> VS.freeze mv'
-- [10,20,30,0,0]
--
-- Having the extra space we can write new values in there:
--
-- >>> MVS.write mv' 3 999
-- >>> VS.freeze mv'
-- [10,20,30,999,0]
--
-- It is important to note that the source mutable vector is not affected when
-- the newly allocated one is mutated.
--
-- >>> MVS.write mv' 2 888
-- >>> VS.freeze mv'
-- [10,20,888,999,0]
-- >>> VS.freeze mv
-- [10,20,30]
--
-- @since 0.5
grow :: (PrimMonad m, Storable a)
     => MVector (PrimState m) a -> Int -> m (MVector (PrimState m) a)
{-# INLINE grow #-}
grow = G.grow

-- | Grow a vector by the given number of elements. The number must be non-negative, but
-- this is not checked. This has the same semantics as 'G.unsafeGrow' for generic vectors.
--
-- @since 0.5
unsafeGrow :: (PrimMonad m, Storable a)
           => MVector (PrimState m) a -> Int -> m (MVector (PrimState m) a)
{-# INLINE unsafeGrow #-}
unsafeGrow = G.unsafeGrow

-- Restricting memory usage
-- ------------------------

-- | Reset all elements of the vector to some undefined value, clearing all
-- references to external objects. This is a noop.
clear :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> m ()
{-# INLINE clear #-}
clear = G.clear

-- Accessing individual elements
-- -----------------------------

-- | Yield the element at the given position. Will throw an exception if
-- the index is out of range.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector.Storable.Mutable as MVS
-- >>> v <- MVS.generate 10 (\x -> x*x)
-- >>> MVS.read v 3
-- 9
read :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> Int -> m a
{-# INLINE read #-}
read = G.read

-- | Yield the element at the given position. Returns 'Nothing' if
-- the index is out of range.
--
-- @since 0.13
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector.Storable.Mutable as MVS
-- >>> v <- MVS.generate 10 (\x -> x*x)
-- >>> MVS.readMaybe v 3
-- Just 9
-- >>> MVS.readMaybe v 13
-- Nothing
readMaybe :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> Int -> m (Maybe a)
{-# INLINE readMaybe #-}
readMaybe = G.readMaybe

-- | Replace the element at the given position.
write
    :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> Int -> a -> m ()
{-# INLINE write #-}
write = G.write

-- | Modify the element at the given position.
modify :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> (a -> a) -> Int -> m ()
{-# INLINE modify #-}
modify = G.modify

-- | Modify the element at the given position using a monadic function.
--
-- @since 0.12.3.0
modifyM :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> (a -> m a) -> Int -> m ()
{-# INLINE modifyM #-}
modifyM = G.modifyM

-- | Swap the elements at the given positions.
swap
    :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> Int -> Int -> m ()
{-# INLINE swap #-}
swap = G.swap

-- | Replace the element at the given position and return the old element.
exchange :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> Int -> a -> m a
{-# INLINE exchange #-}
exchange = G.exchange

-- | Yield the element at the given position. No bounds checks are performed.
unsafeRead :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> Int -> m a
{-# INLINE unsafeRead #-}
unsafeRead = G.unsafeRead

-- | Replace the element at the given position. No bounds checks are performed.
unsafeWrite :: (PrimMonad m, Storable a) =>  MVector (PrimState m) a -> Int -> a -> m ()
{-# INLINE unsafeWrite #-}
unsafeWrite = G.unsafeWrite

-- | Modify the element at the given position. No bounds checks are performed.
unsafeModify :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> (a -> a) -> Int -> m ()
{-# INLINE unsafeModify #-}
unsafeModify = G.unsafeModify

-- | Modify the element at the given position using a monadic
-- function. No bounds checks are performed.
--
-- @since 0.12.3.0
unsafeModifyM :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> (a -> m a) -> Int -> m ()
{-# INLINE unsafeModifyM #-}
unsafeModifyM = G.unsafeModifyM

-- | Swap the elements at the given positions. No bounds checks are performed.
unsafeSwap :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> Int -> Int -> m ()
{-# INLINE unsafeSwap #-}
unsafeSwap = G.unsafeSwap

-- | Replace the element at the given position and return the old element. No
-- bounds checks are performed.
unsafeExchange :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> Int -> a -> m a
{-# INLINE unsafeExchange #-}
unsafeExchange = G.unsafeExchange

-- Filling and copying
-- -------------------

-- | Set all elements of the vector to the given value.
set :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> a -> m ()
{-# INLINE set #-}
set = G.set

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap.
copy :: (PrimMonad m, Storable a)
     => MVector (PrimState m) a   -- ^ target
     -> MVector (PrimState m) a   -- ^ source
     -> m ()
{-# INLINE copy #-}
copy = G.copy

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap, but this is not checked.
unsafeCopy :: (PrimMonad m, Storable a)
           => MVector (PrimState m) a   -- ^ target
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
move :: (PrimMonad m, Storable a)
     => MVector (PrimState m) a   -- ^ target
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
unsafeMove :: (PrimMonad m, Storable a)
           => MVector (PrimState m) a   -- ^ target
           -> MVector (PrimState m) a   -- ^ source
           -> m ()
{-# INLINE unsafeMove #-}
unsafeMove = G.unsafeMove

-- Modifying vectors
-- -----------------


-- | Modify vector in place by applying function to each element.
--
-- @since NEXT_VERSION
mapInPlace :: (PrimMonad m, Storable a) => (a -> a) -> MVector (PrimState m) a -> m ()
{-# INLINE mapInPlace #-}
mapInPlace = G.mapInPlace

-- | Modify vector in place by applying function to each element and its index.
--
-- @since NEXT_VERSION
imapInPlace :: (PrimMonad m, Storable a) => (Int -> a -> a) -> MVector (PrimState m) a -> m ()
{-# INLINE imapInPlace #-}
imapInPlace = G.imapInPlace

-- | Modify vector in place by applying monadic function to each element in order.
--
-- @since NEXT_VERSION
mapInPlaceM :: (PrimMonad m, Storable a) => (a -> m a) -> MVector (PrimState m) a -> m ()
{-# INLINE mapInPlaceM #-}
mapInPlaceM = G.mapInPlaceM

-- | Modify vector in place by applying monadic function to each element and its index in order.
--
-- @since NEXT_VERSION
imapInPlaceM :: (PrimMonad m, Storable a) => (Int -> a -> m a) -> MVector (PrimState m) a -> m ()
{-# INLINE imapInPlaceM #-}
imapInPlaceM = G.imapInPlaceM

-- | Compute the (lexicographically) next permutation of the given vector in-place.
-- Returns False when the input is the last item in the enumeration, i.e., if it is in
-- weakly descending order. In this case the vector will not get updated,
-- as opposed to the behavior of the C++ function @std::next_permutation@.
nextPermutation :: (PrimMonad m, Storable e, Ord e) => MVector (PrimState m) e -> m Bool
{-# INLINE nextPermutation #-}
nextPermutation = G.nextPermutation

-- | Compute the (lexicographically) next permutation of the given vector in-place,
-- using the provided comparison function.
-- Returns False when the input is the last item in the enumeration, i.e., if it is in
-- weakly descending order. In this case the vector will not get updated,
-- as opposed to the behavior of the C++ function @std::next_permutation@.
--
-- @since 0.13.2.0
nextPermutationBy :: (PrimMonad m, Storable e) => (e -> e -> Ordering) -> MVector (PrimState m) e -> m Bool
{-# INLINE nextPermutationBy #-}
nextPermutationBy = G.nextPermutationBy

-- | Compute the (lexicographically) previous permutation of the given vector in-place.
-- Returns False when the input is the last item in the enumeration, i.e., if it is in
-- weakly ascending order. In this case the vector will not get updated,
-- as opposed to the behavior of the C++ function @std::prev_permutation@.
--
-- @since 0.13.2.0
prevPermutation :: (PrimMonad m, Storable e, Ord e) => MVector (PrimState m) e -> m Bool
{-# INLINE prevPermutation #-}
prevPermutation = G.prevPermutation

-- | Compute the (lexicographically) previous permutation of the given vector in-place,
-- using the provided comparison function.
-- Returns False when the input is the last item in the enumeration, i.e., if it is in
-- weakly ascending order. In this case the vector will not get updated,
-- as opposed to the behavior of the C++ function @std::prev_permutation@.
--
-- @since 0.13.2.0
prevPermutationBy :: (PrimMonad m, Storable e) => (e -> e -> Ordering) -> MVector (PrimState m) e -> m Bool
{-# INLINE prevPermutationBy #-}
prevPermutationBy = G.prevPermutationBy

-- Folds
-- -----

-- | /O(n)/ Apply the monadic action to every element of the vector, discarding the results.
--
-- @since 0.12.3.0
mapM_ :: (PrimMonad m, Storable a) => (a -> m b) -> MVector (PrimState m) a -> m ()
{-# INLINE mapM_ #-}
mapM_ = G.mapM_

-- | /O(n)/ Apply the monadic action to every element of the vector and its index, discarding the results.
--
-- @since 0.12.3.0
imapM_ :: (PrimMonad m, Storable a) => (Int -> a -> m b) -> MVector (PrimState m) a -> m ()
{-# INLINE imapM_ #-}
imapM_ = G.imapM_

-- | /O(n)/ Apply the monadic action to every element of the vector,
-- discarding the results. It's the same as @flip mapM_@.
--
-- @since 0.12.3.0
forM_ :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> (a -> m b) -> m ()
{-# INLINE forM_ #-}
forM_ = G.forM_

-- | /O(n)/ Apply the monadic action to every element of the vector
-- and its index, discarding the results. It's the same as @flip imapM_@.
--
-- @since 0.12.3.0
iforM_ :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> (Int -> a -> m b) -> m ()
{-# INLINE iforM_ #-}
iforM_ = G.iforM_

-- | /O(n)/ Pure left fold.
--
-- @since 0.12.3.0
foldl :: (PrimMonad m, Storable a) => (b -> a -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldl #-}
foldl = G.foldl

-- | /O(n)/ Pure left fold with strict accumulator.
--
-- @since 0.12.3.0
foldl' :: (PrimMonad m, Storable a) => (b -> a -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldl' #-}
foldl' = G.foldl'

-- | /O(n)/ Pure left fold using a function applied to each element and its index.
--
-- @since 0.12.3.0
ifoldl :: (PrimMonad m, Storable a) => (b -> Int -> a -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldl #-}
ifoldl = G.ifoldl

-- | /O(n)/ Pure left fold with strict accumulator using a function applied to each element and its index.
--
-- @since 0.12.3.0
ifoldl' :: (PrimMonad m, Storable a) => (b -> Int -> a -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldl' #-}
ifoldl' = G.ifoldl'

-- | /O(n)/ Pure right fold.
--
-- @since 0.12.3.0
foldr :: (PrimMonad m, Storable a) => (a -> b -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldr #-}
foldr = G.foldr

-- | /O(n)/ Pure right fold with strict accumulator.
--
-- @since 0.12.3.0
foldr' :: (PrimMonad m, Storable a) => (a -> b -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldr' #-}
foldr' = G.foldr'

-- | /O(n)/ Pure right fold using a function applied to each element and its index.
--
-- @since 0.12.3.0
ifoldr :: (PrimMonad m, Storable a) => (Int -> a -> b -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldr #-}
ifoldr = G.ifoldr

-- | /O(n)/ Pure right fold with strict accumulator using a function applied
-- to each element and its index.
--
-- @since 0.12.3.0
ifoldr' :: (PrimMonad m, Storable a) => (Int -> a -> b -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldr' #-}
ifoldr' = G.ifoldr'

-- | /O(n)/ Monadic fold.
--
-- @since 0.12.3.0
foldM :: (PrimMonad m, Storable a) => (b -> a -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldM #-}
foldM = G.foldM

-- | /O(n)/ Monadic fold with strict accumulator.
--
-- @since 0.12.3.0
foldM' :: (PrimMonad m, Storable a) => (b -> a -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldM' #-}
foldM' = G.foldM'

-- | /O(n)/ Monadic fold using a function applied to each element and its index.
--
-- @since 0.12.3.0
ifoldM :: (PrimMonad m, Storable a) => (b -> Int -> a -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldM #-}
ifoldM = G.ifoldM

-- | /O(n)/ Monadic fold with strict accumulator using a function applied to each element and its index.
--
-- @since 0.12.3.0
ifoldM' :: (PrimMonad m, Storable a) => (b -> Int -> a -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldM' #-}
ifoldM' = G.ifoldM'

-- | /O(n)/ Monadic right fold.
--
-- @since 0.12.3.0
foldrM :: (PrimMonad m, Storable a) => (a -> b -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldrM #-}
foldrM = G.foldrM

-- | /O(n)/ Monadic right fold with strict accumulator.
--
-- @since 0.12.3.0
foldrM' :: (PrimMonad m, Storable a) => (a -> b -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldrM' #-}
foldrM' = G.foldrM'

-- | /O(n)/ Monadic right fold using a function applied to each element and its index.
--
-- @since 0.12.3.0
ifoldrM :: (PrimMonad m, Storable a) => (Int -> a -> b -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldrM #-}
ifoldrM = G.ifoldrM

-- | /O(n)/ Monadic right fold with strict accumulator using a function applied
-- to each element and its index.
--
-- @since 0.12.3.0
ifoldrM' :: (PrimMonad m, Storable a) => (Int -> a -> b -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldrM' #-}
ifoldrM' = G.ifoldrM'

-- $setup
-- >>> import Prelude (($), Num(..))
