{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Vector.Primitive.Mutable
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
-- Mutable primitive vectors.

module Data.Vector.Primitive.Mutable (
  -- * Mutable vectors of primitive types
  MVector(..), IOVector, STVector,

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
  nextPermutation,

  -- ** Filling and copying
  set, copy, move, unsafeCopy, unsafeMove,

  -- * Unsafe conversions
  unsafeCoerceMVector, unsafeCast,
  -- * Re-exports
  Prim, PrimMonad, PrimState, RealWorld
) where

import qualified Data.Vector.Generic.Mutable as G
import           Data.Primitive.ByteArray
import           Data.Primitive ( Prim, sizeOf )
import           Data.Vector.Internal.Check
import           Data.Word ( Word8 )
import           Control.Monad.Primitive
import           Control.Monad ( liftM )

import Control.DeepSeq ( NFData(rnf)
#if MIN_VERSION_deepseq(1,4,3)
                       , NFData1(liftRnf)
#endif
                       )

import Prelude hiding ( length, null, replicate, reverse, map, read,
                        take, drop, splitAt, init, tail, foldr, foldl, mapM_ )

import Data.Typeable ( Typeable )
import Data.Coerce
import Unsafe.Coerce

-- Data.Vector.Internal.Check is unnecessary
#define NOT_VECTOR_MODULE
#include "vector.h"

type role MVector nominal nominal

-- | /O(1)/ Unsafely coerce a mutable vector from one element type to another,
-- representationally equal type. The operation just changes the type of the
-- underlying pointer and does not modify the elements.
--
-- Note that this function is unsafe. The @Coercible@ constraint guarantees
-- that the element types are representationally equal. It however cannot
-- guarantee that their respective 'Prim' instances are compatible.
unsafeCoerceMVector :: Coercible a b => MVector s a -> MVector s b
unsafeCoerceMVector = unsafeCoerce

-- | Mutable vectors of primitive types.
data MVector s a = MVector {-# UNPACK #-} !Int                  -- ^ offset
                           {-# UNPACK #-} !Int                  -- ^ length
                           {-# UNPACK #-} !(MutableByteArray s) -- ^ underlying mutable byte array
        deriving ( Typeable )

type IOVector = MVector RealWorld
type STVector s = MVector s

instance NFData (MVector s a) where
  rnf (MVector _ _ _) = ()

#if MIN_VERSION_deepseq(1,4,3)
instance NFData1 (MVector s) where
  liftRnf _ (MVector _ _ _) = ()
#endif

instance Prim a => G.MVector MVector a where
  basicLength (MVector _ n _) = n
  basicUnsafeSlice j m (MVector i _ arr)
    = MVector (i+j) m arr

  {-# INLINE basicOverlaps #-}
  basicOverlaps (MVector i m arr1) (MVector j n arr2)
    = sameMutableByteArray arr1 arr2
      && (between i j (j+n) || between j i (i+m))
    where
      between x y z = x >= y && x < z

  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew n
    | n < 0 = error $ "Primitive.basicUnsafeNew: negative length: " ++ show n
    | n > mx = error $ "Primitive.basicUnsafeNew: length too large: " ++ show n
    | otherwise = MVector 0 n `liftM` newByteArray (n * size)
    where
      size = sizeOf (undefined :: a)
      mx = maxBound `div` size :: Int

  {-# INLINE basicInitialize #-}
  basicInitialize (MVector off n v) =
      setByteArray v (off * size) (n * size) (0 :: Word8)
    where
      size = sizeOf (undefined :: a)


  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (MVector i _ arr) j = readByteArray arr (i+j)

  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (MVector i _ arr) j x = writeByteArray arr (i+j) x

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MVector i n dst) (MVector j _ src)
    = copyMutableByteArray dst (i*sz) src (j*sz) (n*sz)
    where
      sz = sizeOf (undefined :: a)

  {-# INLINE basicUnsafeMove #-}
  basicUnsafeMove (MVector i n dst) (MVector j _ src)
    = moveByteArray dst (i*sz) src (j*sz) (n * sz)
    where
      sz = sizeOf (undefined :: a)

  {-# INLINE basicSet #-}
  basicSet (MVector i n arr) x = setByteArray arr i n x

-- Length information
-- ------------------

-- | Length of the mutable vector.
length :: Prim a => MVector s a -> Int
{-# INLINE length #-}
length = G.length

-- | Check whether the vector is empty.
null :: Prim a => MVector s a -> Bool
{-# INLINE null #-}
null = G.null

-- Extracting subvectors
-- ---------------------

-- | Yield a part of the mutable vector without copying it. The vector must
-- contain at least @i+n@ elements.
slice :: Prim a
      => Int  -- ^ @i@ starting index
      -> Int  -- ^ @n@ length
      -> MVector s a
      -> MVector s a
{-# INLINE slice #-}
slice = G.slice

-- | Take the @n@ first elements of the mutable vector without making a
-- copy. For negative @n@, the empty vector is returned. If @n@ is larger
-- than the vector's length, the vector is returned unchanged.
take :: Prim a => Int -> MVector s a -> MVector s a
{-# INLINE take #-}
take = G.take

-- | Drop the @n@ first element of the mutable vector without making a
-- copy. For negative @n@, the vector is returned unchanged. If @n@ is
-- larger than the vector's length, the empty vector is returned.
drop :: Prim a => Int -> MVector s a -> MVector s a
{-# INLINE drop #-}
drop = G.drop

-- | /O(1)/ Split the mutable vector into the first @n@ elements
-- and the remainder, without copying.
--
-- Note that @'splitAt' n v@ is equivalent to @('take' n v, 'drop' n v)@,
-- but slightly more efficient.
splitAt :: Prim a => Int -> MVector s a -> (MVector s a, MVector s a)
{-# INLINE splitAt #-}
splitAt = G.splitAt

-- | Drop the last element of the mutable vector without making a copy.
-- If the vector is empty, an exception is thrown.
init :: Prim a => MVector s a -> MVector s a
{-# INLINE init #-}
init = G.init

-- | Drop the first element of the mutable vector without making a copy.
-- If the vector is empty, an exception is thrown.
tail :: Prim a => MVector s a -> MVector s a
{-# INLINE tail #-}
tail = G.tail

-- | Yield a part of the mutable vector without copying it. No bounds checks
-- are performed.
unsafeSlice :: Prim a
            => Int  -- ^ starting index
            -> Int  -- ^ length of the slice
            -> MVector s a
            -> MVector s a
{-# INLINE unsafeSlice #-}
unsafeSlice = G.unsafeSlice

-- | Unsafe variant of 'take'. If @n@ is out of range, it will
-- simply create an invalid slice that likely violate memory safety.
unsafeTake :: Prim a => Int -> MVector s a -> MVector s a
{-# INLINE unsafeTake #-}
unsafeTake = G.unsafeTake

-- | Unsafe variant of 'drop'. If @n@ is out of range, it will
-- simply create an invalid slice that likely violate memory safety.
unsafeDrop :: Prim a => Int -> MVector s a -> MVector s a
{-# INLINE unsafeDrop #-}
unsafeDrop = G.unsafeDrop

-- | Same as 'init', but doesn't do range checks.
unsafeInit :: Prim a => MVector s a -> MVector s a
{-# INLINE unsafeInit #-}
unsafeInit = G.unsafeInit

-- | Same as 'tail', but doesn't do range checks.
unsafeTail :: Prim a => MVector s a -> MVector s a
{-# INLINE unsafeTail #-}
unsafeTail = G.unsafeTail

-- Overlapping
-- -----------

-- | Check whether two vectors overlap.
overlaps :: Prim a => MVector s a -> MVector s a -> Bool
{-# INLINE overlaps #-}
overlaps = G.overlaps

-- Initialisation
-- --------------

-- | Create a mutable vector of the given length.
new :: (PrimMonad m, Prim a) => Int -> m (MVector (PrimState m) a)
{-# INLINE new #-}
new = G.new

-- | Create a mutable vector of the given length. The vector content
-- is uninitialized, which means it is filled with whatever the
-- underlying memory buffer happens to contain.
--
-- @since 0.5
unsafeNew :: (PrimMonad m, Prim a) => Int -> m (MVector (PrimState m) a)
{-# INLINE unsafeNew #-}
unsafeNew = G.unsafeNew

-- | Create a mutable vector of the given length (0 if the length is negative)
-- and fill it with an initial value.
replicate :: (PrimMonad m, Prim a) => Int -> a -> m (MVector (PrimState m) a)
{-# INLINE replicate #-}
replicate = G.replicate

-- | Create a mutable vector of the given length (0 if the length is negative)
-- and fill it with values produced by repeatedly executing the monadic action.
replicateM :: (PrimMonad m, Prim a) => Int -> m a -> m (MVector (PrimState m) a)
{-# INLINE replicateM #-}
replicateM = G.replicateM

-- | /O(n)/ Create a mutable vector of the given length (0 if the length is negative)
-- and fill it with the results of applying the function to each index.
-- Iteration starts at index 0.
--
-- @since 0.12.3.0
generate :: (PrimMonad m, Prim a) => Int -> (Int -> a) -> m (MVector (PrimState m) a)
{-# INLINE generate #-}
generate = G.generate

-- | /O(n)/ Create a mutable vector of the given length (0 if the length is
-- negative) and fill it with the results of applying the monadic function to each
-- index. Iteration starts at index 0.
--
-- @since 0.12.3.0
generateM :: (PrimMonad m, Prim a) => Int -> (Int -> m a) -> m (MVector (PrimState m) a)
{-# INLINE generateM #-}
generateM = G.generateM

-- | Create a copy of a mutable vector.
clone :: (PrimMonad m, Prim a)
      => MVector (PrimState m) a -> m (MVector (PrimState m) a)
{-# INLINE clone #-}
clone = G.clone

-- Growing
-- -------

-- | Grow a primitive vector by the given number of elements. The number must be
-- non-negative. This has the same semantics as 'G.grow' for generic vectors.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector.Primitive as VP
-- >>> import qualified Data.Vector.Primitive.Mutable as MVP
-- >>> mv <- VP.thaw $ VP.fromList ([10, 20, 30] :: [Int])
-- >>> mv' <- MVP.grow mv 2
--
-- Extra memory at the end of the newly allocated vector is initialized to 0
-- bytes, which for 'Prim' instances will usually correspond to some default
-- value for a particular type, e.g. @0@ for @Int@, @\NUL@ for @Char@,
-- etc. However, if 'unsafeGrow' was used instead, this would not have been
-- guaranteed and some garbage would be there instead.
--
-- >>> VP.freeze mv'
-- [10,20,30,0,0]
--
-- Having the extra space we can write new values in there:
--
-- >>> MVP.write mv' 3 999
-- >>> VP.freeze mv'
-- [10,20,30,999,0]
--
-- It is important to note that the source mutable vector is not affected when
-- the newly allocated one is mutated.
--
-- >>> MVP.write mv' 2 888
-- >>> VP.freeze mv'
-- [10,20,888,999,0]
-- >>> VP.freeze mv
-- [10,20,30]
--
-- @since 0.5
grow :: (PrimMonad m, Prim a)
     => MVector (PrimState m) a -> Int -> m (MVector (PrimState m) a)
{-# INLINE grow #-}
grow = G.grow

-- | Grow a vector by the given number of elements. The number must be non-negative, but
-- this is not checked. This has the same semantics as 'G.unsafeGrow' for generic vectors.
--
-- @since 0.5
unsafeGrow :: (PrimMonad m, Prim a)
           => MVector (PrimState m) a -> Int -> m (MVector (PrimState m) a)
{-# INLINE unsafeGrow #-}
unsafeGrow = G.unsafeGrow

-- Restricting memory usage
-- ------------------------

-- | Reset all elements of the vector to some undefined value, clearing all
-- references to external objects. This is a noop.
clear :: (PrimMonad m, Prim a) => MVector (PrimState m) a -> m ()
{-# INLINE clear #-}
clear = G.clear

-- Accessing individual elements
-- -----------------------------

-- | Yield the element at the given position. Will throw an exception if
-- the index is out of range.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector.Primitive.Mutable as MVP
-- >>> v <- MVP.generate 10 (\x -> x*x)
-- >>> MVP.read v 3
-- 9
read :: (PrimMonad m, Prim a) => MVector (PrimState m) a -> Int -> m a
{-# INLINE read #-}
read = G.read

-- | Yield the element at the given position. Returns 'Nothing' if
-- the index is out of range.
--
-- @since 0.13
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector.Primitive.Mutable as MVP
-- >>> v <- MVP.generate 10 (\x -> x*x)
-- >>> MVP.readMaybe v 3
-- Just 9
-- >>> MVP.readMaybe v 13
-- Nothing
readMaybe :: (PrimMonad m, Prim a) => MVector (PrimState m) a -> Int -> m (Maybe a)
{-# INLINE readMaybe #-}
readMaybe = G.readMaybe

-- | Replace the element at the given position.
write :: (PrimMonad m, Prim a) => MVector (PrimState m) a -> Int -> a -> m ()
{-# INLINE write #-}
write = G.write

-- | Modify the element at the given position.
modify :: (PrimMonad m, Prim a) => MVector (PrimState m) a -> (a -> a) -> Int -> m ()
{-# INLINE modify #-}
modify = G.modify

-- | Modify the element at the given position using a monadic function.
--
-- @since 0.12.3.0
modifyM :: (PrimMonad m, Prim a) => MVector (PrimState m) a -> (a -> m a) -> Int -> m ()
{-# INLINE modifyM #-}
modifyM = G.modifyM

-- | Swap the elements at the given positions.
swap :: (PrimMonad m, Prim a) => MVector (PrimState m) a -> Int -> Int -> m ()
{-# INLINE swap #-}
swap = G.swap

-- | Replace the element at the given position and return the old element.
exchange :: (PrimMonad m, Prim a) => MVector (PrimState m) a -> Int -> a -> m a
{-# INLINE exchange #-}
exchange = G.exchange

-- | Yield the element at the given position. No bounds checks are performed.
unsafeRead :: (PrimMonad m, Prim a) => MVector (PrimState m) a -> Int -> m a
{-# INLINE unsafeRead #-}
unsafeRead = G.unsafeRead

-- | Replace the element at the given position. No bounds checks are performed.
unsafeWrite :: (PrimMonad m, Prim a) =>  MVector (PrimState m) a -> Int -> a -> m ()
{-# INLINE unsafeWrite #-}
unsafeWrite = G.unsafeWrite

-- | Modify the element at the given position. No bounds checks are performed.
unsafeModify :: (PrimMonad m, Prim a) => MVector (PrimState m) a -> (a -> a) -> Int -> m ()
{-# INLINE unsafeModify #-}
unsafeModify = G.unsafeModify

-- | Modify the element at the given position using a monadic
-- function. No bounds checks are performed.
--
-- @since 0.12.3.0
unsafeModifyM :: (PrimMonad m, Prim a) => MVector (PrimState m) a -> (a -> m a) -> Int -> m ()
{-# INLINE unsafeModifyM #-}
unsafeModifyM = G.unsafeModifyM

-- | Swap the elements at the given positions. No bounds checks are performed.
unsafeSwap :: (PrimMonad m, Prim a) => MVector (PrimState m) a -> Int -> Int -> m ()
{-# INLINE unsafeSwap #-}
unsafeSwap = G.unsafeSwap

-- | Replace the element at the given position and return the old element. No
-- bounds checks are performed.
unsafeExchange :: (PrimMonad m, Prim a) => MVector (PrimState m) a -> Int -> a -> m a
{-# INLINE unsafeExchange #-}
unsafeExchange = G.unsafeExchange

-- Filling and copying
-- -------------------

-- | Set all elements of the vector to the given value.
set :: (PrimMonad m, Prim a) => MVector (PrimState m) a -> a -> m ()
{-# INLINE set #-}
set = G.set

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap.
copy :: (PrimMonad m, Prim a)
     => MVector (PrimState m) a   -- ^ target
     -> MVector (PrimState m) a   -- ^ source
     -> m ()
{-# INLINE copy #-}
copy = G.copy

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap, but this is not checked.
unsafeCopy :: (PrimMonad m, Prim a)
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
move :: (PrimMonad m, Prim a)
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
unsafeMove :: (PrimMonad m, Prim a)
                          => MVector (PrimState m) a   -- ^ target
                          -> MVector (PrimState m) a   -- ^ source
                          -> m ()
{-# INLINE unsafeMove #-}
unsafeMove = G.unsafeMove

-- Modifying vectors
-- -----------------

-- | Compute the (lexicographically) next permutation of the given vector in-place.
-- Returns False when the input is the last permutation.
nextPermutation :: (PrimMonad m,Ord e,Prim e) => MVector (PrimState m) e -> m Bool
{-# INLINE nextPermutation #-}
nextPermutation = G.nextPermutation

-- Folds
-- -----

-- | /O(n)/ Apply the monadic action to every element of the vector, discarding the results.
--
-- @since 0.12.3.0
mapM_ :: (PrimMonad m, Prim a) => (a -> m b) -> MVector (PrimState m) a -> m ()
{-# INLINE mapM_ #-}
mapM_ = G.mapM_

-- | /O(n)/ Apply the monadic action to every element of the vector and its index, discarding the results.
--
-- @since 0.12.3.0
imapM_ :: (PrimMonad m, Prim a) => (Int -> a -> m b) -> MVector (PrimState m) a -> m ()
{-# INLINE imapM_ #-}
imapM_ = G.imapM_

-- | /O(n)/ Apply the monadic action to every element of the vector,
-- discarding the results. It's the same as @flip mapM_@.
--
-- @since 0.12.3.0
forM_ :: (PrimMonad m, Prim a) => MVector (PrimState m) a -> (a -> m b) -> m ()
{-# INLINE forM_ #-}
forM_ = G.forM_

-- | /O(n)/ Apply the monadic action to every element of the vector
-- and its index, discarding the results. It's the same as @flip imapM_@.
--
-- @since 0.12.3.0
iforM_ :: (PrimMonad m, Prim a) => MVector (PrimState m) a -> (Int -> a -> m b) -> m ()
{-# INLINE iforM_ #-}
iforM_ = G.iforM_

-- | /O(n)/ Pure left fold.
--
-- @since 0.12.3.0
foldl :: (PrimMonad m, Prim a) => (b -> a -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldl #-}
foldl = G.foldl

-- | /O(n)/ Pure left fold with strict accumulator.
--
-- @since 0.12.3.0
foldl' :: (PrimMonad m, Prim a) => (b -> a -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldl' #-}
foldl' = G.foldl'

-- | /O(n)/ Pure left fold using a function applied to each element and its index.
--
-- @since 0.12.3.0
ifoldl :: (PrimMonad m, Prim a) => (b -> Int -> a -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldl #-}
ifoldl = G.ifoldl

-- | /O(n)/ Pure left fold with strict accumulator using a function applied to each element and its index.
--
-- @since 0.12.3.0
ifoldl' :: (PrimMonad m, Prim a) => (b -> Int -> a -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldl' #-}
ifoldl' = G.ifoldl'

-- | /O(n)/ Pure right fold.
--
-- @since 0.12.3.0
foldr :: (PrimMonad m, Prim a) => (a -> b -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldr #-}
foldr = G.foldr

-- | /O(n)/ Pure right fold with strict accumulator.
--
-- @since 0.12.3.0
foldr' :: (PrimMonad m, Prim a) => (a -> b -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldr' #-}
foldr' = G.foldr'

-- | /O(n)/ Pure right fold using a function applied to each element and its index.
--
-- @since 0.12.3.0
ifoldr :: (PrimMonad m, Prim a) => (Int -> a -> b -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldr #-}
ifoldr = G.ifoldr

-- | /O(n)/ Pure right fold with strict accumulator using a function applied
-- to each element and its index.
--
-- @since 0.12.3.0
ifoldr' :: (PrimMonad m, Prim a) => (Int -> a -> b -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldr' #-}
ifoldr' = G.ifoldr'

-- | /O(n)/ Monadic fold.
--
-- @since 0.12.3.0
foldM :: (PrimMonad m, Prim a) => (b -> a -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldM #-}
foldM = G.foldM

-- | /O(n)/ Monadic fold with strict accumulator.
--
-- @since 0.12.3.0
foldM' :: (PrimMonad m, Prim a) => (b -> a -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldM' #-}
foldM' = G.foldM'

-- | /O(n)/ Monadic fold using a function applied to each element and its index.
--
-- @since 0.12.3.0
ifoldM :: (PrimMonad m, Prim a) => (b -> Int -> a -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldM #-}
ifoldM = G.ifoldM

-- | /O(n)/ Monadic fold with strict accumulator using a function applied to each element and its index.
--
-- @since 0.12.3.0
ifoldM' :: (PrimMonad m, Prim a) => (b -> Int -> a -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldM' #-}
ifoldM' = G.ifoldM'

-- | /O(n)/ Monadic right fold.
--
-- @since 0.12.3.0
foldrM :: (PrimMonad m, Prim a) => (a -> b -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldrM #-}
foldrM = G.foldrM

-- | /O(n)/ Monadic right fold with strict accumulator.
--
-- @since 0.12.3.0
foldrM' :: (PrimMonad m, Prim a) => (a -> b -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldrM' #-}
foldrM' = G.foldrM'

-- | /O(n)/ Monadic right fold using a function applied to each element and its index.
--
-- @since 0.12.3.0
ifoldrM :: (PrimMonad m, Prim a) => (Int -> a -> b -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldrM #-}
ifoldrM = G.ifoldrM

-- | /O(n)/ Monadic right fold with strict accumulator using a function applied
-- to each element and its index.
--
-- @since 0.12.3.0
ifoldrM' :: (PrimMonad m, Prim a) => (Int -> a -> b -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldrM' #-}
ifoldrM' = G.ifoldrM'

-- Unsafe conversions
-- ------------------

-- | /O(1)/ Unsafely cast a vector from one element type to another.
-- This operation just changes the type of the vector and does not
-- modify the elements.
--
-- This function will throw an error if elements are of mismatching sizes.
--
-- | @since 0.13.0.0
unsafeCast :: forall a b s. (HasCallStack, Prim a, Prim b) => MVector s a -> MVector s b
{-# INLINE unsafeCast #-}
unsafeCast (MVector o n ba)
  | sizeOf (undefined :: a) == sizeOf (undefined :: b) = MVector o n ba
  | otherwise = error "Element size mismatch"
