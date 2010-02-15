{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- |
-- Module      : Data.Vector.Storable.Mutable
-- Copyright   : (c) Roman Leshchinskiy 2009
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
-- 
-- Mutable vectors based on Storable.
--

module Data.Vector.Storable.Mutable(
  -- * Mutable vectors of 'Storable' types
  MVector(..), IOVector, STVector, Storable,

  -- * Operations on mutable vectors
  length, overlaps, slice, new, newWith, read, write, swap,
  clear, set, copy, grow,

  -- * Unsafe operations
  unsafeSlice, unsafeNew, unsafeNewWith, unsafeRead, unsafeWrite, unsafeSwap,
  unsafeCopy, unsafeGrow
) where

import qualified Data.Vector.Generic.Mutable as G

import Foreign.Storable
import Foreign.ForeignPtr

import Control.Monad.Primitive

import Prelude hiding( length, read )

#include "vector.h"

-- | Mutable 'Storable'-based vectors
data MVector s a = MVector {-# UNPACK #-} !Int
                           {-# UNPACK #-} !Int
                           {-# UNPACK #-} !(ForeignPtr a)

type IOVector = MVector RealWorld
type STVector s = MVector s

instance Storable a => G.MVector MVector a where
  {-# INLINE basicLength #-}
  basicLength (MVector _ n _) = n

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice j m (MVector i n p) = MVector (i+j) m p

  -- FIXME: implement this properly
  {-# INLINE basicOverlaps #-}
  basicOverlaps (MVector i m p) (MVector j n q) = True

  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew n
    = unsafePrimToPrim
    $ MVector 0 n `fmap` mallocForeignPtrArray n

  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (MVector i n p) j
    = unsafePrimToPrim
    $ withForeignPtr p $ \ptr -> peekElemOff ptr (i+j)

  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (MVector i n p) j x
    = unsafePrimToPrim
    $ withForeignPtr p $ \ptr -> pokeElemOff ptr (i+j) x


-- | Yield a part of the mutable vector without copying it. No bounds checks
-- are performed.
unsafeSlice :: Storable a => Int  -- ^ starting index
                          -> Int  -- ^ length of the slice
                          -> MVector s a
                          -> MVector s a
{-# INLINE unsafeSlice #-}
unsafeSlice = G.unsafeSlice

-- | Create a mutable vector of the given length. The length is not checked.
unsafeNew :: (PrimMonad m, Storable a) => Int -> m (MVector (PrimState m) a)
{-# INLINE unsafeNew #-}
unsafeNew = G.unsafeNew

-- | Create a mutable vector of the given length and fill it with an
-- initial value. The length is not checked.
unsafeNewWith :: (PrimMonad m, Storable a)
                                => Int -> a -> m (MVector (PrimState m) a)
{-# INLINE unsafeNewWith #-}
unsafeNewWith = G.unsafeNewWith

-- | Yield the element at the given position. No bounds checks are performed.
unsafeRead :: (PrimMonad m, Storable a)
                                => MVector (PrimState m) a -> Int -> m a
{-# INLINE unsafeRead #-}
unsafeRead = G.unsafeRead

-- | Replace the element at the given position. No bounds checks are performed.
unsafeWrite :: (PrimMonad m, Storable a)
                                => MVector (PrimState m) a -> Int -> a -> m ()
{-# INLINE unsafeWrite #-}
unsafeWrite = G.unsafeWrite

-- | Swap the elements at the given positions. No bounds checks are performed.
unsafeSwap :: (PrimMonad m, Storable a)
                => MVector (PrimState m) a -> Int -> Int -> m ()
{-# INLINE unsafeSwap #-}
unsafeSwap = G.unsafeSwap

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap. This is not checked.
unsafeCopy :: (PrimMonad m, Storable a)
                                => MVector (PrimState m) a   -- ^ target
                                -> MVector (PrimState m) a   -- ^ source
                                -> m ()
{-# INLINE unsafeCopy #-}
unsafeCopy = G.unsafeCopy

-- | Grow a vector by the given number of elements. The number must be
-- positive but this is not checked.
unsafeGrow :: (PrimMonad m, Storable a)
               => MVector (PrimState m) a -> Int -> m (MVector (PrimState m) a)
{-# INLINE unsafeGrow #-}
unsafeGrow = G.unsafeGrow

-- | Length of the mutable vector.
length :: Storable a => MVector s a -> Int
{-# INLINE length #-}
length = G.length

-- Check whether two vectors overlap.
overlaps :: Storable a => MVector s a -> MVector s a -> Bool
{-# INLINE overlaps #-}
overlaps = G.overlaps

-- | Yield a part of the mutable vector without copying it.
slice :: Storable a => Int -> Int -> MVector s a -> MVector s a
{-# INLINE slice #-}
slice = G.slice

-- | Create a mutable vector of the given length.
new :: (PrimMonad m, Storable a) => Int -> m (MVector (PrimState m) a)
{-# INLINE new #-}
new = G.new

-- | Create a mutable vector of the given length and fill it with an
-- initial value.
newWith :: (PrimMonad m, Storable a) => Int -> a -> m (MVector (PrimState m) a)
{-# INLINE newWith #-}
newWith = G.newWith

-- | Yield the element at the given position.
read :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> Int -> m a
{-# INLINE read #-}
read = G.read

-- | Replace the element at the given position.
write :: (PrimMonad m, Storable a)
                                => MVector (PrimState m) a -> Int -> a -> m ()
{-# INLINE write #-}
write = G.write

-- | Swap the elements at the given positions.
swap :: (PrimMonad m, Storable a)
                => MVector (PrimState m) a -> Int -> Int -> m ()
{-# INLINE swap #-}
swap = G.swap

-- | Reset all elements of the vector to some undefined value, clearing all
-- references to external objects. This is usually a noop for unboxed vectors. 
clear :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> m ()
{-# INLINE clear #-}
clear = G.clear

-- | Set all elements of the vector to the given value.
set :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> a -> m ()
{-# INLINE set #-}
set = G.set

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap.
copy :: (PrimMonad m, Storable a)
                => MVector (PrimState m) a -> MVector (PrimState m) a -> m ()
{-# INLINE copy #-}
copy = G.copy

-- | Grow a vector by the given number of elements. The number must be
-- positive.
grow :: (PrimMonad m, Storable a)
              => MVector (PrimState m) a -> Int -> m (MVector (PrimState m) a)
{-# INLINE grow #-}
grow = G.grow

