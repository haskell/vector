-- |
-- Module      : Data.Vector.Unboxed.Mutable
-- Copyright   : (c) Roman Leshchinskiy 2009
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
--
-- Mutable adaptive unboxed vectors
--

module Data.Vector.Unboxed.Mutable (
  -- * Mutable vectors of primitive types
  MVector(..), IOVector, STVector, Unbox,

  -- * Operations on mutable vectors
  length, overlaps, slice, new, newWith, read, write, clear, set, copy, grow,
  zip, zip3, zip4, zip5, zip6,
  unzip, unzip3, unzip4, unzip5, unzip6,

  -- * Unsafe operations
  unsafeSlice, unsafeNew, unsafeNewWith, unsafeRead, unsafeWrite,
  unsafeCopy, unsafeGrow
) where

import Data.Vector.Unboxed.Base
import qualified Data.Vector.Generic.Mutable as G
import Control.Monad.Primitive

import Prelude hiding ( zip, zip3, unzip, unzip3, length, read )

#include "vector.h"

-- | Yield a part of the mutable vector without copying it. No bounds checks
-- are performed.
unsafeSlice :: Unbox a => MVector s a -> Int  -- ^ starting index
                                     -> Int  -- ^ length of the slice
                                     -> MVector s a
{-# INLINE unsafeSlice #-}
unsafeSlice = G.unsafeSlice


-- | Create a mutable vector of the given length. The length is not checked.
unsafeNew :: (PrimMonad m, Unbox a) => Int -> m (MVector (PrimState m) a)
{-# INLINE unsafeNew #-}
unsafeNew = G.unsafeNew

-- | Create a mutable vector of the given length and fill it with an
-- initial value. The length is not checked.
unsafeNewWith :: (PrimMonad m, Unbox a)
                                => Int -> a -> m (MVector (PrimState m) a)
{-# INLINE unsafeNewWith #-}
unsafeNewWith = G.unsafeNewWith

-- | Yield the element at the given position. No bounds checks are performed.
unsafeRead :: (PrimMonad m, Unbox a) => MVector (PrimState m) a -> Int -> m a
{-# INLINE unsafeRead #-}
unsafeRead = G.unsafeRead

-- | Replace the element at the given position. No bounds checks are performed.
unsafeWrite :: (PrimMonad m, Unbox a)
                                => MVector (PrimState m) a -> Int -> a -> m ()
{-# INLINE unsafeWrite #-}
unsafeWrite = G.unsafeWrite


-- | Copy a vector. The two vectors must have the same length and may not
-- overlap. This is not checked.
unsafeCopy :: (PrimMonad m, Unbox a) => MVector (PrimState m) a   -- ^ target
                                    -> MVector (PrimState m) a   -- ^ source
                                    -> m ()
{-# INLINE unsafeCopy #-}
unsafeCopy = G.unsafeCopy

-- | Grow a vector by the given number of elements. The number must be
-- positive but this is not checked.
unsafeGrow :: (PrimMonad m, Unbox a)
               => MVector (PrimState m) a -> Int -> m (MVector (PrimState m) a)
{-# INLINE unsafeGrow #-}
unsafeGrow = G.unsafeGrow

-- | Length of the mutable vector.
length :: Unbox a => MVector s a -> Int
{-# INLINE length #-}
length = G.length

-- Check whether two vectors overlap.
overlaps :: Unbox a => MVector s a -> MVector s a -> Bool
{-# INLINE overlaps #-}
overlaps = G.overlaps

-- | Yield a part of the mutable vector without copying it.
slice :: Unbox a => MVector s a -> Int -> Int -> MVector s a
{-# INLINE slice #-}
slice = G.slice

-- | Create a mutable vector of the given length.
new :: (PrimMonad m, Unbox a) => Int -> m (MVector (PrimState m) a)
{-# INLINE new #-}
new = G.new

-- | Create a mutable vector of the given length and fill it with an
-- initial value.
newWith :: (PrimMonad m, Unbox a) => Int -> a -> m (MVector (PrimState m) a)
{-# INLINE newWith #-}
newWith = G.newWith

-- | Yield the element at the given position.
read :: (PrimMonad m, Unbox a) => MVector (PrimState m) a -> Int -> m a
{-# INLINE read #-}
read = G.read

-- | Replace the element at the given position.
write :: (PrimMonad m, Unbox a) => MVector (PrimState m) a -> Int -> a -> m ()
{-# INLINE write #-}
write = G.write

-- | Reset all elements of the vector to some undefined value, clearing all
-- references to external objects. This is usually a noop for unboxed vectors. 
clear :: (PrimMonad m, Unbox a) => MVector (PrimState m) a -> m ()
{-# INLINE clear #-}
clear = G.clear

-- | Set all elements of the vector to the given value.
set :: (PrimMonad m, Unbox a) => MVector (PrimState m) a -> a -> m ()
{-# INLINE set #-}
set = G.set

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap.
copy :: (PrimMonad m, Unbox a)
                => MVector (PrimState m) a -> MVector (PrimState m) a -> m ()
{-# INLINE copy #-}
copy = G.copy

-- | Grow a vector by the given number of elements. The number must be
-- positive.
grow :: (PrimMonad m, Unbox a)
              => MVector (PrimState m) a -> Int -> m (MVector (PrimState m) a)
{-# INLINE grow #-}
grow = G.grow

#define DEFINE_MUTABLE
#include "unbox-tuple-instances"

