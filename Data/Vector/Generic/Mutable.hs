{-# LANGUAGE MultiParamTypeClasses, BangPatterns #-}
-- |
-- Module      : Data.Vector.Generic.Mutable
-- Copyright   : (c) Roman Leshchinskiy 2008-2009
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
-- 
-- Generic interface to mutable vectors
--

module Data.Vector.Generic.Mutable (
  MVector(..),

  slice, new, newWith, read, write, copy, grow,
  unstream, transform,
  accum, update, reverse
) where

import qualified Data.Vector.Fusion.Stream      as Stream
import           Data.Vector.Fusion.Stream      ( Stream, MStream )
import qualified Data.Vector.Fusion.Stream.Monadic as MStream
import           Data.Vector.Fusion.Stream.Size

import Control.Monad.Primitive ( PrimMonad, PrimState )

import GHC.Float (
    double2Int, int2Double
  )

import Prelude hiding ( length, reverse, map, read )

#include "vector.h"

gROWTH_FACTOR :: Double
gROWTH_FACTOR = 1.5

-- | Class of mutable vectors parametrised with a primitive state token.
--
class MVector v a where
  -- | Length of the mutable vector
  length           :: v s a -> Int

  -- | Yield a part of the mutable vector without copying it. No range checks!
  unsafeSlice      :: v s a -> Int  -- ^ starting index
                            -> Int  -- ^ length of the slice
                            -> v s a

  -- Check whether two vectors overlap.
  overlaps         :: v s a -> v s a -> Bool

  -- | Create a mutable vector of the given length. Length is not checked!
  unsafeNew        :: PrimMonad m => Int -> m (v (PrimState m) a)

  -- | Create a mutable vector of the given length and fill it with an
  -- initial value. Length is not checked!
  unsafeNewWith    :: PrimMonad m => Int -> a -> m (v (PrimState m) a)

  -- | Yield the element at the given position. Index is not checked!
  unsafeRead       :: PrimMonad m => v (PrimState m) a -> Int -> m a

  -- | Replace the element at the given position. Index is not checked!
  unsafeWrite      :: PrimMonad m => v (PrimState m) a -> Int -> a -> m ()

  -- | Clear all references to external objects
  clear            :: PrimMonad m => v (PrimState m) a -> m ()

  -- | Write the value at each position.
  set              :: PrimMonad m => v (PrimState m) a -> a -> m ()

  -- | Copy a vector. The two vectors may not overlap. This is not checked!
  unsafeCopy       :: PrimMonad m => v (PrimState m) a   -- ^ target
                                  -> v (PrimState m) a   -- ^ source
                                  -> m ()

  -- | Grow a vector by the given number of elements. The length is not
  -- checked!
  unsafeGrow :: PrimMonad m => v (PrimState m) a -> Int -> m (v (PrimState m) a)

  {-# INLINE unsafeNewWith #-}
  unsafeNewWith n x = UNSAFE_CHECK(checkLength) "unsafeNewWith" n
                    $ do
                        v <- unsafeNew n
                        set v x
                        return v

  {-# INLINE set #-}
  set v x = do_set 0
    where
      n = length v

      do_set i | i < n = do
                            unsafeWrite v i x
                            do_set (i+1)
                | otherwise = return ()

  {-# INLINE unsafeCopy #-}
  unsafeCopy dst src
    = UNSAFE_CHECK(check) "unsafeCopy" "overlapping vectors"
                                          (not (dst `overlaps` src))
    $ UNSAFE_CHECK(check) "unsafeCopy" "length mismatch"
                                          (length dst == length src)
    $ do_copy 0
    where
      n = length src

      do_copy i | i < n = do
                            x <- unsafeRead src i
                            unsafeWrite dst i x
                            do_copy (i+1)
                | otherwise = return ()

  {-# INLINE unsafeGrow #-}
  unsafeGrow v by = UNSAFE_CHECK(checkLength) "unsafeGrow" by
                  $ do
                      v' <- unsafeNew (n+by)
                      unsafeCopy (unsafeSlice v' 0 n) v
                      return v'
    where
      n = length v

-- | Yield a part of the mutable vector without copying it. Safer version of
-- 'unsafeSlice'.
slice :: MVector v a => v s a -> Int -> Int -> v s a
{-# INLINE slice #-}
slice v i n = BOUNDS_CHECK(checkSlice) "slice" i n (length v)
            $ unsafeSlice v i n

-- | Create a mutable vector of the given length. Safer version of
-- 'unsafeNew'.
new :: (PrimMonad m, MVector v a) => Int -> m (v (PrimState m) a)
{-# INLINE new #-}
new n = BOUNDS_CHECK(checkLength) "new" n
      $ unsafeNew n

-- | Create a mutable vector of the given length and fill it with an
-- initial value. Safer version of 'unsafeNewWith'.
newWith :: (PrimMonad m, MVector v a) => Int -> a -> m (v (PrimState m) a)
{-# INLINE newWith #-}
newWith n x = BOUNDS_CHECK(checkLength) "newWith" n
            $ unsafeNewWith n x

-- | Yield the element at the given position. Safer version of 'unsafeRead'.
read :: (PrimMonad m, MVector v a) => v (PrimState m) a -> Int -> m a
{-# INLINE read #-}
read v i = BOUNDS_CHECK(checkIndex) "read" i (length v)
         $ unsafeRead v i

-- | Replace the element at the given position. Safer version of
-- 'unsafeWrite'.
write :: (PrimMonad m, MVector v a) => v (PrimState m) a -> Int -> a -> m ()
{-# INLINE write #-}
write v i x = BOUNDS_CHECK(checkIndex) "write" i (length v)
            $ unsafeWrite v i x

-- | Copy a vector. The two vectors may not overlap. Safer version of
-- 'unsafeCopy'.
copy :: (PrimMonad m, MVector v a)
                => v (PrimState m) a -> v (PrimState m) a -> m ()
{-# INLINE copy #-}
copy dst src = BOUNDS_CHECK(check) "copy" "overlapping vectors"
                                          (not (dst `overlaps` src))
             $ BOUNDS_CHECK(check) "copy" "length mismatch"
                                          (length dst == length src)
             $ unsafeCopy dst src

-- | Grow a vector by the given number of elements. Safer version of
-- 'unsafeGrow'.
grow :: (PrimMonad m, MVector v a)
                => v (PrimState m) a -> Int -> m (v (PrimState m) a)
{-# INLINE grow #-}
grow v by = BOUNDS_CHECK(checkLength) "grow" by
          $ unsafeGrow v by

mstream :: (PrimMonad m, MVector v a) => v (PrimState m) a -> MStream m a
{-# INLINE mstream #-}
mstream v = v `seq` (MStream.unfoldrM get 0 `MStream.sized` Exact n)
  where
    n = length v

    {-# INLINE_INNER get #-}
    get i | i < n     = do x <- unsafeRead v i
                           return $ Just (x, i+1)
          | otherwise = return $ Nothing

internal_munstream :: (PrimMonad m, MVector v a)
        => v (PrimState m) a -> MStream m a -> m (v (PrimState m) a)
{-# INLINE internal_munstream #-}
internal_munstream v s = v `seq` do
                                   n' <- MStream.foldM put 0 s
                                   return $ slice v 0 n'
  where
    {-# INLINE_INNER put #-}
    put i x = do
                INTERNAL_CHECK(checkIndex) "internal_munstream" i (length v)
                  $ unsafeWrite v i x
                return (i+1)

transform :: (PrimMonad m, MVector v a)
  => (MStream m a -> MStream m a) -> v (PrimState m) a -> m (v (PrimState m) a)
{-# INLINE_STREAM transform #-}
transform f v = internal_munstream v (f (mstream v))

-- | Create a new mutable vector and fill it with elements from the 'Stream'.
-- The vector will grow logarithmically if the 'Size' hint of the 'Stream' is
-- inexact.
unstream :: (PrimMonad m, MVector v a) => Stream a -> m (v (PrimState m) a)
{-# INLINE_STREAM unstream #-}
unstream s = case upperBound (Stream.size s) of
               Just n  -> unstreamMax     s n
               Nothing -> unstreamUnknown s

unstreamMax
  :: (PrimMonad m, MVector v a) => Stream a -> Int -> m (v (PrimState m) a)
{-# INLINE unstreamMax #-}
unstreamMax s n
  = do
      v  <- new n
      let put i x = do
                       INTERNAL_CHECK(checkIndex) "unstreamMax" i n
                         $ unsafeWrite v i x
                       return (i+1)
      n' <- Stream.foldM' put 0 s
      return $ INTERNAL_CHECK(checkSlice) "unstreamMax" 0 n' n $ slice v 0 n'

unstreamUnknown
  :: (PrimMonad m, MVector v a) => Stream a -> m (v (PrimState m) a)
{-# INLINE unstreamUnknown #-}
unstreamUnknown s
  = do
      v <- new 0
      (v', n) <- Stream.foldM put (v, 0) s
      return $ slice v' 0 n
  where
    -- NOTE: The case distinction has to be on the outside because
    -- GHC creates a join point for the unsafeWrite even when everything
    -- is inlined. This is bad because with the join point, v isn't getting
    -- unboxed.
    {-# INLINE_INNER put #-}
    put (v, i) x
      | i < length v = do
                         unsafeWrite v i x
                         return (v, i+1)
      | otherwise    = do
                         v' <- enlarge v
                         INTERNAL_CHECK(checkIndex) "unstreamMax" i (length v')
                           $ unsafeWrite v' i x
                         return (v', i+1)

    {-# INLINE_INNER enlarge #-}
    enlarge v = unsafeGrow v
              $ max 1
              $ double2Int
              $ int2Double (length v) * gROWTH_FACTOR

accum :: (PrimMonad m, MVector v a)
        => (a -> b -> a) -> v (PrimState m) a -> Stream (Int, b) -> m ()
{-# INLINE accum #-}
accum f !v s = Stream.mapM_ upd s
  where
    {-# INLINE_INNER upd #-}
    upd (i,b) = do
                  a <- read v i
                  write v i (f a b)

update :: (PrimMonad m, MVector v a)
                        => v (PrimState m) a -> Stream (Int, a) -> m ()
{-# INLINE update #-}
update = accum (const id)

reverse :: (PrimMonad m, MVector v a) => v (PrimState m) a -> m ()
{-# INLINE reverse #-}
reverse !v = reverse_loop 0 (length v - 1)
  where
    reverse_loop i j | i < j = do
                                 x <- unsafeRead v i
                                 y <- unsafeRead v j
                                 unsafeWrite v i y
                                 unsafeWrite v j x
                                 reverse_loop (i + 1) (j - 1)
    reverse_loop _ _ = return ()

