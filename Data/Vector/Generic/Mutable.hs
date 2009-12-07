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
  -- * Class of mutable vector types
  MVector(..),

  -- * Operations on mutable vectors
  length, overlaps, slice, new, newWith, read, write, clear, set, copy, grow,

  -- * Unsafe operations
  unsafeSlice, unsafeNew, unsafeNewWith, unsafeRead, unsafeWrite,
  unsafeCopy, unsafeGrow,

  -- * Internal operations
  unstream, transform, unsafeAccum, accum, unsafeUpdate, update, reverse
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
  -- | Length of the mutable vector. This method should not be
  -- called directly, use 'length' instead.
  basicLength       :: v s a -> Int

  -- | Yield a part of the mutable vector without copying it. This method
  -- should not be called directly, use 'unsafeSlice' instead.
  basicUnsafeSlice :: v s a -> Int  -- ^ starting index
                            -> Int  -- ^ length of the slice
                            -> v s a

  -- Check whether two vectors overlap. This method should not be
  -- called directly, use 'overlaps' instead.
  basicOverlaps    :: v s a -> v s a -> Bool

  -- | Create a mutable vector of the given length. This method should not be
  -- called directly, use 'unsafeNew' instead.
  basicUnsafeNew   :: PrimMonad m => Int -> m (v (PrimState m) a)

  -- | Create a mutable vector of the given length and fill it with an
  -- initial value. This method should not be called directly, use
  -- 'unsafeNewWith' instead.
  basicUnsafeNewWith :: PrimMonad m => Int -> a -> m (v (PrimState m) a)

  -- | Yield the element at the given position. This method should not be
  -- called directly, use 'unsafeRead' instead.
  basicUnsafeRead  :: PrimMonad m => v (PrimState m) a -> Int -> m a

  -- | Replace the element at the given position. This method should not be
  -- called directly, use 'unsafeWrite' instead.
  basicUnsafeWrite :: PrimMonad m => v (PrimState m) a -> Int -> a -> m ()

  -- | Reset all elements of the vector to some undefined value, clearing all
  -- references to external objects. This is usually a noop for unboxed
  -- vectors. This method should not be called directly, use 'clear' instead.
  basicClear       :: PrimMonad m => v (PrimState m) a -> m ()

  -- | Set all elements of the vector to the given value. This method should
  -- not be called directly, use 'set' instead.
  basicSet         :: PrimMonad m => v (PrimState m) a -> a -> m ()

  -- | Copy a vector. The two vectors may not overlap. This method should not
  -- be called directly, use 'unsafeCopy' instead.
  basicUnsafeCopy  :: PrimMonad m => v (PrimState m) a   -- ^ target
                                  -> v (PrimState m) a   -- ^ source
                                  -> m ()

  -- | Grow a vector by the given number of elements. This method should not be
  -- called directly, use 'unsafeGrow' instead.
  basicUnsafeGrow  :: PrimMonad m => v (PrimState m) a -> Int
                                                       -> m (v (PrimState m) a)
  {-# INLINE basicUnsafeNewWith #-}
  basicUnsafeNewWith n x
    = do
        v <- basicUnsafeNew n
        set v x
        return v

  {-# INLINE basicClear #-}
  basicClear _ = return ()

  {-# INLINE basicSet #-}
  basicSet v x = do_set 0
    where
      n = length v

      do_set i | i < n = do
                           basicUnsafeWrite v i x
                           do_set (i+1)
                | otherwise = return ()

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy dst src = do_copy 0
    where
      n = length src

      do_copy i | i < n = do
                            x <- basicUnsafeRead src i
                            basicUnsafeWrite dst i x
                            do_copy (i+1)
                | otherwise = return ()

  {-# INLINE basicUnsafeGrow #-}
  basicUnsafeGrow v by
    = do
        v' <- basicUnsafeNew (n+by)
        basicUnsafeCopy (basicUnsafeSlice v' 0 n) v
        return v'
    where
      n = length v



-- | Yield a part of the mutable vector without copying it. No bounds checks
-- are performed.
unsafeSlice :: MVector v a => v s a -> Int  -- ^ starting index
                                    -> Int  -- ^ length of the slice
                                    -> v s a
{-# INLINE unsafeSlice #-}
unsafeSlice v i n = UNSAFE_CHECK(checkSlice) "unsafeSlice" i n (length v)
                  $ basicUnsafeSlice v i n


-- | Create a mutable vector of the given length. The length is not checked.
unsafeNew :: (PrimMonad m, MVector v a) => Int -> m (v (PrimState m) a)
{-# INLINE unsafeNew #-}
unsafeNew n = UNSAFE_CHECK(checkLength) "unsafeNew" n
            $ basicUnsafeNew n

-- | Create a mutable vector of the given length and fill it with an
-- initial value. The length is not checked.
unsafeNewWith :: (PrimMonad m, MVector v a) => Int -> a -> m (v (PrimState m) a)
{-# INLINE unsafeNewWith #-}
unsafeNewWith n x = UNSAFE_CHECK(checkLength) "unsafeNewWith" n
                  $ basicUnsafeNewWith n x

-- | Yield the element at the given position. No bounds checks are performed.
unsafeRead :: (PrimMonad m, MVector v a) => v (PrimState m) a -> Int -> m a
{-# INLINE unsafeRead #-}
unsafeRead v i = UNSAFE_CHECK(checkIndex) "unsafeRead" i (length v)
               $ basicUnsafeRead v i

-- | Replace the element at the given position. No bounds checks are performed.
unsafeWrite :: (PrimMonad m, MVector v a)
                                => v (PrimState m) a -> Int -> a -> m ()
{-# INLINE unsafeWrite #-}
unsafeWrite v i x = UNSAFE_CHECK(checkIndex) "unsafeWrite" i (length v)
                  $ basicUnsafeWrite v i x


-- | Copy a vector. The two vectors must have the same length and may not
-- overlap. This is not checked.
unsafeCopy :: (PrimMonad m, MVector v a) => v (PrimState m) a   -- ^ target
                                         -> v (PrimState m) a   -- ^ source
                                         -> m ()
{-# INLINE unsafeCopy #-}
unsafeCopy dst src = UNSAFE_CHECK(check) "unsafeCopy" "length mismatch"
                                         (length dst == length src)
                   $ UNSAFE_CHECK(check) "unsafeCopy" "overlapping vectors"
                                         (not (dst `overlaps` src))
                   $ basicUnsafeCopy dst src

-- | Grow a vector by the given number of elements. The number must be
-- positive but this is not checked.
unsafeGrow :: (PrimMonad m, MVector v a)
                        => v (PrimState m) a -> Int -> m (v (PrimState m) a)
{-# INLINE unsafeGrow #-}
unsafeGrow v n = UNSAFE_CHECK(checkLength) "unsafeGrow" n
               $ basicUnsafeGrow v n

-- | Length of the mutable vector.
length :: MVector v a => v s a -> Int
{-# INLINE length #-}
length = basicLength

-- Check whether two vectors overlap.
overlaps :: MVector v a => v s a -> v s a -> Bool
{-# INLINE overlaps #-}
overlaps = basicOverlaps

-- | Yield a part of the mutable vector without copying it.
slice :: MVector v a => v s a -> Int -> Int -> v s a
{-# INLINE slice #-}
slice v i n = BOUNDS_CHECK(checkSlice) "slice" i n (length v)
            $ unsafeSlice v i n

-- | Create a mutable vector of the given length.
new :: (PrimMonad m, MVector v a) => Int -> m (v (PrimState m) a)
{-# INLINE new #-}
new n = BOUNDS_CHECK(checkLength) "new" n
      $ unsafeNew n

-- | Create a mutable vector of the given length and fill it with an
-- initial value.
newWith :: (PrimMonad m, MVector v a) => Int -> a -> m (v (PrimState m) a)
{-# INLINE newWith #-}
newWith n x = BOUNDS_CHECK(checkLength) "newWith" n
            $ unsafeNewWith n x

-- | Yield the element at the given position.
read :: (PrimMonad m, MVector v a) => v (PrimState m) a -> Int -> m a
{-# INLINE read #-}
read v i = BOUNDS_CHECK(checkIndex) "read" i (length v)
         $ unsafeRead v i

-- | Replace the element at the given position.
write :: (PrimMonad m, MVector v a) => v (PrimState m) a -> Int -> a -> m ()
{-# INLINE write #-}
write v i x = BOUNDS_CHECK(checkIndex) "write" i (length v)
            $ unsafeWrite v i x

-- | Reset all elements of the vector to some undefined value, clearing all
-- references to external objects. This is usually a noop for unboxed vectors. 
clear :: (PrimMonad m, MVector v a) => v (PrimState m) a -> m ()
{-# INLINE clear #-}
clear = basicClear

-- | Set all elements of the vector to the given value.
set :: (PrimMonad m, MVector v a) => v (PrimState m) a -> a -> m ()
{-# INLINE set #-}
set = basicSet

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap.
copy :: (PrimMonad m, MVector v a)
                => v (PrimState m) a -> v (PrimState m) a -> m ()
{-# INLINE copy #-}
copy dst src = BOUNDS_CHECK(check) "copy" "overlapping vectors"
                                          (not (dst `overlaps` src))
             $ BOUNDS_CHECK(check) "copy" "length mismatch"
                                          (length dst == length src)
             $ unsafeCopy dst src

-- | Grow a vector by the given number of elements. The number must be
-- positive.
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

munstream :: (PrimMonad m, MVector v a)
           => v (PrimState m) a -> MStream m a -> m (v (PrimState m) a)
{-# INLINE munstream #-}
munstream v s = v `seq` do
                          n' <- MStream.foldM put 0 s
                          return $ unsafeSlice v 0 n'
  where
    {-# INLINE_INNER put #-}
    put i x = do
                INTERNAL_CHECK(checkIndex) "munstream" i (length v)
                  $ unsafeWrite v i x
                return (i+1)

transform :: (PrimMonad m, MVector v a)
  => (MStream m a -> MStream m a) -> v (PrimState m) a -> m (v (PrimState m) a)
{-# INLINE_STREAM transform #-}
transform f v = munstream v (f (mstream v))

mrstream :: (PrimMonad m, MVector v a) => v (PrimState m) a -> MStream m a
{-# INLINE mrstream #-}
mrstream v = v `seq` (MStream.unfoldrM get n `MStream.sized` Exact n)
  where
    n = length v

    {-# INLINE_INNER get #-}
    get i | j >= 0    = do x <- unsafeRead v j
                           return $ Just (x,j)
          | otherwise = return Nothing
      where
        j = i-1

mrunstream :: (PrimMonad m, MVector v a)
           => v (PrimState m) a -> MStream m a -> m (v (PrimState m) a)
{-# INLINE mrunstream #-}
mrunstream v s = v `seq` do
                           i <- MStream.foldM put n s
                           return $ unsafeSlice v i (n-i)
  where
    n = length v

    {-# INLINE_INNER put #-}
    put i x = do
                unsafeWrite v j x
                return j
      where
        j = i-1

rtransform :: (PrimMonad m, MVector v a)
  => (MStream m a -> MStream m a) -> v (PrimState m) a -> m (v (PrimState m) a)
{-# INLINE_STREAM rtransform #-}
rtransform f v = mrunstream v (f (mrstream v))

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

unsafeAccum :: (PrimMonad m, MVector v a)
            => (a -> b -> a) -> v (PrimState m) a -> Stream (Int, b) -> m ()
{-# INLINE unsafeAccum #-}
unsafeAccum f !v s = Stream.mapM_ upd s
  where
    {-# INLINE_INNER upd #-}
    upd (i,b) = do
                  a <- UNSAFE_CHECK(checkIndex) "accum" i (length v)
                     $ unsafeRead v i
                  unsafeWrite v i (f a b)

accum :: (PrimMonad m, MVector v a)
        => (a -> b -> a) -> v (PrimState m) a -> Stream (Int, b) -> m ()
{-# INLINE accum #-}
accum f !v s = Stream.mapM_ upd s
  where
    {-# INLINE_INNER upd #-}
    upd (i,b) = do
                  a <- BOUNDS_CHECK(checkIndex) "accum" i (length v)
                     $ unsafeRead v i
                  unsafeWrite v i (f a b)

unsafeUpdate :: (PrimMonad m, MVector v a)
                        => v (PrimState m) a -> Stream (Int, a) -> m ()
{-# INLINE unsafeUpdate #-}
unsafeUpdate = unsafeAccum (const id)

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

