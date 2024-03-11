{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Data.Vector.Generic.Mutable
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
-- Generic interface to mutable vectors.

module Data.Vector.Generic.Mutable (
  -- * Class of mutable vector types
  MVector(..),

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
  growFront, unsafeGrowFront,

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

  -- * Internal operations
  mstream, mstreamR,
  unstream, unstreamR, vunstream,
  munstream, munstreamR,
  transform, transformR,
  fill, fillR,
  unsafeAccum, accum, unsafeUpdate, update, reverse,
  unstablePartition, unstablePartitionBundle, partitionBundle,
  partitionWithBundle,
  -- * Re-exports
  PrimMonad, PrimState, RealWorld
) where

import           Data.Vector.Generic.Mutable.Base
import qualified Data.Vector.Generic.Base as V

import qualified Data.Vector.Fusion.Bundle      as Bundle
import           Data.Vector.Fusion.Bundle      ( Bundle, MBundle, Chunk(..) )
import qualified Data.Vector.Fusion.Bundle.Monadic as MBundle
import           Data.Vector.Fusion.Stream.Monadic ( Stream )
import qualified Data.Vector.Fusion.Stream.Monadic as Stream
import           Data.Vector.Fusion.Bundle.Size
import           Data.Vector.Fusion.Util        ( delay_inline )
import           Data.Vector.Internal.Check

import Control.Monad.Primitive ( PrimMonad(..), RealWorld, stToPrim )

import Prelude
  ( Ord, Monad, Bool(..), Int, Maybe(..), Either(..)
  , return, otherwise, flip, const, seq, min, max, not, pure
  , (>>=), (+), (-), (<), (<=), (>=), (==), (/=), (.), ($), (=<<), (>>), (<$>) )

#include "vector.h"


-- ------------------
-- Internal functions
-- ------------------

unsafeAppend1 :: (PrimMonad m, MVector v a)
        => v (PrimState m) a -> Int -> a -> m (v (PrimState m) a)
{-# INLINE_INNER unsafeAppend1 #-}
    -- NOTE: The case distinction has to be on the outside because
    -- GHC creates a join point for the unsafeWrite even when everything
    -- is inlined. This is bad because with the join point, v isn't getting
    -- unboxed.
unsafeAppend1 v i x
  | i < length v = do
                     unsafeWrite v i x
                     return v
  | otherwise    = do
                     v' <- enlarge v
                     checkIndex Internal i (length v') $ unsafeWrite v' i x
                     return v'

unsafePrepend1 :: (PrimMonad m, MVector v a)
        => v (PrimState m) a -> Int -> a -> m (v (PrimState m) a, Int)
{-# INLINE_INNER unsafePrepend1 #-}
unsafePrepend1 v i x
  | i /= 0    = do
                  let i' = i-1
                  unsafeWrite v i' x
                  return (v, i')
  | otherwise = do
                  (v', j) <- enlargeFront v
                  let i' = j-1
                  checkIndex Internal i' (length v') $ unsafeWrite v' i' x
                  return (v', i')

mstream :: (PrimMonad m, MVector v a) => v (PrimState m) a -> Stream m a
{-# INLINE mstream #-}
mstream v = v `seq` n `seq` Stream.unfoldrM get 0
  where
    n = length v

    {-# INLINE_INNER get #-}
    get i | i < n     = do x <- unsafeRead v i
                           return $ Just (x, i+1)
          | otherwise = return Nothing

fill :: (PrimMonad m, MVector v a)
     => v (PrimState m) a -> Stream m a -> m (v (PrimState m) a)
{-# INLINE fill #-}
fill v s = v `seq` do
                     n' <- Stream.foldM put 0 s
                     return $ unsafeSlice 0 n' v
  where
    {-# INLINE_INNER put #-}
    put i x = do
                checkIndex Internal i (length v) $ unsafeWrite v i x
                return (i+1)

transform
  :: (PrimMonad m, MVector v a)
  => (Stream m a -> Stream m a) -> v (PrimState m) a -> m (v (PrimState m) a)
{-# INLINE_FUSED transform #-}
transform f v = fill v (f (mstream v))

mstreamR :: (PrimMonad m, MVector v a) => v (PrimState m) a -> Stream m a
{-# INLINE mstreamR #-}
mstreamR v = v `seq` n `seq` Stream.unfoldrM get n
  where
    n = length v

    {-# INLINE_INNER get #-}
    get i | j >= 0    = do x <- unsafeRead v j
                           return $ Just (x,j)
          | otherwise = return Nothing
      where
        j = i-1

fillR :: (PrimMonad m, MVector v a)
      => v (PrimState m) a -> Stream m a -> m (v (PrimState m) a)
{-# INLINE fillR #-}
fillR v s = v `seq` do
                      i <- Stream.foldM put n s
                      return $ unsafeSlice i (n-i) v
  where
    n = length v

    {-# INLINE_INNER put #-}
    put i x = do
                unsafeWrite v j x
                return j
      where
        j = i-1

transformR
  :: (PrimMonad m, MVector v a)
  => (Stream m a -> Stream m a) -> v (PrimState m) a -> m (v (PrimState m) a)
{-# INLINE_FUSED transformR #-}
transformR f v = fillR v (f (mstreamR v))

-- | Create a new mutable vector and fill it with elements from the 'Bundle'.
-- The vector will grow exponentially if the maximum size of the 'Bundle' is
-- unknown.
unstream :: (PrimMonad m, MVector v a)
         => Bundle u a -> m (v (PrimState m) a)
-- NOTE: replace INLINE_FUSED by INLINE? (also in unstreamR)
{-# INLINE_FUSED unstream #-}
unstream s = munstream (Bundle.lift s)

-- | Create a new mutable vector and fill it with elements from the monadic
-- stream. The vector will grow exponentially if the maximum size of the stream
-- is unknown.
munstream :: (PrimMonad m, MVector v a)
          => MBundle m u a -> m (v (PrimState m) a)
{-# INLINE_FUSED munstream #-}
munstream s = case upperBound (MBundle.size s) of
               Just n  -> munstreamMax     s n
               Nothing -> munstreamUnknown s

munstreamMax :: (PrimMonad m, MVector v a)
             => MBundle m u a -> Int -> m (v (PrimState m) a)
{-# INLINE munstreamMax #-}
munstreamMax s n
  = do
      v <- checkLength Internal n $ unsafeNew n
      let put i x = do
                       checkIndex Internal i n $ unsafeWrite v i x
                       return (i+1)
      n' <- MBundle.foldM' put 0 s
      return $ checkSlice Internal 0 n' n
             $ unsafeSlice 0 n' v

munstreamUnknown :: (PrimMonad m, MVector v a)
                 => MBundle m u a -> m (v (PrimState m) a)
{-# INLINE munstreamUnknown #-}
munstreamUnknown s
  = do
      v <- unsafeNew 0
      (v', n) <- MBundle.foldM put (v, 0) s
      return $ checkSlice Internal 0 n (length v')
             $ unsafeSlice 0 n v'
  where
    {-# INLINE_INNER put #-}
    put (v,i) x = do
                    v' <- unsafeAppend1 v i x
                    return (v',i+1)


-- | Create a new mutable vector and fill it with elements from the 'Bundle'.
-- The vector will grow exponentially if the maximum size of the 'Bundle' is
-- unknown.
vunstream :: (PrimMonad m, V.Vector v a)
         => Bundle v a -> m (V.Mutable v (PrimState m) a)
-- NOTE: replace INLINE_FUSED by INLINE? (also in unstreamR)
{-# INLINE_FUSED vunstream #-}
vunstream s = vmunstream (Bundle.lift s)

-- | Create a new mutable vector and fill it with elements from the monadic
-- stream. The vector will grow exponentially if the maximum size of the stream
-- is unknown.
vmunstream :: (PrimMonad m, V.Vector v a)
           => MBundle m v a -> m (V.Mutable v (PrimState m) a)
{-# INLINE_FUSED vmunstream #-}
vmunstream s = case upperBound (MBundle.size s) of
               Just n  -> vmunstreamMax     s n
               Nothing -> vmunstreamUnknown s

vmunstreamMax :: (PrimMonad m, V.Vector v a)
              => MBundle m v a -> Int -> m (V.Mutable v (PrimState m) a)
{-# INLINE vmunstreamMax #-}
vmunstreamMax s n
  = do
      v <- checkLength Internal n $ unsafeNew n
      let {-# INLINE_INNER copyChunk #-}
          copyChunk i (Chunk m f) =
            checkSlice Internal i m (length v) $ do
              f (basicUnsafeSlice i m v)
              return (i+m)

      n' <- Stream.foldlM' copyChunk 0 (MBundle.chunks s)
      return $ checkSlice Internal 0 n' n
             $ unsafeSlice 0 n' v

vmunstreamUnknown :: (PrimMonad m, V.Vector v a)
                 => MBundle m v a -> m (V.Mutable v (PrimState m) a)
{-# INLINE vmunstreamUnknown #-}
vmunstreamUnknown s
  = do
      v <- unsafeNew 0
      (v', n) <- Stream.foldlM copyChunk (v,0) (MBundle.chunks s)
      return $ checkSlice Internal 0 n (length v')
             $ unsafeSlice 0 n v'
  where
    {-# INLINE_INNER copyChunk #-}
    copyChunk (v,i) (Chunk n f)
      = do
          let j = i+n
          v' <- if basicLength v < j
                  then unsafeGrow v (delay_inline max (enlarge_delta v) (j - basicLength v))
                  else return v
          checkSlice Internal i n (length v') $ f (basicUnsafeSlice i n v')
          return (v',j)


-- | Create a new mutable vector and fill it with elements from the 'Bundle'
-- from right to left. The vector will grow exponentially if the maximum size
-- of the 'Bundle' is unknown.
unstreamR :: (PrimMonad m, MVector v a)
          => Bundle u a -> m (v (PrimState m) a)
-- NOTE: replace INLINE_FUSED by INLINE? (also in unstream)
{-# INLINE_FUSED unstreamR #-}
unstreamR s = munstreamR (Bundle.lift s)

-- | Create a new mutable vector and fill it with elements from the monadic
-- stream from right to left. The vector will grow exponentially if the maximum
-- size of the stream is unknown.
munstreamR :: (PrimMonad m, MVector v a)
           => MBundle m u a -> m (v (PrimState m) a)
{-# INLINE_FUSED munstreamR #-}
munstreamR s = case upperBound (MBundle.size s) of
               Just n  -> munstreamRMax     s n
               Nothing -> munstreamRUnknown s

munstreamRMax :: (PrimMonad m, MVector v a)
              => MBundle m u a -> Int -> m (v (PrimState m) a)
{-# INLINE munstreamRMax #-}
munstreamRMax s n
  = do
      v <- checkLength Internal n $ unsafeNew n
      let put i x = do
                      let i' = i-1
                      checkIndex Internal i' n
                        $ unsafeWrite v i' x
                      return i'
      i <- MBundle.foldM' put n s
      return $ checkSlice Internal i (n-i) n
             $ unsafeSlice i (n-i) v

munstreamRUnknown :: (HasCallStack, PrimMonad m, MVector v a)
                  => MBundle m u a -> m (v (PrimState m) a)
{-# INLINE munstreamRUnknown #-}
munstreamRUnknown s
  = do
      v <- unsafeNew 0
      (v', i) <- MBundle.foldM put (v, 0) s
      let n = length v'
      return $ checkSlice Internal i (n-i) n
             $ unsafeSlice i (n-i) v'
  where
    {-# INLINE_INNER put #-}
    put (v,i) x = unsafePrepend1 v i x

-- Length
-- ------

-- | Length of the mutable vector.
length :: MVector v a => v s a -> Int
{-# INLINE length #-}
length = basicLength

-- | Check whether the vector is empty.
null :: MVector v a => v s a -> Bool
{-# INLINE null #-}
null v = length v == 0

-- Extracting subvectors
-- ---------------------

-- | Yield a part of the mutable vector without copying it. The vector must
-- contain at least @i+n@ elements.
slice :: (HasCallStack, MVector v a)
      => Int  -- ^ @i@ starting index
      -> Int  -- ^ @n@ length
      -> v s a
      -> v s a
{-# INLINE slice #-}
slice i n v = checkSlice Bounds i n (length v) $ unsafeSlice i n v

-- | Take the @n@ first elements of the mutable vector without making a
-- copy. For negative @n@, the empty vector is returned. If @n@ is larger
-- than the vector's length, the vector is returned unchanged.
take :: MVector v a => Int -> v s a -> v s a
{-# INLINE take #-}
take n v = unsafeSlice 0 (min (max n 0) (length v)) v

-- | Drop the @n@ first element of the mutable vector without making a
-- copy. For negative @n@, the vector is returned unchanged. If @n@ is
-- larger than the vector's length, the empty vector is returned.
drop :: MVector v a => Int -> v s a -> v s a
{-# INLINE drop #-}
drop n v = unsafeSlice (min m n') (max 0 (m - n')) v
  where
    n' = max n 0
    m  = length v

-- | /O(1)/ Split the mutable vector into the first @n@ elements
-- and the remainder, without copying.
--
-- Note that @'splitAt' n v@ is equivalent to @('take' n v, 'drop' n v)@,
-- but slightly more efficient.
splitAt :: MVector v a => Int -> v s a -> (v s a, v s a)
{-# INLINE splitAt #-}
splitAt n v = ( unsafeSlice 0 m v
              , unsafeSlice m (max 0 (len - n')) v
              )
    where
      m   = min n' len
      n'  = max n 0
      len = length v

-- | Drop the last element of the mutable vector without making a copy.
-- If the vector is empty, an exception is thrown.
init :: MVector v a => v s a -> v s a
{-# INLINE init #-}
init v = slice 0 (length v - 1) v

-- | Drop the first element of the mutable vector without making a copy.
-- If the vector is empty, an exception is thrown.
tail :: MVector v a => v s a -> v s a
{-# INLINE tail #-}
tail v = slice 1 (length v - 1) v

-- | Yield a part of the mutable vector without copying it. No bounds checks
-- are performed.
unsafeSlice :: MVector v a => Int  -- ^ starting index
                           -> Int  -- ^ length of the slice
                           -> v s a
                           -> v s a
{-# INLINE unsafeSlice #-}
unsafeSlice i n v = checkSlice Unsafe i n (length v)
                  $ basicUnsafeSlice i n v

-- | Same as 'init', but doesn't do range checks.
unsafeInit :: MVector v a => v s a -> v s a
{-# INLINE unsafeInit #-}
unsafeInit v = unsafeSlice 0 (length v - 1) v

-- | Same as 'tail', but doesn't do range checks.
unsafeTail :: MVector v a => v s a -> v s a
{-# INLINE unsafeTail #-}
unsafeTail v = unsafeSlice 1 (length v - 1) v

-- | Unsafe variant of 'take'. If @n@ is out of range, it will
-- simply create an invalid slice that likely violate memory safety.
unsafeTake :: MVector v a => Int -> v s a -> v s a
{-# INLINE unsafeTake #-}
unsafeTake n v = unsafeSlice 0 n v

-- | Unsafe variant of 'drop'. If @n@ is out of range, it will
-- simply create an invalid slice that likely violate memory safety.
unsafeDrop :: MVector v a => Int -> v s a -> v s a
{-# INLINE unsafeDrop #-}
unsafeDrop n v = unsafeSlice n (length v - n) v

-- Overlapping
-- -----------

-- | Check whether two vectors overlap.
overlaps :: MVector v a => v s a -> v s a -> Bool
{-# INLINE overlaps #-}
overlaps = basicOverlaps

-- Initialisation
-- --------------

-- | Create a mutable vector of the given length.
new :: (HasCallStack, PrimMonad m, MVector v a) => Int -> m (v (PrimState m) a)
{-# INLINE new #-}
new n = checkLength Bounds n $ stToPrim
      $ unsafeNew n >>= \v -> basicInitialize v >> return v

-- | Create a mutable vector of the given length. The vector content
-- should be assumed to be uninitialized. However, the exact semantics depend
-- on the vector implementation. For example, unboxed and storable
-- vectors will create a vector filled with whatever the underlying memory
-- buffer happens to contain, while boxed vector's elements are
-- initialized to bottoms which will throw exception when evaluated.
--
-- @since 0.4
unsafeNew :: (PrimMonad m, MVector v a) => Int -> m (v (PrimState m) a)
{-# INLINE unsafeNew #-}
unsafeNew n = checkLength Unsafe n $ stToPrim $ basicUnsafeNew n

-- | Create a mutable vector of the given length (0 if the length is negative)
-- and fill it with an initial value.
replicate :: (PrimMonad m, MVector v a) => Int -> a -> m (v (PrimState m) a)
{-# INLINE replicate #-}
replicate n x = stToPrim $ basicUnsafeReplicate (delay_inline max 0 n) x

-- | Create a mutable vector of the given length (0 if the length is negative)
-- and fill it with values produced by repeatedly executing the monadic action.
replicateM :: (PrimMonad m, MVector v a) => Int -> m a -> m (v (PrimState m) a)
{-# INLINE replicateM #-}
replicateM n m = munstream (MBundle.replicateM n m)

-- | /O(n)/ Create a mutable vector of the given length (0 if the length is negative)
-- and fill it with the results of applying the function to each index.
-- Iteration starts at index 0.
--
-- @since 0.12.3.0
generate :: (PrimMonad m, MVector v a) => Int -> (Int -> a) -> m (v (PrimState m) a)
{-# INLINE generate #-}
generate n f = stToPrim $ generateM n (return . f)

-- | /O(n)/ Create a mutable vector of the given length (0 if the length is
-- negative) and fill it with the results of applying the monadic function to each
-- index. Iteration starts at index 0.
--
-- @since 0.12.3.0
generateM :: (PrimMonad m, MVector v a) => Int -> (Int -> m a) -> m (v (PrimState m) a)
{-# INLINE generateM #-}
generateM n f = munstream (MBundle.generateM n f)

-- | Create a copy of a mutable vector.
clone :: (PrimMonad m, MVector v a) => v (PrimState m) a -> m (v (PrimState m) a)
{-# INLINE clone #-}
clone v = do
            v' <- unsafeNew (length v)
            unsafeCopy v' v
            return v'

-- Growing
-- -------

-- | Grow a vector by the given number of elements. The number must not be
-- negative, otherwise an exception is thrown. The semantics of this function
-- are exactly the same as of 'unsafeGrow', except that it will initialize the newly
-- allocated memory first.
--
-- It is important to note that mutating the returned vector will not affect the
-- vector that was used as a source. In other words, it does not, nor will it
-- ever have the semantics of @realloc@ from C.
--
-- > grow mv 0 === clone mv
--
-- @since 0.4.0
grow :: (HasCallStack, PrimMonad m, MVector v a)
     => v (PrimState m) a -> Int -> m (v (PrimState m) a)
{-# INLINE grow #-}
grow v by = checkLength Bounds by
          $ stToPrim
          $ do vnew <- unsafeGrow v by
               basicInitialize $ basicUnsafeSlice (length v) by vnew
               return vnew

-- | Same as 'grow', except that it copies data towards the end of the newly
-- allocated vector, making extra space available at the beginning.
--
-- @since 0.11.0.0
growFront :: (HasCallStack, PrimMonad m, MVector v a)
          => v (PrimState m) a -> Int -> m (v (PrimState m) a)
{-# INLINE growFront #-}
growFront v by = checkLength Bounds by
               $ stToPrim
               $ do vnew <- unsafeGrowFront v by
                    basicInitialize $ basicUnsafeSlice 0 by vnew
                    return vnew

enlarge_delta :: MVector v a => v s a -> Int
enlarge_delta v = max (length v) 1

-- | Grow a vector logarithmically.
enlarge :: (PrimMonad m, MVector v a)
        => v (PrimState m) a -> m (v (PrimState m) a)
{-# INLINE enlarge #-}
enlarge v = stToPrim $ do
  vnew <- unsafeGrow v by
  basicInitialize $ basicUnsafeSlice (length v) by vnew
  return vnew
  where
    by = enlarge_delta v

enlargeFront :: (PrimMonad m, MVector v a)
             => v (PrimState m) a -> m (v (PrimState m) a, Int)
{-# INLINE enlargeFront #-}
enlargeFront v = stToPrim $ do
                   v' <- unsafeGrowFront v by
                   basicInitialize $ basicUnsafeSlice 0 by v'
                   return (v', by)
  where
    by = enlarge_delta v

-- | Grow a vector by allocating a new mutable vector of the same size plus the
-- the given number of elements and copying all the data over to the new vector,
-- starting at its beginning. The newly allocated memory is not initialized and
-- the extra space at the end will likely contain garbage data or bottoms.
-- Use 'unsafeGrowFront' to make the extra space available in the front
-- of the new vector.
--
-- It is important to note that mutating the returned vector will not affect
-- elements of the vector that was used as a source. In other words, it does not,
-- nor will it ever have the semantics of @realloc@ from C. Keep in mind,
-- however, that values themselves can be of a mutable type
-- (eg. 'Foreign.Ptr.Ptr'), in which case it would be possible to affect values
-- stored in both vectors.
--
-- > unsafeGrow mv 0 === clone mv
--
-- @since 0.4.0
unsafeGrow
  :: (PrimMonad m, MVector v a)
  => v (PrimState m) a
  -- ^ mutable vector to copy from
  -> Int
  -- ^ number of elements to grow the vector by (must be non-negative, but
  -- this is not checked)
  -> m (v (PrimState m) a)
{-# INLINE unsafeGrow #-}
unsafeGrow v n = checkLength Unsafe n
               $ stToPrim
               $ basicUnsafeGrow v n

-- | Same as 'unsafeGrow', except that it copies data towards the end of the
-- newly allocated vector, making extra space available at the beginning.
--
-- @since 0.11.0.0
unsafeGrowFront :: (PrimMonad m, MVector v a)
                => v (PrimState m) a -> Int -> m (v (PrimState m) a)
{-# INLINE unsafeGrowFront #-}
unsafeGrowFront v by = checkLength Unsafe by $ stToPrim $ do
                         let n = length v
                         v' <- basicUnsafeNew (by+n)
                         basicUnsafeCopy (basicUnsafeSlice by n v') v
                         return v'

-- Restricting memory usage
-- ------------------------

-- | Reset all elements of the vector to some undefined value, clearing all
-- references to external objects. This is usually a noop for unboxed vectors.
clear :: (PrimMonad m, MVector v a) => v (PrimState m) a -> m ()
{-# INLINE clear #-}
clear = stToPrim . basicClear

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
read :: (HasCallStack, PrimMonad m, MVector v a) => v (PrimState m) a -> Int -> m a
{-# INLINE read #-}
read v i = checkIndex Bounds i (length v)
         $ unsafeRead v i

-- | Yield the element at the given position. Returns 'Nothing' if
-- the index is out of range.
--
-- @since 0.13
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector.Mutable as MV
-- >>> v <- MV.generate 10 (\x -> x*x)
-- >>> MV.readMaybe v 3
-- Just 9
-- >>> MV.readMaybe v 13
-- Nothing
readMaybe :: (PrimMonad m, MVector v a) => v (PrimState m) a -> Int -> m (Maybe a)
{-# INLINE readMaybe #-}
readMaybe v i | i `inRange` (length v) = Just <$> unsafeRead v i
              | otherwise              = pure Nothing

-- | Replace the element at the given position.
write :: (HasCallStack, PrimMonad m, MVector v a) => v (PrimState m) a -> Int -> a -> m ()
{-# INLINE write #-}
write v i x = checkIndex Bounds i (length v)
            $ unsafeWrite v i x

-- | Modify the element at the given position.
modify :: (HasCallStack, PrimMonad m, MVector v a) => v (PrimState m) a -> (a -> a) -> Int -> m ()
{-# INLINE modify #-}
modify v f i = checkIndex Bounds i (length v)
             $ unsafeModify v f i

-- | Modify the element at the given position using a monadic function.
--
-- @since 0.12.3.0
modifyM :: (HasCallStack, PrimMonad m, MVector v a) => v (PrimState m) a -> (a -> m a) -> Int -> m ()
{-# INLINE modifyM #-}
modifyM v f i = checkIndex Bounds i (length v)
              $ unsafeModifyM v f i

-- | Swap the elements at the given positions.
swap :: (HasCallStack, PrimMonad m, MVector v a) => v (PrimState m) a -> Int -> Int -> m ()
{-# INLINE swap #-}
swap v i j = checkIndex Bounds i (length v)
           $ checkIndex Bounds j (length v)
           $ unsafeSwap v i j

-- | Replace the element at the given position and return the old element.
exchange :: (HasCallStack, PrimMonad m, MVector v a) => v (PrimState m) a -> Int -> a -> m a
{-# INLINE exchange #-}
exchange v i x = checkIndex Bounds i (length v) $ unsafeExchange v i x

-- | Yield the element at the given position. No bounds checks are performed.
unsafeRead :: (PrimMonad m, MVector v a) => v (PrimState m) a -> Int -> m a
{-# INLINE unsafeRead #-}
unsafeRead v !i = checkIndex Unsafe i (length v)
               $ stToPrim
               $ basicUnsafeRead v i
-- Why do we need ! before i?
-- The reason is that 'basicUnsafeRead' is a class member and, unless 'unsafeRead' was
-- already specialised to a specific v, GHC has no clue that i is most certainly
-- to be used eagerly. Bang before i hints this vital for optimizer information.

-- | Replace the element at the given position. No bounds checks are performed.
unsafeWrite :: (PrimMonad m, MVector v a) => v (PrimState m) a -> Int -> a -> m ()
{-# INLINE unsafeWrite #-}
unsafeWrite v !i x = checkIndex Unsafe i (length v)
                  $ stToPrim
                  $ basicUnsafeWrite v i x

-- | Modify the element at the given position. No bounds checks are performed.
unsafeModify :: (PrimMonad m, MVector v a) => v (PrimState m) a -> (a -> a) -> Int -> m ()
{-# INLINE unsafeModify #-}
unsafeModify v f !i = checkIndex Unsafe i (length v)
                   $ stToPrim
                   $ basicUnsafeRead v i >>= \x ->
                     basicUnsafeWrite v i (f x)

-- | Modify the element at the given position using a monadic
-- function. No bounds checks are performed.
--
-- @since 0.12.3.0
unsafeModifyM :: (PrimMonad m, MVector v a) => v (PrimState m) a -> (a -> m a) -> Int -> m ()
{-# INLINE unsafeModifyM #-}
unsafeModifyM v f !i = checkIndex Unsafe i (length v)
                    $ stToPrim . basicUnsafeWrite v i =<< f =<< stToPrim (basicUnsafeRead v i)

-- | Swap the elements at the given positions. No bounds checks are performed.
unsafeSwap :: (PrimMonad m, MVector v a) => v (PrimState m) a -> Int -> Int -> m ()
{-# INLINE unsafeSwap #-}
unsafeSwap v i j = checkIndex Unsafe i (length v)
                 $ checkIndex Unsafe j (length v)
                 $ stToPrim $ do
                     x <- unsafeRead v i
                     y <- unsafeRead v j
                     unsafeWrite v i y
                     unsafeWrite v j x

-- | Replace the element at the given position and return the old element. No
-- bounds checks are performed.
unsafeExchange :: (PrimMonad m, MVector v a) => v (PrimState m) a -> Int -> a -> m a
{-# INLINE unsafeExchange #-}
unsafeExchange v i x = checkIndex Unsafe i (length v) $ stToPrim $ do
                         y <- unsafeRead v i
                         unsafeWrite v i x
                         return y

-- Folds
-- -----

forI_ :: (Monad m, MVector v a) => v (PrimState m) a -> (Int -> m b) -> m ()
{-# INLINE forI_ #-}
forI_ v f = loop 0
  where
    loop i | i >= n    = return ()
           | otherwise = f i >> loop (i + 1)
    n = length v

-- | /O(n)/ Apply the monadic action to every element of the vector, discarding the results.
--
-- @since 0.12.3.0
mapM_ :: (PrimMonad m, MVector v a) => (a -> m b) -> v (PrimState m) a -> m ()
{-# INLINE mapM_ #-}
mapM_ f v = forI_ v $ \i -> f =<< unsafeRead v i

-- | /O(n)/ Apply the monadic action to every element of the vector and its index, discarding the results.
--
-- @since 0.12.3.0
imapM_ :: (PrimMonad m, MVector v a) => (Int -> a -> m b) -> v (PrimState m) a -> m ()
{-# INLINE imapM_ #-}
imapM_ f v = forI_ v $ \i -> f i =<< unsafeRead v i

-- | /O(n)/ Apply the monadic action to every element of the vector,
-- discarding the results. It's the same as @flip mapM_@.
--
-- @since 0.12.3.0
forM_ :: (PrimMonad m, MVector v a) => v (PrimState m) a -> (a -> m b) -> m ()
{-# INLINE forM_ #-}
forM_ = flip mapM_

-- | /O(n)/ Apply the monadic action to every element of the vector
-- and its index, discarding the results. It's the same as @flip imapM_@.
--
-- @since 0.12.3.0
iforM_ :: (PrimMonad m, MVector v a) => v (PrimState m) a -> (Int -> a -> m b) -> m ()
{-# INLINE iforM_ #-}
iforM_ = flip imapM_

-- | /O(n)/ Pure left fold.
--
-- @since 0.12.3.0
foldl :: (PrimMonad m, MVector v a) => (b -> a -> b) -> b -> v (PrimState m) a -> m b
{-# INLINE foldl #-}
foldl f = ifoldl (\b _ -> f b)

-- | /O(n)/ Pure left fold with strict accumulator.
--
-- @since 0.12.3.0
foldl' :: (PrimMonad m, MVector v a) => (b -> a -> b) -> b -> v (PrimState m) a -> m b
{-# INLINE foldl' #-}
foldl' f = ifoldl' (\b _ -> f b)

-- | /O(n)/ Pure left fold using a function applied to each element and its index.
--
-- @since 0.12.3.0
ifoldl :: (PrimMonad m, MVector v a) => (b -> Int -> a -> b) -> b -> v (PrimState m) a -> m b
{-# INLINE ifoldl #-}
ifoldl f b0 v = stToPrim $ ifoldM (\b i a -> return $ f b i a) b0 v

-- | /O(n)/ Pure left fold with strict accumulator using a function applied to each element and its index.
--
-- @since 0.12.3.0
ifoldl' :: (PrimMonad m, MVector v a) => (b -> Int -> a -> b) -> b -> v (PrimState m) a -> m b
{-# INLINE ifoldl' #-}
ifoldl' f b0 v = stToPrim $ ifoldM' (\b i a -> return $ f b i a) b0 v

-- | /O(n)/ Pure right fold.
--
-- @since 0.12.3.0
foldr :: (PrimMonad m, MVector v a) => (a -> b -> b) -> b -> v (PrimState m) a -> m b
{-# INLINE foldr #-}
foldr f = ifoldr (const f)

-- | /O(n)/ Pure right fold with strict accumulator.
--
-- @since 0.12.3.0
foldr' :: (PrimMonad m, MVector v a) => (a -> b -> b) -> b -> v (PrimState m) a -> m b
{-# INLINE foldr' #-}
foldr' f = ifoldr' (const f)

-- | /O(n)/ Pure right fold using a function applied to each element and its index.
--
-- @since 0.12.3.0
ifoldr :: (PrimMonad m, MVector v a) => (Int -> a -> b -> b) -> b -> v (PrimState m) a -> m b
{-# INLINE ifoldr #-}
ifoldr f b0 v = stToPrim $ ifoldrM (\i a b -> return $ f i a b) b0 v

-- | /O(n)/ Pure right fold with strict accumulator using a function applied
-- to each element and its index.
--
-- @since 0.12.3.0
ifoldr' :: (PrimMonad m, MVector v a) => (Int -> a -> b -> b) -> b -> v (PrimState m) a -> m b
{-# INLINE ifoldr' #-}
ifoldr' f b0 v = stToPrim $ ifoldrM' (\i a b -> return $ f i a b) b0 v

-- | /O(n)/ Monadic fold.
--
-- @since 0.12.3.0
foldM :: (PrimMonad m, MVector v a) => (b -> a -> m b) -> b -> v (PrimState m) a -> m b
{-# INLINE foldM #-}
foldM f = ifoldM (\x _ -> f x)

-- | /O(n)/ Monadic fold with strict accumulator.
--
-- @since 0.12.3.0
foldM' :: (PrimMonad m, MVector v a) => (b -> a -> m b) -> b -> v (PrimState m) a -> m b
{-# INLINE foldM' #-}
foldM' f = ifoldM' (\x _ -> f x)

-- | /O(n)/ Monadic fold using a function applied to each element and its index.
--
-- @since 0.12.3.0
ifoldM :: (PrimMonad m, MVector v a) => (b -> Int -> a -> m b) -> b -> v (PrimState m) a -> m b
{-# INLINE ifoldM #-}
ifoldM f b0 v = loop 0 b0
  where
    loop i b | i >= n    = return b
             | otherwise = do a <- unsafeRead v i
                              loop (i + 1) =<< f b i a
    n = length v

-- | /O(n)/ Monadic fold with strict accumulator using a function applied to each element and its index.
--
-- @since 0.12.3.0
ifoldM' :: (PrimMonad m, MVector v a) => (b -> Int -> a -> m b) -> b -> v (PrimState m) a -> m b
{-# INLINE ifoldM' #-}
ifoldM' f b0 v = loop 0 b0
  where
    loop i !b | i >= n    = return b
              | otherwise = do a <- unsafeRead v i
                               loop (i + 1) =<< f b i a
    n = length v

-- | /O(n)/ Monadic right fold.
--
-- @since 0.12.3.0
foldrM :: (PrimMonad m, MVector v a) => (a -> b -> m b) -> b -> v (PrimState m) a -> m b
{-# INLINE foldrM #-}
foldrM f = ifoldrM (const f)

-- | /O(n)/ Monadic right fold with strict accumulator.
--
-- @since 0.12.3.0
foldrM' :: (PrimMonad m, MVector v a) => (a -> b -> m b) -> b -> v (PrimState m) a -> m b
{-# INLINE foldrM' #-}
foldrM' f = ifoldrM' (const f)

-- | /O(n)/ Monadic right fold using a function applied to each element and its index.
--
-- @since 0.12.3.0
ifoldrM :: (PrimMonad m, MVector v a) => (Int -> a -> b -> m b) -> b -> v (PrimState m) a -> m b
{-# INLINE ifoldrM #-}
ifoldrM f b0 v = loop (n-1) b0
  where
    loop i b | i < 0     = return b
             | otherwise = do a <- unsafeRead v i
                              loop (i - 1) =<< f i a b
    n = length v

-- | /O(n)/ Monadic right fold with strict accumulator using a function applied
-- to each element and its index.
--
-- @since 0.12.3.0
ifoldrM' :: (PrimMonad m, MVector v a) => (Int -> a -> b -> m b) -> b -> v (PrimState m) a -> m b
{-# INLINE ifoldrM' #-}
ifoldrM' f b0 v = loop (n-1) b0
  where
    loop i !b | i < 0     = return b
              | otherwise = do a <- unsafeRead v i
                               loop (i - 1) =<< f i a b
    n = length v

-- Filling and copying
-- -------------------

-- | Set all elements of the vector to the given value.
set :: (PrimMonad m, MVector v a) => v (PrimState m) a -> a -> m ()
{-# INLINE set #-}
set v = stToPrim . basicSet v

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap.
copy :: (HasCallStack, PrimMonad m, MVector v a)
     => v (PrimState m) a   -- ^ target
     -> v (PrimState m) a   -- ^ source
     -> m ()
{-# INLINE copy #-}
copy dst src = check Bounds "overlapping vectors" (not (dst `overlaps` src))
             $ check Bounds "length mismatch" (length dst == length src)
             $ unsafeCopy dst src

-- | Move the contents of a vector. The two vectors must have the same
-- length.
--
-- If the vectors do not overlap, then this is equivalent to 'copy'.
-- Otherwise, the copying is performed as if the source vector were
-- copied to a temporary vector and then the temporary vector was copied
-- to the target vector.
move :: (HasCallStack, PrimMonad m, MVector v a)
     => v (PrimState m) a   -- ^ target
     -> v (PrimState m) a   -- ^ source
     -> m ()
{-# INLINE move #-}
move dst src = check Bounds "length mismatch" (length dst == length src)
             $ unsafeMove dst src

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap, but this is not checked.
unsafeCopy :: (PrimMonad m, MVector v a)
           => v (PrimState m) a   -- ^ target
           -> v (PrimState m) a   -- ^ source
           -> m ()
{-# INLINE unsafeCopy #-}
unsafeCopy dst src = check Unsafe "length mismatch" (length dst == length src)
                   $ check Unsafe "overlapping vectors" (not (dst `overlaps` src))
                   $ dst `seq` src `seq` stToPrim (basicUnsafeCopy dst src)

-- | Move the contents of a vector. The two vectors must have the same
-- length, but this is not checked.
--
-- If the vectors do not overlap, then this is equivalent to 'unsafeCopy'.
-- Otherwise, the copying is performed as if the source vector were
-- copied to a temporary vector and then the temporary vector was copied
-- to the target vector.
unsafeMove :: (PrimMonad m, MVector v a)
           => v (PrimState m) a   -- ^ target
           -> v (PrimState m) a   -- ^ source
           -> m ()
{-# INLINE unsafeMove #-}
unsafeMove dst src = check Unsafe "length mismatch" (length dst == length src)
                   $ dst `seq` src `seq` stToPrim (basicUnsafeMove dst src)


accum :: forall m v a b u. (HasCallStack, PrimMonad m, MVector v a)
      => (a -> b -> a) -> v (PrimState m) a -> Bundle u (Int, b) -> m ()
{-# INLINE accum #-}
accum f !v s = Bundle.mapM_ upd s
  where
    {-# INLINE_INNER upd #-}
    upd :: HasCallStack => (Int, b) -> m ()
    upd (i,b) = do
                  a <- checkIndex Bounds i n $ unsafeRead v i
                  unsafeWrite v i (f a b)
    !n = length v

update :: forall m v a u. (HasCallStack, PrimMonad m, MVector v a)
       => v (PrimState m) a -> Bundle u (Int, a) -> m ()
{-# INLINE update #-}
update !v s = Bundle.mapM_ upd s
  where
    {-# INLINE_INNER upd #-}
    upd :: HasCallStack => (Int, a) -> m ()
    upd (i,b) = checkIndex Bounds i n $ unsafeWrite v i b

    !n = length v

unsafeAccum :: (PrimMonad m, MVector v a)
            => (a -> b -> a) -> v (PrimState m) a -> Bundle u (Int, b) -> m ()
{-# INLINE unsafeAccum #-}
unsafeAccum f !v s = Bundle.mapM_ upd s
  where
    {-# INLINE_INNER upd #-}
    upd (i,b) = do
                  a <- checkIndex Unsafe i n $ unsafeRead v i
                  unsafeWrite v i (f a b)
    !n = length v

unsafeUpdate :: (PrimMonad m, MVector v a)
                        => v (PrimState m) a -> Bundle u (Int, a) -> m ()
{-# INLINE unsafeUpdate #-}
unsafeUpdate !v s = Bundle.mapM_ upd s
  where
    {-# INLINE_INNER upd #-}
    upd (i,b) = checkIndex Unsafe i n $ unsafeWrite v i b
    !n = length v

reverse :: (PrimMonad m, MVector v a) => v (PrimState m) a -> m ()
{-# INLINE reverse #-}
reverse !v = reverse_loop 0 (length v - 1)
  where
    reverse_loop i j | i < j = do
                                 unsafeSwap v i j
                                 reverse_loop (i + 1) (j - 1)
    reverse_loop _ _ = return ()

unstablePartition :: forall m v a. (PrimMonad m, MVector v a)
                  => (a -> Bool) -> v (PrimState m) a -> m Int
{-# INLINE unstablePartition #-}
unstablePartition f !v = from_left 0 (length v)
  where
    -- NOTE: GHC 6.10.4 panics without the signatures on from_left and
    -- from_right
    from_left :: Int -> Int -> m Int
    from_left i j
      | i == j    = return i
      | otherwise = do
                      x <- unsafeRead v i
                      if f x
                        then from_left (i+1) j
                        else from_right i (j-1)

    from_right :: Int -> Int -> m Int
    from_right i j
      | i == j    = return i
      | otherwise = do
                      x <- unsafeRead v j
                      if f x
                        then do
                               y <- unsafeRead v i
                               unsafeWrite v i x
                               unsafeWrite v j y
                               from_left (i+1) j
                        else from_right i (j-1)

unstablePartitionBundle :: (PrimMonad m, MVector v a)
        => (a -> Bool) -> Bundle u a -> m (v (PrimState m) a, v (PrimState m) a)
{-# INLINE unstablePartitionBundle #-}
unstablePartitionBundle f s
  = case upperBound (Bundle.size s) of
      Just n  -> unstablePartitionMax f s n
      Nothing -> partitionUnknown f s

unstablePartitionMax :: (PrimMonad m, MVector v a)
        => (a -> Bool) -> Bundle u a -> Int
        -> m (v (PrimState m) a, v (PrimState m) a)
{-# INLINE unstablePartitionMax #-}
unstablePartitionMax f s n
  = do
      v <- checkLength Internal n $ unsafeNew n
      let {-# INLINE_INNER put #-}
          put (i, j) x
            | f x       = do
                            unsafeWrite v i x
                            return (i+1, j)
            | otherwise = do
                            unsafeWrite v (j-1) x
                            return (i, j-1)

      (i,j) <- Bundle.foldM' put (0, n) s
      return (unsafeSlice 0 i v, unsafeSlice j (n-j) v)

partitionBundle :: (PrimMonad m, MVector v a)
        => (a -> Bool) -> Bundle u a -> m (v (PrimState m) a, v (PrimState m) a)
{-# INLINE partitionBundle #-}
partitionBundle f s
  = case upperBound (Bundle.size s) of
      Just n  -> partitionMax f s n
      Nothing -> partitionUnknown f s

partitionMax :: (PrimMonad m, MVector v a)
  => (a -> Bool) -> Bundle u a -> Int -> m (v (PrimState m) a, v (PrimState m) a)
{-# INLINE partitionMax #-}
partitionMax f s n
  = do
      v <- checkLength Internal n $ unsafeNew n

      let {-# INLINE_INNER put #-}
          put (i,j) x
            | f x       = do
                            unsafeWrite v i x
                            return (i+1,j)

            | otherwise = let j' = j-1 in
                          do
                            unsafeWrite v j' x
                            return (i,j')

      (i,j) <- Bundle.foldM' put (0,n) s
      check Internal "invalid indices" (i <= j)
        $ return ()
      let l = unsafeSlice 0 i v
          r = unsafeSlice j (n-j) v
      reverse r
      return (l,r)

partitionUnknown :: (PrimMonad m, MVector v a)
        => (a -> Bool) -> Bundle u a -> m (v (PrimState m) a, v (PrimState m) a)
{-# INLINE partitionUnknown #-}
partitionUnknown f s
  = do
      v1 <- unsafeNew 0
      v2 <- unsafeNew 0
      (v1', n1, v2', n2) <- Bundle.foldM' put (v1, 0, v2, 0) s
      checkSlice Internal 0 n1 (length v1')
        $ checkSlice Internal 0 n2 (length v2')
        $ return (unsafeSlice 0 n1 v1', unsafeSlice 0 n2 v2')
  where
    -- NOTE: The case distinction has to be on the outside because
    -- GHC creates a join point for the unsafeWrite even when everything
    -- is inlined. This is bad because with the join point, v isn't getting
    -- unboxed.
    {-# INLINE_INNER put #-}
    put (v1, i1, v2, i2) x
      | f x       = do
                      v1' <- unsafeAppend1 v1 i1 x
                      return (v1', i1+1, v2, i2)
      | otherwise = do
                      v2' <- unsafeAppend1 v2 i2 x
                      return (v1, i1, v2', i2+1)


partitionWithBundle :: (PrimMonad m, MVector v a, MVector v b, MVector v c)
        => (a -> Either b c) -> Bundle u a -> m (v (PrimState m) b, v (PrimState m) c)
{-# INLINE partitionWithBundle #-}
partitionWithBundle f s
  = case upperBound (Bundle.size s) of
      Just n  -> partitionWithMax f s n
      Nothing -> partitionWithUnknown f s

partitionWithMax :: (PrimMonad m, MVector v a, MVector v b, MVector v c)
  => (a -> Either b c) -> Bundle u a -> Int -> m (v (PrimState m) b, v (PrimState m) c)
{-# INLINE partitionWithMax #-}
partitionWithMax f s n
  = do
      v1 <- unsafeNew n
      v2 <- unsafeNew n
      let {-# INLINE_INNER put #-}
          put (i1, i2) x = case f x of
            Left b -> do
              unsafeWrite v1 i1 b
              return (i1+1, i2)
            Right c -> do
              unsafeWrite v2 i2 c
              return (i1, i2+1)
      (n1, n2) <- Bundle.foldM' put (0, 0) s
      checkSlice Internal 0 n1 (length v1)
        $ checkSlice Internal 0 n2 (length v2)
        $ return (unsafeSlice 0 n1 v1, unsafeSlice 0 n2 v2)

partitionWithUnknown :: forall m v u a b c.
     (PrimMonad m, MVector v a, MVector v b, MVector v c)
  => (a -> Either b c) -> Bundle u a -> m (v (PrimState m) b, v (PrimState m) c)
{-# INLINE partitionWithUnknown #-}
partitionWithUnknown f s
  = do
      v1 <- unsafeNew 0
      v2 <- unsafeNew 0
      (v1', n1, v2', n2) <- Bundle.foldM' put (v1, 0, v2, 0) s
      checkSlice Internal 0 n1 (length v1')
        $ checkSlice Internal 0 n2 (length v2')
        $ return (unsafeSlice 0 n1 v1', unsafeSlice 0 n2 v2')
  where
    put :: (v (PrimState m) b, Int, v (PrimState m) c, Int)
        -> a
        -> m (v (PrimState m) b, Int, v (PrimState m) c, Int)
    {-# INLINE_INNER put #-}
    put (v1, i1, v2, i2) x = case f x of
      Left b -> do
        v1' <- unsafeAppend1 v1 i1 b
        return (v1', i1+1, v2, i2)
      Right c -> do
        v2' <- unsafeAppend1 v2 i2 c
        return (v1, i1, v2', i2+1)

-- Modifying vectors
-- -----------------

{-
http://en.wikipedia.org/wiki/Permutation#Algorithms_to_generate_permutations

The following algorithm generates the next permutation lexicographically after
a given permutation. It changes the given permutation in-place.

1. Find the largest index k such that a[k] < a[k + 1]. If no such index exists,
   the permutation is the last permutation.
2. Find the largest index l greater than k such that a[k] < a[l].
3. Swap the value of a[k] with that of a[l].
4. Reverse the sequence from a[k + 1] up to and including the final element a[n]
-}

-- | Compute the (lexicographically) next permutation of the given vector in-place.
-- Returns False when the input is the last permutation.
nextPermutation :: (PrimMonad m,Ord e,MVector v e) => v (PrimState m) e -> m Bool
nextPermutation v
    | dim < 2 = return False
    | otherwise = do
        val <- unsafeRead v 0
        (k,l) <- loop val (-1) 0 val 1
        if k < 0
         then return False
         else unsafeSwap v k l >>
              reverse (unsafeSlice (k+1) (dim-k-1) v) >>
              return True
    where loop !kval !k !l !prev !i
              | i == dim = return (k,l)
              | otherwise  = do
                  cur <- unsafeRead v i
                  -- TODO: make tuple unboxed
                  let (kval',k') = if prev < cur then (prev,i-1) else (kval,k)
                      l' = if kval' < cur then i else l
                  loop kval' k' l' cur (i+1)
          dim = length v

-- $setup
-- >>> import Prelude ((*))
