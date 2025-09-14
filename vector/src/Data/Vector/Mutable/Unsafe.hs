{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
-- |
module Data.Vector.Mutable.Unsafe
  ( MVector(..)
  , IOVector
  , STVector
    -- * Array conversions
  , toMutableArray
  , fromMutableArray
  ) where

import           Control.Monad (when, liftM)
import           Control.Monad.ST (ST)
import qualified Data.Vector.Generic.Mutable as G
import           Data.Vector.Internal.Check
import           Data.Primitive.Array
import           Control.Monad.Primitive

import Prelude
  ( Monad, Ordering(..), Int
  , compare, return, otherwise, error
  , (>>=), (+), (-), (*), (<), (>), (>=), (&&), (||), ($), (>>) )

#include "vector.h"

type role MVector nominal representational

-- | Mutable boxed vectors keyed on the monad they live in ('IO' or @'ST' s@).
data MVector s a = MVector { _offset :: {-# UNPACK #-} !Int
                           -- ^ Offset in underlying array
                           , _size   :: {-# UNPACK #-} !Int
                           -- ^ Size of slice
                           , _array  :: {-# UNPACK #-} !(MutableArray s a)
                           -- ^ Underlying array
                           }

type IOVector = MVector RealWorld
type STVector s = MVector s


-- NOTE: This seems unsafe, see http://trac.haskell.org/vector/ticket/54
{-
instance NFData a => NFData (MVector s a) where
    rnf (MVector i n arr) = unsafeInlineST $ force i
        where
          force !ix | ix < n    = do x <- readArray arr ix
                                     rnf x `seq` force (ix+1)
                    | otherwise = return ()
-}

instance G.MVector MVector a where
  {-# INLINE basicLength #-}
  basicLength (MVector _ n _) = n

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice j m (MVector i _ arr) = MVector (i+j) m arr

  {-# INLINE basicOverlaps #-}
  basicOverlaps (MVector i m arr1) (MVector j n arr2)
    = sameMutableArray arr1 arr2
      && (between i j (j+n) || between j i (i+m))
    where
      between x y z = x >= y && x < z

  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew n
    = do
        arr <- newArray n uninitialised
        return (MVector 0 n arr)

  {-# INLINE basicInitialize #-}
  -- initialization is unnecessary for boxed vectors
  basicInitialize _ = return ()

  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeReplicate n x
    = do
        arr <- newArray n x
        return (MVector 0 n arr)

  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (MVector i _ arr) j = readArray arr (i+j)

  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (MVector i _ arr) j x = writeArray arr (i+j) x

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MVector i n dst) (MVector j _ src)
    = copyMutableArray dst i src j n

  basicUnsafeMove dst@(MVector iDst n arrDst) src@(MVector iSrc _ arrSrc)
    = case n of
        0 -> return ()
        1 -> readArray arrSrc iSrc >>= writeArray arrDst iDst
        2 -> do
               x <- readArray arrSrc iSrc
               y <- readArray arrSrc (iSrc + 1)
               writeArray arrDst iDst x
               writeArray arrDst (iDst + 1) y
        _
          | G.overlaps dst src
             -> case compare iDst iSrc of
                  LT -> moveBackwards arrDst iDst iSrc n
                  EQ -> return ()
                  GT | (iDst - iSrc) * 2 < n
                        -> moveForwardsLargeOverlap arrDst iDst iSrc n
                     | otherwise
                        -> moveForwardsSmallOverlap arrDst iDst iSrc n
          | otherwise -> G.basicUnsafeCopy dst src

  {-# INLINE basicClear #-}
  basicClear v = G.set v uninitialised


{-# INLINE moveBackwards #-}
moveBackwards :: MutableArray s a -> Int -> Int -> Int -> ST s ()
moveBackwards !arr !dstOff !srcOff !len =
  check Internal "not a backwards move" (dstOff < srcOff)
  $ loopM len $ \ i -> readArray arr (srcOff + i) >>= writeArray arr (dstOff + i)

{-# INLINE moveForwardsSmallOverlap #-}
-- Performs a move when dstOff > srcOff, optimized for when the overlap of the intervals is small.
moveForwardsSmallOverlap :: MutableArray s a -> Int -> Int -> Int -> ST s ()
moveForwardsSmallOverlap !arr !dstOff !srcOff !len =
  check Internal "not a forward move" (dstOff > srcOff)
  $ do
      tmp <- newArray overlap uninitialised
      loopM overlap $ \ i -> readArray arr (dstOff + i) >>= writeArray tmp i
      loopM nonOverlap $ \ i -> readArray arr (srcOff + i) >>= writeArray arr (dstOff + i)
      loopM overlap $ \ i -> readArray tmp i >>= writeArray arr (dstOff + nonOverlap + i)
  where nonOverlap = dstOff - srcOff; overlap = len - nonOverlap

-- Performs a move when dstOff > srcOff, optimized for when the overlap of the intervals is large.
moveForwardsLargeOverlap :: MutableArray s a -> Int -> Int -> Int -> ST s ()
moveForwardsLargeOverlap !arr !dstOff !srcOff !len =
  check Internal "not a forward move" (dstOff > srcOff)
  $ do
      queue <- newArray nonOverlap uninitialised
      loopM nonOverlap $ \ i -> readArray arr (srcOff + i) >>= writeArray queue i
      let mov !i !qTop = when (i < dstOff + len) $ do
            x <- readArray arr i
            y <- readArray queue qTop
            writeArray arr i y
            writeArray queue qTop x
            mov (i+1) (if qTop + 1 >= nonOverlap then 0 else qTop + 1)
      mov dstOff 0
  where nonOverlap = dstOff - srcOff

{-# INLINE loopM #-}
loopM :: Monad m => Int -> (Int -> m a) -> m ()
loopM !n k = let
  go i = when (i < n) (k i >> go (i+1))
  in go 0

uninitialised :: a
uninitialised = error "Data.Vector.Mutable: uninitialised element. If you are trying to compact a vector, use the 'Data.Vector.force' function to remove uninitialised elements from the underlying array."


-- Conversions - Arrays
-- -----------------------------

-- | /O(n)/ Make a copy of a mutable array to a new mutable vector.
--
-- @since 0.12.2.0
fromMutableArray :: PrimMonad m => MutableArray (PrimState m) a -> m (MVector (PrimState m) a)
{-# INLINE fromMutableArray #-}
fromMutableArray marr =
  let size = sizeofMutableArray marr
  in MVector 0 size `liftM` cloneMutableArray marr 0 size

-- | /O(n)/ Make a copy of a mutable vector into a new mutable array.
--
-- @since 0.12.2.0
toMutableArray :: PrimMonad m => MVector (PrimState m) a -> m (MutableArray (PrimState m) a)
{-# INLINE toMutableArray #-}
toMutableArray (MVector offset size marr) = cloneMutableArray marr offset size
