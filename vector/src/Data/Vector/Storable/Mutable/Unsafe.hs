{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
module Data.Vector.Storable.Mutable.Unsafe
  ( MVector(..)
  , IOVector
  , STVector
    -- * Unsafe conversions
  , unsafeCast
  , unsafeCoerceMVector
    -- * Working with raw pointers
  , unsafeFromForeignPtr, unsafeFromForeignPtr0
  , unsafeToForeignPtr,   unsafeToForeignPtr0
  , unsafeWith
  ) where

import Control.DeepSeq (NFData(rnf), NFData1(liftRnf))

import qualified Data.Vector.Generic.Mutable as G
import Data.Vector.Storable.Internal

import Foreign.Storable
import Foreign.ForeignPtr

import GHC.ForeignPtr (mallocPlainForeignPtrAlignedBytes)
import GHC.Base ( Int(..) )

import Foreign.Ptr (castPtr,plusPtr)
import Foreign.Marshal.Array ( advancePtr, copyArray, moveArray )

import Control.Monad.Primitive
import Data.Primitive.Types (Prim)
import qualified Data.Primitive.Types as DPT

import GHC.Word (Word8, Word16, Word32, Word64)
import GHC.Ptr (Ptr(..))

import Prelude
  ( IO, return, otherwise, error, undefined, max, div, quot, maxBound, show
  , (-), (*), (<), (>), (>=), (==), (&&), (||), (.), ($), (++) )

import Data.Coerce
import Unsafe.Coerce

#include "vector.h"


type role MVector nominal nominal

-- | Mutable 'Storable'-based vectors.
data MVector s a = MVector {-# UNPACK #-} !Int
                           {-# UNPACK #-} !(ForeignPtr a)

type IOVector = MVector RealWorld
type STVector s = MVector s

instance NFData (MVector s a) where
  rnf (MVector _ _) = ()

instance NFData1 (MVector s) where
  liftRnf _ (MVector _ _) = ()

instance Storable a => G.MVector MVector a where
  {-# INLINE basicLength #-}
  basicLength (MVector n _) = n

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice j m (MVector _ fp) = MVector m (updPtr (`advancePtr` j) fp)

  -- FIXME: this relies on non-portable pointer comparisons
  {-# INLINE basicOverlaps #-}
  basicOverlaps (MVector m fp) (MVector n fq)
    = between p q (q `advancePtr` n) || between q p (p `advancePtr` m)
    where
      between x y z = x >= y && x < z
      p = getPtr fp
      q = getPtr fq

  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew n
    | n < 0 = error $ "Storable.basicUnsafeNew: negative length: " ++ show n
    | n > mx = error $ "Storable.basicUnsafeNew: length too large: " ++ show n
    | otherwise = unsafePrimToPrim $ do
        fp <- mallocVector n
        return $ MVector n fp
    where
      size = sizeOf (undefined :: a) `max` 1
      mx = maxBound `quot` size :: Int

  {-# INLINE basicInitialize #-}
  basicInitialize = storableZero

  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (MVector _ fp) i
    = unsafePrimToPrim
    $ unsafeWithForeignPtr fp (`peekElemOff` i)

  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (MVector _ fp) i x
    = unsafePrimToPrim
    $ unsafeWithForeignPtr fp $ \p -> pokeElemOff p i x

  {-# INLINE basicSet #-}
  basicSet = storableSet

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MVector n fp) (MVector _ fq)
    = unsafePrimToPrim
    $ unsafeWithForeignPtr fp $ \p ->
      unsafeWithForeignPtr fq $ \q ->
      copyArray p q n

  {-# INLINE basicUnsafeMove #-}
  basicUnsafeMove (MVector n fp) (MVector _ fq)
    = unsafePrimToPrim
    $ unsafeWithForeignPtr fp $ \p ->
      unsafeWithForeignPtr fq $ \q ->
      moveArray p q n


{-# INLINE mallocVector #-}
mallocVector :: Storable a => Int -> IO (ForeignPtr a)
mallocVector =
  doMalloc undefined
  where
    doMalloc :: Storable b => b -> Int -> IO (ForeignPtr b)
    doMalloc dummy size =
      mallocPlainForeignPtrAlignedBytes (size * sizeOf dummy) (alignment dummy)

storableZero :: forall a m. (Storable a, PrimMonad m) => MVector (PrimState m) a -> m ()
{-# INLINE storableZero #-}
storableZero (MVector n fp) = unsafePrimToPrim . unsafeWithForeignPtr fp $ \ptr-> do
  memsetPrimPtr_vector (castPtr ptr) byteSize (0 :: Word8)
 where
 x :: a
 x = undefined
 byteSize :: Int
 byteSize = n * sizeOf x

storableSet :: (Storable a, PrimMonad m) => MVector (PrimState m) a -> a -> m ()
{-# INLINE storableSet #-}
storableSet (MVector n fp) x
  | n == 0 = return ()
  | otherwise = unsafePrimToPrim $
                case sizeOf x of
                  1 -> storableSetAsPrim n fp x (undefined :: Word8)
                  2 -> storableSetAsPrim n fp x (undefined :: Word16)
                  4 -> storableSetAsPrim n fp x (undefined :: Word32)
#if !defined(ghcjs_HOST_OS)
                  8 -> storableSetAsPrim n fp x (undefined :: Word64)
#endif
                  _ -> unsafeWithForeignPtr fp $ \p -> do
                       poke p x

                       let do_set i
                             | 2*i < n = do
                                 copyArray (p `advancePtr` i) p i
                                 do_set (2*i)
                             | otherwise = copyArray (p `advancePtr` i) p (n-i)

                       do_set 1

storableSetAsPrim
  :: forall a b . (Storable a, Prim b) => Int -> ForeignPtr a -> a -> b -> IO ()
{-# INLINE [0] storableSetAsPrim #-}
storableSetAsPrim n fp x _y = unsafeWithForeignPtr fp $ \ ptr  -> do
    poke ptr x
     -- we don't equate storable and prim reps, so we need to write to a slot
     -- in storable
     -- then read it back as a prim
    w<- peakPrimPtr_vector (castPtr ptr :: Ptr  b) 0
    memsetPrimPtr_vector (castPtr ptr `plusPtr` sizeOf x ) (n-1)  w

{-
AFTER primitive 0.7 is pretty old, move to using setPtr. which is really
a confusing misnomer for what's often called memset (initialize)
-}
-- Fill a memory block with the given value. The length is in
-- elements of type @a@ rather than in bytes.
memsetPrimPtr_vector :: forall a c m. (Prim c, PrimMonad m) => Ptr a -> Int -> c -> m ()
memsetPrimPtr_vector (Ptr addr#) (I# n#) x = primitive_ (DPT.setOffAddr# addr# 0# n# x)
{-# INLINE memsetPrimPtr_vector #-}


-- Read a value from a memory position given by an address and an offset.
-- The offset is in elements of type @a@ rather than in bytes.
peakPrimPtr_vector :: (Prim a, PrimMonad m) => Ptr a -> Int -> m a
peakPrimPtr_vector (Ptr addr#) (I# i#) = primitive (DPT.readOffAddr# addr# i#)
{-# INLINE peakPrimPtr_vector #-}


-- Unsafe conversions
-- ------------------

-- | /O(1)/ Unsafely cast a mutable vector from one element type to another.
-- The operation just changes the type of the underlying pointer and does not
-- modify the elements.
--
-- The resulting vector contains as many elements as can fit into the
-- underlying memory block.
unsafeCast :: forall a b s.
              (Storable a, Storable b) => MVector s a -> MVector s b
{-# INLINE unsafeCast #-}
unsafeCast (MVector n fp)
  = MVector ((n * sizeOf (undefined :: a)) `div` sizeOf (undefined :: b))
            (castForeignPtr fp)

-- | /O(1)/ Unsafely coerce a mutable vector from one element type to another,
-- representationally equal type. The operation just changes the type of the
-- underlying pointer and does not modify the elements.
--
-- This is marginally safer than 'unsafeCast', since this function imposes an
-- extra 'Coercible' constraint. This function is still not safe, however,
-- since it cannot guarantee that the two types have memory-compatible
-- 'Storable' instances.
unsafeCoerceMVector :: Coercible a b => MVector s a -> MVector s b
unsafeCoerceMVector = unsafeCoerce

-- Raw pointers
-- ------------

-- | /O(1)/ Create a mutable vector from a 'ForeignPtr' with an offset and a length.
--
-- Modifying data through the 'ForeignPtr' afterwards is unsafe if the vector
-- could have been frozen before the modification.
--
-- If your offset is 0, it is more efficient to use 'unsafeFromForeignPtr0'.
unsafeFromForeignPtr :: Storable a
                     => ForeignPtr a    -- ^ pointer
                     -> Int             -- ^ offset
                     -> Int             -- ^ length
                     -> MVector s a
{-# INLINE_FUSED unsafeFromForeignPtr #-}
unsafeFromForeignPtr fp i n = unsafeFromForeignPtr0 fp' n
    where
      fp' = updPtr (`advancePtr` i) fp

{-# RULES
"unsafeFromForeignPtr fp 0 n -> unsafeFromForeignPtr0 fp n " forall fp n.
  unsafeFromForeignPtr fp 0 n = unsafeFromForeignPtr0 fp n   #-}


-- | /O(1)/ Create a mutable vector from a 'ForeignPtr' and a length.
--
-- It is assumed that the pointer points directly to the data (no offset).
-- Use 'unsafeFromForeignPtr' if you need to specify an offset.
--
-- Modifying data through the 'ForeignPtr' afterwards is unsafe if the vector
-- could have been frozen before the modification.
unsafeFromForeignPtr0 :: ForeignPtr a    -- ^ pointer
                      -> Int             -- ^ length
                      -> MVector s a
{-# INLINE unsafeFromForeignPtr0 #-}
unsafeFromForeignPtr0 fp n = MVector n fp

-- | /O(1)/ Yield the underlying 'ForeignPtr' together with the offset to the data
-- and its length. Modifying the data through the 'ForeignPtr' is
-- unsafe if the vector could have been frozen before the modification.
unsafeToForeignPtr :: MVector s a -> (ForeignPtr a, Int, Int)
{-# INLINE unsafeToForeignPtr #-}
unsafeToForeignPtr (MVector n fp) = (fp, 0, n)

-- | /O(1)/ Yield the underlying 'ForeignPtr' together with its length.
--
-- You can assume that the pointer points directly to the data (no offset).
--
-- Modifying the data through the 'ForeignPtr' is unsafe if the vector could
-- have been frozen before the modification.
unsafeToForeignPtr0 :: MVector s a -> (ForeignPtr a, Int)
{-# INLINE unsafeToForeignPtr0 #-}
unsafeToForeignPtr0 (MVector n fp) = (fp, n)

-- | Pass a pointer to the vector's data to the IO action. Modifying data
-- through the pointer is unsafe if the vector could have been frozen before
-- the modification.
unsafeWith :: Storable a => IOVector a -> (Ptr a -> IO b) -> IO b
{-# INLINE unsafeWith #-}
unsafeWith (MVector _ fp) = withForeignPtr fp
