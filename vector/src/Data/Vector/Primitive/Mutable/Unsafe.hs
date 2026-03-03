{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- This module exposes internal representation of mutable vectors backed by
-- single 'ByteArray' and functions that work on that representation
-- directly (as opposed to using 'G.Vector' API).
--
-- Note that working with internal representation of vector is
-- generally unsafe and may violate memory safety.
module Data.Vector.Primitive.Mutable.Unsafe
  ( MVector(..)
  , unsafeCoerceMVector
  , unsafeCast
  ) where

import qualified Data.Vector.Generic.Mutable as MG
import           Data.Primitive.ByteArray
import           Data.Primitive ( Prim, sizeOf )
import           Data.Word ( Word8 )
import           Control.Monad ( liftM )
import           GHC.Stack (HasCallStack)

import Control.DeepSeq ( NFData(rnf), NFData1(liftRnf))

import Prelude
  ( Int, Eq(..), Ord(..)
  , otherwise, error, undefined, div, Show(..),  maxBound
  , (+), (*), (&&), (||), ($), (++) )

import Data.Coerce
import Unsafe.Coerce



----------------------------------------------------------------
-- Mutable
----------------------------------------------------------------

type role MVector nominal nominal

-- | Mutable vectors of primitive types.
data MVector s a = MVector
  { unsafeOffset :: {-# UNPACK #-} !Int
    -- ^ Offset into the `unsafeMutableByteArray` in number of elements, not bytes
  , unsafeSize :: {-# UNPACK #-} !Int
    -- ^ Size of the mutable vector in number of elements, not bytes
  , unsafeMutableByteArray :: {-# UNPACK #-} !(MutableByteArray s)
    -- ^ Underlying mutable byte array
  }

-- | /O(1)/ Unsafely coerce a mutable vector from one element type to another,
-- representationally equal type. The operation just changes the type of the
-- underlying pointer and does not modify the elements.
--
-- Note that this function is unsafe. The @Coercible@ constraint guarantees
-- that the element types are representationally equal. It however cannot
-- guarantee that their respective 'Prim' instances are compatible.
unsafeCoerceMVector :: Coercible a b => MVector s a -> MVector s b
unsafeCoerceMVector = unsafeCoerce

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



instance NFData (MVector s a) where
  rnf (MVector _ _ _) = ()

instance NFData1 (MVector s) where
  liftRnf _ (MVector _ _ _) = ()

instance Prim a => MG.MVector MVector a where
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

