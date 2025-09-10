{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
module Data.Vector.Storable.Unsafe
  ( Vector(..)
  , unsafeCoerceVector
    -- * Raw pointers
  , unsafeFromForeignPtr, unsafeFromForeignPtr0
  , unsafeToForeignPtr,   unsafeToForeignPtr0
  , unsafeWith
  ) where

import qualified Data.Vector.Generic          as G
import           Data.Vector.Storable.Mutable ( MVector(..) )
import Data.Vector.Storable.Internal
import qualified Data.Vector.Fusion.Bundle as Bundle

import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Marshal.Array ( advancePtr, copyArray )

import Control.DeepSeq ( NFData(rnf), NFData1(liftRnf))

import Control.Monad.Primitive

import Prelude
  ( Eq, Ord, Monoid, Read, Show, Ordering(..), Int, IO
  , compare, mempty, mappend, mconcat, showsPrec, return, seq
  , (<), (<=), (>), (>=), (==), (/=), (.), ($) )

import Data.Data      ( Data(..) )
import Text.Read      ( Read(..), readListPrecDefault )
import Data.Semigroup ( Semigroup(..) )
import Data.Coerce
import qualified GHC.Exts as Exts
import Unsafe.Coerce

#include "vector.h"

type role Vector nominal

-- | /O(1)/ Unsafely coerce a mutable vector from one element type to another,
-- representationally equal type. The operation just changes the type of the
-- underlying pointer and does not modify the elements.
--
-- This is marginally safer than 'unsafeCast', since this function imposes an
-- extra 'Coercible' constraint. This function is still not safe, however,
-- since it cannot guarantee that the two types have memory-compatible
-- 'Storable' instances.
unsafeCoerceVector :: Coercible a b => Vector a -> Vector b
unsafeCoerceVector = unsafeCoerce

-- | 'Storable'-based vectors.
data Vector a = Vector {-# UNPACK #-} !Int
                       {-# UNPACK #-} !(ForeignPtr a)

instance NFData (Vector a) where
  rnf (Vector _ _) = ()

-- | @since 0.12.1.0
instance NFData1 Vector where
  liftRnf _ (Vector _ _) = ()

instance (Show a, Storable a) => Show (Vector a) where
  showsPrec = G.showsPrec

instance (Read a, Storable a) => Read (Vector a) where
  readPrec = G.readPrec
  readListPrec = readListPrecDefault

instance (Data a, Storable a) => Data (Vector a) where
  gfoldl       = G.gfoldl
  toConstr _   = G.mkVecConstr "Data.Vector.Storable.Vector"
  gunfold      = G.gunfold
  dataTypeOf _ = G.mkVecType "Data.Vector.Storable.Vector"
  dataCast1    = G.dataCast


type instance G.Mutable Vector = MVector

instance Storable a => G.Vector Vector a where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze (MVector n fp) = return $ Vector n fp

  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw (Vector n fp) = return $ MVector n fp

  {-# INLINE basicLength #-}
  basicLength (Vector n _) = n

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i n (Vector _ fp) = Vector n (updPtr (`advancePtr` i) fp)

  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (Vector _ fp) i = return
                                    . unsafeInlineIO
                                    $ unsafeWithForeignPtr fp $ \p ->
                                      peekElemOff p i

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MVector n fp) (Vector _ fq)
    = unsafePrimToPrim
    $ unsafeWithForeignPtr fp $ \p ->
      unsafeWithForeignPtr fq $ \q ->
      copyArray p q n

  {-# INLINE elemseq #-}
  elemseq _ = seq

-- See http://trac.haskell.org/vector/ticket/12
instance (Storable a, Eq a) => Eq (Vector a) where
  {-# INLINE (==) #-}
  xs == ys = Bundle.eq (G.stream xs) (G.stream ys)

-- See http://trac.haskell.org/vector/ticket/12
instance (Storable a, Ord a) => Ord (Vector a) where
  {-# INLINE compare #-}
  compare xs ys = Bundle.cmp (G.stream xs) (G.stream ys)

  {-# INLINE (<) #-}
  xs < ys = Bundle.cmp (G.stream xs) (G.stream ys) == LT

  {-# INLINE (<=) #-}
  xs <= ys = Bundle.cmp (G.stream xs) (G.stream ys) /= GT

  {-# INLINE (>) #-}
  xs > ys = Bundle.cmp (G.stream xs) (G.stream ys) == GT

  {-# INLINE (>=) #-}
  xs >= ys = Bundle.cmp (G.stream xs) (G.stream ys) /= LT

instance Storable a => Semigroup (Vector a) where
  {-# INLINE (<>) #-}
  (<>) = (G.++)

  {-# INLINE sconcat #-}
  sconcat = G.concatNE

instance Storable a => Monoid (Vector a) where
  {-# INLINE mempty #-}
  mempty = G.empty

  {-# INLINE mappend #-}
  mappend = (<>)

  {-# INLINE mconcat #-}
  mconcat = G.concat

instance Storable a => Exts.IsList (Vector a) where
  type Item (Vector a) = a
  fromList = G.fromList
  fromListN = G.fromListN
  toList = G.toList


-- | /O(1)/ Create a vector from a 'ForeignPtr' with an offset and a length.
--
-- The data may not be modified through the pointer afterwards.
--
-- If your offset is 0 it is more efficient to use 'unsafeFromForeignPtr0'.
unsafeFromForeignPtr :: Storable a
                     => ForeignPtr a    -- ^ pointer
                     -> Int             -- ^ offset
                     -> Int             -- ^ length
                     -> Vector a
{-# INLINE_FUSED unsafeFromForeignPtr #-}
unsafeFromForeignPtr fp i n = unsafeFromForeignPtr0 fp' n
    where
      fp' = updPtr (`advancePtr` i) fp

{-# RULES
"unsafeFromForeignPtr fp 0 n -> unsafeFromForeignPtr0 fp n " forall fp n.
  unsafeFromForeignPtr fp 0 n = unsafeFromForeignPtr0 fp n   #-}


-- | /O(1)/ Create a vector from a 'ForeignPtr' and a length.
--
-- It is assumed the pointer points directly to the data (no offset).
-- Use 'unsafeFromForeignPtr' if you need to specify an offset.
--
-- The data may not be modified through the pointer afterwards.
unsafeFromForeignPtr0 :: ForeignPtr a    -- ^ pointer
                      -> Int             -- ^ length
                      -> Vector a
{-# INLINE unsafeFromForeignPtr0 #-}
unsafeFromForeignPtr0 fp n = Vector n fp

-- | /O(1)/ Yield the underlying 'ForeignPtr' together with the offset to the
-- data and its length. The data may not be modified through the 'ForeignPtr'.
unsafeToForeignPtr :: Vector a -> (ForeignPtr a, Int, Int)
{-# INLINE unsafeToForeignPtr #-}
unsafeToForeignPtr (Vector n fp) = (fp, 0, n)

-- | /O(1)/ Yield the underlying 'ForeignPtr' together with its length.
--
-- You can assume that the pointer points directly to the data (no offset).
--
-- The data may not be modified through the 'ForeignPtr'.
unsafeToForeignPtr0 :: Vector a -> (ForeignPtr a, Int)
{-# INLINE unsafeToForeignPtr0 #-}
unsafeToForeignPtr0 (Vector n fp) = (fp, n)

-- | Pass a pointer to the vector's data to the IO action. The data may not be
-- modified through the 'Ptr.
unsafeWith :: Storable a => Vector a -> (Ptr a -> IO b) -> IO b
{-# INLINE unsafeWith #-}
unsafeWith (Vector _ fp) = withForeignPtr fp
