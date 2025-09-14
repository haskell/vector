{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
module Data.Vector.Strict.Mutable.Unsafe
  ( MVector(..)
  , IOVector
  , STVector
    -- * Conversions
  , toLazy
  , fromLazy
  , toMutableArray
  , fromMutableArray
  ) where

import           Data.Coerce
import qualified Data.Vector.Generic.Mutable as G
import qualified Data.Vector.Mutable as MV
import           Data.Primitive.Array
import           Control.Monad.Primitive

import Prelude ( Monad(..), return, ($), (<$>) )

#include "vector.h"

type role MVector nominal representational

-- | Mutable boxed vectors keyed on the monad they live in ('IO' or @'ST' s@).
newtype MVector s a = MVector (MV.MVector s a)

type IOVector = MVector RealWorld
type STVector s = MVector s

instance G.MVector MVector a where
  {-# INLINE basicLength #-}
  basicLength = coerce (G.basicLength @MV.MVector @a)
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice = coerce (G.basicUnsafeSlice @MV.MVector @a)
  {-# INLINE basicOverlaps #-}
  basicOverlaps = coerce (G.basicOverlaps @MV.MVector @a)
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew = coerce (G.basicUnsafeNew @MV.MVector @a)
  {-# INLINE basicInitialize #-}
  -- initialization is unnecessary for boxed vectors
  basicInitialize _ = return ()
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeReplicate n !x = coerce (G.basicUnsafeReplicate @MV.MVector @a) n x
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead = coerce (G.basicUnsafeRead @MV.MVector @a)
  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite vec j !x = (coerce (G.basicUnsafeWrite @MV.MVector @a)) vec j x

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy = coerce (G.basicUnsafeCopy @MV.MVector @a)

  {-# INLINE basicUnsafeMove #-}
  basicUnsafeMove = coerce (G.basicUnsafeMove @MV.MVector @a)
  {-# INLINE basicClear #-}
  basicClear = coerce (G.basicClear @MV.MVector @a)



-- Conversions - Lazy vectors
-- -----------------------------

-- | /O(1)/ Convert strict mutable vector to lazy mutable
-- vector. Vectors will share mutable buffer
toLazy :: MVector s a -> MV.MVector s a
{-# INLINE toLazy #-}
toLazy (MVector vec) = vec

-- | /O(n)/ Convert lazy mutable vector to strict mutable
-- vector. Vectors will share mutable buffer. This function evaluates
-- vector elements to WHNF.
fromLazy :: PrimMonad m => MV.MVector (PrimState m) a -> m (MVector (PrimState m) a)
fromLazy mvec = stToPrim $ do
  G.foldM' (\_ !_ -> return ()) () mvec
  return $ MVector mvec


-- Conversions - Arrays
-- -----------------------------

-- | /O(n)/ Make a copy of a mutable array to a new mutable
-- vector. All elements of a vector are evaluated to WHNF
--
-- @since 0.13.2.0
fromMutableArray :: PrimMonad m => MutableArray (PrimState m) a -> m (MVector (PrimState m) a)
{-# INLINE fromMutableArray #-}
fromMutableArray marr = stToPrim $ do
  mvec <- MVector <$> MV.fromMutableArray marr
  G.foldM' (\_ !_ -> return ()) () mvec
  return mvec

-- | /O(n)/ Make a copy of a mutable vector into a new mutable array.
--
-- @since 0.13.2.0
toMutableArray :: PrimMonad m => MVector (PrimState m) a -> m (MutableArray (PrimState m) a)
{-# INLINE toMutableArray #-}
toMutableArray (MVector v) = MV.toMutableArray v
