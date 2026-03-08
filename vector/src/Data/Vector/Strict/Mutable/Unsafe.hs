{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- This module exposes internal representation of mutable strict boxed
-- vector and functions that work on that representation directly (as
-- opposed to using 'G.MVector' API.
--
-- Note that working with internal representation of vector is
-- generally unsafe and may violate memory safety
module Data.Vector.Strict.Mutable.Unsafe
  ( MVector(..)
  ) where

import           Data.Coerce
import qualified Data.Vector.Generic.Mutable as G
import qualified Data.Vector.Mutable as MV

import Prelude ( Monad(..), return )

#include "vector.h"

type role MVector nominal representational

-- | Mutable boxed vectors keyed on the monad they live in ('IO' or @'ST' s@).
newtype MVector s a = UnsafeMVector { unsafeLazyMVector :: MV.MVector s a }

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
