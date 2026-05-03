{-# LANGUAGE PatternSynonyms #-}
-- See NOTE: [Constructor deprecation]
--
--
-- This module should be removed once deprecated constructors are dropped.
module Data.Vector.Storable.Pattern
  ( pattern MVector
  ) where

import Foreign.ForeignPtr
import Data.Vector.Storable.Mutable.Unsafe

pattern MVector :: Int -> ForeignPtr a -> MVector s a
pattern MVector i ptr = UnsafeMVector i ptr
{-# COMPLETE MVector #-}
{-# DEPRECATED MVector "Use MVector exported from Data.Vector.Strict.Mutable.Unsafe" #-}
