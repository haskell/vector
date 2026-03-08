{-# LANGUAGE PatternSynonyms #-}
-- See NOTE: [Constructor deprecation]
--
--
-- This module should be removed once deprecated constructors are dropped.
module Data.Vector.Pattern
  ( pattern MVector
  ) where

import Data.Primitive.Array
import Data.Vector.Mutable.Unsafe


pattern MVector :: Int -> Int -> MutableArray s a -> MVector s a
pattern MVector i j arr = UnsafeMVector i j arr
{-# COMPLETE MVector #-}
{-# DEPRECATED MVector "Use MVector exported from \"Data.Vector.Mutable.Unsafe\"" #-}
