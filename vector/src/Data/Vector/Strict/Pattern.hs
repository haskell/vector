{-# LANGUAGE PatternSynonyms #-}
-- See NOTE: [Constructor deprecation]
--
--
-- This module should be removed once deprecated constructors are dropped.
module Data.Vector.Strict.Pattern where

import qualified Data.Vector.Mutable as MV
import           Data.Vector.Strict.Mutable.Unsafe

pattern MVector :: MV.MVector s a -> MVector s a
pattern MVector v = UnsafeMVector v
{-# COMPLETE MVector #-}
{-# DEPRECATED MVector "Use MVector constructor exported from \"Data.Vector.Strict.Unsafe\"" #-}
