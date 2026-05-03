{-# LANGUAGE PatternSynonyms #-}
-- NOTE: [Constructor deprecation]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Constructor could be imported by users in two ways:
--
--  > import Data.Vector.Mutable qualified as MV
--  > import Data.Vector.Mutable (MVector(..))
--
-- Latter method causes problems for us. For that to work pattern
-- synonym must be exported as `, MVector(MVector)` and it's only
-- possible if pattern is imported from another module. It couldn't
-- be Unsafe module becasuse of DEPRECATED pragma quirk. It
-- deprecates both type constructor and data constructor. Quoting GHC
-- 8.8 user guide:
--
-- > If both are in scope, there is currently no way to specify one
-- > without the other
--
-- Since 9.10 is possible to disambiguate but we need to support older
-- compilers so only way is to define pattern for deprecated
-- constructor in separate module.
--
-- This forces us to put compatibility patterns into separate module.
--
--
--
-- This module should be removed once deprecated constructors are
-- dropped.
module Data.Vector.Primitive.Pattern
  ( pattern MVector
  , pattern Vector
  ) where

import Data.Primitive.ByteArray
import Data.Vector.Primitive.Mutable.Unsafe
import Data.Vector.Primitive.Unsafe


pattern MVector :: Int -> Int -> MutableByteArray s -> MVector s a
pattern MVector i j arr = UnsafeMVector i j arr
{-# COMPLETE MVector #-}
{-# DEPRECATED MVector "Use MVector exported from \"Data.Vector.Primitive.Mutable.Unsafe\"" #-}

pattern Vector :: Int -> Int -> ByteArray -> Vector a
pattern Vector i j arr = UnsafeVector i j arr
{-# COMPLETE Vector #-}
{-# DEPRECATED Vector "Use Vector constructor exported from \"Data.Vector.Primitive.Unsafe\"" #-}

