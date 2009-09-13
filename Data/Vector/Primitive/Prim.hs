{-# LANGUAGE MagicHash, UnboxedTuples, CPP #-}

-- |
-- Module      : Data.Vector.Primitive.Prim
-- Copyright   : (c) Roman Leshchinskiy 2008
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
-- 
-- Primitives for manipulating unboxed arrays of primitive types.
--

module Data.Vector.Primitive.Prim (
  Prim(..)
) where

import GHC.Base (
    Int(..), Char(..),
  )
import GHC.Float (
    Float(..), Double(..)
  )
import GHC.Word (
    Word(..), Word8(..), Word16(..), Word32(..), Word64(..)
  )
import GHC.Int (
    Int8(..), Int16(..), Int32(..), Int64(..)
  )

import GHC.Prim
import Data.Array.Base (
    wORD_SCALE, fLOAT_SCALE, dOUBLE_SCALE
  )

-- | Class of types that can be stored in primitive arrays
class Prim a where
  -- | Yield the size in bytes of a 'ByteArray#' which can store @n@ elements
  size#  :: a     -- ^ Dummy type parameter, never evaluated
         -> Int#  -- ^ Number of elements
         -> Int#

  -- | Indexing
  at#    :: ByteArray# -> Int# -> a

  -- | Yield the element at the given position
  read#  :: MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)

  -- | Store the given element at the given position
  write# :: MutableByteArray# s -> Int# -> a -> State# s -> State# s

-- FIXME: use Template Haskell as soon as it properly supports unboxed types
-- and especially tuples
#define derivePrim(ty, ctr, scale, idx, rd, wr)  \
instance Prim ty where {                         \
  size#  _         = scale                       \
; at#    arr# i#   = ctr (idx arr# i#)           \
; read#  arr# i# s# = case rd arr# i# s# of      \
                        { (# s1#, x# #) -> (# s1#, ctr x# #) } \
; write# arr# i# (ctr x#) s# = wr arr# i# x# s# }

derivePrim(Word, W#, wORD_SCALE,
           indexWordArray#, readWordArray#, writeWordArray#)
derivePrim(Word8, W8#, (\n# -> n#),
           indexWord8Array#, readWord8Array#, writeWord8Array#)
derivePrim(Word16, W16#, (*# 2#),
           indexWord16Array#, readWord16Array#, writeWord16Array#)
derivePrim(Word32, W32#, (*# 4#),
           indexWord32Array#, readWord32Array#, writeWord32Array#)
derivePrim(Word64, W64#, (*# 8#),
           indexWord64Array#, readWord64Array#, writeWord64Array#)
derivePrim(Int, I#, wORD_SCALE,
           indexIntArray#, readIntArray#, writeIntArray#)
derivePrim(Int8, I8#, (\n# -> n#),
           indexInt8Array#, readInt8Array#, writeInt8Array#)
derivePrim(Int16, I16#, (*# 2#),
           indexInt16Array#, readInt16Array#, writeInt16Array#)
derivePrim(Int32, I32#, (*# 4#),
           indexInt32Array#, readInt32Array#, writeInt32Array#)
derivePrim(Int64, I64#, (*# 8#),
           indexInt64Array#, readInt64Array#, writeInt64Array#)
derivePrim(Float, F#, fLOAT_SCALE,
           indexFloatArray#, readFloatArray#, writeFloatArray#)
derivePrim(Double, D#, dOUBLE_SCALE,
           indexDoubleArray#, readDoubleArray#, writeDoubleArray#)
derivePrim(Char, C#, (*# 4#),
           indexWideCharArray#, readWideCharArray#, writeWideCharArray#)

