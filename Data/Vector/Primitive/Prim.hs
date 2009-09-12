{-# LANGUAGE MagicHash, UnboxedTuples #-}

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
    Int(..)
  )
import GHC.Float (
    Float(..), Double(..)
  )
import GHC.Word (
    Word(..)
  )

import GHC.Prim (
    ByteArray#, MutableByteArray#, State#,

    Int#, indexIntArray#,    readIntArray#,    writeIntArray#,
          indexWordArray#,   readWordArray#,   writeWordArray#,
          indexFloatArray#,  readFloatArray#,  writeFloatArray#,
          indexDoubleArray#, readDoubleArray#, writeDoubleArray#
  )
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

instance Prim Word where
  size# _                   = wORD_SCALE
  at#    arr# i#            = W# (indexWordArray# arr# i#)
  read#  arr# i# s#         = case readWordArray# arr# i# s# of
                                (# s1#, n# #) -> (# s1#, W# n# #)
  write# arr# i# (W# n#) s# = writeWordArray# arr# i# n# s#


instance Prim Int where
  size#  _                  = wORD_SCALE
  at#    arr# i#            = I# (indexIntArray# arr# i#)
  read#  arr# i# s#         = case readIntArray# arr# i# s# of
                                (# s1#, n# #) -> (# s1#, I# n# #)
  write# arr# i# (I# n#) s# = writeIntArray# arr# i# n# s#

instance Prim Float where
  size#  _                  = fLOAT_SCALE
  at#    arr# i#            = F# (indexFloatArray# arr# i#)
  read#  arr# i# s#         = case readFloatArray# arr# i# s# of
                                (# s1#, x# #) -> (# s1#, F# x# #)
  write# arr# i# (F# x#) s# = writeFloatArray# arr# i# x# s#

instance Prim Double where
  size#  _                  = dOUBLE_SCALE
  at#    arr# i#            = D# (indexDoubleArray# arr# i#)
  read#  arr# i# s#         = case readDoubleArray# arr# i# s# of
                                (# s1#, x# #) -> (# s1#, D# x# #)
  write# arr# i# (D# x#) s# = writeDoubleArray# arr# i# x# s#

