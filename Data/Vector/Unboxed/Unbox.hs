{-# LANGUAGE MagicHash, UnboxedTuples #-}

module Data.Vector.Unboxed.Unbox (
  Unbox(..)
) where

import GHC.Base (
    Int(..)
  )
import GHC.Float (
    Float(..), Double(..)
  )

import GHC.Prim (
    ByteArray#, MutableByteArray#, State#,

    Int#, indexIntArray#,    readIntArray#,    writeIntArray#,
          indexFloatArray#,  readFloatArray#,  writeFloatArray#,
          indexDoubleArray#, readDoubleArray#, writeDoubleArray#
  )
import Data.Array.Base (
    wORD_SCALE, fLOAT_SCALE, dOUBLE_SCALE
  )

class Unbox a where
  size#  :: a -> Int# -> Int#
  at#    :: ByteArray# -> Int# -> a
  read#  :: MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
  write# :: MutableByteArray# s -> Int# -> a -> State# s -> State# s

instance Unbox Int where
  size#  _                  = wORD_SCALE
  at#    arr# i#            = I# (indexIntArray# arr# i#)
  read#  arr# i# s#         = case readIntArray# arr# i# s# of
                                (# s1#, n# #) -> (# s1#, I# n# #)
  write# arr# i# (I# n#) s# = writeIntArray# arr# i# n# s#

instance Unbox Float where
  size#  _                  = fLOAT_SCALE
  at#    arr# i#            = F# (indexFloatArray# arr# i#)
  read#  arr# i# s#         = case readFloatArray# arr# i# s# of
                                (# s1#, x# #) -> (# s1#, F# x# #)
  write# arr# i# (F# x#) s# = writeFloatArray# arr# i# x# s#

instance Unbox Double where
  size#  _                  = dOUBLE_SCALE
  at#    arr# i#            = D# (indexDoubleArray# arr# i#)
  read#  arr# i# s#         = case readDoubleArray# arr# i# s# of
                                (# s1#, x# #) -> (# s1#, D# x# #)
  write# arr# i# (D# x#) s# = writeDoubleArray# arr# i# x# s#

