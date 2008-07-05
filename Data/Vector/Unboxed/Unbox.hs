{-# LANGUAGE MagicHash, UnboxedTuples #-}

module Data.Vector.Unboxed.Unbox (
  Unbox(..)
) where

import GHC.Base (
    Int(..)
  )

import GHC.Prim (
    ByteArray#, MutableByteArray#, State#,

    Int#, indexIntArray#, readIntArray#, writeIntArray#
  )
import Data.Array.Base (
    wORD_SCALE
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

