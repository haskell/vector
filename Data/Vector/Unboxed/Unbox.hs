{-# LANGUAGE MagicHash, UnboxedTuples, ScopedTypeVariables #-}

module Data.Vector.Unboxed.Unbox (
  Unbox(..), Array, MutableArray,

  arraySize, newArray, unsafeFreezeArray, indexArray, readArray, writeArray
) where

import GHC.Prim (
    ByteArray#, MutableByteArray#, State#,
    newByteArray#, unsafeFreezeByteArray#,

    Int#, indexIntArray#, readIntArray#, writeIntArray#
  )
import GHC.ST (
    ST(..)
  )
import GHC.Base (
    Int(..)
  )
import Data.Array.Base (
    wORD_SCALE )

data Array          a = Array        ByteArray#
data MutableArray s a = MutableArray (MutableByteArray# s)

class Unbox a where
  arraySize#  :: a -> Int# -> Int#
  indexArray# :: ByteArray# -> Int# -> a
  readArray#  :: MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
  writeArray# :: MutableByteArray# s -> Int# -> a -> State# s -> State# s

instance Unbox Int where
  arraySize#  _                  = wORD_SCALE
  indexArray# arr# i#            = I# (indexIntArray# arr# i#)
  readArray#  arr# i# s#         = case readIntArray# arr# i# s# of
                                     (# s1#, n# #) -> (# s1#, I# n# #)
  writeArray# arr# i# (I# n#) s# = writeIntArray# arr# i# n# s#

arraySize :: Unbox a => a -> Int -> Int
{-# INLINE arraySize #-}
arraySize a (I# i#) = I# (arraySize# a i#)

newArray :: forall s a. Unbox a => Int -> ST s (MutableArray s a)
{-# INLINE newArray #-}
newArray (I# n#) = ST $ \s# ->
  case newByteArray# (arraySize# (undefined :: a) n#) s# of
    (# s2#, arr# #) -> (# s2#, MutableArray arr# #)

unsafeFreezeArray :: Unbox a => MutableArray s a -> ST s (Array a)
{-# INLINE unsafeFreezeArray #-}
unsafeFreezeArray (MutableArray arr#) = ST $ \s# ->
  case unsafeFreezeByteArray# arr# s# of
    (# s2, frozen# #) -> (# s2, Array frozen# #)

indexArray :: Unbox a => Array a -> Int -> a
{-# INLINE indexArray #-}
indexArray (Array arr#) (I# i#) = indexArray# arr# i#

readArray :: Unbox a => MutableArray s a -> Int -> ST s a
{-# INLINE readArray #-}
readArray (MutableArray arr#) (I# i#) = ST $ readArray# arr# i#

writeArray :: Unbox a => MutableArray s a -> Int -> a -> ST s ()
{-# INLINE writeArray #-}
writeArray (MutableArray arr#) (I# i#) x = ST $ \s# ->
  case writeArray# arr# i# x s# of s2# -> (# s2#, () #)

