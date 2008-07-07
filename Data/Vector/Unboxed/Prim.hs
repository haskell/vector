{-# LANGUAGE MagicHash, UnboxedTuples, ScopedTypeVariables #-}

module Data.Vector.Unboxed.Prim (
  Unbox(..), Vector, MutableVector,

  size, new, unsafeFreeze, at, read, write,
  copy, grow
) where

import Data.Vector.Unboxed.Unbox

import GHC.Prim (
    ByteArray#, MutableByteArray#,
    newByteArray#, unsafeFreezeByteArray#,
  )
import GHC.Float (
    int2Double, double2Int
  )
import GHC.ST (
    ST(..)
  )
import GHC.Base (
    Int(..)
  )

import Prelude hiding ( read )

data Vector          a = Vector        ByteArray#
data MutableVector s a = MutableVector (MutableByteArray# s)

size :: Unbox a => a -> Int -> Int
{-# INLINE size #-}
size a (I# i#) = I# (size# a i#)

new :: forall s a. Unbox a => Int -> ST s (MutableVector s a)
{-# INLINE new #-}
new (I# n#) = ST $ \s# ->
  case newByteArray# (size# (undefined :: a) n#) s# of
    (# s2#, arr# #) -> (# s2#, MutableVector arr# #)

unsafeFreeze :: Unbox a => MutableVector s a -> ST s (Vector a)
{-# INLINE unsafeFreeze #-}
unsafeFreeze (MutableVector arr#) = ST $ \s# ->
  case unsafeFreezeByteArray# arr# s# of
    (# s2, frozen# #) -> (# s2, Vector frozen# #)

at :: Unbox a => Vector a -> Int -> a
{-# INLINE at #-}
at (Vector arr#) (I# i#) = at# arr# i#

read :: Unbox a => MutableVector s a -> Int -> ST s a
{-# INLINE read #-}
read (MutableVector arr#) (I# i#) = ST $ read# arr# i#

write :: Unbox a => MutableVector s a -> Int -> a -> ST s ()
{-# INLINE write #-}
write (MutableVector arr#) (I# i#) x = ST $ \s# ->
  case write# arr# i# x s# of s2# -> (# s2#, () #)

copy :: Unbox a => MutableVector s a -> Int
                -> MutableVector s a -> Int -> Int
                -> ST s ()
{-# INLINE copy #-}
copy mv i mw j n = do_copy i j n
  where
    do_copy i j 0 = return ()
    do_copy i j n = do
                      x <- read mw j
                      write mv i x
                      do_copy (i+1) (j+1) (n-1)

grow :: Unbox a => MutableVector s a -> Int -> Double
                -> ST s (MutableVector s a, Int)
{-# INLINE grow #-}
grow v n r
  = do
      w <- new m
      copy w 0 v 0 n
      return (w, m)
  where
    n' = double2Int (int2Double n * r)
    m | n' <= n   = n+1
      | otherwise = n'

