{-# LANGUAGE MagicHash, UnboxedTuples #-}

module Data.Vector.Prim (
  Vector, MutableVector,
  new, new', unsafeFreeze, at, at', read, write, copy, grow
) where

import GHC.Prim (
    Array#, MutableArray#,
    newArray#, readArray#, writeArray#, indexArray#, unsafeFreezeArray#
  )
import GHC.Float (
    double2Int, int2Double
  )
import GHC.ST (
    ST(..)
  )
import GHC.Base (
    Int(..)
  )

import Prelude hiding ( read )

data Vector          a = Vector (Array# a)
data MutableVector s a = MutableVector (MutableArray# s a)

new :: Int -> ST s (MutableVector s a)
{-# INLINE new #-}
new n = new' n (error "Data.Vector: uninitialised element")

new' :: Int -> a -> ST s (MutableVector s a)
{-# INLINE new' #-}
new' (I# n#) x = ST $ \s# ->
  case newArray# n# x s# of
    (# s2#, arr# #) -> (# s2#, MutableVector arr# #)

unsafeFreeze :: MutableVector s a -> ST s (Vector a)
{-# INLINE unsafeFreeze #-}
unsafeFreeze (MutableVector arr#) = ST $ \s# ->
  case unsafeFreezeArray# arr# s# of
    (# s2, frozen# #) -> (# s2, Vector frozen# #)

at :: Vector a -> Int -> a
{-# INLINE at #-}
at v i = at' v i id

at' :: Vector a -> Int -> (a -> b) -> b
{-# INLINE at' #-}
at' (Vector arr#) (I# n#) f = case indexArray# arr# n# of (# x #) -> f x

read :: MutableVector s a -> Int -> ST s a
{-# INLINE read #-}
read (MutableVector arr#) (I# n#) = ST $ readArray# arr# n#

write :: MutableVector s a -> Int -> a -> ST s ()
{-# INLINE write #-}
write (MutableVector arr#) (I# n#) x = ST $ \s# ->
  case writeArray# arr# n# x s# of s2# -> (# s2#, () #)

copy :: MutableVector s a -> Int -> MutableVector s a -> Int -> Int -> ST s ()
{-# INLINE copy #-}
copy mv i mw j n = do_copy i j n
  where
    do_copy i j 0 = return ()
    do_copy i j n = do
                      x <- read mw j
                      write mv i x
                      do_copy (i+1) (j+1) (n-1)

grow :: MutableVector s a -> Int -> Double -> ST s (MutableVector s a, Int)
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

