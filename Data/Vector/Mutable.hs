{-# LANGUAGE BangPatterns #-}

module Data.Vector.Mutable (
  Vector,

  new, new', length, slice, read, write, fill,
  dataOf
) where

import qualified Data.Vector.Prim as Prim

import qualified Data.Vector.Stream as Stream
import           Data.Vector.Stream ( Stream )

import Control.Exception ( assert )
import Control.Monad.ST  ( ST )

import Prelude hiding ( length, read )

data Vector s a = Vector {-# UNPACK #-} !Int
                         {-# UNPACK #-} !Int
                         {-# UNPACK #-} !(Prim.MutableVector s a)

dataOf :: Vector s a -> (Prim.MutableVector s a, Int, Int)
{-# INLINE dataOf #-}
dataOf (Vector i n v) = (v, i, n)

new :: Int -> ST s (Vector s a)
{-# INLINE new #-}
new n = new' n (error "Data.Vector.Mutable: uninitialised element")

new' :: Int -> a -> ST s (Vector s a)
{-# INLINE new' #-}
new' n x = assert (n >= 0)
         $ Vector 0 n `fmap` Prim.new' n x

length :: Vector s a -> Int
{-# INLINE length #-}
length (Vector _ n _) = n

slice :: Vector s a -> Int -> Int -> Vector s a
{-# INLINE slice #-}
slice (Vector i n v) j m
  = assert (j + m <= n && j >= 0 && m >= 0)
  $ Vector (i+j) m v

read :: Vector s a -> Int -> ST s a
{-# INLINE read #-}
read (Vector i n v) j
  = assert (j < n)
  $ Prim.read v (i+j)

write :: Vector s a -> Int -> a -> ST s ()
{-# INLINE write #-}
write (Vector i n v) j x
  = assert (j < n)
  $ Prim.write v (i+j) x

fill :: Vector s a -> Stream a -> ST s Int
{-# INLINE fill #-}
fill !v s = Stream.foldM put 0 s
  where
    {-# INLINE put #-}
    put i x = do { write v i x; return (i+1) }

