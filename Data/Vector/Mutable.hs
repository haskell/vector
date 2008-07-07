{-# LANGUAGE BangPatterns #-}

module Data.Vector.Mutable (
  Vector,

  new, new', length, slice, read, write, unstream, fill,
  dataOf
) where

import qualified Data.Vector.Prim as Prim

import           Data.Vector.Stream.Size ( upperBound )
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

unstream :: Stream a -> ST s (Vector s a)
{-# INLINE unstream #-}
unstream s = case upperBound (Stream.size s) of
               Just n  -> unstream_known   s n
               Nothing -> unstream_unknown s

gROWTH_FACTOR :: Double
gROWTH_FACTOR = 1.6

unstream_known :: Stream a -> Int -> ST s (Vector s a)
{-# INLINE unstream_known #-}
unstream_known s n
  = do
      v  <- new n
      n' <- fill v s
      return $ slice v 0 n'

unstream_unknown :: Stream a -> ST s (Vector s a)
{-# INLINE unstream_unknown #-}
unstream_unknown s
  = do
      v <- Prim.new 0
      (w, n, _) <- Stream.foldM put (v, 0, 0) s
      return $ Vector 0 n w
  where
    {-# INLINE put #-}
    put (v, i, n) x = do
                        (v', n') <- enlarge v i n
                        Prim.write v' i x
                        return (v', i+1, n')

    {-# INLINE enlarge #-}
    enlarge v i n | i < n     = return (v, n)
                  | otherwise = Prim.grow v n gROWTH_FACTOR

fill :: Vector s a -> Stream a -> ST s Int
{-# INLINE fill #-}
fill !v s = Stream.foldM put 0 s
  where
    {-# INLINE put #-}
    put i x = do { write v i x; return (i+1) }

