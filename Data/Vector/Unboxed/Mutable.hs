{-# LANGUAGE BangPatterns #-}

module Data.Vector.Unboxed.Mutable (
  Vector,

  new, length, slice, read, write, unstream, fill, fillIndexed,
  dataOf
) where

import qualified Data.Vector.Unboxed.Prim as Prim
import           Data.Vector.Unboxed.Unbox ( Unbox )

import           Data.Vector.Stream.Size ( upperBound )
import qualified Data.Vector.Stream as Stream
import           Data.Vector.Stream ( Stream )

import Control.Exception ( assert )
import Control.Monad.ST  ( ST )

import Prelude hiding ( length, read )

data Vector s a = Vector {-# UNPACK #-} !Int                       -- ^ start
                         {-# UNPACK #-} !Int                       -- ^ length
                         {-# UNPACK #-} !(Prim.MutableVector s a)  -- ^ data

dataOf :: Vector s a -> (Prim.MutableVector s a, Int, Int)
{-# INLINE dataOf #-}
dataOf (Vector i n v) = (v, i, n)

new :: Unbox a => Int -> ST s (Vector s a)
{-# INLINE new #-}
new n = assert (n >= 0)
      $ Vector 0 n `fmap` Prim.new n

length :: Unbox a => Vector s a -> Int
{-# INLINE length #-}
length (Vector _ n _) = n

slice :: Unbox a => Vector s a -> Int -> Int -> Vector s a
{-# INLINE slice #-}
slice (Vector i n v) j m
  = assert (j + m <= n && j >= 0 && m >= 0)
  $ Vector (i+j) m v

slicel :: Unbox a => Vector s a -> Int -> Vector s a
{-# INLINE slicel #-}
slicel (Vector i n v) m
  = assert (m <= n && m >= 0)
  $ Vector i m v

read :: Unbox a => Vector s a -> Int -> ST s a
{-# INLINE read #-}
read (Vector i n v) j
  = assert (j < n)
  $ Prim.read v (i+j)

write :: Unbox a => Vector s a -> Int -> a -> ST s ()
{-# INLINE write #-}
write (Vector i n v) j x
  = assert (j < n)
  $ Prim.write v (i+j) x

unstream :: Unbox a => Stream a -> ST s (Vector s a)
{-# INLINE unstream #-}
unstream s = case upperBound (Stream.size s) of
               Just n  -> unstream_known   s n
               Nothing -> unstream_unknown s

gROWTH_FACTOR :: Double
gROWTH_FACTOR = 1.6

unstream_known :: Unbox a => Stream a -> Int -> ST s (Vector s a)
{-# INLINE unstream_known #-}
unstream_known s n
  = do
      v  <- new n
      n' <- fill v s
      return $ slice v 0 n'

unstream_unknown :: Unbox a => Stream a -> ST s (Vector s a)
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

fill :: Unbox a => Vector s a -> Stream a -> ST s Int
{-# INLINE fill #-}
fill !v s = Stream.foldM put 0 s
  where
    {-# INLINE put #-}
    put i x = do { write v i x; return (i+1) }

fillIndexed :: Unbox a => Vector s a -> Stream (Int, a) -> ST s ()
{-# INLINE fillIndexed #-}
fillIndexed !v s = Stream.mapM_ put s
  where
    {-# INLINE put #-}
    put (i,x) = write v i x

copyTo :: Unbox a => Vector s a -> Vector s a -> ST s ()
{-# INLINE copyTo #-}
copyTo !v !w = assert (length v == length w)
             $ copy_loop 0
  where
    n = length v

    copy_loop i | i < n     = do
                                x <- read v i
                                write w i x
                                copy_loop (i+1)
                | otherwise = return ()

clone :: Unbox a => Vector s a -> ST s (Vector s a)
{-# INLINE clone #-}
clone v = do
            w <- new (length v)
            v `copyTo` w
            return w

