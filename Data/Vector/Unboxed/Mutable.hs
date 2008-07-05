{-# LANGUAGE BangPatterns #-}

module Data.Vector.Unboxed.Mutable (
  Vector,

  new, length, slice, read, write, fill, fillIndexed,
  dataOf
) where

import Data.Vector.Unboxed.Unbox

import qualified Data.Vector.Stream as Stream
import           Data.Vector.Stream ( Stream )

import Control.Exception ( assert )
import Control.Monad.ST  ( ST )

import Prelude hiding ( length, read )

data Vector s a = Vector !Int                  -- ^ start
                         !Int                  -- ^ length
                         !(MutableArray s a)   -- ^ data

dataOf :: Vector s a -> (MutableArray s a, Int, Int)
{-# INLINE dataOf #-}
dataOf (Vector i n arr) = (arr, i, n)

new :: Unbox a => Int -> ST s (Vector s a)
{-# INLINE new #-}
new n = assert (n >= 0)
      $ do
          arr <- newArray n
          return $ Vector 0 n arr

length :: Unbox a => Vector s a -> Int
{-# INLINE length #-}
length (Vector _ n _) = n

slice :: Unbox a => Vector s a -> Int -> Int -> Vector s a
{-# INLINE slice #-}
slice (Vector i n arr) j m
  = assert (j + m <= n && j >= 0 && m >= 0)
  $ Vector (i+j) m arr

slicel :: Unbox a => Vector s a -> Int -> Vector s a
{-# INLINE slicel #-}
slicel (Vector i n arr) m
  = assert (m <= n && m >= 0)
  $ Vector i m arr

read :: Unbox a => Vector s a -> Int -> ST s a
{-# INLINE read #-}
read (Vector i n arr) j
  = assert (j < n)
  $ readArray arr (i+j)

write :: Unbox a => Vector s a -> Int -> a -> ST s ()
{-# INLINE write #-}
write (Vector i n arr) j x
  = assert (j < n)
  $ writeArray arr (i+j) x

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

