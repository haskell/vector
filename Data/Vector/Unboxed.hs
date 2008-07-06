{-# LANGUAGE RankNTypes, CPP #-}

#include "phases.h"

module Data.Vector.Unboxed
where

import qualified Data.Vector.Unboxed.Prim as Prim
import qualified Data.Vector.Unboxed.Mutable as Mut
import           Data.Vector.Unboxed.Unbox ( Unbox )

import qualified Data.Vector.Stream as Stream
import           Data.Vector.Stream ( Stream )

import Control.Exception ( assert )
import Control.Monad.ST  ( ST, runST )

import Prelude hiding ( length, (++) )

data Vector a = Vector {-# UNPACK #-} !Int
                       {-# UNPACK #-} !Int
                       {-# UNPACK #-} !(Prim.Vector a)

new :: Unbox a
    => Int -> (forall s. Mut.Vector s a -> ST s (Mut.Vector s a)) -> Vector a
{-# INLINE new #-}
new n init = runST (
  do
    mv  <- Mut.new n
    mv' <- init mv
    let (mprim, i, n') = Mut.dataOf mv'
    prim <- Prim.unsafeFreeze mprim
    return $ Vector i n' prim
  )

stream :: Unbox a => Vector a -> Stream a
{-# INLINE_STREAM stream #-}
stream (Vector i n arr) = Stream.unfold get i n
  where
    n' = n+i

    {-# INLINE get #-}
    get j | j < n'    = Just (Prim.at arr j, j+1)
          | otherwise = Nothing

unstream :: Unbox a => Stream a -> Vector a
{-# INLINE_STREAM unstream #-}
unstream s = new (Stream.bound s) (\mv ->
  do
    n <- Mut.fill mv s
    return $ Mut.slice mv 0 n
  )

{-# RULES

"stream/unstream [Vector.Unboxed]" forall s.
  stream (unstream s) = s

 #-}

length :: Unbox a => Vector a -> Int
{-# INLINE length #-}
length (Vector _ n _) = n

slice :: Unbox a => Vector a -> Int -> Int -> Vector a
{-# INLINE slice #-}
slice (Vector i n arr) j m
  = assert (j + m <= n && j >= 0 && m >= 0)
  $ Vector (i+j) m arr

unsafeAt :: Unbox a => Vector a -> Int -> a
{-# INLINE unsafeAt #-}
unsafeAt (Vector i _ arr) j = Prim.at arr (i+j)

at :: Unbox a => Vector a -> Int -> a
{-# INLINE at #-}
at v i = assert (i >= 0 && i < length v)
       $ unsafeAt v i

infixr ++
(++) :: Unbox a => Vector a -> Vector a -> Vector a
{-# INLINE (++) #-}
v ++ w = unstream (stream v Stream.++ stream w)

map :: (Unbox a, Unbox b) => (a -> b) -> Vector a -> Vector b
{-# INLINE map #-}
map f = unstream . Stream.map f . stream

filter :: Unbox a => (a -> Bool) -> Vector a -> Vector a
{-# INLINE filter #-}
filter f = unstream . Stream.filter f . stream

zipWith :: (Unbox a, Unbox b, Unbox c)
        => (a -> b -> c) -> Vector a -> Vector b -> Vector c
{-# INLINE zipWith #-}
zipWith f v w = unstream
              $ Stream.zipWith f (stream v) (stream w)

foldl' :: Unbox a => (a -> b -> b) -> b -> Vector a -> b
{-# INLINE foldl' #-}
foldl' f z = Stream.foldl' f z . stream

