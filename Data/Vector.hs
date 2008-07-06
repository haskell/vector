{-# LANGUAGE RankNTypes, CPP #-}

#include "phases.h"

module Data.Vector
where

import qualified Data.Vector.Prim    as Prim
import qualified Data.Vector.Mutable as Mut

import qualified Data.Vector.Stream  as Stream
import           Data.Vector.Stream ( Step(..), Stream(..) )

import Control.Exception ( assert )
import Control.Monad.ST  ( ST, runST )

import Prelude hiding ( length, (++) )

data Vector a = Vector {-# UNPACK #-} !Int
                       {-# UNPACK #-} !Int
                       {-# UNPACK #-} !(Prim.Vector a)

new :: Int -> (forall s. Mut.Vector s a -> ST s (Mut.Vector s a)) -> Vector a
{-# INLINE new #-}
new n init = runST (
  do
    mv <- Mut.new n
    mv' <- init mv
    let (mprim, i, n') = Mut.dataOf mv'
    prim <- Prim.unsafeFreeze mprim
    return $ Vector i n' prim
  )

new' :: Int -> a -> (forall s. Mut.Vector s a -> ST s (Mut.Vector s a)) -> Vector a
{-# INLINE new' #-}
new' n x init = runST (
  do
    mv <- Mut.new' n x
    mv' <- init mv
    let (mprim, i, n') = Mut.dataOf mv'
    prim <- Prim.unsafeFreeze mprim
    return $ Vector i n' prim
  )

stream :: Vector a -> Stream a
{-# INLINE_STREAM stream #-}
stream (Vector i n arr) = Stream get i n
  where
    n' = n+i

    {-# INLINE get #-}
    get j | j < n'    = Prim.at' arr j $ \x -> Yield x (j+1)
          | otherwise = Done

unstream :: Stream a -> Vector a
{-# INLINE_STREAM unstream #-}
unstream s@(Stream _ _ n) = new n (\mv ->
  do
    n' <- Mut.fill mv s
    return $ Mut.slice mv 0 n'
  )

{-# RULES

"stream/unstream [Vector]" forall s.
  stream (unstream s) = s

 #-}

length :: Vector a -> Int
{-# INLINE length #-}
length (Vector _ n _) = n

slice :: Vector a -> Int -> Int -> Vector a
{-# INLINE slice #-}
slice (Vector i n arr) j m
  = assert (j + m <= n && j >= 0 && m >= 0)
  $ Vector (i+j) m arr

unsafeAt ::Vector a -> Int -> a
{-# INLINE unsafeAt #-}
unsafeAt (Vector i _ arr) j = Prim.at arr (i+j)

at :: Vector a -> Int -> a
{-# INLINE at #-}
at v i = assert (i >= 0 && i < length v)
       $ unsafeAt v i

infixr ++
(++) :: Vector a -> Vector a -> Vector a
{-# INLINE (++) #-}
v ++ w = unstream (stream v Stream.++ stream w)

map :: (a -> b) -> Vector a -> Vector b
{-# INLINE map #-}
map f = unstream . Stream.map f . stream

filter :: (a -> Bool) -> Vector a -> Vector a
{-# INLINE filter #-}
filter f = unstream . Stream.filter f . stream

zipWith :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
{-# INLINE zipWith #-}
zipWith f v w = unstream
              $ Stream.zipWith f (stream v) (stream w)

foldl' :: (a -> b -> b) -> b -> Vector a -> b
{-# INLINE foldl' #-}
foldl' f z = Stream.foldl' f z . stream

