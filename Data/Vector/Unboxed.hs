{-# LANGUAGE RankNTypes #-}

module Data.Vector.Unboxed
where

import Data.Vector.Unboxed.Unbox
import qualified Data.Vector.Unboxed.Mutable as Mut

import qualified Data.Vector.Stream as Stream
import           Data.Vector.Stream ( Step(..), Stream(..) )

import Control.Exception ( assert )
import Control.Monad.ST  ( ST, runST )

import Prelude hiding ( length )

data Vector a = Vector !Int
                       !Int
                       !(Array a)

new :: Unbox a
    => Int -> (forall s. Mut.Vector s a -> ST s (Mut.Vector s a)) -> Vector a
{-# INLINE new #-}
new n init = runST (
  do
    mv  <- Mut.new n
    mv' <- init mv
    let (marr, i, n') = Mut.dataOf mv'
    arr <- unsafeFreezeArray marr
    return $ Vector i n' arr
  )

stream :: Unbox a => Vector a -> Stream a
{-# INLINE_STREAM stream #-}
stream (Vector i n arr) = Stream get i n
  where
    n' = n+i

    {-# INLINE get #-}
    get j | j < n'    = Yield (indexArray arr j) (j+1)
          | otherwise = Done

unstream :: Unbox a => Stream a -> Vector a
{-# INLINE_STREAM unstream #-}
unstream s@(Stream _ _ n) = new n (\mv ->
  do
    n' <- Mut.fill mv s
    return $ Mut.slice mv 0 n'
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
unsafeAt (Vector i _ arr) j = indexArray arr (i+j)

at :: Unbox a => Vector a -> Int -> a
{-# INLINE at #-}
at v i = assert (i >= 0 && i < length v)
       $ unsafeAt v i


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

