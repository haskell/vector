{-# LANGUAGE TypeFamilies, FlexibleContexts, MultiParamTypeClasses #-}
module Data.Vector.Base.Mutable (
  Base(..),

  slice, new, newWith, read, write, copy, grow, unstream
) where

import qualified Data.Vector.Stream      as Stream
import           Data.Vector.Stream      ( Stream )
import           Data.Vector.Stream.Size

import Control.Monad.ST ( ST )
import Control.Exception ( assert )

import GHC.Float (
    double2Int, int2Double
  )

import Prelude hiding ( length, read )

gROWTH_FACTOR :: Double
gROWTH_FACTOR = 1.5

class Monad (Trans v) => Base v a where
  type Trans   v :: * -> *

  length           :: v a -> Int
  unsafeSlice      :: v a -> Int -> Int -> v a

  unsafeNew        :: Int -> Trans v (v a)
  unsafeNewWith    :: Int -> a -> Trans v (v a)

  unsafeRead       :: v a -> Int -> Trans v a
  unsafeWrite      :: v a -> Int -> a -> Trans v ()

  set              :: v a -> a -> Trans v ()
  unsafeCopy       :: v a -> v a -> Trans v ()
  unsafeGrow       :: v a -> Int -> Trans v (v a)

  overlaps         :: v a -> v a -> Bool

  {-# INLINE unsafeNewWith #-}
  unsafeNewWith n x = do
                        v <- unsafeNew n
                        set v x
                        return v

  {-# INLINE set #-}
  set v x = do_set 0
    where
      n = length v

      do_set i | i < n = do
                            unsafeWrite v i x
                            do_set (i+1)
                | otherwise = return ()

  {-# INLINE unsafeCopy #-}
  unsafeCopy dst src = do_copy 0
    where
      n = length src

      do_copy i | i < n = do
                            x <- unsafeRead src i
                            unsafeWrite dst i x
                            do_copy (i+1)
                | otherwise = return ()

  {-# INLINE unsafeGrow #-}
  unsafeGrow v by = do
                      v' <- unsafeNew (n+by)
                      unsafeCopy (unsafeSlice v' 0 n) v
                      return v'
    where
      n = length v

inBounds :: Base v a => v a -> Int -> Bool
{-# INLINE inBounds #-}
inBounds v i = i >= 0 && i < length v

slice :: Base v a => v a -> Int -> Int -> v a
{-# INLINE slice #-}
slice v i n = assert (i >=0 && n >= 0 && i+n <= length v)
            $ unsafeSlice v i n

new :: (Base v a, m ~ Trans v) => Int -> m (v a)
{-# INLINE new #-}
new n = assert (n >= 0) $ unsafeNew n

newWith :: (Base v a, m ~ Trans v) => Int -> a -> m (v a)
{-# INLINE newWith #-}
newWith n x = assert (n >= 0) $ unsafeNewWith n x

read :: (Base v a, m ~ Trans v) => v a -> Int -> m a
{-# INLINE read #-}
read v i = assert (inBounds v i) $ unsafeRead v i

write :: (Base v a, m ~ Trans v) => v a -> Int -> a -> m ()
{-# INLINE write #-}
write v i x = assert (inBounds v i) $ unsafeWrite v i x

copy :: (Base v a, m ~ Trans v) => v a -> v a -> m ()
{-# INLINE copy #-}
copy dst src = assert (not (dst `overlaps` src) && length dst == length src)
             $ unsafeCopy dst src

grow :: (Base v a, m ~ Trans v) => v a -> Int -> m (v a)
{-# INLINE grow #-}
grow v by = assert (by >= 0)
          $ unsafeGrow v by


unstream :: (Base v a, m ~ Trans v) => Stream a -> m (v a)
{-# INLINE unstream #-}
unstream s = case upperBound (Stream.size s) of
               Just n  -> unstreamMax     s n
               Nothing -> unstreamUnknown s

unstreamMax :: (Base v a, m ~ Trans v) => Stream a -> Int -> m (v a)
{-# INLINE unstreamMax #-}
unstreamMax s n
  = do
      v  <- new n
      let put i x = do { write v i x; return (i+1) }
      n' <- Stream.foldM put 0 s
      return $ slice v 0 n'

unstreamUnknown :: (Base v a, m ~ Trans v) => Stream a -> m (v a)
{-# INLINE unstreamUnknown #-}
unstreamUnknown s
  = do
      v <- new 0
      (v', n) <- Stream.foldM put (v, 0) s
      return $ slice v' 0 n
  where
    {-# INLINE put #-}
    put (v, i) x = do
                     v' <- enlarge v i
                     unsafeWrite v' i x
                     return (v', i+1)

    {-# INLINE enlarge #-}
    enlarge v i | i < length v = return v
                | otherwise    = unsafeGrow v
                                 . max 1
                                 . double2Int
                                 $ int2Double (length v) * gROWTH_FACTOR

