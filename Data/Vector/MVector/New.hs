{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}

#include "phases.h"

module Data.Vector.MVector.New (
  New(..), run, unstream, transform, update, reverse,
  slice, init, tail, take, drop
) where

import qualified Data.Vector.MVector as MVector
import           Data.Vector.MVector ( MVector, MVectorPure )

import           Data.Vector.Fusion.Stream ( Stream, MStream )
import qualified Data.Vector.Fusion.Stream as Stream

import qualified Data.Vector.Fusion.Stream.Monadic as MStream

import Control.Monad  ( liftM )
import Prelude hiding ( init, tail, take, drop, reverse, map, filter )

newtype New a = New (forall m mv. MVector mv m a => m (mv a))

run :: MVector mv m a => New a -> m (mv a)
{-# INLINE run #-}
run (New p) = p

apply :: (forall mv a. MVectorPure mv a => mv a -> mv a) -> New a -> New a
{-# INLINE apply #-}
apply f (New p) = New (liftM f p)

modify :: New a -> (forall m mv. MVector mv m a => mv a -> m ()) -> New a
{-# INLINE modify #-}
modify (New p) q = New (do { v <- p; q v; return v })

unstream :: Stream a -> New a
{-# INLINE_STREAM unstream #-}
unstream s = New (MVector.unstream s)

transform :: (forall m. Monad m => MStream m a -> MStream m a) -> New a -> New a
{-# INLINE_STREAM transform #-}
transform f (New p) = New (MVector.transform f =<< p)

{-# RULES

"transform/transform [New]"
  forall (f :: forall m. Monad m => MStream m a -> MStream m a)
         (g :: forall m. Monad m => MStream m a -> MStream m a)
         p .
  transform f (transform g p) = transform (f . g) p

"transform/unstream [New]"
  forall (f :: forall m. Monad m => MStream m a -> MStream m a)
         s.
  transform f (unstream s) = unstream (f s)

 #-}

slice :: New a -> Int -> Int -> New a
{-# INLINE_STREAM slice #-}
slice m i n = apply (\v -> MVector.slice v i n) m

init :: New a -> New a
{-# INLINE_STREAM init #-}
init m = apply (\v -> MVector.slice v 0 (MVector.length v - 1)) m

tail :: New a -> New a
{-# INLINE_STREAM tail #-}
tail m = apply (\v -> MVector.slice v 1 (MVector.length v - 1)) m

take :: Int -> New a -> New a
{-# INLINE_STREAM take #-}
take n m = apply (\v -> MVector.slice v 0 (min n (MVector.length v))) m

drop :: Int -> New a -> New a
{-# INLINE_STREAM drop #-}
drop n m = apply (\v -> MVector.slice v n (max 0 (MVector.length v - n))) m

update :: New a -> Stream (Int, a) -> New a
{-# INLINE_STREAM update #-}
update m s = modify m (\v -> MVector.update v s)

reverse :: New a -> New a
{-# INLINE_STREAM reverse #-}
reverse m = modify m (MVector.reverse)

