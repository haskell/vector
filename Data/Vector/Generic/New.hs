{-# LANGUAGE Rank2Types, FlexibleContexts #-}

-- |
-- Module      : Data.Vector.Generic.New
-- Copyright   : (c) Roman Leshchinskiy 2008-2009
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
-- 
-- Purely functional interface to initialisation of mutable vectors
--

module Data.Vector.Generic.New (
  New(..), run, unstream, transform, accum, update, reverse,
  slice, init, tail, take, drop,
  unsafeSlice
) where

import qualified Data.Vector.Generic.Mutable as MVector
import           Data.Vector.Generic.Mutable ( MVector )

import           Data.Vector.Fusion.Stream ( Stream, MStream )
import qualified Data.Vector.Fusion.Stream as Stream

import Control.Monad.ST ( ST )
import Control.Monad  ( liftM )
import Prelude hiding ( init, tail, take, drop, reverse, map, filter )

#include "vector.h"

newtype New a = New (forall mv s. MVector mv a => ST s (mv s a))

run :: MVector mv a => New a -> ST s (mv s a)
{-# INLINE run #-}
run (New p) = p

apply :: (forall mv s a. MVector mv a => mv s a -> mv s a) -> New a -> New a
{-# INLINE apply #-}
apply f (New p) = New (liftM f p)

modify :: New a -> (forall mv s. MVector mv a => mv s a -> ST s ()) -> New a
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

unsafeSlice :: New a -> Int -> Int -> New a
{-# INLINE_STREAM unsafeSlice #-}
unsafeSlice m i n = apply (\v -> MVector.unsafeSlice v i n) m

init :: New a -> New a
{-# INLINE_STREAM init #-}
init m = apply (\v -> MVector.slice v 0 (MVector.length v - 1)) m

tail :: New a -> New a
{-# INLINE_STREAM tail #-}
tail m = apply (\v -> MVector.slice v 1 (MVector.length v - 1)) m

take :: Int -> New a -> New a
{-# INLINE_STREAM take #-}
take n m = apply (\v -> MVector.unsafeSlice v 0
                                (min (max 0 n) (MVector.length v))) m

drop :: Int -> New a -> New a
{-# INLINE_STREAM drop #-}
drop n m = apply (\v -> MVector.unsafeSlice v
                                (min (max 0 n) (MVector.length v))
                                (max 0 (MVector.length v - n))) m

{-# RULES

"slice/unstream [New]" forall s i n.
  slice (unstream s) i n = unstream (Stream.extract s i n)

"unsafeSlice/unstream [New]" forall s i n.
  unsafeSlice (unstream s) i n = unstream (Stream.extract s i n)

"init/unstream [New]" forall s.
  init (unstream s) = unstream (Stream.init s)

"tail/unstream [New]" forall s.
  tail (unstream s) = unstream (Stream.tail s)

"take/unstream [New]" forall n s.
  take n (unstream s) = unstream (Stream.take n s)

"drop/unstream [New]" forall n s.
  drop n (unstream s) = unstream (Stream.drop n s)

  #-}

accum :: (a -> b -> a) -> New a -> Stream (Int, b) -> New a
{-# INLINE_STREAM accum #-}
accum f m s = modify m (\v -> MVector.accum f v s)

update :: New a -> Stream (Int, a) -> New a
{-# INLINE_STREAM update #-}
update m s = modify m (\v -> MVector.update v s)

reverse :: New a -> New a
{-# INLINE_STREAM reverse #-}
reverse m = modify m (MVector.reverse)

