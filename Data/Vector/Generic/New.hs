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
  unsafeSlice, unsafeInit, unsafeTail,
  unsafeAccum, unsafeUpdate
) where

import qualified Data.Vector.Generic.Mutable as MVector
import           Data.Vector.Generic.Mutable ( MVector )

import           Data.Vector.Fusion.Stream ( Stream, MStream )
import qualified Data.Vector.Fusion.Stream as Stream

import Control.Monad.ST ( ST )
import Control.Monad  ( liftM )
import Prelude hiding ( init, tail, take, drop, reverse, map, filter )

#include "vector.h"

data New a = New (forall mv s. MVector mv a => ST s (mv s a))

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
unstream s = s `seq` New (MVector.unstream s)

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

slice :: Int -> Int -> New a -> New a
{-# INLINE_STREAM slice #-}
slice i n m = apply (MVector.slice i n) m

init :: New a -> New a
{-# INLINE_STREAM init #-}
init m = apply MVector.init m

tail :: New a -> New a
{-# INLINE_STREAM tail #-}
tail m = apply MVector.tail m

take :: Int -> New a -> New a
{-# INLINE_STREAM take #-}
take n m = apply (MVector.take n) m

drop :: Int -> New a -> New a
{-# INLINE_STREAM drop #-}
drop n m = apply (MVector.drop n) m

unsafeSlice :: Int -> Int -> New a -> New a
{-# INLINE_STREAM unsafeSlice #-}
unsafeSlice i n m = apply (MVector.unsafeSlice i n) m

unsafeInit :: New a -> New a
{-# INLINE_STREAM unsafeInit #-}
unsafeInit m = apply MVector.unsafeInit m

unsafeTail :: New a -> New a
{-# INLINE_STREAM unsafeTail #-}
unsafeTail m = apply MVector.unsafeTail m

{-# RULES

"slice/unstream [New]" forall i n s.
  slice i n (unstream s) = unstream (Stream.slice i n s)

"init/unstream [New]" forall s.
  init (unstream s) = unstream (Stream.init s)

"tail/unstream [New]" forall s.
  tail (unstream s) = unstream (Stream.tail s)

"take/unstream [New]" forall n s.
  take n (unstream s) = unstream (Stream.take n s)

"drop/unstream [New]" forall n s.
  drop n (unstream s) = unstream (Stream.drop n s)

"unsafeSlice/unstream [New]" forall i n s.
  unsafeSlice i n (unstream s) = unstream (Stream.slice i n s)

"unsafeInit/unstream [New]" forall s.
  unsafeInit (unstream s) = unstream (Stream.init s)

"unsafeTail/unstream [New]" forall s.
  unsafeTail (unstream s) = unstream (Stream.tail s)

  #-}

unsafeAccum :: (a -> b -> a) -> New a -> Stream (Int, b) -> New a
{-# INLINE_STREAM unsafeAccum #-}
unsafeAccum f m s = s `seq` modify m (\v -> MVector.unsafeAccum f v s)

accum :: (a -> b -> a) -> New a -> Stream (Int, b) -> New a
{-# INLINE_STREAM accum #-}
accum f m s = s `seq` modify m (\v -> MVector.accum f v s)

unsafeUpdate :: New a -> Stream (Int, a) -> New a
{-# INLINE_STREAM unsafeUpdate #-}
unsafeUpdate m s = s `seq` modify m (\v -> MVector.unsafeUpdate v s)

update :: New a -> Stream (Int, a) -> New a
{-# INLINE_STREAM update #-}
update m s = s `seq` modify m (\v -> MVector.update v s)

reverse :: New a -> New a
{-# INLINE_STREAM reverse #-}
reverse m = modify m (MVector.reverse)

