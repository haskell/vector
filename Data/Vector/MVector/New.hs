{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}

#include "phases.h"

module Data.Vector.MVector.New (
  New(..), run, unstream, inplace, update, reverse, map, filter
) where

import qualified Data.Vector.MVector as MVector
import           Data.Vector.MVector ( MVector )

import           Data.Vector.Fusion.Stream ( Stream, MStream )
import qualified Data.Vector.Fusion.Stream as Stream

import qualified Data.Vector.Fusion.Stream.Monadic as MStream

import Control.Monad  ( liftM )
import Prelude hiding ( reverse, map, filter )

newtype New a = New (forall m mv. MVector mv m a => m (mv a))

run :: MVector mv m a => New a -> m (mv a)
{-# INLINE run #-}
run (New p) = p

modify :: New a -> (forall m mv. MVector mv m a => mv a -> m ()) -> New a
{-# INLINE modify #-}
modify (New p) q = New (do { v <- p; q v; return v })

unstream :: Stream a -> New a
{-# INLINE_STREAM unstream #-}
unstream s = New (MVector.unstream s)

inplace :: (forall m. Monad m => MStream m a -> MStream m a) -> New a -> New a
{-# INLINE_STREAM inplace #-}
inplace f (New p) = New (MVector.transform f =<< p)

{-# RULES

"inplace/inplace [New]"
  forall (f :: forall m. Monad m => MStream m a -> MStream m a)
         (g :: forall m. Monad m => MStream m a -> MStream m a)
         p .
  inplace f (inplace g p) = inplace (f . g) p

 #-}

update :: New a -> Stream (Int, a) -> New a
{-# INLINE_STREAM update #-}
update m s = modify m (\v -> MVector.update v s)

reverse :: New a -> New a
{-# INLINE_STREAM reverse #-}
reverse m = modify m (MVector.reverse)

map :: (a -> a) -> New a -> New a
{-# INLINE map #-}
map f = inplace (MStream.map f)

filter :: (a -> Bool) -> New a -> New a
{-# INLINE filter #-}
filter f = inplace (MStream.filter f)

