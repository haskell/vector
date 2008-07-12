{-# LANGUAGE Rank2Types #-}

#include "phases.h"

module Data.Vector.MVector.Mut (
  Mut(..), run, unstream, update
) where

import qualified Data.Vector.MVector as MVector
import           Data.Vector.MVector ( MVector )

import           Data.Vector.Stream ( Stream )

data Mut a = Mut (forall m mv. MVector mv m a => m (mv a))

run :: MVector mv m a => Mut a -> m (mv a)
{-# INLINE run #-}
run (Mut p) = p

unstream :: Stream a -> Mut a
{-# INLINE_STREAM unstream #-}
unstream s = Mut (MVector.unstream s)

update :: Mut a -> Stream (Int, a) -> Mut a
{-# INLINE_STREAM update #-}
update (Mut p) s = Mut (do { v <- p; MVector.update v s; return v })

