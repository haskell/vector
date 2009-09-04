{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- |
-- Module      : Data.Vector.Mutable.Unboxed.IO
-- Copyright   : (c) Roman Leshchinskiy 2009
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
-- 
-- Mutable unboxed vectors in the IO monad.
--

module Data.Vector.Unboxed.Mutable.IO ( Vector(..) )
where

import           Data.Vector.MVector ( MVector(..), MVectorPure(..) )
import qualified Data.Vector.Unboxed.Mutable.ST as STV
import           Data.Vector.Unboxed.Unbox ( Unbox )

import GHC.Base   ( RealWorld )
import GHC.ST     ( ST(..) )
import GHC.IOBase ( IO(..) )

import Prelude hiding ( length )

-- | IO-based mutable vectors
newtype Vector a = Vector (STV.Vector RealWorld a)

instance Unbox a => MVectorPure Vector a where
  {-# INLINE length #-}
  length (Vector v) = length v

  {-# INLINE unsafeSlice #-}
  unsafeSlice (Vector v) j m = Vector (unsafeSlice v j m)

  {-# INLINE overlaps #-}
  overlaps (Vector v1) (Vector v2) = overlaps v1 v2

instance Unbox a => MVector Vector IO a where
  {-# INLINE unsafeNew #-}
  unsafeNew n = Vector `fmap` stToIO (unsafeNew n)

  {-# INLINE unsafeNewWith #-}
  unsafeNewWith n x = Vector `fmap` stToIO (unsafeNewWith n x)

  {-# INLINE unsafeRead #-}
  unsafeRead (Vector v) i = stToIO (unsafeRead v i)

  {-# INLINE unsafeWrite #-}
  unsafeWrite (Vector v) i x = stToIO (unsafeWrite v i x)

  {-# INLINE clear #-}
  clear (Vector v) = stToIO (clear v)

stToIO :: ST RealWorld a -> IO a
stToIO (ST m) = IO m

