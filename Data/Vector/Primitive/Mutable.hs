{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables,
             FlexibleContexts #-}

-- |
-- Module      : Data.Vector.Primitive.Mutable
-- Copyright   : (c) Roman Leshchinskiy 2008-2009
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
-- 
-- Mutable primitive vectors.
--

module Data.Vector.Primitive.Mutable ( MVector(..), IOVector, STVector )
where

import qualified Data.Vector.Generic.Mutable as G
import           Data.Primitive.ByteArray
import           Data.Primitive ( Prim, sizeOf )
import           Control.Monad.Primitive
import           Control.Monad.ST ( ST )

#include "vector.h"

-- | Mutable unboxed vectors. They live in the 'ST' monad.
data MVector s a = MVector {-# UNPACK #-} !Int
                           {-# UNPACK #-} !Int
                           {-# UNPACK #-} !(MutableByteArray s)

type IOVector = MVector RealWorld
type STVector s = MVector s

instance Prim a => G.MVector MVector a where
  length (MVector _ n _) = n
  unsafeSlice (MVector i n arr) j m
    = UNSAFE_CHECK(checkSlice) "unsafeSlice" j m n
    $ MVector (i+j) m arr

  {-# INLINE overlaps #-}
  overlaps (MVector i m arr1) (MVector j n arr2)
    = sameMutableByteArray arr1 arr2
      && (between i j (j+n) || between j i (i+m))
    where
      between x y z = x >= y && x < z

  {-# INLINE unsafeNew #-}
  unsafeNew n = do
                  arr <- newByteArray (n * sizeOf (undefined :: a))
                  return (MVector 0 n arr)

  {-# INLINE unsafeRead #-}
  unsafeRead (MVector i n arr) j = UNSAFE_CHECK(checkIndex) "unsafeRead" j n
                                 $ readByteArray arr (i+j)

  {-# INLINE unsafeWrite #-}
  unsafeWrite (MVector i n arr) j x = UNSAFE_CHECK(checkIndex) "unsafeWrite" j n
                                    $ writeByteArray arr (i+j) x

  {-# INLINE clear #-}
  clear _ = return ()

