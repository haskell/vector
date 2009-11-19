{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables #-}

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

-- | Mutable unboxed vectors. They live in the 'ST' monad.
data MVector m a = MVector {-# UNPACK #-} !Int
                         {-# UNPACK #-} !Int
                         {-# UNPACK #-} !(MutableByteArray m)

type IOVector = MVector IO
type STVector s = MVector (ST s)

instance Prim a => G.MVectorPure (MVector m) a where
  length (MVector _ n _) = n
  unsafeSlice (MVector i _ arr) j m = MVector (i+j) m arr

  {-# INLINE overlaps #-}
  overlaps (MVector i m arr1) (MVector j n arr2)
    = sameMutableByteArray arr1 arr2
      && (between i j (j+n) || between j i (i+m))
    where
      between x y z = x >= y && x < z


instance (Prim a, PrimMonad m) => G.MVector (MVector m) m a where
  {-# INLINE unsafeNew #-}
  unsafeNew n = do
                  arr <- newByteArray (n * sizeOf (undefined :: a))
                  return (MVector 0 n arr)

  {-# INLINE unsafeRead #-}
  unsafeRead (MVector i _ arr) j = readByteArray arr (i+j)

  {-# INLINE unsafeWrite #-}
  unsafeWrite (MVector i _ arr) j x = writeByteArray arr (i+j) x

  {-# INLINE clear #-}
  clear _ = return ()

