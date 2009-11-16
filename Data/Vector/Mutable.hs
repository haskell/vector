{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- |
-- Module      : Data.Vector.Mutable
-- Copyright   : (c) Roman Leshchinskiy 2008-2009
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
-- 
-- Mutable boxed vectors.
--

module Data.Vector.Mutable ( Vector(..), IOVector, STVector )
where

import qualified Data.Vector.MVector as MVector
import           Data.Vector.MVector ( MVector, MVectorPure )
import           Data.Primitive.Array
import           Control.Monad.Primitive ( PrimMonad )
import           Control.Monad.ST ( ST )

-- | Mutable boxed vectors keyed on the monad they live in ('IO' or @'ST' s@).
data Vector m a = Vector {-# UNPACK #-} !Int
                         {-# UNPACK #-} !Int
                         {-# UNPACK #-} !(MutableArray m a)

type IOVector = Vector IO
type STVector s = Vector (ST s)

instance MVectorPure (Vector m) a where
  length (Vector _ n _) = n
  unsafeSlice (Vector i _ arr) j m = Vector (i+j) m arr

  {-# INLINE overlaps #-}
  overlaps (Vector i m arr1) (Vector j n arr2)
    = sameMutableArray arr1 arr2
      && (between i j (j+n) || between j i (i+m))
    where
      between x y z = x >= y && x < z


instance PrimMonad m => MVector (Vector m) m a where
  {-# INLINE unsafeNew #-}
  unsafeNew n = do
                  arr <- newArray n uninitialised
                  return (Vector 0 n arr)

  {-# INLINE unsafeNewWith #-}
  unsafeNewWith n x = do
                        arr <- newArray n x
                        return (Vector 0 n arr)

  {-# INLINE unsafeRead #-}
  unsafeRead (Vector i _ arr) j = readArray arr (i+j)

  {-# INLINE unsafeWrite #-}
  unsafeWrite (Vector i _ arr) j x = writeArray arr (i+j) x

  {-# INLINE clear #-}
  clear v = MVector.set v uninitialised

uninitialised :: a
uninitialised = error "Data.Vector.Mutable: uninitialised element"

