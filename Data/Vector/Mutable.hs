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

module Data.Vector.Mutable ( MVector(..), IOVector, STVector )
where

import qualified Data.Vector.Generic.Mutable as G
import           Data.Primitive.Array
import           Control.Monad.Primitive ( PrimMonad )
import           Control.Monad.ST ( ST )

#include "vector.h"

-- | Mutable boxed vectors keyed on the monad they live in ('IO' or @'ST' s@).
data MVector m a = MVector {-# UNPACK #-} !Int
                           {-# UNPACK #-} !Int
                           {-# UNPACK #-} !(MutableArray m a)

type IOVector = MVector IO
type STVector s = MVector (ST s)

instance G.MVectorPure (MVector m) a where
  length (MVector _ n _) = n
  unsafeSlice (MVector i n arr) j m
    = UNSAFE_CHECK(checkSlice) "unsafeSlice" j m n
    $ MVector (i+j) m arr

  {-# INLINE overlaps #-}
  overlaps (MVector i m arr1) (MVector j n arr2)
    = sameMutableArray arr1 arr2
      && (between i j (j+n) || between j i (i+m))
    where
      between x y z = x >= y && x < z


instance PrimMonad m => G.MVector (MVector m) m a where
  {-# INLINE unsafeNew #-}
  unsafeNew n = UNSAFE_CHECK(checkLength) "unsafeNew" n
              $ do
                  arr <- newArray n uninitialised
                  return (MVector 0 n arr)

  {-# INLINE unsafeNewWith #-}
  unsafeNewWith n x = UNSAFE_CHECK(checkLength) "unsafeNewWith" n
                    $ do
                        arr <- newArray n x
                        return (MVector 0 n arr)

  {-# INLINE unsafeRead #-}
  unsafeRead (MVector i n arr) j = UNSAFE_CHECK(checkIndex) "unsafeRead" j n
                                 $ readArray arr (i+j)

  {-# INLINE unsafeWrite #-}
  unsafeWrite (MVector i n arr) j x = UNSAFE_CHECK(checkIndex) "unsafeWrite" j n
                                    $ writeArray arr (i+j) x

  {-# INLINE clear #-}
  clear v = G.set v uninitialised

uninitialised :: a
uninitialised = error "Data.Vector.Mutable: uninitialised element"

