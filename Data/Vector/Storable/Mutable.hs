{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- |
-- Module      : Data.Vector.Storable.Mutable
-- Copyright   : (c) Roman Leshchinskiy 2009
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
-- 
-- Mutable vectors based on Storable.
--

module Data.Vector.Storable.Mutable( MVector(..) )
where

import qualified Data.Vector.Generic.Mutable as G

import Foreign.Storable
import Foreign.ForeignPtr

#include "vector.h"

-- | Mutable 'Storable'-based vectors in the 'IO' monad.
data MVector a = MVector {-# UNPACK #-} !Int
                         {-# UNPACK #-} !Int
                         {-# UNPACK #-} !(ForeignPtr a)

instance G.MVectorPure MVector a where
  {-# INLINE length #-}
  length (MVector _ n _) = n

  {-# INLINE unsafeSlice #-}
  unsafeSlice (MVector i n p) j m
    = UNSAFE_CHECK(checkSlice) "unsafeSlice" j m n
    $ MVector (i+j) m p

  -- FIXME: implement this properly
  {-# INLINE overlaps #-}
  overlaps (MVector i m p) (MVector j n q)
    = True

instance Storable a => G.MVector MVector IO a where
  {-# INLINE unsafeNew #-}
  unsafeNew n = UNSAFE_CHECK(checkLength) "unsafeNew" n
              $ MVector 0 n `fmap` mallocForeignPtrArray n

  {-# INLINE unsafeRead #-}
  unsafeRead (MVector i n p) j = UNSAFE_CHECK(checkIndex) "unsafeRead" j n
                               $ withForeignPtr p $ \ptr ->
                                 peekElemOff ptr (i+j)
     
  {-# INLINE unsafeWrite #-}
  unsafeWrite (MVector i n p) j x = UNSAFE_CHECK(checkIndex) "unsafeWrite" j n
                                  $ withForeignPtr p $ \ptr ->
                                    pokeElemOff ptr (i+j) x 

  {-# INLINE clear #-}
  clear _ = return ()

