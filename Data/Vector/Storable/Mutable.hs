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

module Data.Vector.Storable.Mutable( Vector(..) )
where

import Data.Vector.Generic.Mutable ( MVector(..), MVectorPure(..) )

import Foreign.Storable
import Foreign.ForeignPtr

-- | Mutable 'Storable'-based vectors in the 'IO' monad.
data Vector a = Vector {-# UNPACK #-} !Int
                       {-# UNPACK #-} !Int
                       {-# UNPACK #-} !(ForeignPtr a)

instance MVectorPure Vector a where
  {-# INLINE length #-}
  length (Vector _ n _) = n

  {-# INLINE unsafeSlice #-}
  unsafeSlice (Vector i _ p) j m = Vector (i+j) m p

  -- FIXME: implement this properly
  {-# INLINE overlaps #-}
  overlaps (Vector i m p) (Vector j n q)
    = True

instance Storable a => MVector Vector IO a where
  {-# INLINE unsafeNew #-}
  unsafeNew n = Vector 0 n `fmap` mallocForeignPtrArray n

  {-# INLINE unsafeRead #-}
  unsafeRead (Vector i n p) j = withForeignPtr p $ \ptr ->
                                peekElemOff ptr (i+j)
     
  {-# INLINE unsafeWrite #-}
  unsafeWrite (Vector i n p) j x = withForeignPtr p $ \ptr ->
                                   pokeElemOff ptr (i+j) x 

  {-# INLINE clear #-}
  clear _ = return ()

