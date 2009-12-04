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

import Control.Monad.Primitive ( RealWorld )
import Control.Monad.ST ( ST, unsafeIOToST )

#include "vector.h"

-- | Mutable 'Storable'-based vectors
data MVector s a = MVector {-# UNPACK #-} !Int
                           {-# UNPACK #-} !Int
                           {-# UNPACK #-} !(ForeignPtr a)

instance G.MVectorPure (MVector s) a where
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

instance Storable a => G.MVector (MVector s) (ST s) a where
  {-# INLINE unsafeNew #-}
  unsafeNew n = unsafeIOToST (unsafeNewIO n)

  {-# INLINE unsafeRead #-}
  unsafeRead v i = unsafeIOToST (unsafeReadIO v i)
    
  {-# INLINE unsafeWrite #-}
  unsafeWrite v i x = unsafeIOToST (unsafeWriteIO v i x)

  {-# INLINE clear #-}
  clear _ = return ()


instance Storable a => G.MVector (MVector RealWorld) IO a where
  {-# INLINE unsafeNew #-}
  unsafeNew = unsafeNewIO

  {-# INLINE unsafeRead #-}
  unsafeRead = unsafeReadIO
     
  {-# INLINE unsafeWrite #-}
  unsafeWrite = unsafeWriteIO

  {-# INLINE clear #-}
  clear _ = return ()

unsafeNewIO :: Storable a => Int -> IO (MVector s a)
{-# unsafeNewIO #-}
unsafeNewIO n = UNSAFE_CHECK(checkLength) "unsafeNew" n
              $ MVector 0 n `fmap` mallocForeignPtrArray n

unsafeReadIO :: Storable a => MVector s a -> Int -> IO a
{-# INLINE unsafeReadIO #-}
unsafeReadIO (MVector i n p) j = UNSAFE_CHECK(checkIndex) "unsafeRead" j n
                               $ withForeignPtr p $ \ptr ->
                                peekElemOff ptr (i+j)

unsafeWriteIO :: Storable a => MVector s a -> Int -> a -> IO ()
{-# INLINE unsafeWriteIO #-}
unsafeWriteIO (MVector i n p) j x = UNSAFE_CHECK(checkIndex) "unsafeWrite" j n
                                  $ withForeignPtr p $ \ptr ->
                                    pokeElemOff ptr (i+j) x 

