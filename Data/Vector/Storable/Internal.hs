{-# LANGUAGE MagicHash, UnboxedTuples, ScopedTypeVariables #-}

-- |
-- Module      : Data.Vector.Storable.Internal
-- Copyright   : (c) Roman Leshchinskiy 2009-2010
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
-- 
-- Ugly internal utility functions for implementing 'Storable'-based vectors.
--

module Data.Vector.Storable.Internal (
  ptrToOffset, offsetToPtr
) where

import Control.Monad.Primitive ( unsafeInlineIO )
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Marshal.Array ( advancePtr )
import GHC.Base         ( quotInt )

distance :: forall a. Storable a => Ptr a -> Ptr a -> Int
{-# INLINE distance #-}
distance p q = (p `minusPtr` q) `quotInt` sizeOf (undefined :: a)

ptrToOffset :: Storable a => ForeignPtr a -> Ptr a -> Int
{-# INLINE ptrToOffset #-}
ptrToOffset fp q = unsafeInlineIO
                 $ withForeignPtr fp $ \p -> return (distance q p)

offsetToPtr :: Storable a => ForeignPtr a -> Int -> Ptr a
{-# INLINE offsetToPtr #-}
offsetToPtr fp i = unsafeInlineIO
                 $ withForeignPtr fp $ \p -> return (advancePtr p i)

