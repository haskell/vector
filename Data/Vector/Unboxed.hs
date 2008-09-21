{-# LANGUAGE MagicHash, UnboxedTuples, FlexibleInstances, MultiParamTypeClasses #-}

-- |
-- Module      : Data.Vector.Unboxed
-- Copyright   : (c) Roman Leshchinskiy 2008
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
-- 
-- Unboxed vectors based on 'Unbox'.
--

module Data.Vector.Unboxed (
  Vector(..), module Data.Vector.IVector
) where

import           Data.Vector.IVector
import qualified Data.Vector.Unboxed.Mutable as Mut
import           Data.Vector.Unboxed.Unbox

import Control.Monad.ST ( runST )

import GHC.ST   ( ST(..) )
import GHC.Prim ( ByteArray#, unsafeFreezeByteArray#, (+#) )
import GHC.Base ( Int(..) )

-- | Unboxed vectors
data Vector a = Vector {-# UNPACK #-} !Int
                       {-# UNPACK #-} !Int
                                      ByteArray#

instance Unbox a => IVector Vector a where
  {-# INLINE vnew #-}
  vnew init = runST (do
                       Mut.Vector i n marr# <- init
                       ST (\s# -> case unsafeFreezeByteArray# marr# s# of
                            (# s2#, arr# #) -> (# s2#, Vector i n arr# #)))

  {-# INLINE vlength #-}
  vlength (Vector _ n _) = n

  {-# INLINE unsafeSlice #-}
  unsafeSlice (Vector i _ arr#) j n = Vector (i+j) n arr#

  {-# INLINE unsafeIndex #-}
  unsafeIndex (Vector (I# i#) _ arr#) (I# j#) f = f (at# arr# (i# +# j#))

instance (Unbox a, Eq a) => Eq (Vector a) where
  {-# INLINE (==) #-}
  (==) = eq

instance (Unbox a, Ord a) => Ord (Vector a) where
  {-# INLINE compare #-}
  compare = cmp

