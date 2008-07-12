{-# LANGUAGE MagicHash, UnboxedTuples, FlexibleInstances, MultiParamTypeClasses #-}

-- |
-- Module      : Data.Vector
-- Copyright   : (c) Roman Leshchinskiy 2008
-- License     : BSD-style
--
-- Maintainer  : rl@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : non-portable
-- 
-- Boxed vectors
--

module Data.Vector (
  Vector(..), module Data.Vector.IVector
) where

import           Data.Vector.IVector
import qualified Data.Vector.Mutable as Mut

import Control.Monad.ST ( runST )

import GHC.ST   ( ST(..) )
import GHC.Prim ( Array#, unsafeFreezeArray#, indexArray#, (+#) )
import GHC.Base ( Int(..) )

data Vector a = Vector {-# UNPACK #-} !Int
                       {-# UNPACK #-} !Int
                                      (Array# a)

instance IVector Vector a where
  {-# INLINE create #-}
  create init = runST (do_create init)
    where
      do_create :: ST s (Mut.Vector s a) -> ST s (Vector a)
      do_create init = do
                         Mut.Vector i n marr# <- init
                         ST (\s# -> case unsafeFreezeArray# marr# s# of
                              (# s2#, arr# #) -> (# s2#, Vector i n arr# #)
                            )

  {-# INLINE vlength #-}
  vlength (Vector _ n _) = n

  {-# INLINE unsafeSlice #-}
  unsafeSlice (Vector i _ arr#) j n = Vector (i+j) n arr#

  {-# INLINE unsafeIndex #-}
  unsafeIndex (Vector (I# i#) _ arr#) (I# j#) f
    = case indexArray# arr# (i# +# j#) of (# x #) -> f x

