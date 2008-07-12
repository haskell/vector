{-# LANGUAGE MagicHash, UnboxedTuples, MultiParamTypeClasses, FlexibleInstances, GADTs, ScopedTypeVariables #-}

-- |
-- Module      : Data.Vector.Unboxed.Mutable
-- Copyright   : (c) Roman Leshchinskiy 2008
-- License     : BSD-style
--
-- Maintainer  : rl@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : non-portable
-- 
-- Mutable unboxed vectors based on 'Unbox'.
--

module Data.Vector.Unboxed.Mutable ( Vector(..) )
where

import qualified Data.Vector.MVector as MVector
import           Data.Vector.MVector ( MVector )
import           Data.Vector.Unboxed.Unbox

import GHC.Prim ( MutableByteArray#,
                  newByteArray#, sameMutableByteArray#, (+#) )

import GHC.ST   ( ST(..) )

import GHC.Base ( Int(..) )

#ifndef __HADDOCK__
data Vector m a where
   Vector :: {-# UNPACK #-} !Int
          -> {-# UNPACK #-} !Int
          -> MutableByteArray# s
          -> Vector (ST s) a
#else
-- | Type of mutable unboxed vectors. This is actually a GADT:
--
-- > data Vector m a where
-- >   Vector :: !Int -> !Int -> MutableByteArray# s -> Vector (ST s) a
--
data Vector m a = forall s. Vector !Int !Int (MutableByteArray# s)
#endif

instance Unbox a => MVector Vector (ST s) a where
  length (Vector _ n _) = n
  unsafeSlice (Vector i _ arr#) j m = Vector (i+j) m arr#

  {-# INLINE unsafeNew #-}
  unsafeNew (I# n#) = ST (\s# ->
      case newByteArray# (size# (undefined :: a) n#) s# of
        (# s2#, arr# #) -> (# s2#, Vector 0 (I# n#) arr# #)
    )

  {-# INLINE unsafeRead #-}
  unsafeRead (Vector (I# i#) _ arr#) (I# j#) = ST (read# arr# (i# +# j#))

  {-# INLINE unsafeWrite #-}
  unsafeWrite (Vector (I# i#) _ arr#) (I# j#) x = ST (\s# ->
      case write# arr# (i# +# j#) x s# of s2# -> (# s2#, () #)
    )

  {-# INLINE overlaps #-}
  overlaps (Vector i m arr1#) (Vector j n arr2#)
    = sameMutableByteArray# arr1# arr2#
      && (between i j (j+n) || between j i (i+m))
    where
      between x y z = x >= y && x < z

