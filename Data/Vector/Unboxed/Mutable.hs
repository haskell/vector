{-# LANGUAGE MagicHash, UnboxedTuples, MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables #-}

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
import           Data.Vector.MVector ( MVector, MVectorPure )
import           Data.Vector.Unboxed.Unbox

import GHC.Prim ( MutableByteArray#,
                  newByteArray#, sameMutableByteArray#, (+#) )

import GHC.ST   ( ST(..) )

import GHC.Base ( Int(..) )

-- | Mutable unboxed vectors. They live in the 'ST' monad.
data Vector s a = Vector {-# UNPACK #-} !Int
                         {-# UNPACK #-} !Int
                                        (MutableByteArray# s)

instance Unbox a => MVectorPure (Vector s) a where
  length (Vector _ n _) = n
  unsafeSlice (Vector i _ arr#) j m = Vector (i+j) m arr#

  {-# INLINE overlaps #-}
  overlaps (Vector i m arr1#) (Vector j n arr2#)
    = sameMutableByteArray# arr1# arr2#
      && (between i j (j+n) || between j i (i+m))
    where
      between x y z = x >= y && x < z


instance Unbox a => MVector (Vector s) (ST s) a where
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


