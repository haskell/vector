{-# LANGUAGE MagicHash, UnboxedTuples, MultiParamTypeClasses, FlexibleInstances #-}

-- |
-- Module      : Data.Vector.Mutable.ST
-- Copyright   : (c) Roman Leshchinskiy 2008
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
-- 
-- Mutable boxed vectors in the ST monad.
--

module Data.Vector.Mutable.ST ( Vector(..) )
where

import qualified Data.Vector.MVector as MVector
import           Data.Vector.MVector ( MVector, MVectorPure )

import GHC.Prim ( MutableArray#,
                  newArray#, readArray#, writeArray#, sameMutableArray#, (+#) )

import GHC.ST   ( ST(..) )

import GHC.Base ( Int(..) )

-- | Mutable boxed vectors. They live in the 'ST' monad.
data Vector s a = Vector {-# UNPACK #-} !Int
                         {-# UNPACK #-} !Int
                                        (MutableArray# s a)

instance MVectorPure (Vector s) a where
  length (Vector _ n _) = n
  unsafeSlice (Vector i _ arr#) j m = Vector (i+j) m arr#

  {-# INLINE overlaps #-}
  overlaps (Vector i m arr1#) (Vector j n arr2#)
    = sameMutableArray# arr1# arr2#
      && (between i j (j+n) || between j i (i+m))
    where
      between x y z = x >= y && x < z


instance MVector (Vector s) (ST s) a where
  {-# INLINE unsafeNew #-}
  unsafeNew = unsafeNew

  {-# INLINE unsafeNewWith #-}
  unsafeNewWith = unsafeNewWith

  {-# INLINE unsafeRead #-}
  unsafeRead (Vector (I# i#) _ arr#) (I# j#) = ST (readArray# arr# (i# +# j#))

  {-# INLINE unsafeWrite #-}
  unsafeWrite (Vector (I# i#) _ arr#) (I# j#) x = ST (\s# ->
      case writeArray# arr# (i# +# j#) x s# of s2# -> (# s2#, () #)
    )

  {-# INLINE clear #-}
  clear v = MVector.set v uninitialised


uninitialised :: a
uninitialised = error "Data.Vector.Mutable: uninitialised elemen t"

unsafeNew :: Int -> ST s (Vector s a)
{-# INLINE unsafeNew #-}
unsafeNew n = unsafeNewWith n uninitialised

unsafeNewWith :: Int -> a -> ST s (Vector s a)
{-# INLINE unsafeNewWith #-}
unsafeNewWith (I# n#) x = ST (\s# ->
    case newArray# n# x s# of
      (# s2#, arr# #) -> (# s2#, Vector 0 (I# n#) arr# #)
  )

