{-# LANGUAGE MagicHash, UnboxedTuples, MultiParamTypeClasses, GADTs, FlexibleInstances #-}

module Data.Vector.Mutable ( Vector(..) )
where

import qualified Data.Vector.MVector as MVector
import           Data.Vector.MVector ( MVector )

import GHC.Prim ( MutableArray#,
                  newArray#, readArray#, writeArray#, sameMutableArray#, (+#) )

import GHC.ST   ( ST(..) )

import GHC.Base ( Int(..) )

#ifndef __HADDOCK__
data Vector m a where
  Vector :: {-# UNPACK #-} !Int
         -> {-# UNPACK #-} !Int
         -> MutableArray# s a
         -> Vector (ST s) a
#else
data Vector m a = forall s. Vector !Int !Int (MutableArray# s a)
#endif

instance MVector Vector (ST s) a where
  length (Vector _ n _) = n
  unsafeSlice (Vector i _ arr#) j m = Vector (i+j) m arr#

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

  {-# INLINE overlaps #-}
  overlaps (Vector i m arr1#) (Vector j n arr2#)
    = sameMutableArray# arr1# arr2#
      && (between i j (j+n) || between j i (i+m))
    where
      between x y z = x >= y && x < z

unsafeNew :: Int -> ST s (Vector (ST s) a)
{-# INLINE unsafeNew #-}
unsafeNew n = unsafeNewWith n (error "Data.Vector.Mutable: uninitialised elemen t")

unsafeNewWith :: Int -> a -> ST s (Vector (ST s) a)
{-# INLINE unsafeNewWith #-}
unsafeNewWith (I# n#) x = ST (\s# ->
    case newArray# n# x s# of
      (# s2#, arr# #) -> (# s2#, Vector 0 (I# n#) arr# #)
  )

