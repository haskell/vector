{-# LANGUAGE MagicHash, UnboxedTuples, TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}

module Data.Vector.Mutable ( Vector(..) )
where

import qualified Data.Vector.Base.Mutable as Base

import GHC.Prim ( MutableArray#,
                  newArray#, readArray#, writeArray#, sameMutableArray#, (+#) )

import GHC.ST   ( ST(..) )

import GHC.Base ( Int(..) )

data Vector s a = Vector {-# UNPACK #-} !Int
                         {-# UNPACK #-} !Int
                                        (MutableArray# s a)

instance Base.Base (Vector s) a where
  type Base.Trans (Vector s) = ST s

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

unsafeNew :: Int -> ST s (Vector s a)
{-# INLINE unsafeNew #-}
unsafeNew n = unsafeNewWith n (error "Data.Vector.Mutable: uninitialised elemen t")

unsafeNewWith :: Int -> a -> ST s (Vector s a)
{-# INLINE unsafeNewWith #-}
unsafeNewWith (I# n#) x = ST (\s# ->
    case newArray# n# x s# of
      (# s2#, arr# #) -> (# s2#, Vector 0 (I# n#) arr# #)
  )

