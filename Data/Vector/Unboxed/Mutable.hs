{-# LANGUAGE MagicHash, UnboxedTuples, TypeFamilies, MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables #-}

module Data.Vector.Unboxed.Mutable ( Vector(..) )
where

import qualified Data.Vector.Base.Mutable as Base
import           Data.Vector.Unboxed.Unbox

import GHC.Prim ( MutableByteArray#,
                  newByteArray#, sameMutableByteArray#, (+#) )

import GHC.ST   ( ST(..) )

import GHC.Base ( Int(..) )

data Vector s a = Vector {-# UNPACK #-} !Int
                         {-# UNPACK #-} !Int
                                        (MutableByteArray# s)


instance Unbox a => Base.Base (Vector s) a where
  type Base.Trans (Vector s) = ST s

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

