{-# LANGUAGE MagicHash, UnboxedTuples, FlexibleInstances, MultiParamTypeClasses #-}

module Data.Vector.Unboxed (
  Vector(..), module Data.Vector.Base
) where

import           Data.Vector.Base
import qualified Data.Vector.Unboxed.Mutable as Mut
import           Data.Vector.Unboxed.Unbox

import Control.Monad.ST ( runST )

import GHC.ST   ( ST(..) )
import GHC.Prim ( ByteArray#, unsafeFreezeByteArray#, (+#) )
import GHC.Base ( Int(..) )

data Vector a = Vector {-# UNPACK #-} !Int
                       {-# UNPACK #-} !Int
                                      ByteArray#

instance Unbox a => Base Vector a where
  {-# INLINE create #-}
  create init = runST (do_create init)
    where
      do_create :: ST s (Mut.Vector (ST s) a) -> ST s (Vector a)
      do_create init = do
                         Mut.Vector i n marr# <- init
                         ST (\s# -> case unsafeFreezeByteArray# marr# s# of
                              (# s2#, arr# #) -> (# s2#, Vector i n arr# #)
                            )

  {-# INLINE length #-}
  length (Vector _ n _) = n

  {-# INLINE unsafeSlice #-}
  unsafeSlice (Vector i _ arr#) j n = Vector (i+j) n arr#

  {-# INLINE unsafeIndex #-}
  unsafeIndex (Vector (I# i#) _ arr#) (I# j#) f = f (at# arr# (i# +# j#))

