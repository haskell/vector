{-# LANGUAGE MagicHash, UnboxedTuples, FlexibleInstances, MultiParamTypeClasses #-}

module Data.Vector (
  Vector(..), module Data.Vector.Base
) where

import           Data.Vector.Base
import qualified Data.Vector.Mutable as Mut

import Control.Monad.ST ( runST )

import GHC.ST   ( ST(..) )
import GHC.Prim ( Array#, unsafeFreezeArray#, indexArray#, (+#) )
import GHC.Base ( Int(..) )

data Vector a = Vector {-# UNPACK #-} !Int
                       {-# UNPACK #-} !Int
                                      (Array# a)

instance Base Vector a where
  {-# INLINE create #-}
  create init = runST (do
      Mut.Vector i n marr# <- init
      ST (\s# -> case unsafeFreezeArray# marr# s# of
                   (# s2#, arr# #) -> (# s2#, Vector i n arr# #)
         )
    )

  {-# INLINE length #-}
  length (Vector _ n _) = n

  {-# INLINE unsafeSlice #-}
  unsafeSlice (Vector i _ arr#) j n = Vector (i+j) n arr#

  {-# INLINE unsafeIndex #-}
  unsafeIndex (Vector (I# i#) _ arr#) (I# j#) f
    = case indexArray# arr# (i# +# j#) of (# x #) -> f x

