{-# LANGUAGE Rank2Types, MultiParamTypeClasses, FlexibleContexts,
             TypeFamilies, ScopedTypeVariables #-}

-- |
-- Module      : Data.Vector.Generic.Base
-- Copyright   : (c) Roman Leshchinskiy 2008-2010
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
-- 
-- Class of pure vectors
--

module Data.Vector.Generic.Base (
  Vector(..), Mutable
) where

import           Data.Vector.Generic.Mutable ( MVector )
import qualified Data.Vector.Generic.Mutable as M

import Control.Monad.Primitive

-- | @Mutable v s a@ is the mutable version of the pure vector type @v a@ with
-- the state token @s@
--
type family Mutable (v :: * -> *) :: * -> * -> *

-- | Class of immutable vectors. Every immutable vector is associated with its
-- mutable version through the 'Mutable' type family. Methods of this class
-- should not be used directly. Instead, "Data.Vector.Generic" and other
-- Data.Vector modules provide safe and fusible wrappers.
--
-- Minimum complete implementation:
--
--   * 'unsafeFreeze'
--
--   * 'basicLength'
--
--   * 'basicUnsafeSlice'
--
--   * 'basicUnsafeIndexM'
--
class MVector (Mutable v) a => Vector v a where
  -- | Unsafely convert a mutable vector to its immutable version
  -- without copying. The mutable vector may not be used after
  -- this operation.
  unsafeFreeze :: PrimMonad m => Mutable v (PrimState m) a -> m (v a)

  -- | Length of the vector.
  basicLength      :: v a -> Int

  -- | Yield a slice of the vector without copying it. No range checks are
  -- performed.
  basicUnsafeSlice  :: Int -- ^ starting index
                    -> Int -- ^ length
                    -> v a -> v a

  -- | Yield the element at the given position in a monad. No range checks are
  -- performed.
  --
  -- The monad allows us to be strict in the vector if we want. Suppose we had
  --
  -- > unsafeIndex :: v a -> Int -> a
  --
  -- instead. Now, if we wanted to copy a vector, we'd do something like
  --
  -- > copy mv v ... = ... unsafeWrite mv i (unsafeIndex v i) ...
  --
  -- For lazy vectors, the indexing would not be evaluated which means that we
  -- would retain a reference to the original vector in each element we write.
  -- This is not what we want!
  --
  -- With 'basicUnsafeIndexM', we can do
  --
  -- > copy mv v ... = ... case basicUnsafeIndexM v i of
  -- >                       Box x -> unsafeWrite mv i x ...
  --
  -- which does not have this problem because indexing (but not the returned
  -- element!) is evaluated immediately.
  --
  basicUnsafeIndexM  :: Monad m => v a -> Int -> m a

  -- | Copy an immutable vector into a mutable one.
  --
  -- Instances of 'Vector' should redefine this method if they wish to support
  -- an efficient block copy operation.
  --
  basicUnsafeCopy :: PrimMonad m => Mutable v (PrimState m) a -> v a -> m ()

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy dst src = do_copy 0
    where
      n = basicLength src

      do_copy i | i < n = do
                            x <- basicUnsafeIndexM src i
                            M.basicUnsafeWrite dst i x
                            do_copy (i+1)
                | otherwise = return ()

  -- | Evaluate @a@ as far as storing it in a vector would and yield @b@.
  -- The @v a@ argument only fixes the type and is not touched. The method is
  -- only used for optimisation purposes. Thus, it is safe for instances of
  -- 'Vector' to evaluate @a@ less than it would be when stored in a vector
  -- although this might result in suboptimal code.
  --
  -- > elemseq v x y = (singleton x `asTypeOf` v) `seq` y
  --
  elemseq :: v a -> a -> b -> b

  {-# INLINE elemseq #-}
  elemseq _ = \_ x -> x


