{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_HADDOCK hide #-}

-- |
-- Module      : Data.Vector.Generic.Base
-- Copyright   : (c) Roman Leshchinskiy 2008-2010
--                   Alexey Kuleshevich 2020-2022
--                   Aleksey Khudyakov 2020-2022
--                   Andrew Lelechenko 2020-2022
-- License     : BSD-style
--
-- Maintainer  : Haskell Libraries Team <libraries@haskell.org>
-- Stability   : experimental
-- Portability : non-portable
--
-- Class of immutable vectors.

module Data.Vector.Generic.Base (
  Vector(..), Mutable
) where

import           Data.Vector.Generic.Mutable.Base ( MVector )
import qualified Data.Vector.Generic.Mutable.Base as M
import           Data.Vector.Fusion.Util (Box(..), liftBox)

import Control.Monad.ST
import Data.Kind (Type)

-- | @Mutable v s a@ is the mutable version of the immutable vector type @v a@ with
-- the state token @s@. It is injective on GHC 8 and newer.
type family Mutable (v :: Type -> Type) = (mv :: Type -> Type -> Type) | mv -> v

-- | Class of immutable vectors. Every immutable vector is associated with its
-- mutable version through the 'Mutable' type family. Methods of this class
-- should not be used directly. Instead, "Data.Vector.Generic" and other
-- @Data.Vector@ modules provide safe and fusible wrappers.
--
-- Minimum complete implementation:
--
--   * 'basicUnsafeFreeze'
--
--   * 'basicUnsafeThaw'
--
--   * 'basicLength'
--
--   * 'basicUnsafeSlice'
--
--   * 'basicUnsafeIndexM'
--
class MVector (Mutable v) a => Vector v a where
  -- | /Assumed complexity: O(1)/
  --
  -- Unsafely convert a mutable vector to its immutable version
  -- without copying. The mutable vector may not be used after
  -- this operation.
  basicUnsafeFreeze :: Mutable v s a -> ST s (v a)

  -- | /Assumed complexity: O(1)/
  --
  -- Unsafely convert an immutable vector to its mutable version without
  -- copying. The immutable vector may not be used after this operation.
  basicUnsafeThaw :: v a -> ST s (Mutable v s a)

  -- | /Assumed complexity: O(1)/
  --
  -- Yield the length of the vector.
  basicLength      :: v a -> Int

  -- | /Assumed complexity: O(1)/
  --
  -- Yield a slice of the vector without copying it. No range checks are
  -- performed.
  basicUnsafeSlice  :: Int -- ^ starting index
                    -> Int -- ^ length
                    -> v a -> v a

  -- | /Assumed complexity: O(1)/
  --
  -- Yield the element at the given position in a monad. No range checks are
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
  -- For lazy vectors, the indexing would not be evaluated, which means that we
  -- would retain a reference to the original vector in each element we write.
  -- This is not what we want!
  --
  -- With 'basicUnsafeIndexM', we can do
  --
  -- > copy mv v ... = ... case basicUnsafeIndexM v i of
  -- >                       Box x -> unsafeWrite mv i x ...
  --
  -- which does not have this problem, because indexing (but not the returned
  -- element!) is evaluated immediately.
  basicUnsafeIndexM  :: v a -> Int -> Box a

  -- |  /Assumed complexity: O(n)/
  --
  -- Copy an immutable vector into a mutable one. The two vectors must have
  -- the same length, but this is not checked.
  --
  -- Instances of 'Vector' should redefine this method if they wish to support
  -- an efficient block copy operation.
  --
  -- Default definition: copying based on 'basicUnsafeIndexM' and
  -- 'basicUnsafeWrite'.
  basicUnsafeCopy :: Mutable v s a -> v a -> ST s ()

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy !dst !src = do_copy 0
    where
      !n = basicLength src

      do_copy i | i < n = do
                            x <- liftBox $ basicUnsafeIndexM src i
                            M.basicUnsafeWrite dst i x
                            do_copy (i+1)
                | otherwise = return ()

  -- | Evaluate @a@ as far as storing it in a vector would and yield @b@.
  -- The @v a@ argument only fixes the type and is not touched. This method is
  -- only used for optimisation purposes. Thus, it is safe for instances of
  -- 'Vector' to evaluate @a@ less than it would be when stored in a vector,
  -- although this might result in suboptimal code.
  --
  -- > elemseq v x y = (singleton x `asTypeOf` v) `seq` y
  --
  -- Default defintion: @a@ is not evaluated at all.
  elemseq :: v a -> a -> b -> b

  {-# INLINE elemseq #-}
  elemseq _ = \_ x -> x

  {-# MINIMAL basicUnsafeFreeze, basicUnsafeThaw, basicLength,
              basicUnsafeSlice, basicUnsafeIndexM #-}
