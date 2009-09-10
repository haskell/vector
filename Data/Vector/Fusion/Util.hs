-- |
-- Module      : Data.Vector.Fusion.Util
-- Copyright   : (c) Roman Leshchinskiy 2009
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : portable
-- 
-- Fusion-related utility types
--

module Data.Vector.Fusion.Util ( Id(..), Box(..) )
where

-- | Identity monad
newtype Id a = Id { unId :: a }

instance Functor Id where
  fmap f (Id x) = Id (f x)

instance Monad Id where
  return     = Id
  Id x >>= f = f x

-- | Box monad
data Box a = Box { unBox :: a }

instance Functor Box where
  fmap f (Box x) = Box (f x)

instance Monad Box where
  return      = Box
  Box x >>= f = f x

