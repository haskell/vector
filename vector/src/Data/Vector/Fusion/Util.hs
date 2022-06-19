-- |
-- Module      : Data.Vector.Fusion.Util
-- Copyright   : (c) Roman Leshchinskiy 2009
--                   Alexey Kuleshevich 2020-2022
--                   Aleksey Khudyakov 2020-2022
--                   Andrew Lelechenko 2020-2022
-- License     : BSD-style
--
-- Maintainer  : Haskell Libraries Team <libraries@haskell.org>
-- Stability   : experimental
-- Portability : portable
--
-- Fusion-related utility types
--

module Data.Vector.Fusion.Util (
  Id(..), Box(..), liftBox,

  delay_inline, delayed_min
) where

import Data.Stream.Monadic (Box(..), liftBox)

-- | Identity monad
newtype Id a = Id { unId :: a }

instance Functor Id where
  fmap f (Id x) = Id (f x)

instance Applicative Id where
  pure = Id
  Id f <*> Id x = Id (f x)

instance Monad Id where
  return = pure
  Id x >>= f = f x

-- | Delay inlining a function until late in the game (simplifier phase 0).
delay_inline :: (a -> b) -> a -> b
{-# INLINE [0] delay_inline #-}
delay_inline f = f

-- | `min` inlined in phase 0
delayed_min :: Int -> Int -> Int
{-# INLINE [0] delayed_min #-}
delayed_min m n = min m n
