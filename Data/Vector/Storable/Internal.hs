{-# LANGUAGE MagicHash, UnboxedTuples #-}

-- |
-- Module      : Data.Vector.Storable.Internal
-- Copyright   : (c) Roman Leshchinskiy 2009
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
-- 
-- Ugly internal utility functions for implementing 'Storable'-based vectors.
--

module Data.Vector.Storable.Internal
where

import GHC.Base         ( realWorld# )
import GHC.IOBase       ( IO(..) )

-- Stolen from the ByteString library
inlinePerformIO :: IO a -> a
{-# INLINE inlinePerformIO #-}
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r

