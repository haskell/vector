{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{- |
Here we test that GHC is able to optimize well construction of vector
using monadic\/applicative actions. Well is understood as able to
generate code which does not allocate except for buffer and some
constant overhead.
-}
module Inspect.Alloc where

import Control.Monad.ST
import Data.Int
-- import Data.Monoid
import Data.Functor.Identity
import Test.Tasty
import Test.Tasty.HUnit
import System.Mem
import Test.Alloc

import qualified Data.Vector.Unboxed as VU


tests :: TestTree
tests = testGroup "allocations"
  [ testGroup "traversable"
    [ testCase "IO"
      $ checkAllocations (linear 8)
      $ whnfIO (VU.traverse (\_ -> getAllocationCounter) vector)

#if MIN_VERSION_base(4,17,0)
    -- GHC<9.4 doesn't optimize well.
    , testCase "ST"
      $ checkAllocations (linear 8)
      $ (\v -> runST $ VU.traverse (pureST . fromIntegral) v) `whnf` vector
#endif

#if MIN_VERSION_base(4,15,0)
      -- GHC<9.0 doesn't optimize this well. And there's no appetite
      -- for finding out why. Thus it's disabled for them. We'll still
      -- catch regression going forward.
    , testCase "Identity"
      $ checkAllocations (linear 8)
      $ VU.traverse (\n -> Identity (10*n)) `whnf` vector
#endif

    -- NOTE: Naive traversal is lazy and allocated 2 words per element
    --
    -- , testCase "Const Sum"
    --   $ checkAllocations constant
    --   $ whnf (VU.traverse (Const @_ @() . Sum)) vector
    ]
  , testGroup "unstreamM"
    [ testCase "IO"
      $ checkAllocations (linear 8)
      $ whnfIO (VU.replicateM size getAllocationCounter)

#if MIN_VERSION_base(4,17,0)
    -- GHC<9.4 doesn't optimize well.
    , testCase "ST"
      $ checkAllocations (linear 8)
      $ (\sz -> runST $ VU.generateM sz pureST) `whnf` size
#endif

    -- , testCase "Identity"
    --   $ checkAllocations (linear 8)
    --   $ (\sz -> VU.generateM sz (\n -> Identity (fromIntegral n :: Int64))) `whnf` size
    ]
  ]


pureST :: Int -> ST s Int64
{-# NOINLINE pureST #-}
pureST i = pure $! fromIntegral i

-- | Constant overhead. Measurement precision is 4k
overhead :: Int64
overhead = 4096*2

-- | Vector size. It should be large so 1byte per element will be
--   large than page.
size :: Int
size = 100000

vector :: VU.Vector Int64
{-# NOINLINE vector #-}
vector = VU.generate size fromIntegral

-- | N bytes per element + constant overhead. We also check that bound
--   is tight.
linear :: Int -> Range
linear n = Range
  { allocHi = fromIntegral (n * size) + overhead
  , allocLo = fromIntegral (n * size)
  }

-- | Only constant overhead
constant :: Range
constant = Range { allocHi = overhead
                 , allocLo = 0
                 }
