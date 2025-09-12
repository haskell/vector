{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{- |
Here we test that GHC is able to optimize well construction of vector
using monadic\/applicative actions. Well is understood as able to
generate code which does not allocate except for buffer and some
constant overhead.
-}
module Inspect.Alloc where

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
    , testCase "Identity"
      $ checkAllocations (linear 8)
      $ VU.traverse (\n -> Identity (10*n)) `whnf` vector
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
    -- , testCase "Identity"
    --   $ checkAllocations (linear 8)
    --   $ (\sz -> VU.generateM sz (\n -> Identity (fromIntegral n :: Int64))) `whnf` size
    ]
  ]


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
