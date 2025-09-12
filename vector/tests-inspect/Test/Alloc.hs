{-# LANGUAGE RecordWildCards #-}
-- |
-- Test that function allocates is in range. This is good way to test
-- that GHC produces tight non-allocating loops.
module Test.Alloc where

import Control.Exception
import Data.Int
import System.Mem
import Test.Tasty.HUnit
import Text.Printf

----------------------------------------------------------------
-- Benchmarking machinery copied from tasty-bench
----------------------------------------------------------------

newtype Benchmarkable = Benchmarkable (IO ())

whnf :: (a -> b) -> a -> Benchmarkable
{-# NOINLINE whnf #-}
whnf f a = Benchmarkable $ do _ <- evaluate (f a)
                              return ()

whnfIO :: IO a -> Benchmarkable
{-# NOINLINE whnfIO #-}
whnfIO io = Benchmarkable $ do _ <- evaluate =<< io
                               return ()


-- | Measure allocations. Measurements use 'getAllocationCounter' so
--   it's accurate up to 4k bytes.
allocations :: Benchmarkable -> IO Int64
allocations (Benchmarkable io) = do
  -- We need to run `io` twice in order to ensure that all constant
  -- parameters are evaluated.
  io
  n1 <- getAllocationCounter
  io
  n2 <- getAllocationCounter
  return $! n1 - n2


-- | Expected allocations range
data Range = Range { allocLo :: !Int64
                   , allocHi :: !Int64
                   }
             deriving Show

-- | Check that computation's allocations lie in range
checkAllocations :: Range -> Benchmarkable -> IO ()
checkAllocations Range{..} bench = do
  alloc <- allocations bench
  let msg = unlines [ printf "allocated = %12d" alloc
                    , printf "Low bound = %12d" allocLo
                    , printf "Hi  bound = %12d" allocHi
                    ]
  assertBool msg $ alloc <= allocHi
                && alloc >= allocLo

