{-# LANGUAGE BangPatterns #-}

module Bench.Vector.Algo.MutableSet
where

import Prelude hiding(length, read)

import Data.Vector.Mutable

mutableSet :: IOVector Int -> IO Int
{-# NOINLINE mutableSet #-}
mutableSet v = do
  let repetitions = 100 -- we repeat to reduce the standard deviation in measurements.
      l = length v

      -- This function is tail recursive.
      f :: Int -> Int -> IO Int
      f i !curSum =
       if i == 0
         then
           return curSum
         else do
           -- 'set' is what we want to benchmark.
           set v i
           -- In order to make it difficult for ghc to optimize the 'set' call
           -- away, we read the value of one element and add it to a running sum
           -- which is returned by the function.
           val <- read v (l-1)
           f (i-1) (curSum+val)
  f repetitions 0
