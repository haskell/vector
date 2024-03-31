{-# LANGUAGE FlexibleContexts #-}
-- |
-- This module contains benchmark for measuring impact of missed
-- specialization.
module Bench.Vector.Algo.Stddev where

import qualified Data.Vector.Generic as VG


-- | Inlined variant. It should be specialized at call site in
-- bechmark
varianceInline :: (VG.Vector v Double) => v Double -> Double
{-# INLINE varianceInline #-}
varianceInline xs
  = VG.sum (VG.map (\x -> (x - s)^(2::Int)) xs) / n
  where
    n = fromIntegral $ VG.length xs
    s = VG.sum xs / n

-- | This function is prevented from being inlined
varianceNoInline :: (VG.Vector v Double) => v Double -> Double
{-# NOINLINE varianceNoInline #-}
varianceNoInline xs
  = VG.sum (VG.map (\x -> (x - s)^(2::Int)) xs) / n
  where
    n = fromIntegral $ VG.length xs
    s = VG.sum xs / n

