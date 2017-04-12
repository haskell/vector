{-# LANGUAGE BangPatterns #-}

module Algo.StressGrow where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

stressGrow :: Int -> V.Vector Int
stressGrow n = V.create (go 0 =<< MV.new 0)
  where
  go !x !mv
    | x == n = return mv
    | otherwise = do
        mv' <- MV.unsafeGrow mv 1
        MV.unsafeWrite mv' x x
        go (x + 1) mv'

