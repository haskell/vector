module Bench.Vector.TestData.Graph
  ( randomGraph
  ) where

import System.Random.Stateful
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as U

randomGraph
  :: (StatefulGen g m, MV.PrimMonad m)
  => g
  -> Int
  -> m (Int, U.Vector Int, U.Vector Int)
randomGraph g edges = do
  let vertices = edges `div` 10
  marr <- MV.replicate vertices []
  addRandomEdges g vertices marr edges
  arr <- V.unsafeFreeze marr
  let (as, bs) = unzip [ (i, j) | i <- [0 .. vertices - 1], j <- arr V.! i ]
  return (vertices, U.fromList as, U.fromList bs)

addRandomEdges
  :: (StatefulGen g m, MV.PrimMonad m)
  => g
  -> Int
  -> MV.MVector (MV.PrimState m) [Int]
  -> Int
  -> m ()
addRandomEdges g vertices arr = fill
  where
    fill 0 = return ()
    fill e = do
      m1 <- uniformRM (0, vertices - 1) g
      m2 <- uniformRM (0, vertices - 1) g
      let lo = min m1 m2
          hi = max m1 m2
      ns <- MV.read arr lo
      if lo == hi || hi `elem` ns
        then fill e
        else MV.write arr lo (hi : ns) >> fill (e - 1)
