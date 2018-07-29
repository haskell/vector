module Main where

import Criterion.Main

import Algo.ListRank  (listRank)
import Algo.Rootfix   (rootfix)
import Algo.Leaffix   (leaffix)
import Algo.AwShCC    (awshcc)
import Algo.HybCC     (hybcc)
import Algo.Quickhull (quickhull)
import Algo.Spectral  ( spectral )
import Algo.Tridiag   ( tridiag )

import TestData.ParenTree ( parenTree )
import TestData.Graph     ( randomGraph )
import TestData.Random    ( randomVector )

import Data.Vector.Unboxed ( Vector )

import System.Environment
import Data.Word

size :: Int
size = 100000

seed :: Word32
seed = 42

main = do
  args <- getArgs
  case args of
    ("--seed":s:args') -> do
      withArgs args' (main' (read s))
    _ -> main' seed

main' seed =
       lparens `seq` rparens `seq`
       nodes `seq` edges1 `seq` edges2 `seq`
       do
         as <- randomVector seed size :: IO (Vector Double)
         bs <- randomVector seed size :: IO (Vector Double)
         cs <- randomVector seed size :: IO (Vector Double)
         ds <- randomVector seed size :: IO (Vector Double)
         sp <- randomVector seed (floor $ sqrt $ fromIntegral size)
                                 :: IO (Vector Double)
         as `seq` bs `seq` cs `seq` ds `seq` sp `seq`
           defaultMain [ bench "listRank"  $ whnf listRank size
                       , bench "rootfix"   $ whnf rootfix (lparens, rparens)
                       , bench "leaffix"   $ whnf leaffix (lparens, rparens)
                       , bench "awshcc"    $ whnf awshcc (nodes, edges1, edges2)
                       , bench "hybcc"     $ whnf hybcc  (nodes, edges1, edges2)
                       , bench "quickhull" $ whnf quickhull (as,bs)
                       , bench "spectral"  $ whnf spectral sp
                       , bench "tridiag"   $ whnf tridiag (as,bs,cs,ds)
                       ]
  where
    (lparens, rparens) = parenTree size
    (nodes, edges1, edges2) = randomGraph seed size


