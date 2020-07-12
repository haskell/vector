module Main where

import Gauge.Main

import Algo.MutableSet(mutableSet)
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
import qualified Data.Vector.Mutable as M( IOVector, new )

import System.Environment
import Data.Word

import Data.Word

useSize :: Int
useSize = 2000000

useSeed :: Word32
useSeed = 42

main :: IO ()
main = do
  let (lparens, rparens) = parenTree useSize
  let (nodes, edges1, edges2) = randomGraph useSeed useSize
  lparens `seq` rparens `seq`
    nodes `seq` edges1 `seq` edges2 `seq` return ()

  vi <- M.new useSize                :: IO (M.IOVector Int)
  as <- randomVector useSeed useSize :: IO (Vector Double)
  bs <- randomVector useSeed useSize :: IO (Vector Double)
  cs <- randomVector useSeed useSize :: IO (Vector Double)
  ds <- randomVector useSeed useSize :: IO (Vector Double)
  sp <- randomVector useSeed (floor $ sqrt $ fromIntegral useSize)
                          :: IO (Vector Double)
  as `seq` bs `seq` cs `seq` ds `seq` sp `seq` return ()
  defaultMain
                [ bench "listRank"  $ whnf listRank useSize
                , bench "rootfix"   $ whnf rootfix (lparens, rparens)
                , bench "leaffix"   $ whnf leaffix (lparens, rparens)
                , bench "awshcc"    $ whnf awshcc (nodes, edges1, edges2)
                , bench "hybcc"     $ whnf hybcc  (nodes, edges1, edges2)
                , bench "quickhull" $ whnf quickhull (as,bs)
                , bench "spectral"  $ whnf spectral sp
                , bench "tridiag"   $ whnf tridiag (as,bs,cs,ds)
                , bench "mutableSet"$ nfIO $ mutableSet vi
                ]
