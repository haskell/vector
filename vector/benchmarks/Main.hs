{-# LANGUAGE BangPatterns #-}
module Main where

import Bench.Vector.Algo.MutableSet      (mutableSet)
import Bench.Vector.Algo.ListRank        (listRank)
import Bench.Vector.Algo.Rootfix         (rootfix)
import Bench.Vector.Algo.Leaffix         (leaffix)
import Bench.Vector.Algo.AwShCC          (awshcc)
import Bench.Vector.Algo.HybCC           (hybcc)
import Bench.Vector.Algo.Quickhull       (quickhull)
import Bench.Vector.Algo.Spectral        (spectral)
import Bench.Vector.Algo.Tridiag         (tridiag)
import Bench.Vector.Algo.FindIndexR      (findIndexR, findIndexR_naive, findIndexR_manual)
import Bench.Vector.Algo.NextPermutation (generatePermTests)

import Bench.Vector.TestData.ParenTree (parenTree)
import Bench.Vector.TestData.Graph     (randomGraph)
import Bench.Vector.Tasty

import Data.Proxy
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as U
import Data.Word
import System.Random.Stateful
import Test.Tasty
import Test.Tasty.Bench
import Test.Tasty.Options
import Test.Tasty.Runners


indexFindThreshold :: Double
indexFindThreshold = 2e-5

main :: IO ()
main = do
  let ourOpts = [Option (Proxy :: Proxy VectorSize), Option (Proxy :: Proxy RandomSeed)]
      ingredients = includingOptions ourOpts : benchIngredients
  opts <- parseOptions ingredients (bench "Fake" (nf id ()))
  let VectorSize useSize = lookupOption opts
      RandomSeed useSeed = lookupOption opts

  gen <- newIOGenM (mkStdGen useSeed)

  let (!lparens, !rparens) = parenTree useSize
  (!nodes, !edges1, !edges2) <- randomGraph gen useSize

  let randomVector l = U.replicateM l (uniformDoublePositive01M gen)
  !as <- randomVector useSize
  !bs <- randomVector useSize
  !cs <- randomVector useSize
  !ds <- randomVector useSize
  !sp <- randomVector (floor $ sqrt $ fromIntegral useSize)
  vi <- MV.new useSize
  permTests <- generatePermTests gen useSize

  defaultMainWithIngredients ingredients $ bgroup "All"
    [ bench "listRank"   $ whnf listRank useSize
    , bench "rootfix"    $ whnf rootfix (lparens, rparens)
    , bench "leaffix"    $ whnf leaffix (lparens, rparens)
    , bench "awshcc"     $ whnf awshcc (nodes, edges1, edges2)
    , bench "hybcc"      $ whnf hybcc  (nodes, edges1, edges2)
    , bench "quickhull"  $ whnf quickhull (as,bs)
    , bench "spectral"   $ whnf spectral sp
    , bench "tridiag"    $ whnf tridiag (as,bs,cs,ds)
    , bench "mutableSet" $ nfIO $ mutableSet vi
    , bench "findIndexR" $ whnf findIndexR ((<indexFindThreshold), as)
    , bench "findIndexR_naïve" $ whnf findIndexR_naive ((<indexFindThreshold), as)
    , bench "findIndexR_manual" $ whnf findIndexR_manual ((<indexFindThreshold), as)
    , bench "minimumOn"  $ whnf (U.minimumOn (\x -> x*x*x)) as
    , bench "maximumOn"  $ whnf (U.maximumOn (\x -> x*x*x)) as
    , bgroup "(next|prev)Permutation" $ map (\(name, act) -> bench name $ whnfIO act) permTests
    ]
