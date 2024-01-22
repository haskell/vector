module Main where

import Algo.MutableSet (mutableSet)
import Algo.ListRank   (listRank)
import Algo.Rootfix    (rootfix)
import Algo.Leaffix    (leaffix)
import Algo.AwShCC     (awshcc)
import Algo.HybCC      (hybcc)
import Algo.Quickhull  (quickhull)
import Algo.Spectral   (spectral)
import Algo.Tridiag    (tridiag)
import Algo.FindIndexR (findIndexR, findIndexR_naive, findIndexR_manual)

import TestData.ParenTree (parenTree)
import TestData.Graph     (randomGraph)

import Data.Proxy
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as U
import Data.Word
import System.Random.Stateful
import Test.Tasty
import Test.Tasty.Bench
import Test.Tasty.Options
import Test.Tasty.Runners

newtype VectorSize = VectorSize Int

instance IsOption VectorSize where
  defaultValue = VectorSize 2000000
  parseValue = fmap VectorSize . safeRead
  optionName = pure "size"
  optionHelp = pure "Size of vectors used in benchmarks"

newtype RandomSeed = RandomSeed Int

instance IsOption RandomSeed where
  defaultValue = RandomSeed 42
  parseValue = fmap RandomSeed . safeRead
  optionName = pure "seed"
  optionHelp = pure "Random seed used in benchmarks"

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

  let (lparens, rparens) = parenTree useSize
  (nodes, edges1, edges2) <- randomGraph gen useSize
  lparens `seq` rparens `seq` nodes `seq` edges1 `seq` edges2 `seq` return ()

  let randomVector l = U.replicateM l (uniformDoublePositive01M gen)
  as <- randomVector useSize
  bs <- randomVector useSize
  cs <- randomVector useSize
  ds <- randomVector useSize
  sp <- randomVector (floor $ sqrt $ fromIntegral useSize)
  as `seq` bs `seq` cs `seq` ds `seq` sp `seq` return ()

  vi <- MV.new useSize

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
    ]
