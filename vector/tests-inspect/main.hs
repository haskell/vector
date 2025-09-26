module Main (main) where

import qualified Inspect.Alloc
import qualified Inspect.DerivingVia
import qualified Inspect.Fusion
import Test.Tasty (defaultMain,testGroup)

main :: IO ()
main = defaultMain $ testGroup "tests"
  [ Inspect.DerivingVia.tests
  , Inspect.Alloc.tests
  , Inspect.Fusion.tests
  ]
