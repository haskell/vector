module Main (main) where

import qualified Inspect
import qualified Inspect.DerivingVia
import Test.Tasty (defaultMain,testGroup)

main :: IO ()
main = defaultMain $ testGroup "tests"
  [ Inspect.tests
  , Inspect.DerivingVia.tests
  ]
