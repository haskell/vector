module Main (main) where

import qualified Inspect

import Test.Tasty (defaultMain,testGroup)

main :: IO ()
main = defaultMain $ testGroup "tests"
  [ Inspect.tests
  ]
