-- |
{-# OPTIONS_GHC -fplugin=Test.Tasty.Inspection.Plugin #-}
module Inspect where

import Test.Tasty
import qualified Data.Vector as V

tests :: TestTree
tests = testGroup "inspection"
  [
  ]
