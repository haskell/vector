{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=Test.Tasty.Inspection.Plugin #-}
{-# OPTIONS_GHC -dsuppress-all                        #-}
{-# OPTIONS_GHC -dno-suppress-type-signatures         #-}
-- | Most basic inspection tests
module Inspect where

import Test.Tasty
import Test.Tasty.Inspection
import qualified Data.Vector as V

simple_fusion :: Int -> Int
simple_fusion n = V.sum $ V.generate n id


tests :: TestTree
tests = testGroup "inspection"
  [ $(inspectObligations [(`hasNoType` ''V.Vector), hasNoTypeClasses] 'simple_fusion)
  ]
