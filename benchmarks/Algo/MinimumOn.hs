module Algo.MinimumOn ( minOn, maxOn ) where

import Data.Vector.Unboxed as V

minOn :: (Double -> Double) -> V.Vector Double -> Double
minOn = V.minimumOn

maxOn :: (Double -> Double) -> V.Vector Double -> Double
maxOn = V.maximumOn

