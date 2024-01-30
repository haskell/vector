module Bench.Vector.Algo.FindIndexR (findIndexR, findIndexR_naive, findIndexR_manual)
where

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Generic as V

findIndexR :: (Double -> Bool, Vector Double) -> Maybe Int
{-# NOINLINE findIndexR #-}
findIndexR = uncurry V.findIndexR

findIndexR_naive :: (Double -> Bool, Vector Double) -> Maybe Int
{-# NOINLINE findIndexR_naive #-}
findIndexR_naive (pred, v) = fmap (V.length v - 1 -)
    $ V.foldl (\a x -> if pred x
                        then Just 1
                        else succ<$>a) Nothing v

findIndexR_manual :: (Double -> Bool, Vector Double) -> Maybe Int
{-# NOINLINE findIndexR_manual #-}
findIndexR_manual (pred, v) = go $ V.length v - 1
 where go i | i < 0                     = Nothing
            | pred (V.unsafeIndex v i)  = Just i
            | otherwise                 = go $ i-1

