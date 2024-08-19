{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Bench.Vector.Algo.NextPermutation (generatePermTests) where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Generic.Mutable as G
import System.Random.Stateful
    ( StatefulGen, UniformRange(uniformRM) )

-- | Generate a list of benchmarks for permutation algorithms.
-- The list contains pairs of benchmark names and corresponding actions.
-- The actions are to be executed by the benchmarking framework.
-- 
-- The list contains the following benchmarks:
-- - @(next|prev)Permutation@ on a small vector repeated until the end of the permutation cycle
-- - Bijective versions of @(next|prev)Permutation@ on a vector of size @n@, repeated @n@ times
--  - ascending permutation
--  - descending permutation
--  - random permutation
-- - Baseline for bijective versions: just copying a vector of size @n@. Note that the tests for
--   bijective versions begins with copying a vector.
generatePermTests :: StatefulGen g IO => g -> Int -> IO [(String, IO ())]
generatePermTests gen useSize = do
  let !k = useSizeToPermLen useSize
  let !vasc = V.generate useSize id
      !vdesc = V.generate useSize (useSize-1-)
  !vrnd <- randomPermutationWith gen useSize
  return
    [ ("nextPermutation (small vector, until end)", loopPermutations k)
    , ("nextPermutationBijective (ascending perm of size n, n times)", repeatNextPermutation vasc useSize)
    , ("nextPermutationBijective (descending perm of size n, n times)", repeatNextPermutation vdesc useSize)
    , ("nextPermutationBijective (random perm of size n, n times)", repeatNextPermutation vrnd useSize)
    , ("prevPermutation (small vector, until end)", loopRevPermutations k)
    , ("prevPermutationBijective (ascending perm of size n, n times)", repeatPrevPermutation vasc useSize)
    , ("prevPermutationBijective (descending perm of size n, n times)", repeatPrevPermutation vdesc useSize)
    , ("prevPermutationBijective (random perm of size n, n times)", repeatPrevPermutation vrnd useSize)
    , ("baseline for *Bijective (just copying the vector of size n)", V.thaw vrnd >> return ())
    ]

-- | Given a PRNG and a length @n@, generate a random permutation of @[0..n-1]@.
randomPermutationWith :: (StatefulGen g IO) => g -> Int -> IO (V.Vector Int)
randomPermutationWith gen n = do
  v <- M.generate n id
  V.forM_ (V.generate (n-1) id) $ \ !i -> do
    j <- uniformRM (i,n-1) gen
    M.swap v i j
  V.unsafeFreeze v

-- | Given @useSize@ benchmark option, compute the largest @n <= 12@ such that @n! <= useSize@.
-- Repeat-nextPermutation-until-end benchmark will use @n@ as the length of the vector.
-- Note that 12 is the largest @n@ such that @n!@ can be represented as an 'Int32'.
useSizeToPermLen :: Int -> Int
useSizeToPermLen us = case V.findIndex (> max 0 us) $ V.scanl' (*) 1 $ V.generate 12 (+1) of
    Just i -> i-1
    Nothing -> 12

-- | A bijective version of @G.nextPermutation@ that reverses the vector
-- if it is already in descending order.
-- "Bijective" here means that the function forms a cycle over all permutations
-- of the vector's elements.
--
-- This has a nice property that should be benchmarked: 
-- this function takes amortized constant time each call,
-- if successively called either Omega(n) times on a single vector having distinct elements,
-- or arbitrary times on a single vector initially in strictly ascending order.
nextPermutationBijective :: (G.MVector v a, Ord a) => v G.RealWorld a -> IO Bool
nextPermutationBijective v = do
  res <- G.nextPermutation v
  if res then return True else G.reverse v >> return False

-- | A bijective version of @G.prevPermutation@ that reverses the vector
-- if it is already in ascending order.
-- "Bijective" here means that the function forms a cycle over all permutations
-- of the vector's elements.
--
-- This has a nice property that should be benchmarked:
-- this function takes amortized constant time each call,
-- if successively called either Omega(n) times on a single vector having distinct elements,
-- or arbitrary times on a single vector initially in strictly descending order.
prevPermutationBijective :: (G.MVector v a, Ord a) => v G.RealWorld a -> IO Bool
prevPermutationBijective v = do
  res <- G.prevPermutation v
  if res then return True else G.reverse v >> return False

-- | Repeat @nextPermutation@ on @[0..n-1]@ until the end.
loopPermutations :: Int -> IO ()
loopPermutations n = do
  v <- M.generate n id
  let loop = do
        res <- M.nextPermutation v
        if res then loop else return ()
  loop

-- | Repeat @prevPermutation@ on @[n-1,n-2..0]@ until the end.
loopRevPermutations :: Int -> IO ()
loopRevPermutations n = do
  v <- M.generate n (n-1-)
  let loop = do
        res <- M.prevPermutation v
        if res then loop else return ()
  loop

-- | Repeat @nextPermutationBijective@ on a given vector given times.
repeatNextPermutation :: V.Vector Int -> Int -> IO ()
repeatNextPermutation !v !n = do
  !mv <- V.thaw v
  let loop !i | i <= 0 = return ()
      loop !i = do
        _ <- nextPermutationBijective mv
        loop (i-1)
  loop n

-- | Repeat @prevPermutationBijective@ on a given vector given times.
repeatPrevPermutation :: V.Vector Int -> Int -> IO ()
repeatPrevPermutation !v !n = do
  !mv <- V.thaw v
  let loop !i | i <= 0 = return ()
      loop !i = do
        _ <- prevPermutationBijective mv
        loop (i-1)
  loop n
