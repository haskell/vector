module Tests.Move (tests) where

import Test.QuickCheck
import Test.Tasty.QuickCheck
import Test.QuickCheck.Property (Property(..))

import Utilities ()

import Control.Monad (replicateM)
import Control.Monad.ST (ST, runST)
import Data.List (sort,sortBy,permutations)

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M

import qualified Data.Vector as V
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed as U

basicMove :: G.Vector v a => v a -> Int -> Int -> Int -> v a
basicMove v dstOff srcOff len
  | len > 0 = G.modify (\ mv -> G.copy (M.slice dstOff len mv) (G.slice srcOff len v)) v
  | otherwise = v

testMove :: (G.Vector v a, Show (v a), Eq (v a)) => v a -> Property
testMove v = G.length v > 0 ==> (MkProperty $ do
  dstOff <- choose (0, G.length v - 1)
  srcOff <- choose (0, G.length v - 1)
  len <- choose (1, G.length v - max dstOff srcOff)
  expected <- return $ basicMove v dstOff srcOff len
  actual <- return $  G.modify (\ mv -> M.move (M.slice dstOff len mv) (M.slice srcOff len mv)) v
  unProperty $ counterexample ("Move: " ++ show (v, dstOff, srcOff, len)) (expected == actual))

checkPermutations :: Int -> Bool
checkPermutations n = runST $ do
    vec <- U.thaw (U.fromList [1..n])
    res <- replicateM (product [1..n]) $ M.nextPermutation vec >> U.freeze vec >>= return . U.toList
    return $! ([1..n] : res) == sort (permutations [1..n]) ++ [[n,n-1..1]]

testPermutations :: Bool
testPermutations = all checkPermutations [1..7]

checkRevPermutations :: Int -> Bool
checkRevPermutations n = runST $ do
    vec <- U.thaw (U.fromList [n,n-1..1])
    res <- replicateM (product [1..n]) $ M.prevPermutation vec >> U.freeze vec >>= return . U.toList
    return $! ([n,n-1..1] : res) == sortBy (flip compare) (permutations [n,n-1..1]) ++ [[1..n]]

testRevPermutations :: Bool
testRevPermutations = all checkRevPermutations [1..7]

nextPermutationBijective :: (M.MVector v a, Ord a) => v s a -> ST s ()
nextPermutationBijective v = do
  res <- M.nextPermutation v
  if res then return () else M.reverse v

prevPermutationBijective :: (M.MVector v a, Ord a) => v s a -> ST s ()
prevPermutationBijective v = do
  res <- M.prevPermutation v
  if res then return () else M.reverse v

testNPPermutationIsId :: (G.Vector v a, Ord a, Show (v a), Eq (v a)) => v a -> Property 
testNPPermutationIsId v = v === G.modify (\mv -> nextPermutationBijective mv >> prevPermutationBijective mv) v

testPNPermutationIsId :: (G.Vector v a, Ord a, Show (v a), Eq (v a)) => v a -> Property
testPNPermutationIsId v = v === G.modify (\mv -> prevPermutationBijective mv >> nextPermutationBijective mv) v

tests =
    [testProperty "Data.Vector.Mutable (Move)" (testMove :: V.Vector Int -> Property),
     testProperty "Data.Vector.Primitive.Mutable (Move)" (testMove :: P.Vector Int -> Property),
     testProperty "Data.Vector.Unboxed.Mutable (Move)" (testMove :: U.Vector Int -> Property),
     testProperty "Data.Vector.Storable.Mutable (Move)" (testMove :: S.Vector Int -> Property),
     testProperty "Data.Vector.Generic.Mutable (nextPermutation)" testPermutations,
     testProperty "Data.Vector.Generic.Mutable (prevPermutation)" testRevPermutations,
     testProperty "Data.Vector.Generic.Mutable (nextPermutation then prevPermutation = id)" 
     (testNPPermutationIsId :: U.Vector Int -> Property),
     testProperty "Data.Vector.Generic.Mutable (prevPermutation then nextPermutation = id)"
     (testPNPermutationIsId :: U.Vector Int -> Property)]
