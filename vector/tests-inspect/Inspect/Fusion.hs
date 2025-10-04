{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -dsuppress-all                        #-}
{-# OPTIONS_GHC -dno-suppress-type-signatures         #-}
-- |
module Inspect.Fusion where

import Test.Tasty
-- import Test.Tasty.Inspection
import qualified Data.Vector.Fusion.Bundle.Monadic as B
import           Data.Vector.Fusion.Bundle.Size (Size(..))
import qualified Data.Stream.Monadic as S
import qualified Data.Vector.Unboxed as VU
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Generic as VG
import           Data.Vector.Fusion.Util (Box)

import Test.InspectExtra


-- We need to define this function to test rewrite rules in vector-stream.Hv
-- Rewrite rules in Bundle do not reuse rules in vector-stream
enumFromToStream :: (Enum a, VG.Vector v a) => a -> a -> v a
{-# INLINE enumFromToStream #-}
enumFromToStream x y = VG.unstream $ B.fromStream (S.enumFromTo x y) Unknown


-- NOTE: [Fusion tests]
-- ~~~~~~~~~~~~~~~~~~~~
--
-- In this module we define functions to be tested. All test functions
-- are constructed in the that there should be no vector allocations
-- if fusion happens. There are two tests for each function:
--
--  1. Inspection tests which tests that GHC successfully eliminate
--     stream data types. They also allow to inspect core of offending
--     function easily using coreOf property.
--
--  2. Allocation tests which measure memory allocation during
--     function execution. It's difficult to check that fusion happens
--     by inspecting core so we fall back to checking function
--     behavior. This methods also requires that GHC is able to
--     compile function to nonallocating loops, but that's desirable
--     property as well.

goodConsumer
  :: (VG.Vector v a, Num a)
  => (v a -> b) -> (v a -> b)
{-# INLINE goodConsumer #-}
goodConsumer f = f . VG.map (+1)
  
goodTransformer
  :: (VG.Vector v a, VG.Vector v b, Num a, Num b)
  => (v a -> v b) -> (v a -> b)
{-# INLINE goodTransformer #-}
goodTransformer f = VG.sum . f . VG.map (+1)

goodProducer
  :: (VG.Vector v a, Num a)
  => (b -> v a) -> (b -> a)
{-# INLINE goodProducer #-}
goodProducer f = VG.sum . f



----------------------------------------------------------------
-- Functions transforming vectors
----------------------------------------------------------------

test_map :: Vector Int -> Int
test_map = goodTransformer (VU.map (*2))

test_imap :: Vector Int -> Int
test_imap = goodTransformer (VU.imap (+))

test_mapMaybe :: Vector Int -> Int
test_mapMaybe = goodTransformer (VU.mapMaybe (\x -> if odd x then Just x else Nothing))

test_cons :: Vector Int -> Int
test_cons = goodTransformer (VU.cons 123)

test_snoc :: Vector Int -> Int
test_snoc = goodTransformer (`VU.snoc` 123)

test_concatMap_singleton :: Vector Int -> Int
test_concatMap_singleton = goodTransformer (VU.concatMap VU.singleton)

test_concatMap_replicate :: Vector Int -> Int
test_concatMap_replicate = goodTransformer (VU.concatMap (VU.replicate 10))

test_appendR, test_appendL :: Vector Int -> Vector Int -> Int
test_appendR v = goodTransformer (v VU.++)
test_appendL v = goodTransformer (VU.++ v)

test_indexed :: Vector Int -> Int
test_indexed = goodTransformer (VU.map (\(i,j) -> i+j) . VU.indexed)


----------------------------------------------------------------
-- Update/accumulate
----------------------------------------------------------------

test_upd :: [(Int,Int)] -> Vector Int -> Int
test_upd xs = goodTransformer (VU.// xs)

test_update_1 :: Vector (Int,Int) -> Vector Int -> Int
test_update_1 xs
  = goodTransformer (\vec -> VU.update vec xs)

test_update_2 :: Vector Int -> Vector Int -> Int
test_update_2 vec
  = goodTransformer (VU.update vec . VU.map (\i -> (i`div`3, i)))

test_update__1, test_update__2, test_update__3
  :: Vector Int -> Vector Int -> Vector Int -> Int
-- NOTE: We need to ensure that index won't get out of range
test_update__1 y z = goodTransformer (\x -> VU.update_ x y z)
test_update__2 x z = goodTransformer (\y -> VU.update_ x (VU.map (`div` 3) y) z)
test_update__3 x y = goodTransformer (\z -> VU.update_ x y z)


test_unsafeUpdate_1 :: Vector (Int,Int) -> Vector Int -> Int
test_unsafeUpdate_1 xs
  = goodTransformer (\vec -> VU.unsafeUpdate vec xs)

test_unsafeUpdate_2 :: Vector Int -> Vector Int -> Int
test_unsafeUpdate_2 vec
  = goodTransformer (VU.unsafeUpdate vec . VU.map (\i -> (i`div`3, i)))

test_unsafeUpdate__1, test_unsafeUpdate__2, test_unsafeUpdate__3
  :: Vector Int -> Vector Int -> Vector Int -> Int
-- NOTE: We need to ensure that index won't get out of range
test_unsafeUpdate__1 y z = goodTransformer (\x -> VU.unsafeUpdate_ x y z)
test_unsafeUpdate__2 x z = goodTransformer (\y -> VU.unsafeUpdate_ x (VU.map (`div` 3) y) z)
test_unsafeUpdate__3 x y = goodTransformer (\z -> VU.unsafeUpdate_ x y z)


test_accumulate_1 :: Vector (Int,Int) -> Vector Int -> Int
test_accumulate_1 xs
  = goodTransformer (\vec -> VU.accumulate (+) vec xs)

test_accumulate_2 :: Vector Int -> Vector Int -> Int
test_accumulate_2 vec
  = goodTransformer (VU.accumulate (+) vec . VU.map (\i -> (i`div`3, i)))

test_accumulate__1, test_accumulate__2, test_accumulate__3
  :: Vector Int -> Vector Int -> Vector Int -> Int
-- NOTE: We need to ensure that index won't get out of range
test_accumulate__1 y z = goodTransformer (\x -> VU.accumulate_ (+) x y z)
test_accumulate__2 x z = goodTransformer (\y -> VU.accumulate_ (+) x (VU.map (`div` 3) y) z)
test_accumulate__3 x y = goodTransformer (\z -> VU.accumulate_ (+) x y z)


test_unsafeAccumulate_1 :: Vector (Int,Int) -> Vector Int -> Int
test_unsafeAccumulate_1 xs
  = goodTransformer (\vec -> VU.unsafeAccumulate (+) vec xs)

test_unsafeAccumulate_2 :: Vector Int -> Vector Int -> Int
test_unsafeAccumulate_2 vec
  = goodTransformer (VU.unsafeAccumulate (+) vec . VU.map (\i -> (i`div`3, i)))

test_unsafeAccumulate__1, test_unsafeAccumulate__2, test_unsafeAccumulate__3
  :: Vector Int -> Vector Int -> Vector Int -> Int
-- NOTE: We need to ensure that index won't get out of range
test_unsafeAccumulate__1 y z = goodTransformer (\x -> VU.unsafeAccumulate_ (+) x y z)
test_unsafeAccumulate__2 x z = goodTransformer (\y -> VU.unsafeAccumulate_ (+) x (VU.map (`div` 3) y) z)
test_unsafeAccumulate__3 x y = goodTransformer (\z -> VU.unsafeAccumulate_ (+) x y z)


----------------------------------------------------------------
-- Function creating vectors
----------------------------------------------------------------

test_replicate :: Int -> Double
test_replicate = goodProducer (\n -> VU.replicate n 12.0)

test_generate :: Int -> Int
test_generate = goodProducer (\n -> VU.generate n id)

test_iterateN :: Int -> Int
test_iterateN = goodProducer (\n -> VU.iterateN n (+1) 0)

test_unfoldr :: Int -> Int
test_unfoldr = goodProducer (\n -> VU.unfoldr (\i -> if i > n then Nothing else Just (i,i+1)) 0)

test_unfoldrN :: Int -> Int
test_unfoldrN = goodProducer (\n -> VU.unfoldrN n (\i -> Just (i,i+1)) 0)

test_enumFromN, test_enumFromStepN :: Int -> Double
test_enumFromN      = goodProducer (\n -> VU.enumFromN 123 n)
test_enumFromStepN  = goodProducer (\n -> VU.enumFromStepN 123 2 n)


-- NOTE: [enumFromTo]
-- ~~~~~~~~~~~~~~~~~
--
-- both enumFromTo and enumFromThenTo are wrapping methods of Enum
-- type class and thus has to create list and allocate. However we
-- have extensive set of rewrite rules which produce specializations
-- for base types.
--
-- For this reason we need to write test for all specializations

test_enumFromTo :: (Enum a, VU.Unbox a) => (a -> Int) -> a -> a -> Int
{-# INLINE test_enumFromTo #-}
test_enumFromTo fun a
  = goodProducer (VU.map fun . VU.enumFromTo a)

test_enumFromThenTo :: (Enum a, VU.Unbox a) => (a -> Int) -> a -> a -> a -> Int
test_enumFromThenTo fun a b
  = goodProducer (VU.map fun . VU.enumFromThenTo a b)

test_enumFromToStream :: (Enum a, VU.Unbox a) => (a -> Int) -> a -> a -> Int
{-# INLINE test_enumFromToStream #-}
test_enumFromToStream fun a
  = goodProducer (VU.map fun . enumFromToStream a)

----------------------------------------------------------------
-- Function consuming vectors
----------------------------------------------------------------

test_bang,test_unsafeIndex :: Vector Int -> Int
test_bang        = goodConsumer (VU.! 42000)
test_unsafeIndex = goodConsumer (`VU.unsafeIndex` 42)

test_safeBang :: Vector Int -> Maybe Int
test_safeBang = goodConsumer (VU.!? 42000)

test_head, test_last, test_unsafeHead, test_unsafeLast :: Vector Int -> Int
test_head       = goodConsumer VU.head
test_last       = goodConsumer VU.last
test_unsafeHead = goodConsumer VU.unsafeHead
test_unsafeLast = goodConsumer VU.unsafeLast

test_headM, test_lastM, test_unsafeHeadM, test_unsafeLastM, test_indexM :: Vector Int -> Box Int
test_indexM      = goodConsumer (`VU.indexM` 43)
test_headM       = goodConsumer VU.headM
test_lastM       = goodConsumer VU.lastM
test_unsafeHeadM = goodConsumer VU.unsafeHeadM
test_unsafeLastM = goodConsumer VU.unsafeLastM

----------------------------------------------------------------
-- Functions involving lists
----------------------------------------------------------------

test_concat :: [Vector Int] -> Int
test_concat = VU.sum . VU.map (+1) . VU.concat



----------------------------------------------------------------
-- Inspection tests
--
-- They have to be defined in this module
----------------------------------------------------------------

tests :: TestTree
tests = testGroup "Fusion"
  [ testGroup "transformers"
    [ $(inspectFusion 'test_map)
    , $(inspectFusion 'test_imap)
    , $(inspectFusion 'test_mapMaybe)
    , $(inspectFusion 'test_cons)
    , $(inspectFusion 'test_snoc)
    , $(inspectFusion 'test_concatMap_singleton)
    , $(inspectFusion 'test_concatMap_replicate)
    , $(inspectFusion 'test_appendL)
    , $(inspectFusion 'test_appendR)
    , $(inspectFusion 'test_indexed)
    ]
  , testGroup "updates"
    [ $(inspectFusion 'test_upd)
    , $(inspectFusion 'test_update_1)
    , $(inspectFusion 'test_update_2)
    , $(inspectFusion 'test_update__1)
    , $(inspectFusion 'test_update__2)
    , $(inspectFusion 'test_update__3)
    , $(inspectFusion 'test_unsafeUpdate_1)
    , $(inspectFusion 'test_unsafeUpdate_2)
    , $(inspectFusion 'test_unsafeUpdate__1)
    , $(inspectFusion 'test_unsafeUpdate__2)
    , $(inspectFusion 'test_unsafeUpdate__3)
    , $(inspectFusion 'test_accumulate_1)
    , $(inspectFusion 'test_accumulate_2)
    , $(inspectFusion 'test_accumulate__1)
    , $(inspectFusion 'test_accumulate__2)
    , $(inspectFusion 'test_accumulate__3)
    , $(inspectFusion 'test_unsafeAccumulate_1)
    , $(inspectFusion 'test_unsafeAccumulate_2)
    , $(inspectFusion 'test_unsafeAccumulate__1)
    , $(inspectFusion 'test_unsafeAccumulate__2)
    , $(inspectFusion 'test_unsafeAccumulate__3)
    ]    
  , testGroup "producers"
    [ $(inspectFusion 'test_replicate)
    , $(inspectFusion 'test_generate)
    , $(inspectFusion 'test_iterateN)
    , $(inspectFusion 'test_unfoldr)
    , $(inspectFusion 'test_unfoldrN)
    , $(inspectFusion 'test_enumFromN)
    , $(inspectFusion 'test_enumFromStepN)
    , $(inspectClassyFusion 'test_enumFromTo)
    , $(inspectClassyFusion 'test_enumFromThenTo)
    , $(inspectClassyFusion 'test_enumFromToStream)
    ]
  , testGroup "consumers"
    [ $(inspectFusion 'test_bang)
    , $(inspectFusion 'test_safeBang)
    , $(inspectFusion 'test_head)
    , $(inspectFusion 'test_last)
    , $(inspectFusion 'test_unsafeHead)
    , $(inspectFusion 'test_unsafeLast)
    , $(inspectFusion 'test_indexM)
    , $(inspectFusion 'test_headM)
    , $(inspectFusion 'test_lastM)
    , $(inspectFusion 'test_unsafeHeadM)
    , $(inspectFusion 'test_unsafeLastM)
    ]
  , testGroup "other"
    [ $(inspectFusion 'test_concat)
    ]
  ]
