{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{- |
Here we test that GHC is able to optimize well construction of vector
using monadic\/applicative actions. Well is understood as able to
generate code which does not allocate except for buffer and some
constant overhead.
-}
module Inspect.Alloc where

import Control.Monad.ST
import Data.Int
import Data.Word
import Data.Char
-- import Data.Monoid
import Data.Functor.Identity
import Foreign.Storable (sizeOf)
import Test.Tasty
import Test.Tasty.HUnit
import System.Mem
import Test.Alloc

import qualified Data.Vector.Unboxed as VU
import Inspect.Fusion

tests :: TestTree
tests = testGroup "allocations"
  [ testGroup "traversable"
    [ testCase "IO"
      $ checkAllocations (linear 8)
      $ whnfIO (VU.traverse (\_ -> getAllocationCounter) vector)

#if MIN_VERSION_base(4,17,0)
    -- GHC<9.4 doesn't optimize well.
    , testCase "ST"
      $ checkAllocations (linear 8)
      $ (\v -> runST $ VU.traverse (pureST . fromIntegral) v) `whnf` vector
#endif

#if MIN_VERSION_base(4,15,0)
      -- GHC<9.0 doesn't optimize this well. And there's no appetite
      -- for finding out why. Thus it's disabled for them. We'll still
      -- catch regression going forward.
    , testCase "Identity"
      $ checkAllocations (linear 8)
      $ VU.traverse (\n -> Identity (10*n)) `whnf` vector
#endif

    -- NOTE: Naive traversal is lazy and allocated 2 words per element
    --
    -- , testCase "Const Sum"
    --   $ checkAllocations constant
    --   $ whnf (VU.traverse (Const @_ @() . Sum)) vector
    ]
  , testGroup "unstreamM"
    [ testCase "IO"
      $ checkAllocations (linear 8)
      $ whnfIO (VU.replicateM size getAllocationCounter)

#if MIN_VERSION_base(4,17,0)
    -- GHC<9.4 doesn't optimize well.
    , testCase "ST"
      $ checkAllocations (linear 8)
      $ (\sz -> runST $ VU.generateM sz pureST) `whnf` size
#endif

    -- , testCase "Identity"
    --   $ checkAllocations (linear 8)
    --   $ (\sz -> VU.generateM sz (\n -> Identity (fromIntegral n :: Int64))) `whnf` size
    ]
  , testGroup "Fusion"
    [ testGroup "transformers"
      [ allocWHNF "test_map"                 test_map                 vectorI
      , allocWHNF "test_imap"                test_imap                vectorI
      , allocWHNF "test_mapMaybe"            test_mapMaybe            vectorI
      , allocWHNF "test_cons"                test_cons                vectorI
      , allocWHNF "test_snoc"                test_snoc                vectorI
      -- FIXME: GHC does not fuse intermediate vectors in concatMap
      --
      -- , allocWHNF "test_concatMap_singleton" test_concatMap_singleton vectorI
      -- , allocWHNF "test_concatMap_replicate" test_concatMap_replicate vectorI
      , allocWHNF "test_appendL"            (test_appendL vectorI)    vectorI
      , allocWHNF "test_appendR"            (test_appendR vectorI)    vectorI
      , allocWHNF "test_indexed"             test_indexed             vectorI
      ]
    , testGroup "producers"
      [ allocWHNF "test_replicate"      test_replicate      size
      , allocWHNF "test_generate"       test_generate       size
      , allocWHNF "test_iterateN"       test_iterateN       size
      , allocWHNF "test_unfoldr"        test_unfoldr        size
      , allocWHNF "test_unfoldrN"       test_unfoldrN       size
      , allocWHNF "test_enumFromN"      test_enumFromN      size
      , allocWHNF "test_enumFromStepN"  test_enumFromStepN  size

      , allocWHNF "test_enumFromTo[Int]"      (test_enumFromTo @Int    fromIntegral 0) 100000
      , allocWHNF "test_enumFromTo[Int64]"    (test_enumFromTo @Int64  fromIntegral 0) 100000
      , allocWHNF "test_enumFromTo[Int32]"    (test_enumFromTo @Int32  fromIntegral 0) 100000
      , allocWHNF "test_enumFromTo[Int16]"    (test_enumFromTo @Int16  fromIntegral 0) maxBound
      , allocWHNF "test_enumFromTo[Word]"     (test_enumFromTo @Word   fromIntegral 0) 100000
      , allocWHNF "test_enumFromTo[Word64]"   (test_enumFromTo @Word64 fromIntegral 0) 100000
      , allocWHNF "test_enumFromTo[Word32]"   (test_enumFromTo @Word32 fromIntegral 0) 100000
      , allocWHNF "test_enumFromTo[Word16]"   (test_enumFromTo @Word16 fromIntegral 0) maxBound
      , allocWHNF "test_enumFromTo[Float]"    (test_enumFromTo @Float  round        0) 100000
      , allocWHNF "test_enumFromTo[Double]"   (test_enumFromTo @Double round        0) 100000
      , allocWHNF "test_enumFromTo[Char]"     (test_enumFromTo @Char ord (chr 32)) (chr 8000)
      -- FIXME: We don't have specializations for enumFromThenTo
      --
      -- , allocWHNF "test_enumFromThenTo" test_enumFromThenTo size
      ]
    , testGroup "consumers"
      [ allocWHNF "test_bang"      test_bang     vectorI
      , allocWHNF "test_safeBang"  test_safeBang vectorI
      , allocWHNF "test_head"  test_head vectorI
      , allocWHNF "test_last"  test_last vectorI
      , allocWHNF "test_unsafeHead"  test_unsafeHead vectorI
      , allocWHNF "test_unsafeLast"  test_unsafeLast vectorI
      , allocWHNF "test_indexM"  test_indexM vectorI
      , allocWHNF "test_headM"  test_headM vectorI
      , allocWHNF "test_lastM"  test_lastM vectorI
      , allocWHNF "test_unsafeHeadM"  test_unsafeHeadM vectorI
      , allocWHNF "test_unsafeLastM"  test_unsafeLastM vectorI
      ]
    , testGroup "update"
      [ allocVecWHNF "test_upd"                 (test_upd listUpd) vectorI
      , allocVecWHNF "test_update_1"        (test_update_1  vectorIdx) vectorI
      , allocVecWHNF "test_update_2"        (test_update_2  vectorI) vectorI
      , allocVecWHNF "test_update__1"       (test_update__1 vectorI vectorI) vectorI
      , allocVecWHNF "test_update__2"       (test_update__2 vectorI vectorI) vectorI
      , allocVecWHNF "test_update__3"       (test_update__3 vectorI vectorI) vectorI
      , allocVecWHNF "test_unsafeUpdate_1"        (test_unsafeUpdate_1  vectorIdx) vectorI
      , allocVecWHNF "test_unsafeUpdate_2"        (test_unsafeUpdate_2  vectorI) vectorI
      , allocVecWHNF "test_unsafeUpdate__1"       (test_unsafeUpdate__1 vectorI vectorI) vectorI
      , allocVecWHNF "test_unsafeUpdate__2"       (test_unsafeUpdate__2 vectorI vectorI) vectorI
      , allocVecWHNF "test_unsafeUpdate__3"       (test_unsafeUpdate__3 vectorI vectorI) vectorI
      , allocVecWHNF "test_accumulate_1"        (test_accumulate_1  vectorIdx) vectorI
      , allocVecWHNF "test_accumulate_2"        (test_accumulate_2  vectorI) vectorI
      , allocVecWHNF "test_accumulate__1"       (test_accumulate__1 vectorI vectorI) vectorI
      , allocVecWHNF "test_accumulate__2"       (test_accumulate__2 vectorI vectorI) vectorI
      , allocVecWHNF "test_accumulate__3"       (test_accumulate__3 vectorI vectorI) vectorI
      , allocVecWHNF "test_unsafeAccumulate_1"  (test_unsafeAccumulate_1 vectorIdx) vectorI
      , allocVecWHNF "test_unsafeAccumulate_2"  (test_unsafeAccumulate_2 vectorI) vectorI
      , allocVecWHNF "test_unsafeAccumulate__1" (test_unsafeAccumulate__1 vectorI vectorI) vectorI
      , allocVecWHNF "test_unsafeAccumulate__2" (test_unsafeAccumulate__2 vectorI vectorI) vectorI
      , allocVecWHNF "test_unsafeAccumulate__3" (test_unsafeAccumulate__3 vectorI vectorI) vectorI
      ]
    , testGroup "other"
      [ allocWHNF "test_concat" test_concat listVectorI
      ]
    ]
  ]

allocWHNF :: String -> (a -> b) -> a -> TestTree
{-# INLINE allocWHNF #-}
allocWHNF name f a = testCase name $ checkAllocations constant (f `whnf` a)

allocVecWHNF :: String -> (a -> b) -> a -> TestTree
{-# INLINE allocVecWHNF #-}
allocVecWHNF name f a
  = testCase name
  $ checkAllocations (linear (sizeOf (undefined::Int))) (f `whnf` a)


pureST :: Int -> ST s Int64
{-# NOINLINE pureST #-}
pureST i = pure $! fromIntegral i

-- | Constant overhead. Measurement precision is 4k
overhead :: Int64
overhead = 4096*2

-- | Vector size. It should be large so 1byte per element will be
--   large than page.
size :: Int
size = 100000

vector :: VU.Vector Int64
{-# NOINLINE vector #-}
vector = VU.generate size fromIntegral

vectorI :: VU.Vector Int
{-# NOINLINE vectorI #-}
vectorI = VU.generate size fromIntegral

vectorIdx :: VU.Vector (Int,Int)
{-# NOINLINE vectorIdx #-}
vectorIdx = VU.map (\i -> (i`div`3, i)) vectorI

listVectorI :: [VU.Vector Int]
{-# NOINLINE listVectorI #-}
listVectorI = replicate 8 vectorI

listUpd :: [(Int,Int)]
{-# NOINLINE listUpd #-}
listUpd = [(0,0), (1000,0), (100,0)]


-- | N bytes per element + constant overhead. We also check that bound
--   is tight.
linear :: Int -> Range
linear n = Range
  { allocHi = fromIntegral (n * size) + overhead
  , allocLo = fromIntegral (n * size)
  }

-- | Only constant overhead
constant :: Range
constant = Range { allocHi = overhead
                 , allocLo = 0
                 }
