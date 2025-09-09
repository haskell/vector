{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- This module provides benchmarks for functions which use API based
-- on applicative. We use @generateA@ based benchmark for state and IO
-- and also benchmark folds and mapping using lens since it's one of
-- important consumers of this API.
module Bench.Vector.Algo.Applicative
  ( -- * Standard benchmarks
    generateState
  , generateStateUnfold
  , generateIO
  , generateIOPrim
    -- * Lens benchmarks
  , lensSum
  , baselineSum
  , lensMap
  , baselineMap
  ) where

import Control.Applicative
import Data.Coerce
import Data.Functor.Identity
import Data.Int
import Data.Monoid
import Data.Word
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as MVG
import qualified Data.Vector.Unboxed as VU
import System.Random.Stateful
import System.Mem (getAllocationCounter)

-- | Benchmark which is running in state monad.
generateState :: Int -> VU.Vector Word64
generateState n
  = runStateGen_ (mkStdGen 42)
  $ \g -> VG.generateA n (\_ -> uniformM g)

-- | Benchmark which is running in state monad.
generateStateUnfold :: Int -> VU.Vector Word64
generateStateUnfold n = VU.unfoldrExactN n genWord64 (mkStdGen 42) 

-- | Benchmark for running @generateA@ in IO monad.
generateIO :: Int -> IO (VU.Vector Int64)
generateIO n = VG.generateA n (\_ -> getAllocationCounter)

-- | Baseline for 'generateIO' it uses primitive operations
generateIOPrim :: Int -> IO (VU.Vector Int64)
generateIOPrim n = VG.unsafeFreeze =<< MVG.replicateM n getAllocationCounter

-- | Sum using lens
lensSum :: VU.Vector Double -> Double
{-# NOINLINE lensSum #-}
lensSum = foldlOf' VG.traverse (+) 0 

-- | Baseline for sum.
baselineSum :: VU.Vector Double -> Double
{-# NOINLINE baselineSum #-}
baselineSum = VU.sum

-- | Mapping over vector elements using 
lensMap :: VU.Vector Double -> VU.Vector Double
{-# NOINLINE lensMap #-}
lensMap = over VG.traverse (*2)

-- | Baseline for map
baselineMap :: VU.Vector Double -> VU.Vector Double
{-# NOINLINE baselineMap #-}
baselineMap = VU.map (*2)

----------------------------------------------------------------
-- Bits and pieces of lens
--
-- We don't want to depend on lens so we just copy relevant
-- parts. After all we don't need much
----------------------------------------------------------------

type ASetter s t a b = (a -> Identity b) -> s -> Identity t
type Getting r s a = (a -> Const r a) -> s -> Const r s

foldlOf' :: Getting (Endo (Endo r)) s a -> (r -> a -> r) -> r -> s -> r
foldlOf' l f z0 = \xs ->
  let f' x (Endo k) = Endo $ \z -> k $! f z x
  in foldrOf l f' (Endo id) xs `appEndo` z0
{-# INLINE foldlOf' #-}

foldrOf :: Getting (Endo r) s a -> (a -> r -> r) -> r -> s -> r
foldrOf l f z = flip appEndo z . foldMapOf l (Endo #. f)
{-# INLINE foldrOf #-}

foldMapOf :: Getting r s a -> (a -> r) -> s -> r
foldMapOf = coerce
{-# INLINE foldMapOf #-}

( #. ) :: Coercible c b => (b -> c) -> (a -> b) -> (a -> c)
( #. ) _ = coerce (\x -> x :: b) :: forall a b. Coercible b a => a -> b
{-# INLINE (#.) #-}

over :: ASetter s t a b -> (a -> b) -> s -> t
over = coerce
{-# INLINE over #-}
