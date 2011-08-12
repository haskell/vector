#if __GLASGOW_HASKELL__ >= 701 && defined(VECTOR_BOUNDS_CHECKS)
{-# LANGUAGE Trustworthy #-}
#endif

-- |
-- Module      : Data.Vector.Unboxed.Safe
-- Copyright   : (c) Roman Leshchinskiy 2009-2010
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
--
-- Adaptive unboxed vectors. The implementation is based on type families
-- and picks an efficient, specialised representation for every element type.
-- In particular, unboxed vectors of pairs are represented as pairs of unboxed
-- vectors.
--
-- Safe API only.
--
-- Implementing unboxed vectors for new data types can be very easy. Here is
-- how the library does this for 'Complex' by simply wrapping vectors of
-- pairs.
--
-- @
-- newtype instance 'MVector' s ('Complex' a) = MV_Complex ('MVector' s (a,a))
-- newtype instance 'Vector'    ('Complex' a) = V_Complex  ('Vector'    (a,a))
--
-- instance ('RealFloat' a, 'Unbox' a) => 'Data.Vector.Generic.Mutable.MVector' 'MVector' ('Complex' a) where
--   {-\# INLINE basicLength \#-}
--   basicLength (MV_Complex v) = 'Data.Vector.Generic.Mutable.basicLength' v
--   ...
--
-- instance ('RealFloat' a, 'Unbox' a) => Data.Vector.Generic.Vector 'Vector' ('Complex' a) where
--   {-\# INLINE basicLength \#-}
--   basicLength (V_Complex v) = Data.Vector.Generic.basicLength v
--   ...
--
-- instance ('RealFloat' a, 'Unbox' a) => 'Unbox' ('Complex' a)
-- @

module Data.Vector.Unboxed.Safe (
  -- * Unboxed vectors
  Vector, MVector, Unbox,

  -- * Accessors

  -- ** Length information
  length, null,

  -- ** Indexing
  (!), (!?), head, last,

  -- ** Monadic indexing
  indexM, headM, lastM,

  -- ** Extracting subvectors (slicing)
  slice, init, tail, take, drop, splitAt,

  -- * Construction

  -- ** Initialisation
  empty, singleton, replicate, generate, iterateN,

  -- ** Monadic initialisation
  replicateM, generateM, create,

  -- ** Unfolding
  unfoldr, unfoldrN,

  -- ** Enumeration
  enumFromN, enumFromStepN, enumFromTo, enumFromThenTo,

  -- ** Concatenation
  cons, snoc, (++), concat,

  -- ** Restricting memory usage
  force,

  -- * Modifying vectors

  -- ** Bulk updates
  (//), update, update_,

  -- ** Accumulations
  accum, accumulate, accumulate_,

  -- ** Permutations 
  reverse, backpermute,

  -- ** Safe destructive updates
  modify,

  -- * Elementwise operations

  -- ** Indexing
  indexed,

  -- ** Mapping
  map, imap, concatMap,

  -- ** Monadic mapping
  mapM, mapM_, forM, forM_,

  -- ** Zipping
  zipWith, zipWith3, zipWith4, zipWith5, zipWith6,
  izipWith, izipWith3, izipWith4, izipWith5, izipWith6,
  zip, zip3, zip4, zip5, zip6,

  -- ** Monadic zipping
  zipWithM, zipWithM_,

  -- ** Unzipping
  unzip, unzip3, unzip4, unzip5, unzip6,

  -- * Working with predicates

  -- ** Filtering
  filter, ifilter, filterM,
  takeWhile, dropWhile,

  -- ** Partitioning
  partition, unstablePartition, span, break,

  -- ** Searching
  elem, notElem, find, findIndex, findIndices, elemIndex, elemIndices,

  -- * Folding
  foldl, foldl1, foldl', foldl1', foldr, foldr1, foldr', foldr1',
  ifoldl, ifoldl', ifoldr, ifoldr',

  -- ** Specialised folds
  all, any, and, or,
  sum, product,
  maximum, maximumBy, minimum, minimumBy,
  minIndex, minIndexBy, maxIndex, maxIndexBy,

  -- ** Monadic folds
  foldM, foldM', fold1M, fold1M',
  foldM_, foldM'_, fold1M_, fold1M'_,

  -- * Prefix sums (scans)
  prescanl, prescanl',
  postscanl, postscanl',
  scanl, scanl', scanl1, scanl1',
  prescanr, prescanr',
  postscanr, postscanr',
  scanr, scanr', scanr1, scanr1',

  -- * Conversions

  -- ** Lists
  toList, fromList, fromListN,

  -- ** Other vector types
  G.convert,

  -- ** Mutable vectors
  freeze, thaw, copy
) where

import Data.Vector.Unboxed.Safe

