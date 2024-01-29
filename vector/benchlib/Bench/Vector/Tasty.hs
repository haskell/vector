-- |
-- Tasty integration for vector benchmarks.
module Bench.Vector.Tasty
  ( VectorSize(..)
  , RandomSeed(..)
  ) where

import Test.Tasty.Options


-- | Size of vector used in benchmarks
newtype VectorSize = VectorSize Int

instance IsOption VectorSize where
  defaultValue = VectorSize 2000000
  parseValue = fmap VectorSize . safeRead
  optionName = pure "size"
  optionHelp = pure "Size of vectors used in benchmarks"

-- | Random seed used for generation of the test data
newtype RandomSeed = RandomSeed Int

instance IsOption RandomSeed where
  defaultValue = RandomSeed 42
  parseValue = fmap RandomSeed . safeRead
  optionName = pure "seed"
  optionHelp = pure "Random seed used for generation of the test data"
