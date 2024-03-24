{-# LANGUAGE ConstraintKinds #-}
module Tests.Vector.Strict (tests) where

import Test.Tasty
import qualified Data.Vector.Strict
import Tests.Vector.Property

import GHC.Exts (inline)


testGeneralBoxedVector
  :: forall a. (CommonContext a Data.Vector.Strict.Vector, Ord a, Data a)
  => Data.Vector.Strict.Vector a -> [TestTree]
testGeneralBoxedVector dummy = concatMap ($ dummy)
  [
    testSanity
  , inline testPolymorphicFunctions
  , testOrdFunctions
  , testTuplyFunctions
  , testNestedVectorFunctions
  , testMonoidFunctions
  , testFunctorFunctions
  , testMonadFunctions
  , testApplicativeFunctions
  , testAlternativeFunctions
  , testSequenceFunctions
  , testDataFunctions
  ]

testBoolBoxedVector dummy = concatMap ($ dummy)
  [
    testGeneralBoxedVector
  , testBoolFunctions
  ]

testNumericBoxedVector
  :: forall a. (CommonContext a Data.Vector.Strict.Vector, Ord a, Num a, Enum a, Random a, Data a)
  => Data.Vector.Strict.Vector a -> [TestTree]
testNumericBoxedVector dummy = concatMap ($ dummy)
  [
    testGeneralBoxedVector
  , testNumFunctions
  , testEnumFunctions
  ]

tests =
  [ testGroup "Bool" $
    testBoolBoxedVector (undefined :: Data.Vector.Strict.Vector Bool)
  , testGroup "Int" $
    testNumericBoxedVector (undefined :: Data.Vector.Strict.Vector Int)
  , testGroup "unstream" $ testUnstream (undefined :: Data.Vector.Strict.Vector Int)
  ]
