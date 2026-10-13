{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE StandaloneDeriving #-}
module Tests.Vector.Unboxed (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import Tests.Vector.Property



testGeneralUnboxedVector
  :: forall a. (CommonContext a VU.Vector, VU.Unbox a, Ord a, Data a)
  => VU.Vector a -> [TestTree]
testGeneralUnboxedVector dummy = concatMap ($ dummy)
  [
    testSanity
  , testPolymorphicFunctions
  , testOrdFunctions
  , testTuplyFunctions
  , testMonoidFunctions
  , testTraverseFunctions
  , testDataFunctions
  ]

testUnitUnboxedVector dummy = concatMap ($ dummy)
  [
    testGeneralUnboxedVector
  ]

testBoolUnboxedVector dummy = concatMap ($ dummy)
  [
    testGeneralUnboxedVector
  , testBoolFunctions
  ]

testNumericUnboxedVector
  :: forall a. ( CommonContext a VU.Vector
               , VU.Unbox a, Ord a, Num a, Enum a, Random a, Data a)
  => VU.Vector a -> [TestTree]
testNumericUnboxedVector dummy = concatMap ($ dummy)
  [
    testGeneralUnboxedVector
  , testNumFunctions
  , testEnumFunctions
  ]

testTupleUnboxedVector
  :: forall a. ( CommonContext a VU.Vector
               , VU.Unbox a, Ord a, Data a) => VU.Vector a -> [TestTree]
testTupleUnboxedVector dummy = concatMap ($ dummy)
  [
    testGeneralUnboxedVector
  ]

tests =
  [ testGroup "()" $
    testUnitUnboxedVector (undefined :: VU.Vector ())
  , testGroup "(Bool)" $
    testBoolUnboxedVector (undefined :: VU.Vector Bool)
  , testGroup "(Int)" $
    testNumericUnboxedVector (undefined :: VU.Vector Int)
  , testGroup "(Float)" $
    testNumericUnboxedVector (undefined :: VU.Vector Float)
  , testGroup "(Double)" $
    testNumericUnboxedVector (undefined :: VU.Vector Double)
  , testGroup "(Int,Bool)" $
    testTupleUnboxedVector (undefined :: VU.Vector (Int, Bool))
  , testGroup "(Int,Bool,Int)" $
    testTupleUnboxedVector
      (undefined :: VU.Vector (Int, Bool, Int))
  , testGroup "unstream" $ testUnstream (undefined :: VU.Vector Int)
  ]
