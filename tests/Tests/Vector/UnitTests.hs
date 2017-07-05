{-# LANGUAGE ScopedTypeVariables #-}

module Tests.Vector.UnitTests (tests) where

import Control.Applicative as Applicative
import Control.Monad.Fix (mfix)
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as Storable
import Foreign.Ptr
import Foreign.Storable
import Text.Printf

import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertBool)

newtype Aligned a = Aligned { getAligned :: a }

instance (Storable a) => Storable (Aligned a) where
  sizeOf _    = sizeOf (undefined :: a)
  alignment _ = 128
  peek ptr    = Aligned Applicative.<$> peek (castPtr ptr)
  poke ptr    = poke (castPtr ptr) . getAligned

tests :: [Test]
tests =
  [ testGroup "Data.Vector.Storable.Vector"
      [ testCase "Aligned Double" $
          checkAddressAlignment alignedDoubleVec
      , testCase "Aligned Int" $
          checkAddressAlignment alignedIntVec
      ]
  , testGroup "Data.Vector"
      [ testCase "MonadFix" checkMonadFix
      ]
  ]

checkAddressAlignment :: forall a. (Storable a) => Storable.Vector a -> Assertion
checkAddressAlignment xs = Storable.unsafeWith xs $ \ptr -> do
  let ptr'  = ptrToWordPtr ptr
      msg   = printf "Expected pointer with alignment %d but got 0x%08x" (toInteger align) (toInteger ptr')
      align :: WordPtr
      align = fromIntegral $ alignment dummy
  assertBool msg $ (ptr' `mod` align) == 0
  where
    dummy :: a
    dummy = undefined

alignedDoubleVec :: Storable.Vector (Aligned Double)
alignedDoubleVec = Storable.fromList $ map Aligned [1, 2, 3, 4, 5]

alignedIntVec :: Storable.Vector (Aligned Int)
alignedIntVec = Storable.fromList $ map Aligned [1, 2, 3, 4, 5]

checkMonadFix :: Assertion
checkMonadFix = assertBool "checkMonadFix" $
    Vector.toList fewV == fewL &&
    Vector.toList none == []
  where
    facty _ 0 = 1; facty f n = n * f (n - 1)
    fewV :: Vector.Vector Int
    fewV = fmap ($ 12) $ mfix (\i -> Vector.fromList [facty i, facty (+1), facty (+2)])
    fewL :: [Int]
    fewL = fmap ($ 12) $ mfix (\i -> [facty i, facty (+1), facty (+2)])

    none :: Vector.Vector Int
    none = mfix (const Vector.empty)
