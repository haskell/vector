{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tests.Vector.UnitTests (tests) where

import Control.Applicative as Applicative
import Control.Monad.Primitive
import Data.Int
import Data.Word
import Data.Typeable
import qualified Data.Vector.Generic  as Generic
import qualified Data.Vector.Storable as Storable
import qualified Data.Vector          as Vector
import Foreign.Ptr
import Foreign.Storable
import Text.Printf

import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertBool, (@=?))

newtype Aligned a = Aligned { getAligned :: a }

instance (Storable a) => Storable (Aligned a) where
  sizeOf _    = sizeOf (undefined :: a)
  alignment _ = 128
  peek ptr    = Aligned Applicative.<$> peek (castPtr ptr)
  poke ptr    = poke (castPtr ptr) . getAligned

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

tests :: [Test]
tests =
  [ testGroup "Data.Vector.Storable.Vector Alignment"
      [ testCase "Aligned Double" $
          checkAddressAlignment alignedDoubleVec
      , testCase "Aligned Int" $
          checkAddressAlignment alignedIntVec
      ]
  , testGroup "Regression tests"
    [ testGroup "enumFromTo crash #188" $
      [ regression188 (Proxy :: Proxy Word8)
      , regression188 (Proxy :: Proxy Word16)
      , regression188 (Proxy :: Proxy Word32)
      , regression188 (Proxy :: Proxy Word64)
      , regression188 (Proxy :: Proxy Word)
      , regression188 (Proxy :: Proxy Int8)
      , regression188 (Proxy :: Proxy Int16)
      , regression188 (Proxy :: Proxy Int32)
      , regression188 (Proxy :: Proxy Int64)
      , regression188 (Proxy :: Proxy Int)
      , regression188 (Proxy :: Proxy Char)
      ]
    ]
  ]

regression188
  :: forall a. (Typeable a, Enum a, Bounded a, Eq a, Show a)
  => Proxy a -> Test
regression188 _ = testCase (show (typeOf (undefined :: a)))
  $ Vector.fromList [maxBound::a] @=? Vector.enumFromTo maxBound maxBound
{-# INLINE regression188 #-}

alignedDoubleVec :: Storable.Vector (Aligned Double)
alignedDoubleVec = Storable.fromList $ map Aligned [1, 2, 3, 4, 5]

alignedIntVec :: Storable.Vector (Aligned Int)
alignedIntVec = Storable.fromList $ map Aligned [1, 2, 3, 4, 5]

#if __GLASGOW_HASKELL__ >= 800
-- Ensure that Mutable is really an injective type family by typechecking a
-- function which relies on injectivity.
_f :: (Generic.Vector v a, Generic.Vector w a, PrimMonad f)
   => Generic.Mutable v (PrimState f) a -> f (w a)
_f v = Generic.convert `fmap` Generic.unsafeFreeze v
#endif
