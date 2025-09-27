{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- We provide custom specialized versions for some functions so we
-- need that specializations perform same as nonspecialized versions
module Tests.Specialization (tests) where

import Data.Char
import Data.Int
import Data.Word
import Data.Typeable
import qualified Data.Vector as Boxed

import Test.Tasty
import Test.Tasty.QuickCheck


tests :: [TestTree]
tests =
  [ testGroup "specializations"
    [ testGroup "enumFromTo"
      [ specEnumFromTo (Proxy @Word8)
      , specEnumFromTo (Proxy @Word16)
      , specEnumFromTo (Proxy @Word32)
      , specEnumFromTo (Proxy @Word64)
      , specEnumFromTo (Proxy @Word)
      , specEnumFromTo (Proxy @Int8)
      , specEnumFromTo (Proxy @Int16)
      , specEnumFromTo (Proxy @Int32)
      , specEnumFromTo (Proxy @Int64)
      , specEnumFromTo (Proxy @Int)
      , specEnumFromTo (Proxy @Integer)
      , specEnumFromToFloat (Proxy @Float)
      , specEnumFromToFloat (Proxy @Double)
      , specEnumFromToChar
      ]
    ]
  ]


-- For integral data type we need to make sure we never generate slice
-- too big.
specEnumFromTo
  :: forall a. (Enum a, Eq a, Show a, Arbitrary a, Typeable a, Integral a)
  => Proxy a -> TestTree
{-# INLINE specEnumFromTo #-}
specEnumFromTo _ = testProperty (show (typeOf (undefined :: a)))
  $ \(a::a) (d::Int16) ->
      let b = fromInteger $ fromIntegral a + fromIntegral d
          diff = abs $ fromIntegral a - fromIntegral b :: Integer
      in diff < 65000 ==> 
         ( counterexample ("[" ++ show a ++ " .. " ++ show b ++ "]")
         $ [a .. b] == Boxed.toList (Boxed.enumFromTo a b)
         )

specEnumFromToFloat
  :: forall a. (Enum a, Eq a, Show a, Arbitrary a, Typeable a, Fractional a, Real a)
  => Proxy a -> TestTree
{-# INLINE specEnumFromToFloat #-}
specEnumFromToFloat _ = testProperty (show (typeOf (undefined :: a)))
  $ \(a::a) (d::Int16) ->
      let b    = a + realToFrac d / 3
          diff = abs $ a - b
      in diff < 65000 ==>
         ( counterexample ("[" ++ show a ++ " .. " ++ show b ++ "]")
         $ [a .. b] == Boxed.toList (Boxed.enumFromTo a b)
         )

specEnumFromToChar :: TestTree
specEnumFromToChar = testProperty "Char"
  $ \(i1::Word8) (i2::Word8) ->
      let c1 = chr $ 256 + fromIntegral i1
          c2 = chr $ 256 + fromIntegral i2
      in ( counterexample (show (c1,c2))
         $ [c1 .. c2] == Boxed.toList (Boxed.enumFromTo c1 c2)
         )
