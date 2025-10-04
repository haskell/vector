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
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Fusion.Bundle.Monadic as B
import           Data.Vector.Fusion.Bundle.Size (Size(..))
import qualified Data.Stream.Monadic as S

import Test.Tasty
import Test.Tasty.QuickCheck


tests :: [TestTree]
tests =
  [ testGroup "specializations"
    [ testGroup "enumFromTo"
      [ specEnumFromTo (Boxed.enumFromTo @Word8)
      , specEnumFromTo (Boxed.enumFromTo @Word16)
      , specEnumFromTo (Boxed.enumFromTo @Word32)
      , specEnumFromTo (Boxed.enumFromTo @Word64)
      , specEnumFromTo (Boxed.enumFromTo @Word)
      , specEnumFromTo (Boxed.enumFromTo @Int8)
      , specEnumFromTo (Boxed.enumFromTo @Int16)
      , specEnumFromTo (Boxed.enumFromTo @Int32)
      , specEnumFromTo (Boxed.enumFromTo @Int64)
      , specEnumFromTo (Boxed.enumFromTo @Int)
      , specEnumFromTo (Boxed.enumFromTo @Integer)
      , specEnumFromToFloat (Boxed.enumFromTo @Float)
      , specEnumFromToFloat (Boxed.enumFromTo @Double)
      , specEnumFromToChar Boxed.enumFromTo
      ]
    , testGroup "enumFromTo streaming"
      [ specEnumFromTo (enumFromToStream @Word8)
      , specEnumFromTo (enumFromToStream @Word16)
      , specEnumFromTo (enumFromToStream @Word32)
      , specEnumFromTo (enumFromToStream @Word64)
      , specEnumFromTo (enumFromToStream @Word)
      , specEnumFromTo (enumFromToStream @Int8)
      , specEnumFromTo (enumFromToStream @Int16)
      , specEnumFromTo (enumFromToStream @Int32)
      , specEnumFromTo (enumFromToStream @Int64)
      , specEnumFromTo (enumFromToStream @Int)
      , specEnumFromTo (enumFromToStream @Integer)
      , specEnumFromToFloat (enumFromToStream @Float)
      , specEnumFromToFloat (enumFromToStream @Double)
      , specEnumFromToChar enumFromToStream
      ]
    ]
  ]

-- We need to define this function to test rewrite rules in vector-stream.
-- Rewrite rules in Bundle do not reuse rules in vector-stream
enumFromToStream :: (Enum a) => a -> a -> Boxed.Vector a
{-# INLINE enumFromToStream #-}
enumFromToStream x y = VG.unstream $ B.fromStream (S.enumFromTo x y) Unknown

-- For integral data type we need to make sure we never generate slice
-- too big.
specEnumFromTo
  :: forall a. (Enum a, Eq a, Show a, Arbitrary a, Typeable a, Integral a)
  => (a -> a -> Boxed.Vector a) -> TestTree
{-# INLINE specEnumFromTo #-}
specEnumFromTo enumFromTo' = testProperty (show (typeOf (undefined :: a)))
  $ \(a::a) (d::Int16) ->
      let b = fromInteger $ fromIntegral a + fromIntegral d
          diff = abs $ fromIntegral a - fromIntegral b :: Integer
      in diff < 65000 ==> 
         ( counterexample ("[" ++ show a ++ " .. " ++ show b ++ "]")
         $ [a .. b] == Boxed.toList (enumFromTo' a b)
         )

specEnumFromToFloat
  :: forall a. (Enum a, Eq a, Show a, Arbitrary a, Typeable a, Fractional a, Real a)
  => (a -> a -> Boxed.Vector a) -> TestTree
{-# INLINE specEnumFromToFloat #-}
specEnumFromToFloat enumFromTo' = testProperty (show (typeOf (undefined :: a)))
  $ \(a::a) (d::Int16) ->
      let b    = a + realToFrac d / 3
          diff = abs $ a - b
      in diff < 65000 ==>
         ( counterexample ("[" ++ show a ++ " .. " ++ show b ++ "]")
         $ [a .. b] == Boxed.toList (enumFromTo' a b)
         )

specEnumFromToChar :: (Char -> Char -> Boxed.Vector Char) -> TestTree
specEnumFromToChar enumFromTo' = testProperty "Char"
  $ \(i1::Word8) (i2::Word8) ->
      let c1 = chr $ 256 + fromIntegral i1
          c2 = chr $ 256 + fromIntegral i2
      in ( counterexample (show (c1,c2))
         $ [c1 .. c2] == Boxed.toList (enumFromTo' c1 c2)
         )
