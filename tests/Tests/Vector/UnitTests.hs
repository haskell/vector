{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tests.Vector.UnitTests (tests) where

import Control.Applicative as Applicative
import Control.Exception
import Control.Monad.Primitive
import qualified Data.List as List
import qualified Data.Vector.Generic  as Generic
import qualified Data.Vector as Boxed
import qualified Data.Vector.Primitive as Primitive
import qualified Data.Vector.Storable as Storable
import qualified Data.Vector.Unboxed as Unboxed
import Foreign.Ptr
import Foreign.Storable
import Text.Printf

import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertBool, assertFailure)

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
  , testGroup "Negative tests"
    [ testGroup "slice out of bounds #257"
      [ testGroup "Boxed" $ testsSliceOutOfBounds Boxed.slice
      , testGroup "Primitive" $ testsSliceOutOfBounds Primitive.slice
      , testGroup "Storable" $ testsSliceOutOfBounds Storable.slice
      , testGroup "Unboxed" $ testsSliceOutOfBounds Unboxed.slice
      ]
    ]
  ]

testsSliceOutOfBounds ::
     (Show (v Int), Generic.Vector v Int) => (Int -> Int -> v Int -> v Int) -> [Test]
testsSliceOutOfBounds sliceWith =
  [ testCase "Negative ix" $ sliceTest sliceWith (-2) 2 xs
  , testCase "Negative size" $ sliceTest sliceWith 2 (-2) xs
  , testCase "Negative ix and size" $ sliceTest sliceWith (-2) (-1) xs
  , testCase "Too large ix" $ sliceTest sliceWith 6 2 xs
  , testCase "Too large size" $ sliceTest sliceWith 2 6 xs
  , testCase "Too large ix and size" $ sliceTest sliceWith 6 6 xs
  , testCase "Overflow" $ sliceTest sliceWith 1 maxBound xs
  , testCase "OutOfMemory" $ sliceTest sliceWith 1 (maxBound `div` intSize) xs
  ]
  where
    intSize = sizeOf (undefined :: Int)
    xs = [1, 2, 3, 4, 5] :: [Int]
{-# INLINE testsSliceOutOfBounds #-}

sliceTest ::
     (Show (v Int), Generic.Vector v Int)
  => (Int -> Int -> v Int -> v Int)
  -> Int
  -> Int
  -> [Int]
  -> Assertion
sliceTest sliceWith i m xs = do
  let vec = Generic.fromList xs
  eRes <- try (pure $! sliceWith i m vec)
  case eRes of
    Right v ->
      assertFailure $
      "Data.Vector.Internal.Check.checkSlice failed to check: " ++ show v
    Left (ErrorCall err) ->
      let assertMsg =
            List.concat
              [ "Expected slice function to produce an 'error' ending with: \""
              , errSuffix
              , "\" instead got: \""
              , err
              ]
       in assertBool assertMsg (errSuffix `List.isSuffixOf` err)
  where
    errSuffix =
      "(slice): invalid slice (" ++
      show i ++ "," ++ show m ++ "," ++ show (List.length xs) ++ ")"
{-# INLINE sliceTest #-}



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
