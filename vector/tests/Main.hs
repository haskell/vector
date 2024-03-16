module Main (main) where

import qualified Tests.Vector.UnitTests
import qualified Tests.Vector.Boxed
import qualified Tests.Vector.Primitive
import qualified Tests.Vector.Storable
import qualified Tests.Vector.Strict
import qualified Tests.Vector.Unboxed
import qualified Tests.Bundle
import qualified Tests.Move

import Test.Tasty (defaultMain,testGroup)

main :: IO ()
main = defaultMain $ testGroup "toplevel" $ concat
  [ Tests.Bundle.tests
  , [ testGroup "Tests.Vector.Boxed" Tests.Vector.Boxed.tests
    , testGroup "Tests.Vector.Primitive" Tests.Vector.Primitive.tests
    , testGroup "Tests.Vector.Storable" Tests.Vector.Storable.tests
    , testGroup "Tests.Vector.Strict" Tests.Vector.Strict.tests
    , testGroup "Tests.Vector.Unboxed" Tests.Vector.Unboxed.tests
    ]
  , Tests.Vector.UnitTests.tests
  , Tests.Move.tests
  ]
