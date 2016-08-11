module Main (main) where

import qualified Tests.Vector
import qualified Tests.Vector.UnitTests
import qualified Tests.Bundle
import qualified Tests.Move

import Test.Framework (defaultMain)

main :: IO ()
main = defaultMain $ Tests.Bundle.tests
                  ++ Tests.Vector.tests
                  ++ Tests.Vector.UnitTests.tests
                  ++ Tests.Move.tests

