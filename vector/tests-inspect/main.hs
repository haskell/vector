{-# LANGUAGE CPP #-}
module Main (main) where

import qualified Inspect
#if MIN_VERSION_base(4,12,0)
import qualified Inspect.DerivingVia
#endif
import Test.Tasty (defaultMain,testGroup)

main :: IO ()
main = defaultMain $ testGroup "tests"
  [ Inspect.tests
#if MIN_VERSION_base(4,12,0)
  , Inspect.DerivingVia.tests
#endif
  ]
