{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- |
module Test.Ignore
  ( modifyTests
  , ignoreTest
  ) where

import Data.Coerce
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.Providers


data WithTest where
  WithTest :: IsTest t => t -> WithTest

modifyTests
  :: (forall a. (IsTest a) => a -> WithTest)
  -> TestTree -> TestTree
modifyTests fun = go where
  go = \case
    SingleTest      nm t      -> case fun t of
                                   WithTest t' -> SingleTest nm t'
    TestGroup       nm ts     -> TestGroup nm (go <$> ts)
    PlusTestOptions plus tree -> PlusTestOptions plus (go tree)
    WithResource    spec f    -> WithResource    spec (go . f)
    AskOptions      f         -> AskOptions (go . f)
    After           d p t     -> After d p (go t)


ignoreTest :: TestTree -> TestTree
ignoreTest = modifyTests (\t -> WithTest $ Ignored t)

newtype Ignored t = Ignored t

instance IsTest t => IsTest (Ignored t) where
  run ops (Ignored t) f = do
    _ <- run ops t f
    pure $ (testPassed ""){ resultShortDescription = "IGNORED" }
  testOptions = coerce (testOptions @t)
