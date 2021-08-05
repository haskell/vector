{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -dno-suppress-type-signatures #-}
{-# OPTIONS_GHC -dsuppress-all #-}
{-# OPTIONS_GHC -fplugin=Test.Tasty.Inspection.Plugin #-}
-- | Most basic inspection tests
module Inspect.DerivingVia where

import Test.Tasty
import Test.Tasty.Inspection
import qualified Data.Vector.Generic         as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed         as VU
import GHC.Generics (Generic)

import Inspect.DerivingVia.OtherFoo


-- | Simple product data type for which we derive Unbox instances
-- using generics and iso-deriving. This one is used in same module
-- where it's defined. It's used to check that there's no difference
-- between data type defined in same and different module (see
-- 'OtherFoo').
data Foo a = Foo Int a
  deriving (Show,Generic)

instance VU.IsoUnbox (Foo a) (Int,a) where

newtype instance VU.MVector s (Foo a) = MV_Int (VU.MVector s (Int, a))
newtype instance VU.Vector    (Foo a) = V_Int  (VU.Vector    (Int, a))

instance VU.Unbox a => VU.Unbox (Foo a)
deriving via (Foo a `VU.As` (Int, a)) instance VU.Unbox a => VGM.MVector VU.MVector (Foo a)
deriving via (Foo a `VU.As` (Int, a)) instance VU.Unbox a => VG.Vector   VU.Vector  (Foo a)

map_Foo :: VU.Vector (Foo Double) -> VU.Vector (Foo Double)
map_Foo = VU.map (\(Foo a b) -> Foo (a*10) (b*100))

pipeline_Foo :: Int -> Double
pipeline_Foo n
  = VU.foldl' (\acc (Foo a b) -> acc + b^^a) 0
  $ VU.filter (\(Foo a _) -> a < 4)
  $ VU.map (\(Foo a b) -> Foo (a + 2) (log b))
  $ VU.generate n (\i -> Foo i (log (fromIntegral i)))

map_OtherFoo :: VU.Vector (OtherFoo Double) -> VU.Vector (OtherFoo Double)
map_OtherFoo = VU.map (\(OtherFoo a b) -> OtherFoo (a*10) (b*100))

pipeline_OtherFoo :: Int -> Double
pipeline_OtherFoo n
  = VU.foldl' (\acc (OtherFoo a b) -> acc + b^^a) 0
  $ VU.filter (\(OtherFoo a _) -> a < 4)
  $ VU.map (\(OtherFoo a b) -> OtherFoo (a + 2) (log b))
  $ VU.generate n (\i -> OtherFoo i (log (fromIntegral i)))


-- | Here we test that optimizer successfully eliminated all generics
-- and even mentions of Foo data type.
tests :: TestTree
tests = testGroup "iso-deriving"
  [ $(inspectObligations [(`doesNotUse` 'Foo), hasNoGenerics, hasNoTypeClasses]
       'map_Foo)
  , $(inspectObligations [(`doesNotUse` 'OtherFoo), hasNoGenerics, hasNoTypeClasses]
       'pipeline_Foo)
  , $(inspectObligations [(`doesNotUse` 'OtherFoo), hasNoGenerics, hasNoTypeClasses]
       'map_OtherFoo)
  , $(inspectObligations [(`doesNotUse` 'OtherFoo), hasNoGenerics, hasNoTypeClasses]
       'pipeline_OtherFoo)
  ]
