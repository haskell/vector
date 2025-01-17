{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UnboxedTuples              #-}
#if MIN_VERSION_base(4,12,0)
{-# LANGUAGE DerivingVia                #-}
#endif
-- |
-- These tests make sure that derived Unbox instances actually works.
-- It's distressingly easy to forget to export some constructor and
-- make seemingly fine code noncompilable.
--
-- We're only interested in checking whether examples compiling.
-- Doctests aren't reliable in ensuring that!
module Tests.Deriving () where

import Control.DeepSeq
import qualified Data.Vector.Generic         as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector                 as V
import qualified Data.Vector.Strict          as VV
import qualified Data.Vector.Storable        as VS
import qualified Data.Vector.Primitive       as VP
import qualified Data.Vector.Unboxed         as VU

#if MIN_VERSION_base(4,12,0)
----------------------------------------------------------------
-- Primitive 

newtype FooP1 = FooP1 Int deriving VP.Prim

newtype instance VU.MVector s FooP1 = MV_FooP1 (VP.MVector s FooP1)
newtype instance VU.Vector    FooP1 = V_FooP1  (VP.Vector    FooP1)
deriving via (VU.UnboxViaPrim FooP1) instance VGM.MVector VU.MVector FooP1
deriving via (VU.UnboxViaPrim FooP1) instance VG.Vector   VU.Vector  FooP1
instance VU.Unbox FooP1



newtype FooP2 = FooP2 Int

newtype instance VU.MVector s FooP2 = MV_FooP2 (VP.MVector s Int)
newtype instance VU.Vector    FooP2 = V_FooP2  (VP.Vector    Int)
deriving via (VU.UnboxViaPrim Int) instance VGM.MVector VU.MVector FooP2
deriving via (VU.UnboxViaPrim Int) instance VG.Vector   VU.Vector  FooP2
instance VU.Unbox FooP2


----------------------------------------------------------------
-- Storable

newtype FooS1 = FooS1 Int deriving VS.Storable

newtype instance VU.MVector s FooS1 = MV_FooS1 (VS.MVector s FooS1)
newtype instance VU.Vector    FooS1 = V_FooS1  (VS.Vector    FooS1)
deriving via (VU.UnboxViaStorable FooS1) instance VGM.MVector VU.MVector FooS1
deriving via (VU.UnboxViaStorable FooS1) instance VG.Vector   VU.Vector  FooS1
instance VU.Unbox FooS1


newtype FooS2 = FooS2 Int

newtype instance VU.MVector s FooS2 = MV_FooS2 (VS.MVector s Int)
newtype instance VU.Vector    FooS2 = V_FooS2  (VS.Vector    Int)
deriving via (VU.UnboxViaStorable Int) instance VGM.MVector VU.MVector FooS2
deriving via (VU.UnboxViaStorable Int) instance VG.Vector   VU.Vector  FooS2
instance VU.Unbox FooS2


----------------------------------------------------------------
-- Boxed variants


data FooLazy a = FooLazy Int a
  deriving (Eq, Ord, Show)

instance VU.IsoUnbox (FooLazy a) (Int, VU.DoNotUnboxLazy a) where
  toURepr (FooLazy i a) = (i, VU.DoNotUnboxLazy a)
  fromURepr (i, VU.DoNotUnboxLazy a) = FooLazy i a
  {-# INLINE toURepr #-}
  {-# INLINE fromURepr #-}

newtype instance VU.MVector s (FooLazy a) = MV_FooLazy (VU.MVector s (Int, VU.DoNotUnboxLazy a))
newtype instance VU.Vector    (FooLazy a) = V_FooLazy  (VU.Vector    (Int, VU.DoNotUnboxLazy a))
deriving via (FooLazy a `VU.As` (Int, VU.DoNotUnboxLazy a)) instance VGM.MVector VU.MVector (FooLazy a)
deriving via (FooLazy a `VU.As` (Int, VU.DoNotUnboxLazy a)) instance VG.Vector   VU.Vector   (FooLazy a)
instance VU.Unbox (FooLazy a)



data FooStrict a = FooStrict Int a
  deriving (Eq, Ord, Show)

instance VU.IsoUnbox (FooStrict a) (Int, VU.DoNotUnboxStrict a) where
  toURepr (FooStrict i a) = (i, VU.DoNotUnboxStrict a)
  fromURepr (i, VU.DoNotUnboxStrict a) = FooStrict i a
  {-# INLINE toURepr #-}
  {-# INLINE fromURepr #-}

newtype instance VU.MVector s (FooStrict a) = MV_FooStrict (VU.MVector s (Int, VU.DoNotUnboxStrict a))
newtype instance VU.Vector    (FooStrict a) = V_FooStrict  (VU.Vector    (Int, VU.DoNotUnboxStrict a))
deriving via (FooStrict a `VU.As` (Int, VU.DoNotUnboxStrict a)) instance VGM.MVector VU.MVector (FooStrict a)
deriving via (FooStrict a `VU.As` (Int, VU.DoNotUnboxStrict a)) instance VG.Vector   VU.Vector   (FooStrict a)
instance VU.Unbox (FooStrict a)


data FooNormalForm a = FooNormalForm Int a
  deriving (Eq, Ord, Show)

instance VU.IsoUnbox (FooNormalForm a) (Int, VU.DoNotUnboxNormalForm a) where
  toURepr (FooNormalForm i a) = (i, VU.DoNotUnboxNormalForm a)
  fromURepr (i, VU.DoNotUnboxNormalForm a) = FooNormalForm i a
  {-# INLINE toURepr #-}
  {-# INLINE fromURepr #-}

newtype instance VU.MVector s (FooNormalForm a) = MV_FooNormalForm (VU.MVector s (Int, VU.DoNotUnboxNormalForm a))
newtype instance VU.Vector    (FooNormalForm a) = V_FooNormalForm  (VU.Vector    (Int, VU.DoNotUnboxNormalForm a))
deriving via (FooNormalForm a `VU.As` (Int, VU.DoNotUnboxNormalForm a))
    instance NFData a => VGM.MVector VU.MVector (FooNormalForm a)
deriving via (FooNormalForm a `VU.As` (Int, VU.DoNotUnboxNormalForm a))
    instance NFData a => VG.Vector VU.Vector (FooNormalForm a)
instance NFData a => VU.Unbox (FooNormalForm a)



data BoxedLazy = BoxedLazy Int
  deriving (Eq, Ord, Show)

newtype instance VU.MVector s BoxedLazy = MV_BoxedLazy (V.MVector s BoxedLazy)
newtype instance VU.Vector    BoxedLazy = V_BoxedLazy  (V.Vector    BoxedLazy)
deriving via (VU.DoNotUnboxLazy BoxedLazy) instance VGM.MVector VU.MVector BoxedLazy
deriving via (VU.DoNotUnboxLazy BoxedLazy) instance VG.Vector   VU.Vector  BoxedLazy
instance VU.Unbox BoxedLazy


data BoxedStrict = BoxedStrict Int
  deriving (Eq, Ord, Show)

newtype instance VU.MVector s BoxedStrict = MV_BoxedStrict (VV.MVector s BoxedStrict)
newtype instance VU.Vector    BoxedStrict = V_BoxedStrict  (VV.Vector    BoxedStrict)
deriving via (VU.DoNotUnboxStrict BoxedStrict) instance VGM.MVector VU.MVector BoxedStrict
deriving via (VU.DoNotUnboxStrict BoxedStrict) instance VG.Vector   VU.Vector  BoxedStrict
instance VU.Unbox BoxedStrict


data BoxedNormalForm = BoxedNormalForm Int
  deriving (Eq, Ord, Show)

instance NFData BoxedNormalForm where
  rnf (BoxedNormalForm i) = rnf i

newtype instance VU.MVector s BoxedNormalForm = MV_BoxedNormalForm (VV.MVector s BoxedNormalForm)
newtype instance VU.Vector    BoxedNormalForm = V_BoxedNormalForm  (VV.Vector    BoxedNormalForm)
deriving via (VU.DoNotUnboxNormalForm BoxedNormalForm) instance VGM.MVector VU.MVector BoxedNormalForm
deriving via (VU.DoNotUnboxNormalForm BoxedNormalForm) instance VG.Vector   VU.Vector  BoxedNormalForm
instance VU.Unbox BoxedNormalForm


----------------------------------------------------------------
-- Unboxed


data FooAs a = FooAs Int a
  deriving Show

instance VU.IsoUnbox (FooAs a) (Int,a) where
  toURepr (FooAs i a) = (i,a)
  fromURepr (i,a) = FooAs i a
  {-# INLINE toURepr #-}
  {-# INLINE fromURepr #-}

newtype instance VU.MVector s (FooAs a) = MV_FooAs (VU.MVector s (Int, a))
newtype instance VU.Vector    (FooAs a) = V_FooAs  (VU.Vector    (Int, a))
deriving via (FooAs a `VU.As` (Int, a)) instance VU.Unbox a => VGM.MVector VU.MVector (FooAs a)
deriving via (FooAs a `VU.As` (Int, a)) instance VU.Unbox a => VG.Vector   VU.Vector   (FooAs a)
instance VU.Unbox a => VU.Unbox (FooAs a)

#endif
