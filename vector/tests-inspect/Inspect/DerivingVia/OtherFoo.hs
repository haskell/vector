{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Inspect.DerivingVia.OtherFoo where

import qualified Data.Vector.Generic         as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed         as VU
import GHC.Generics (Generic)


-- | Simple product data type for which we derive Unbox instances
-- using generics and iso-deriving. It's defined in separate module in
-- order to test that it doesn't impede optimizer
data OtherFoo a = OtherFoo Int a
  deriving (Show,Generic)

instance VU.IsoUnbox (OtherFoo a) (Int,a) where

newtype instance VU.MVector s (OtherFoo a) = MV_Int (VU.MVector s (Int, a))
newtype instance VU.Vector    (OtherFoo a) = V_Int  (VU.Vector    (Int, a))

instance VU.Unbox a => VU.Unbox (OtherFoo a)
deriving via (OtherFoo a `VU.As` (Int, a)) instance VU.Unbox a => VGM.MVector VU.MVector (OtherFoo a)
deriving via (OtherFoo a `VU.As` (Int, a)) instance VU.Unbox a => VG.Vector   VU.Vector  (OtherFoo a)
