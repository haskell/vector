{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Vector.Unboxed.Base
-- Copyright   : (c) Roman Leshchinskiy 2009-2010
--                   Alexey Kuleshevich 2020-2022
--                   Aleksey Khudyakov 2020-2022
--                   Andrew Lelechenko 2020-2022
-- License     : BSD-style
--
-- Maintainer  : Haskell Libraries Team <libraries@haskell.org>
-- Stability   : experimental
-- Portability : non-portable
--
-- Adaptive unboxed vectors: basic implementation.

module Data.Vector.Unboxed.Base (
  MVector(..), IOVector, STVector, Vector(..), Unbox,
  UnboxViaPrim(..), As(..), IsoUnbox(..),
  DoNotUnboxLazy(..), DoNotUnboxNormalForm(..), DoNotUnboxStrict(..)
) where

import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector                 as B
import qualified Data.Vector.Strict          as S

import qualified Data.Vector.Primitive as P

import Control.Applicative (Const(..))

import Control.DeepSeq ( NFData(rnf)
#if MIN_VERSION_deepseq(1,4,3)
                       , NFData1(liftRnf)
#endif
                       , force
                       )

import Control.Monad.Primitive
import Control.Monad ( liftM )

import Data.Functor.Identity
import Data.Functor.Compose
import Data.Word ( Word8, Word16, Word32, Word64 )
import Data.Int  ( Int8, Int16, Int32, Int64 )
import Data.Complex
import Data.Monoid (Dual(..),Sum(..),Product(..),All(..),Any(..))
import Data.Monoid (Alt(..))
import Data.Semigroup (Min(..),Max(..),First(..),Last(..),WrappedMonoid(..),Arg(..))
import Data.Typeable ( Typeable )
import Data.Data     ( Data(..) )
import GHC.Exts      ( Down(..) )
import GHC.Generics
import Data.Coerce
import Data.Kind     (Type)

-- Data.Vector.Internal.Check is unused
#define NOT_VECTOR_MODULE
#include "vector.h"

data family MVector s a
data family Vector    a

type IOVector = MVector RealWorld
type STVector s = MVector s

type instance G.Mutable Vector = MVector

class (G.Vector Vector a, M.MVector MVector a) => Unbox a

instance NFData (Vector a) where rnf !_ = ()
instance NFData (MVector s a) where rnf !_ = ()

#if MIN_VERSION_deepseq(1,4,3)
-- | @since 0.12.1.0
instance NFData1 Vector where
  liftRnf _ !_ = ()
-- | @since 0.12.1.0
instance NFData1 (MVector s) where
  liftRnf _ !_ = ()
#endif

-- -----------------
-- Data and Typeable
-- -----------------
deriving instance Typeable Vector
deriving instance Typeable MVector

instance (Data a, Unbox a) => Data (Vector a) where
  gfoldl       = G.gfoldl
  toConstr _   = G.mkVecConstr "Data.Vector.Unboxed.Vector"
  gunfold      = G.gunfold
  dataTypeOf _ = G.mkVecType "Data.Vector.Unboxed.Vector"
  dataCast1    = G.dataCast

-- ----
-- Unit
-- ----

newtype instance MVector s () = MV_Unit Int
newtype instance Vector    () = V_Unit Int

instance Unbox ()

instance M.MVector MVector () where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}

  basicLength (MV_Unit n) = n

  basicUnsafeSlice _ m (MV_Unit _) = MV_Unit m

  basicOverlaps _ _ = False

  basicUnsafeNew n = return (MV_Unit n)

  -- Nothing to initialize
  basicInitialize _ = return ()

  basicUnsafeRead (MV_Unit _) _ = return ()

  basicUnsafeWrite (MV_Unit _) _ () = return ()

  basicClear _ = return ()

  basicSet (MV_Unit _) () = return ()

  basicUnsafeCopy (MV_Unit _) (MV_Unit _) = return ()

  basicUnsafeGrow (MV_Unit n) m = return $ MV_Unit (n+m)

instance G.Vector Vector () where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze (MV_Unit n) = return $ V_Unit n

  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw (V_Unit n) = return $ MV_Unit n

  {-# INLINE basicLength #-}
  basicLength (V_Unit n) = n

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice _ m (V_Unit _) = V_Unit m

  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (V_Unit _) _ = return ()

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MV_Unit _) (V_Unit _) = return ()

  {-# INLINE elemseq #-}
  elemseq _ = seq


-- ---------------
-- Primitive types
-- ---------------

-- | Newtype wrapper which allows to derive unboxed vector in term of
-- primitive vectors using @DerivingVia@ mechanism. This is mostly
-- used as illustration of use of @DerivingVia@ for vector, see examples below.
--
-- First is rather straightforward: we define newtype and use GND to
-- derive 'P.Prim' instance. Newtype instances should be defined
-- manually. Then we use deriving via to define necessary instances.
--
-- >>> :set -XTypeFamilies -XStandaloneDeriving -XDerivingVia -XMultiParamTypeClasses
-- >>> -- Needed to derive Prim
-- >>> :set -XGeneralizedNewtypeDeriving -XDataKinds -XUnboxedTuples -XPolyKinds
-- >>>
-- >>> import qualified Data.Vector.Generic         as VG
-- >>> import qualified Data.Vector.Generic.Mutable as VGM
-- >>> import qualified Data.Vector.Primitive       as VP
-- >>> import qualified Data.Vector.Unboxed         as VU
-- >>>
-- >>> newtype Foo = Foo Int deriving VP.Prim
-- >>>
-- >>> newtype instance VU.MVector s Foo = MV_Int (VP.MVector s Foo)
-- >>> newtype instance VU.Vector    Foo = V_Int  (VP.Vector    Foo)
-- >>> deriving via (VU.UnboxViaPrim Foo) instance VGM.MVector VU.MVector Foo
-- >>> deriving via (VU.UnboxViaPrim Foo) instance VG.Vector   VU.Vector  Foo
-- >>> instance VU.Unbox Foo
--
-- Second example is essentially same but with a twist. Instead of
-- using @Prim@ instance of data type, we use underlying instance of @Int@:
--
-- >>> :set -XTypeFamilies -XStandaloneDeriving -XDerivingVia -XMultiParamTypeClasses
-- >>>
-- >>> import qualified Data.Vector.Generic         as VG
-- >>> import qualified Data.Vector.Generic.Mutable as VGM
-- >>> import qualified Data.Vector.Primitive       as VP
-- >>> import qualified Data.Vector.Unboxed         as VU
-- >>>
-- >>> newtype Foo = Foo Int
-- >>>
-- >>> newtype instance VU.MVector s Foo = MV_Int (VP.MVector s Int)
-- >>> newtype instance VU.Vector    Foo = V_Int  (VP.Vector    Int)
-- >>> deriving via (VU.UnboxViaPrim Int) instance VGM.MVector VU.MVector Foo
-- >>> deriving via (VU.UnboxViaPrim Int) instance VG.Vector   VU.Vector  Foo
-- >>> instance VU.Unbox Foo
--
-- @since 0.13.0.0
newtype UnboxViaPrim a = UnboxViaPrim a

newtype instance MVector s (UnboxViaPrim a) = MV_UnboxViaPrim (P.MVector s a)
newtype instance Vector    (UnboxViaPrim a) = V_UnboxViaPrim (P.Vector a)

instance P.Prim a => M.MVector MVector (UnboxViaPrim a) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength          = coerce $ M.basicLength          @P.MVector @a
  basicUnsafeSlice     = coerce $ M.basicUnsafeSlice     @P.MVector @a
  basicOverlaps        = coerce $ M.basicOverlaps        @P.MVector @a
  basicUnsafeNew       = coerce $ M.basicUnsafeNew       @P.MVector @a
  basicInitialize      = coerce $ M.basicInitialize      @P.MVector @a
  basicUnsafeReplicate = coerce $ M.basicUnsafeReplicate @P.MVector @a
  basicUnsafeRead      = coerce $ M.basicUnsafeRead      @P.MVector @a
  basicUnsafeWrite     = coerce $ M.basicUnsafeWrite     @P.MVector @a
  basicClear           = coerce $ M.basicClear           @P.MVector @a
  basicSet             = coerce $ M.basicSet             @P.MVector @a
  basicUnsafeCopy      = coerce $ M.basicUnsafeCopy      @P.MVector @a
  basicUnsafeMove      = coerce $ M.basicUnsafeMove      @P.MVector @a
  basicUnsafeGrow      = coerce $ M.basicUnsafeGrow      @P.MVector @a

instance P.Prim a => G.Vector Vector (UnboxViaPrim a) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze = coerce $ G.basicUnsafeFreeze @P.Vector @a
  basicUnsafeThaw   = coerce $ G.basicUnsafeThaw   @P.Vector @a
  basicLength       = coerce $ G.basicLength       @P.Vector @a
  basicUnsafeSlice  = coerce $ G.basicUnsafeSlice  @P.Vector @a
  basicUnsafeIndexM = coerce $ G.basicUnsafeIndexM @P.Vector @a
  basicUnsafeCopy   = coerce $ G.basicUnsafeCopy   @P.Vector @a
  elemseq _ = seq

-- | Isomorphism between type @a@ and its representation in unboxed
-- vector @b@. Default instance coerces between generic
-- representations of @a@ and @b@ which means they have same shape and
-- corresponding fields could be coerced to each other. Note that this
-- means it's possible to have fields that have different types:
--
-- >>> :set -XMultiParamTypeClasses -XDeriveGeneric -XFlexibleInstances
-- >>> import GHC.Generics (Generic)
-- >>> import Data.Monoid
-- >>> import qualified Data.Vector.Unboxed as VU
-- >>> :{
-- data Foo a = Foo Int a
--   deriving (Show,Generic)
-- instance VU.IsoUnbox (Foo a) (Int, a)
-- instance VU.IsoUnbox (Foo a) (Sum Int, Product a)
-- :}
--
-- @since 0.13.0.0
class IsoUnbox a b where
  -- | Convert value into it representation in unboxed vector.
  toURepr   :: a -> b
  default toURepr :: (Generic a, Generic b, Coercible (Rep a ()) (Rep b ())) => a -> b
  toURepr = to . idU . coerce . idU . from
  -- | Convert value representation in unboxed vector back to value.
  fromURepr :: b -> a
  default fromURepr :: (Generic a, Generic b, Coercible (Rep b ()) (Rep a ())) => b -> a
  fromURepr = to . idU . coerce . idU . from

idU :: f () -> f ()
idU = id


-- | Newtype which allows to derive unbox instances for type @a@ which
-- uses @b@ as underlying representation (usually tuple). Type @a@ and
-- its representation @b@ are connected by type class
-- 'IsoUnbox'. Here's example which uses explicit 'IsoUnbox' instance:
--
--
-- >>> :set -XTypeFamilies -XStandaloneDeriving -XDerivingVia
-- >>> :set -XMultiParamTypeClasses -XTypeOperators -XFlexibleInstances
-- >>> import qualified Data.Vector.Unboxed         as VU
-- >>> import qualified Data.Vector.Unboxed.Mutable as MVU
-- >>> import qualified Data.Vector.Generic         as VG
-- >>> import qualified Data.Vector.Generic.Mutable as VGM
-- >>> :{
-- data Foo a = Foo Int a
--   deriving Show
-- instance VU.IsoUnbox (Foo a) (Int,a) where
--   toURepr (Foo i a) = (i,a)
--   fromURepr (i,a) = Foo i a
--   {-# INLINE toURepr #-}
--   {-# INLINE fromURepr #-}
-- newtype instance VU.MVector s (Foo a) = MV_Foo (VU.MVector s (Int, a))
-- newtype instance VU.Vector    (Foo a) = V_Foo  (VU.Vector    (Int, a))
-- deriving via (Foo a `VU.As` (Int, a)) instance VU.Unbox a => VGM.MVector MVU.MVector (Foo a)
-- deriving via (Foo a `VU.As` (Int, a)) instance VU.Unbox a => VG.Vector   VU.Vector   (Foo a)
-- instance VU.Unbox a => VU.Unbox (Foo a)
-- :}
--
--
-- It's also possible to use generic-based instance for 'IsoUnbox'
-- which should work for all product types.
--
-- >>> :set -XMultiParamTypeClasses -XTypeOperators -XFlexibleInstances -XDeriveGeneric
-- >>> :set -XDerivingVia
-- >>> import qualified Data.Vector.Unboxed         as VU
-- >>> import qualified Data.Vector.Generic         as VG
-- >>> import qualified Data.Vector.Generic.Mutable as VGM
-- >>> :{
-- data Bar a = Bar Int a
--   deriving (Show,Generic)
-- instance VU.IsoUnbox (Bar a) (Int,a) where
-- newtype instance VU.MVector s (Bar a) = MV_Bar (VU.MVector s (Int, a))
-- newtype instance VU.Vector    (Bar a) = V_Bar  (VU.Vector    (Int, a))
-- deriving via (Bar a `VU.As` (Int, a)) instance VU.Unbox a => VGM.MVector VU.MVector (Bar a)
-- deriving via (Bar a `VU.As` (Int, a)) instance VU.Unbox a => VG.Vector  VU.Vector  (Bar a)
-- instance VU.Unbox a => VU.Unbox (Bar a)
-- :}
--
-- @since 0.13.0.0
newtype As (a :: Type) (b :: Type) = As a

newtype instance MVector s (As a b) = MV_UnboxAs (MVector s b)
newtype instance Vector    (As a b) = V_UnboxAs  (Vector b)

instance (IsoUnbox a b, Unbox b) => M.MVector MVector (As a b) where
  -- Methods that just use underlying vector
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeMove #-}
  {-# INLINE basicUnsafeGrow #-}
  {-# INLINE basicClear #-}
  basicLength      = coerce $ M.basicLength      @MVector @b
  basicUnsafeSlice = coerce $ M.basicUnsafeSlice @MVector @b
  basicOverlaps    = coerce $ M.basicOverlaps    @MVector @b
  basicUnsafeNew   = coerce $ M.basicUnsafeNew   @MVector @b
  basicInitialize  = coerce $ M.basicInitialize  @MVector @b
  basicUnsafeCopy  = coerce $ M.basicUnsafeCopy  @MVector @b
  basicUnsafeMove  = coerce $ M.basicUnsafeMove  @MVector @b
  basicUnsafeGrow  = coerce $ M.basicUnsafeGrow  @MVector @b
  basicClear       = coerce $ M.basicClear       @MVector @b
  -- Conversion to/from underlying representation
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicSet #-}
  basicUnsafeReplicate n (As x) = MV_UnboxAs <$> M.basicUnsafeReplicate n (toURepr x)
  basicUnsafeRead (MV_UnboxAs v) i = As . fromURepr <$> M.basicUnsafeRead v i
  basicUnsafeWrite (MV_UnboxAs v) i (As x) = M.basicUnsafeWrite v i (toURepr x)
  basicSet (MV_UnboxAs v) (As x) = M.basicSet v (toURepr x)

instance (IsoUnbox a b, Unbox b) => G.Vector Vector (As a b) where
  -- Method that just use underlying vector
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze = coerce $ G.basicUnsafeFreeze @Vector @b
  basicUnsafeThaw   = coerce $ G.basicUnsafeThaw   @Vector @b
  basicLength       = coerce $ G.basicLength       @Vector @b
  basicUnsafeSlice  = coerce $ G.basicUnsafeSlice  @Vector @b
  basicUnsafeCopy   = coerce $ G.basicUnsafeCopy   @Vector @b
  elemseq _         = seq
  -- Conversion to/from underlying representation
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (V_UnboxAs v) i = As . fromURepr <$> G.basicUnsafeIndexM v i


#define primMVector(ty,con)                                             \
instance M.MVector MVector ty where {                                   \
  {-# INLINE basicLength #-}                                            \
; {-# INLINE basicUnsafeSlice #-}                                       \
; {-# INLINE basicOverlaps #-}                                          \
; {-# INLINE basicUnsafeNew #-}                                         \
; {-# INLINE basicInitialize #-}                                        \
; {-# INLINE basicUnsafeReplicate #-}                                   \
; {-# INLINE basicUnsafeRead #-}                                        \
; {-# INLINE basicUnsafeWrite #-}                                       \
; {-# INLINE basicClear #-}                                             \
; {-# INLINE basicSet #-}                                               \
; {-# INLINE basicUnsafeCopy #-}                                        \
; {-# INLINE basicUnsafeGrow #-}                                        \
; basicLength (con v) = M.basicLength v                                 \
; basicUnsafeSlice i n (con v) = con $ M.basicUnsafeSlice i n v         \
; basicOverlaps (con v1) (con v2) = M.basicOverlaps v1 v2               \
; basicUnsafeNew n = con `liftM` M.basicUnsafeNew n                     \
; basicInitialize (con v) = M.basicInitialize v                         \
; basicUnsafeReplicate n x = con `liftM` M.basicUnsafeReplicate n x     \
; basicUnsafeRead (con v) i = M.basicUnsafeRead v i                     \
; basicUnsafeWrite (con v) i x = M.basicUnsafeWrite v i x               \
; basicClear (con v) = M.basicClear v                                   \
; basicSet (con v) x = M.basicSet v x                                   \
; basicUnsafeCopy (con v1) (con v2) = M.basicUnsafeCopy v1 v2           \
; basicUnsafeMove (con v1) (con v2) = M.basicUnsafeMove v1 v2           \
; basicUnsafeGrow (con v) n = con `liftM` M.basicUnsafeGrow v n }

#define primVector(ty,con,mcon)                                         \
instance G.Vector Vector ty where {                                     \
  {-# INLINE basicUnsafeFreeze #-}                                      \
; {-# INLINE basicUnsafeThaw #-}                                        \
; {-# INLINE basicLength #-}                                            \
; {-# INLINE basicUnsafeSlice #-}                                       \
; {-# INLINE basicUnsafeIndexM #-}                                      \
; {-# INLINE elemseq #-}                                                \
; basicUnsafeFreeze (mcon v) = con `liftM` G.basicUnsafeFreeze v        \
; basicUnsafeThaw (con v) = mcon `liftM` G.basicUnsafeThaw v            \
; basicLength (con v) = G.basicLength v                                 \
; basicUnsafeSlice i n (con v) = con $ G.basicUnsafeSlice i n v         \
; basicUnsafeIndexM (con v) i = G.basicUnsafeIndexM v i                 \
; basicUnsafeCopy (mcon mv) (con v) = G.basicUnsafeCopy mv v            \
; elemseq _ = seq }

newtype instance MVector s Int = MV_Int (P.MVector s Int)
newtype instance Vector    Int = V_Int  (P.Vector    Int)
instance Unbox Int
primMVector(Int, MV_Int)
primVector(Int, V_Int, MV_Int)

newtype instance MVector s Int8 = MV_Int8 (P.MVector s Int8)
newtype instance Vector    Int8 = V_Int8  (P.Vector    Int8)
instance Unbox Int8
primMVector(Int8, MV_Int8)
primVector(Int8, V_Int8, MV_Int8)

newtype instance MVector s Int16 = MV_Int16 (P.MVector s Int16)
newtype instance Vector    Int16 = V_Int16  (P.Vector    Int16)
instance Unbox Int16
primMVector(Int16, MV_Int16)
primVector(Int16, V_Int16, MV_Int16)

newtype instance MVector s Int32 = MV_Int32 (P.MVector s Int32)
newtype instance Vector    Int32 = V_Int32  (P.Vector    Int32)
instance Unbox Int32
primMVector(Int32, MV_Int32)
primVector(Int32, V_Int32, MV_Int32)

newtype instance MVector s Int64 = MV_Int64 (P.MVector s Int64)
newtype instance Vector    Int64 = V_Int64  (P.Vector    Int64)
instance Unbox Int64
primMVector(Int64, MV_Int64)
primVector(Int64, V_Int64, MV_Int64)


newtype instance MVector s Word = MV_Word (P.MVector s Word)
newtype instance Vector    Word = V_Word  (P.Vector    Word)
instance Unbox Word
primMVector(Word, MV_Word)
primVector(Word, V_Word, MV_Word)

newtype instance MVector s Word8 = MV_Word8 (P.MVector s Word8)
newtype instance Vector    Word8 = V_Word8  (P.Vector    Word8)
instance Unbox Word8
primMVector(Word8, MV_Word8)
primVector(Word8, V_Word8, MV_Word8)

newtype instance MVector s Word16 = MV_Word16 (P.MVector s Word16)
newtype instance Vector    Word16 = V_Word16  (P.Vector    Word16)
instance Unbox Word16
primMVector(Word16, MV_Word16)
primVector(Word16, V_Word16, MV_Word16)

newtype instance MVector s Word32 = MV_Word32 (P.MVector s Word32)
newtype instance Vector    Word32 = V_Word32  (P.Vector    Word32)
instance Unbox Word32
primMVector(Word32, MV_Word32)
primVector(Word32, V_Word32, MV_Word32)

newtype instance MVector s Word64 = MV_Word64 (P.MVector s Word64)
newtype instance Vector    Word64 = V_Word64  (P.Vector    Word64)
instance Unbox Word64
primMVector(Word64, MV_Word64)
primVector(Word64, V_Word64, MV_Word64)


newtype instance MVector s Float = MV_Float (P.MVector s Float)
newtype instance Vector    Float = V_Float  (P.Vector    Float)
instance Unbox Float
primMVector(Float, MV_Float)
primVector(Float, V_Float, MV_Float)

newtype instance MVector s Double = MV_Double (P.MVector s Double)
newtype instance Vector    Double = V_Double  (P.Vector    Double)
instance Unbox Double
primMVector(Double, MV_Double)
primVector(Double, V_Double, MV_Double)


newtype instance MVector s Char = MV_Char (P.MVector s Char)
newtype instance Vector    Char = V_Char  (P.Vector    Char)
instance Unbox Char
primMVector(Char, MV_Char)
primVector(Char, V_Char, MV_Char)

-- ----
-- Bool
-- ----

fromBool :: Bool -> Word8
{-# INLINE fromBool #-}
fromBool True = 1
fromBool False = 0

toBool :: Word8 -> Bool
{-# INLINE toBool #-}
toBool 0 = False
toBool _ = True

newtype instance MVector s Bool = MV_Bool (P.MVector s Word8)
newtype instance Vector    Bool = V_Bool  (P.Vector    Word8)

instance Unbox Bool

instance M.MVector MVector Bool where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_Bool v) = M.basicLength v
  basicUnsafeSlice i n (MV_Bool v) = MV_Bool $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_Bool v1) (MV_Bool v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = MV_Bool `liftM` M.basicUnsafeNew n
  basicInitialize (MV_Bool v) = M.basicInitialize v
  basicUnsafeReplicate n x = MV_Bool `liftM` M.basicUnsafeReplicate n (fromBool x)
  basicUnsafeRead (MV_Bool v) i = toBool `liftM` M.basicUnsafeRead v i
  basicUnsafeWrite (MV_Bool v) i x = M.basicUnsafeWrite v i (fromBool x)
  basicClear (MV_Bool v) = M.basicClear v
  basicSet (MV_Bool v) x = M.basicSet v (fromBool x)
  basicUnsafeCopy (MV_Bool v1) (MV_Bool v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_Bool v1) (MV_Bool v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_Bool v) n = MV_Bool `liftM` M.basicUnsafeGrow v n

instance G.Vector Vector Bool where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Bool v) = V_Bool `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Bool v) = MV_Bool `liftM` G.basicUnsafeThaw v
  basicLength (V_Bool v) = G.basicLength v
  basicUnsafeSlice i n (V_Bool v) = V_Bool $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Bool v) i = toBool `liftM` G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Bool mv) (V_Bool v) = G.basicUnsafeCopy mv v
  elemseq _ = seq

-- -------
-- Complex
-- -------

newtype instance MVector s (Complex a) = MV_Complex (MVector s (a,a))
newtype instance Vector    (Complex a) = V_Complex  (Vector    (a,a))

instance (Unbox a) => Unbox (Complex a)

instance (Unbox a) => M.MVector MVector (Complex a) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeMove #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength      = coerce $ M.basicLength      @MVector @(a,a)
  basicUnsafeSlice = coerce $ M.basicUnsafeSlice @MVector @(a,a)
  basicOverlaps    = coerce $ M.basicOverlaps    @MVector @(a,a)
  basicUnsafeNew   = coerce $ M.basicUnsafeNew   @MVector @(a,a)
  basicInitialize  = coerce $ M.basicInitialize  @MVector @(a,a)
  basicUnsafeCopy  = coerce $ M.basicUnsafeCopy  @MVector @(a,a)
  basicUnsafeMove  = coerce $ M.basicUnsafeMove  @MVector @(a,a)
  basicUnsafeGrow  = coerce $ M.basicUnsafeGrow  @MVector @(a,a)
  basicClear       = coerce $ M.basicClear       @MVector @(a,a)
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicSet #-}
  basicUnsafeReplicate n (x :+ y) = MV_Complex <$> M.basicUnsafeReplicate n (x,y)
  basicUnsafeRead (MV_Complex v) i = uncurry (:+) <$> M.basicUnsafeRead v i
  basicUnsafeWrite (MV_Complex v) i (x :+ y) = M.basicUnsafeWrite v i (x,y)
  basicSet (MV_Complex v) (x :+ y) = M.basicSet v (x,y)

instance (Unbox a) => G.Vector Vector (Complex a) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeFreeze = coerce $ G.basicUnsafeFreeze @Vector @(a,a)
  basicUnsafeThaw   = coerce $ G.basicUnsafeThaw   @Vector @(a,a)
  basicLength       = coerce $ G.basicLength       @Vector @(a,a)
  basicUnsafeSlice  = coerce $ G.basicUnsafeSlice  @Vector @(a,a)
  basicUnsafeCopy   = coerce $ G.basicUnsafeCopy   @Vector @(a,a)
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeIndexM (V_Complex v) i
                = uncurry (:+) <$> G.basicUnsafeIndexM v i
  elemseq _ (x :+ y) z = G.elemseq (undefined :: Vector a) x
                       $ G.elemseq (undefined :: Vector a) y z

-- -------
-- Identity
-- -------
#define newtypeMVector(inst_ctxt,inst_head,tyC,con) \
instance inst_ctxt => M.MVector MVector (inst_head) where { \
; {-# INLINE basicLength          #-}                                         \
; {-# INLINE basicUnsafeSlice     #-}                                         \
; {-# INLINE basicOverlaps        #-}                                         \
; {-# INLINE basicUnsafeNew       #-}                                         \
; {-# INLINE basicInitialize      #-}                                         \
; {-# INLINE basicUnsafeReplicate #-}                                         \
; {-# INLINE basicUnsafeRead      #-}                                         \
; {-# INLINE basicUnsafeWrite     #-}                                         \
; {-# INLINE basicClear           #-}                                         \
; {-# INLINE basicSet             #-}                                         \
; {-# INLINE basicUnsafeCopy      #-}                                         \
; {-# INLINE basicUnsafeGrow      #-}                                         \
; basicLength (con v)                = M.basicLength v                        \
; basicUnsafeSlice i n (con v)       = con $ M.basicUnsafeSlice i n v         \
; basicOverlaps (con v1) (con v2)    = M.basicOverlaps v1 v2                  \
; basicUnsafeNew n                   = con `liftM` M.basicUnsafeNew n         \
; basicInitialize (con v)            = M.basicInitialize v                    \
; basicUnsafeReplicate n (tyC x)     = con `liftM` M.basicUnsafeReplicate n x \
; basicUnsafeRead (con v) i          = tyC `liftM` M.basicUnsafeRead v i      \
; basicUnsafeWrite (con v) i (tyC x) = M.basicUnsafeWrite v i x               \
; basicClear (con v)                 = M.basicClear v                         \
; basicSet (con v) (tyC x)           = M.basicSet v x                         \
; basicUnsafeCopy (con v1) (con v2)  = M.basicUnsafeCopy v1 v2                \
; basicUnsafeMove (con v1) (con v2)  = M.basicUnsafeMove v1 v2                \
; basicUnsafeGrow (con v) n          = con `liftM` M.basicUnsafeGrow v n      \
}
#define newtypeVector(inst_ctxt,inst_head,tyC,con,mcon) \
instance inst_ctxt => G.Vector Vector (inst_head) where { \
; {-# INLINE basicUnsafeFreeze  #-}                                       \
; {-# INLINE basicUnsafeThaw    #-}                                       \
; {-# INLINE basicLength        #-}                                       \
; {-# INLINE basicUnsafeSlice   #-}                                       \
; {-# INLINE basicUnsafeIndexM  #-}                                       \
; {-# INLINE elemseq            #-}                                       \
; basicUnsafeFreeze (mcon v)        = con `liftM` G.basicUnsafeFreeze v   \
; basicUnsafeThaw (con v)           = mcon `liftM` G.basicUnsafeThaw v    \
; basicLength (con v)               = G.basicLength v                     \
; basicUnsafeSlice i n (con v)      = con $ G.basicUnsafeSlice i n v      \
; basicUnsafeIndexM (con v) i       = tyC `liftM` G.basicUnsafeIndexM v i \
; basicUnsafeCopy (mcon mv) (con v) = G.basicUnsafeCopy mv v              \
; elemseq _ (tyC a)                 = G.elemseq (undefined :: Vector x) a \
}
#define deriveNewtypeInstances(inst_ctxt,inst_head,rep,tyC,con,mcon) \
newtype instance MVector s (inst_head) = mcon (MVector s (rep)) ;\
newtype instance Vector    (inst_head) = con  (Vector (rep))    ;\
instance inst_ctxt => Unbox (inst_head)                         ;\
newtypeMVector(inst_ctxt, inst_head, tyC, mcon)                 ;\
newtypeVector(inst_ctxt,  inst_head, tyC, con, mcon)

deriveNewtypeInstances(Unbox a, Identity a, a, Identity, V_Identity, MV_Identity)
deriveNewtypeInstances(Unbox a, Down a,    a, Down,    V_Down,    MV_Down)
deriveNewtypeInstances(Unbox a, Dual a,    a, Dual,    V_Dual,    MV_Dual)
deriveNewtypeInstances(Unbox a, Sum a,     a, Sum,     V_Sum,     MV_Sum)
deriveNewtypeInstances(Unbox a, Product a, a, Product, V_Product, MV_Product)


-- --------------
-- Data.Semigroup
-- --------------

deriveNewtypeInstances(Unbox a, Min a,   a, Min,   V_Min,   MV_Min)
deriveNewtypeInstances(Unbox a, Max a,   a, Max,   V_Max,   MV_Max)
deriveNewtypeInstances(Unbox a, First a, a, First, V_First, MV_First)
deriveNewtypeInstances(Unbox a, Last a,  a, Last,  V_Last,  MV_Last)
deriveNewtypeInstances(Unbox a, WrappedMonoid a, a, WrapMonoid, V_WrappedMonoid, MV_WrappedMonoid)

-- ------------------
-- Data.Semigroup.Arg
-- ------------------

newtype instance MVector s (Arg a b) = MV_Arg (MVector s (a,b))
newtype instance Vector    (Arg a b) = V_Arg  (Vector    (a,b))

instance (Unbox a, Unbox b) => Unbox (Arg a b)

instance (Unbox a, Unbox b) => M.MVector MVector (Arg a b) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeMove #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength      = coerce $ M.basicLength      @MVector @(a,b)
  basicUnsafeSlice = coerce $ M.basicUnsafeSlice @MVector @(a,b)
  basicOverlaps    = coerce $ M.basicOverlaps    @MVector @(a,b)
  basicUnsafeNew   = coerce $ M.basicUnsafeNew   @MVector @(a,b)
  basicInitialize  = coerce $ M.basicInitialize  @MVector @(a,b)
  basicUnsafeCopy  = coerce $ M.basicUnsafeCopy  @MVector @(a,b)
  basicUnsafeMove  = coerce $ M.basicUnsafeMove  @MVector @(a,b)
  basicUnsafeGrow  = coerce $ M.basicUnsafeGrow  @MVector @(a,b)
  basicClear       = coerce $ M.basicClear       @MVector @(a,b)
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicSet #-}
  basicUnsafeReplicate n (Arg x y)        = MV_Arg <$> M.basicUnsafeReplicate n (x,y)
  basicUnsafeRead (MV_Arg v) i            = uncurry Arg <$> M.basicUnsafeRead v i
  basicUnsafeWrite (MV_Arg v) i (Arg x y) = M.basicUnsafeWrite v i (x,y)
  basicSet (MV_Arg v) (Arg x y)           = M.basicSet v (x,y)


instance (Unbox a, Unbox b) => G.Vector Vector (Arg a b) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeFreeze = coerce $ G.basicUnsafeFreeze @Vector @(a,b)
  basicUnsafeThaw   = coerce $ G.basicUnsafeThaw   @Vector @(a,b)
  basicLength       = coerce $ G.basicLength       @Vector @(a,b)
  basicUnsafeSlice  = coerce $ G.basicUnsafeSlice  @Vector @(a,b)
  basicUnsafeCopy   = coerce $ G.basicUnsafeCopy   @Vector @(a,b)
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeIndexM (V_Arg v) i  = uncurry Arg `liftM` G.basicUnsafeIndexM v i
  elemseq _ (Arg x y) z          = G.elemseq (undefined :: Vector a) x
                                 $ G.elemseq (undefined :: Vector b) y z

-- -------
-- Unboxing the boxed values
-- -------

-- | Newtype which allows to derive unbox instances for type @a@ which
-- is normally a "boxed" type. The newtype does not alter the strictness
-- semantics of the underlying type and inherits the laizness of said type.
-- For a strict newtype wrapper, see 'DoNotUnboxStrict'.
--
-- 'DoNotUnboxLazy' is intended to be unsed in conjunction with the newtype 'As'
-- and the type class 'IsoUnbox'. Here's an example which uses the following
-- explicit 'IsoUnbox' instance:
--
--
-- >>> :set -XTypeFamilies -XStandaloneDeriving -XDerivingVia
-- >>> :set -XMultiParamTypeClasses -XTypeOperators -XFlexibleInstances
-- >>> import qualified Data.Vector.Unboxed         as VU
-- >>> import qualified Data.Vector.Unboxed.Mutable as VUM
-- >>> import qualified Data.Vector.Generic         as VG
-- >>> import qualified Data.Vector.Generic.Mutable as VGM
-- >>> :{
-- >>> data Foo a = Foo Int a
-- >>>   deriving (Eq, Ord, Show)
-- >>> instance VU.IsoUnbox (Foo a) (Int, VU.DoNotUnboxLazy a) where
-- >>>   toURepr (Foo i a) = (i, VU.DoNotUnboxLazy a)
-- >>>   fromURepr (i, VU.DoNotUnboxLazy a) = Foo i a
-- >>>   {-# INLINE toURepr #-}
-- >>>   {-# INLINE fromURepr #-}
-- >>> newtype instance VU.MVector s (Foo a) = MV_Foo (VU.MVector s (Int, VU.DoNotUnboxLazy a))
-- >>> newtype instance VU.Vector    (Foo a) = V_Foo  (VU.Vector    (Int, VU.DoNotUnboxLazy a))
-- >>> deriving via (Foo a `VU.As` (Int, VU.DoNotUnboxLazy a)) instance VGM.MVector VUM.MVector (Foo a)
-- >>> deriving via (Foo a `VU.As` (Int, VU.DoNotUnboxLazy a)) instance VG.Vector   VU.Vector   (Foo a)
-- >>> instance VU.Unbox (Foo a)
-- >>> :}
--
-- >>> VU.fromListN 3 [ Foo 4 "Haskell's", Foo 8 "strong", Foo 16 "types" ]
-- [Foo 4 "Haskell's",Foo 8 "strong",Foo 16 "types"]
--
-- @since 0.13.2.0
newtype DoNotUnboxLazy a = DoNotUnboxLazy a

newtype instance MVector s (DoNotUnboxLazy a) = MV_DoNotUnboxLazy (B.MVector s a)
newtype instance Vector    (DoNotUnboxLazy a) = V_DoNotUnboxLazy  (B.Vector    a)

instance M.MVector MVector (DoNotUnboxLazy a) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength          = coerce $ M.basicLength          @B.MVector @a
  basicUnsafeSlice     = coerce $ M.basicUnsafeSlice     @B.MVector @a
  basicOverlaps        = coerce $ M.basicOverlaps        @B.MVector @a
  basicUnsafeNew       = coerce $ M.basicUnsafeNew       @B.MVector @a
  basicInitialize      = coerce $ M.basicInitialize      @B.MVector @a
  basicUnsafeReplicate = coerce $ M.basicUnsafeReplicate @B.MVector @a
  basicUnsafeRead      = coerce $ M.basicUnsafeRead      @B.MVector @a
  basicUnsafeWrite     = coerce $ M.basicUnsafeWrite     @B.MVector @a
  basicClear           = coerce $ M.basicClear           @B.MVector @a
  basicSet             = coerce $ M.basicSet             @B.MVector @a
  basicUnsafeCopy      = coerce $ M.basicUnsafeCopy      @B.MVector @a
  basicUnsafeMove      = coerce $ M.basicUnsafeMove      @B.MVector @a
  basicUnsafeGrow      = coerce $ M.basicUnsafeGrow      @B.MVector @a

instance G.Vector Vector (DoNotUnboxLazy a) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze = coerce $ G.basicUnsafeFreeze @B.Vector @a
  basicUnsafeThaw   = coerce $ G.basicUnsafeThaw   @B.Vector @a
  basicLength       = coerce $ G.basicLength       @B.Vector @a
  basicUnsafeSlice  = coerce $ G.basicUnsafeSlice  @B.Vector @a
  basicUnsafeIndexM = coerce $ G.basicUnsafeIndexM @B.Vector @a
  basicUnsafeCopy   = coerce $ G.basicUnsafeCopy   @B.Vector @a
  elemseq _ = seq

instance Unbox (DoNotUnboxLazy a)

-- | Newtype which allows to derive unbox instances for type @a@ which
-- is normally a "boxed" type. The newtype stictly evaluates the wrapped values
-- ensuring that the unboxed vector contains no (direct) thunks.
-- For a less strict newtype wrapper, see 'DoNotUnboxLazy'.
-- For a more strict newtype wrapper, see 'DoNotUnboxNormalForm'.
--
-- 'DoNotUnboxStrict' is intended to be unsed in conjunction with the newtype 'As'
-- and the type class 'IsoUnbox'. Here's an example which uses the following
-- explicit 'IsoUnbox' instance:
--
--
-- >>> :set -XBangPatterns -XTypeFamilies -XStandaloneDeriving -XDerivingVia
-- >>> :set -XMultiParamTypeClasses -XTypeOperators -XFlexibleInstances
-- >>> import qualified Data.Vector.Unboxed         as VU
-- >>> import qualified Data.Vector.Unboxed.Mutable as VUM
-- >>> import qualified Data.Vector.Generic         as VG
-- >>> import qualified Data.Vector.Generic.Mutable as VGM
-- >>> :{
-- >>> data Bar a = Bar Int a
-- >>>   deriving Show
-- >>> instance VU.IsoUnbox (Bar a) (Int, VU.DoNotUnboxStrict a) where
-- >>>   toURepr (Bar i !a) = (i, VU.DoNotUnboxStrict a)
-- >>>   fromURepr (i, VU.DoNotUnboxStrict a) = Bar i a
-- >>>   {-# INLINE toURepr #-}
-- >>>   {-# INLINE fromURepr #-}
-- >>> newtype instance VU.MVector s (Bar a) = MV_Bar (VU.MVector s (Int, VU.DoNotUnboxStrict a))
-- >>> newtype instance VU.Vector    (Bar a) = V_Bar  (VU.Vector    (Int, VU.DoNotUnboxStrict a))
-- >>> deriving via (Bar a `VU.As` (Int, VU.DoNotUnboxStrict a)) instance VGM.MVector VUM.MVector (Bar a)
-- >>> deriving via (Bar a `VU.As` (Int, VU.DoNotUnboxStrict a)) instance VG.Vector   VU.Vector   (Bar a)
-- >>> instance VU.Unbox (Bar a)
-- >>> :}
--
-- >>> VU.fromListN 3 [ Bar 3 "Bye", Bar 2 "for", Bar 1 "now" ]
-- [Bar 3 "Bye",Bar 2 "for",Bar 1 "now"]
--
-- @since 0.13.2.0
newtype DoNotUnboxStrict a = DoNotUnboxStrict a

newtype instance MVector s (DoNotUnboxStrict a) = MV_DoNotUnboxStrict (S.MVector s a)
newtype instance Vector    (DoNotUnboxStrict a) = V_DoNotUnboxStrict  (S.Vector a)

instance M.MVector MVector (DoNotUnboxStrict a) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength          = coerce $ M.basicLength          @S.MVector @a
  basicUnsafeSlice     = coerce $ M.basicUnsafeSlice     @S.MVector @a
  basicOverlaps        = coerce $ M.basicOverlaps        @S.MVector @a
  basicUnsafeNew       = coerce $ M.basicUnsafeNew       @S.MVector @a
  basicInitialize      = coerce $ M.basicInitialize      @S.MVector @a
  basicUnsafeReplicate = coerce $ M.basicUnsafeReplicate @S.MVector @a
  basicUnsafeRead      = coerce $ M.basicUnsafeRead      @S.MVector @a
  basicUnsafeWrite     = coerce $ M.basicUnsafeWrite     @S.MVector @a
  basicClear           = coerce $ M.basicClear           @S.MVector @a
  basicSet             = coerce $ M.basicSet             @S.MVector @a
  basicUnsafeCopy      = coerce $ M.basicUnsafeCopy      @S.MVector @a
  basicUnsafeMove      = coerce $ M.basicUnsafeMove      @S.MVector @a
  basicUnsafeGrow      = coerce $ M.basicUnsafeGrow      @S.MVector @a

instance G.Vector Vector (DoNotUnboxStrict a) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze = coerce $ G.basicUnsafeFreeze @S.Vector @a
  basicUnsafeThaw   = coerce $ G.basicUnsafeThaw   @S.Vector @a
  basicLength       = coerce $ G.basicLength       @S.Vector @a
  basicUnsafeSlice  = coerce $ G.basicUnsafeSlice  @S.Vector @a
  basicUnsafeIndexM = coerce $ G.basicUnsafeIndexM @S.Vector @a
  basicUnsafeCopy   = coerce $ G.basicUnsafeCopy   @S.Vector @a
  elemseq _ = seq

instance Unbox (DoNotUnboxStrict a)

-- | Newtype which allows to derive unbox instances for type @a@ which
-- is normally a "boxed" type. The newtype stictly evaluates the wrapped values
-- via thier requisite 'NFData' instance, ensuring that the unboxed vector
-- contains only values reduced to normal form.
-- For a less strict newtype wrappers, see 'DoNotUnboxLazy' and 'DoNotUnboxStrict'.
--
-- 'DoNotUnboxNormalForm' is intended to be unsed in conjunction with the newtype 'As'
-- and the type class 'IsoUnbox'. Here's an example which uses the following
-- explicit 'IsoUnbox' instance:
--
--
-- >>> :set -XTypeFamilies -XStandaloneDeriving -XDerivingVia
-- >>> :set -XMultiParamTypeClasses -XTypeOperators -XFlexibleInstances
-- >>> import qualified Data.Vector.Unboxed         as VU
-- >>> import qualified Data.Vector.Unboxed.Mutable as VUM
-- >>> import qualified Data.Vector.Generic         as VG
-- >>> import qualified Data.Vector.Generic.Mutable as VGM
-- >>> import qualified Control.DeepSeq             as NF
-- >>> :{
-- >>> data Baz a = Baz Int a
-- >>>   deriving Show
-- >>> instance NF.NFData a => VU.IsoUnbox (Baz a) (Int, VU.DoNotUnboxNormalForm a) where
-- >>>   toURepr (Baz i a) = (i, VU.DoNotUnboxNormalForm $ NF.force a)
-- >>>   fromURepr (i, VU.DoNotUnboxNormalForm a) = Baz i a
-- >>>   {-# INLINE toURepr #-}
-- >>>   {-# INLINE fromURepr #-}
-- >>> newtype instance VU.MVector s (Baz a) = MV_Baz (VU.MVector s (Int, VU.DoNotUnboxNormalForm a))
-- >>> newtype instance VU.Vector    (Baz a) = V_Baz  (VU.Vector    (Int, VU.DoNotUnboxNormalForm a))
-- >>> deriving via (Baz a `VU.As` (Int, VU.DoNotUnboxNormalForm a)) instance NF.NFData a => VGM.MVector VUM.MVector (Baz a)
-- >>> deriving via (Baz a `VU.As` (Int, VU.DoNotUnboxNormalForm a)) instance NF.NFData a => VG.Vector   VU.Vector   (Baz a)
-- >>> instance NF.NFData a => VU.Unbox (Baz a)
-- >>> :}
--
-- >>> VU.fromListN 3 [ Baz 3 "Fully", Baz 9 "evaluated", Baz 27 "data" ]
-- [Baz 3 "Fully",Baz 9 "evaluated",Baz 27 "data"]
--
-- @since 0.13.2.0
newtype DoNotUnboxNormalForm a = DoNotUnboxNormalForm a

newtype instance MVector s (DoNotUnboxNormalForm a) = MV_DoNotUnboxNormalForm (S.MVector s a)
newtype instance Vector    (DoNotUnboxNormalForm a) = V_DoNotUnboxNormalForm  (S.Vector a)

instance NFData a => M.MVector MVector (DoNotUnboxNormalForm a) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength          = coerce $ M.basicLength          @S.MVector @a
  basicUnsafeSlice     = coerce $ M.basicUnsafeSlice     @S.MVector @a
  basicOverlaps        = coerce $ M.basicOverlaps        @S.MVector @a
  basicUnsafeNew       = coerce $ M.basicUnsafeNew       @S.MVector @a
  basicInitialize      = coerce $ M.basicInitialize      @S.MVector @a
  basicUnsafeReplicate = coerce (\i x -> M.basicUnsafeReplicate @S.MVector @a i (force x))
  basicUnsafeRead      = coerce $ M.basicUnsafeRead      @S.MVector @a
  basicUnsafeWrite     = coerce (\v i x -> M.basicUnsafeWrite @S.MVector @a v i (force x))
  basicClear           = coerce $ M.basicClear           @S.MVector @a
  basicSet             = coerce (\v x -> M.basicSet @S.MVector @a v (force x))
  basicUnsafeCopy      = coerce $ M.basicUnsafeCopy      @S.MVector @a
  basicUnsafeMove      = coerce $ M.basicUnsafeMove      @S.MVector @a
  basicUnsafeGrow      = coerce $ M.basicUnsafeGrow      @S.MVector @a

instance NFData a => G.Vector Vector (DoNotUnboxNormalForm a) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze = coerce $ G.basicUnsafeFreeze @S.Vector @a
  basicUnsafeThaw   = coerce $ G.basicUnsafeThaw   @S.Vector @a
  basicLength       = coerce $ G.basicLength       @S.Vector @a
  basicUnsafeSlice  = coerce $ G.basicUnsafeSlice  @S.Vector @a
  basicUnsafeIndexM = coerce $ G.basicUnsafeIndexM @S.Vector @a
  basicUnsafeCopy   = coerce $ G.basicUnsafeCopy   @S.Vector @a
  elemseq _ x y = rnf (coerce x :: a) `seq` y

instance NFData a => Unbox (DoNotUnboxNormalForm a)

instance NFData a => NFData (DoNotUnboxNormalForm a) where
  {-# INLINE rnf #-}
  rnf = rnf . coerce @(DoNotUnboxNormalForm a) @a

deriveNewtypeInstances((), Any, Bool, Any, V_Any, MV_Any)
deriveNewtypeInstances((), All, Bool, All, V_All, MV_All)

-- -------
-- Const
-- -------

deriveNewtypeInstances(Unbox a, Const a b, a, Const, V_Const, MV_Const)

-- ---
-- Alt
-- ---

deriveNewtypeInstances(Unbox (f a), Alt f a, f a, Alt, V_Alt, MV_Alt)

-- -------
-- Compose
-- -------

deriveNewtypeInstances(Unbox (f (g a)), Compose f g a, f (g a), Compose, V_Compose, MV_Compose)

-- ------
-- Tuples
-- ------

#define DEFINE_INSTANCES
#include "unbox-tuple-instances"
