{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Vector.Unboxed.Base
-- Copyright   : (c) Roman Leshchinskiy 2009-2010
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
--
-- Adaptive unboxed vectors: basic implementation.

module Data.Vector.Unboxed.Base (
  MVector(..), IOVector, STVector, Vector(..), Unbox,
  UnboxViaPrim(..), As(..), IsoUnbox(..)
) where

import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M

import qualified Data.Vector.Primitive as P

import Control.Applicative (Const(..))

import Control.DeepSeq ( NFData(rnf)
#if MIN_VERSION_deepseq(1,4,3)
                       , NFData1(liftRnf)
#endif
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
-- >>> import qualified Data.Vector.Unboxed         as U
-- >>> import qualified Data.Vector.Primitive       as P
-- >>> import qualified Data.Vector.Generic         as G
-- >>> import qualified Data.Vector.Generic.Mutable as M
-- >>>
-- >>> newtype Foo = Foo Int deriving P.Prim
-- >>>
-- >>> newtype instance U.MVector s Foo = MV_Int (P.MVector s Foo)
-- >>> newtype instance U.Vector    Foo = V_Int  (P.Vector    Foo)
-- >>> deriving via (U.UnboxViaPrim Foo) instance M.MVector MVector Foo
-- >>> deriving via (U.UnboxViaPrim Foo) instance G.Vector  Vector  Foo
-- >>> instance Unbox Foo
--
-- Second example is essentially same but with a twist. Instead of
-- using @Prim@ instance of data type, we use underlying instance of @Int@:
--
-- >>> :set -XTypeFamilies -XStandaloneDeriving -XDerivingVia -XMultiParamTypeClasses
-- >>>
-- >>> import qualified Data.Vector.Unboxed         as U
-- >>> import qualified Data.Vector.Primitive       as P
-- >>> import qualified Data.Vector.Generic         as G
-- >>> import qualified Data.Vector.Generic.Mutable as M
-- >>>
-- >>> newtype Foo = Foo Int
-- >>>
-- >>> newtype instance U.MVector s Foo = MV_Int (P.MVector s Int)
-- >>> newtype instance U.Vector    Foo = V_Int  (P.Vector    Int)
-- >>> deriving via (U.UnboxViaPrim Int) instance M.MVector MVector Foo
-- >>> deriving via (U.UnboxViaPrim Int) instance G.Vector  Vector  Foo
-- >>> instance Unbox Foo
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
  basicLength (MV_UnboxViaPrim v) = M.basicLength v
  basicUnsafeSlice i n (MV_UnboxViaPrim v) = MV_UnboxViaPrim $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_UnboxViaPrim v1) (MV_UnboxViaPrim v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = MV_UnboxViaPrim `liftM` M.basicUnsafeNew n
  basicInitialize (MV_UnboxViaPrim v) = M.basicInitialize v
  basicUnsafeReplicate n (UnboxViaPrim x) = MV_UnboxViaPrim `liftM` M.basicUnsafeReplicate n x
  basicUnsafeRead (MV_UnboxViaPrim v) i = UnboxViaPrim `liftM` M.basicUnsafeRead v i
  basicUnsafeWrite (MV_UnboxViaPrim v) i (UnboxViaPrim x) = M.basicUnsafeWrite v i x
  basicClear (MV_UnboxViaPrim v) = M.basicClear v
  basicSet (MV_UnboxViaPrim v) (UnboxViaPrim x) = M.basicSet v x
  basicUnsafeCopy (MV_UnboxViaPrim v1) (MV_UnboxViaPrim v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_UnboxViaPrim v1) (MV_UnboxViaPrim v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_UnboxViaPrim v) n = MV_UnboxViaPrim `liftM` M.basicUnsafeGrow v n

instance P.Prim a => G.Vector Vector (UnboxViaPrim a) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_UnboxViaPrim v) = V_UnboxViaPrim `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_UnboxViaPrim v) = MV_UnboxViaPrim `liftM` G.basicUnsafeThaw v
  basicLength (V_UnboxViaPrim v) = G.basicLength v
  basicUnsafeSlice i n (V_UnboxViaPrim v) = V_UnboxViaPrim $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_UnboxViaPrim v) i = UnboxViaPrim <$> G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_UnboxViaPrim mv) (V_UnboxViaPrim v) = G.basicUnsafeCopy mv v
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
-- deriving via (Foo a `VU.As` (Int, a)) instance VU.Unbox a => VGM.MVector MVector (Foo a)
-- deriving via (Foo a `VU.As` (Int, a)) instance VU.Unbox a => VG.Vector  Vector  (Foo a)
-- instance VU.Unbox a => VU.Unbox (Foo a)
-- :}
--
--
-- It's also possible to use generic-based instance for 'IsoUnbox'
-- which should work for all product types.
--
-- >>> :set -XTypeFamilies -XStandaloneDeriving -XDerivingVia -XDeriveGeneric
-- >>> :set -XMultiParamTypeClasses -XTypeOperators -XFlexibleInstances
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
newtype As (a :: Type) (b :: Type) = As a

newtype instance MVector s (As a b) = MV_UnboxAs (MVector s b)
newtype instance Vector    (As a b) = V_UnboxAs  (Vector b)

instance (IsoUnbox a b, Unbox b) => M.MVector MVector (As a b) where
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
  basicLength (MV_UnboxAs v) = M.basicLength v
  basicUnsafeSlice i n (MV_UnboxAs v) = MV_UnboxAs $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_UnboxAs v1) (MV_UnboxAs v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = MV_UnboxAs `liftM` M.basicUnsafeNew n
  basicInitialize (MV_UnboxAs v) = M.basicInitialize v
  basicUnsafeReplicate n (As x) = MV_UnboxAs `liftM` M.basicUnsafeReplicate n (toURepr x)
  basicUnsafeRead (MV_UnboxAs v) i = (As . fromURepr) `liftM` M.basicUnsafeRead v i
  basicUnsafeWrite (MV_UnboxAs v) i (As x) = M.basicUnsafeWrite v i (toURepr x)
  basicClear (MV_UnboxAs v) = M.basicClear v
  basicSet (MV_UnboxAs v) (As x) = M.basicSet v (toURepr x)
  basicUnsafeCopy (MV_UnboxAs v1) (MV_UnboxAs v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_UnboxAs v1) (MV_UnboxAs v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_UnboxAs v) n = MV_UnboxAs `liftM` M.basicUnsafeGrow v n

instance (IsoUnbox a b, Unbox b) => G.Vector Vector (As a b) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_UnboxAs v) = V_UnboxAs `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_UnboxAs v) = MV_UnboxAs `liftM` G.basicUnsafeThaw v
  basicLength (V_UnboxAs v) = G.basicLength v
  basicUnsafeSlice i n (V_UnboxAs v) = V_UnboxAs $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_UnboxAs v) i = As . fromURepr <$> G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_UnboxAs mv) (V_UnboxAs v) = G.basicUnsafeCopy mv v
  elemseq _ = seq


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
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_Complex v) = M.basicLength v
  basicUnsafeSlice i n (MV_Complex v) = MV_Complex $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_Complex v1) (MV_Complex v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = MV_Complex `liftM` M.basicUnsafeNew n
  basicInitialize (MV_Complex v) = M.basicInitialize v
  basicUnsafeReplicate n (x :+ y) = MV_Complex `liftM` M.basicUnsafeReplicate n (x,y)
  basicUnsafeRead (MV_Complex v) i = uncurry (:+) `liftM` M.basicUnsafeRead v i
  basicUnsafeWrite (MV_Complex v) i (x :+ y) = M.basicUnsafeWrite v i (x,y)
  basicClear (MV_Complex v) = M.basicClear v
  basicSet (MV_Complex v) (x :+ y) = M.basicSet v (x,y)
  basicUnsafeCopy (MV_Complex v1) (MV_Complex v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_Complex v1) (MV_Complex v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_Complex v) n = MV_Complex `liftM` M.basicUnsafeGrow v n

instance (Unbox a) => G.Vector Vector (Complex a) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Complex v) = V_Complex `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Complex v) = MV_Complex `liftM` G.basicUnsafeThaw v
  basicLength (V_Complex v) = G.basicLength v
  basicUnsafeSlice i n (V_Complex v) = V_Complex $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Complex v) i
                = uncurry (:+) `liftM` G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Complex mv) (V_Complex v)
                = G.basicUnsafeCopy mv v
  elemseq _ (x :+ y) z = G.elemseq (undefined :: Vector a) x
                       $ G.elemseq (undefined :: Vector a) y z

-- -------
-- Identity
-- -------

newtype instance MVector s (Identity a) = MV_Identity (MVector s a)
newtype instance Vector    (Identity a) = V_Identity  (Vector a)
deriving instance Unbox a => G.Vector  Vector  (Identity a)
deriving instance Unbox a => M.MVector MVector (Identity a)
instance Unbox a => Unbox (Identity a)

newtype instance MVector s (Down a) = MV_Down (MVector s a)
newtype instance Vector    (Down a) = V_Down  (Vector a)
deriving instance Unbox a => G.Vector  Vector  (Down a)
deriving instance Unbox a => M.MVector MVector (Down a)
instance Unbox a => Unbox (Down a)

newtype instance MVector s (Dual a) = MV_Dual (MVector s a)
newtype instance Vector    (Dual a) = V_Dual  (Vector a)
deriving instance Unbox a => G.Vector  Vector  (Dual a)
deriving instance Unbox a => M.MVector MVector (Dual a)
instance Unbox a => Unbox (Dual a)

newtype instance MVector s (Sum a) = MV_Sum (MVector s a)
newtype instance Vector    (Sum a) = V_Sum  (Vector a)
deriving instance Unbox a => G.Vector  Vector  (Sum a)
deriving instance Unbox a => M.MVector MVector (Sum a)
instance Unbox a => Unbox (Sum a)

newtype instance MVector s (Product a) = MV_Product (MVector s a)
newtype instance Vector    (Product a) = V_Product  (Vector a)
deriving instance Unbox a => G.Vector  Vector  (Product a)
deriving instance Unbox a => M.MVector MVector (Product a)
instance Unbox a => Unbox (Product a)

-- --------------
-- Data.Semigroup
-- --------------


newtype instance MVector s (Min a) = MV_Min (MVector s a)
newtype instance Vector    (Min a) = V_Min  (Vector a)
deriving instance Unbox a => G.Vector  Vector  (Min a)
deriving instance Unbox a => M.MVector MVector (Min a)
instance Unbox a => Unbox (Min a)

newtype instance MVector s (Max a) = MV_Max (MVector s a)
newtype instance Vector    (Max a) = V_Max  (Vector a)
deriving instance Unbox a => G.Vector  Vector  (Max a)
deriving instance Unbox a => M.MVector MVector (Max a)
instance Unbox a => Unbox (Max a)

newtype instance MVector s (First a) = MV_First (MVector s a)
newtype instance Vector    (First a) = V_First  (Vector a)
deriving instance Unbox a => G.Vector  Vector  (First a)
deriving instance Unbox a => M.MVector MVector (First a)
instance Unbox a => Unbox (First a)

newtype instance MVector s (Last a) = MV_Last (MVector s a)
newtype instance Vector    (Last a) = V_Last  (Vector a)
deriving instance Unbox a => G.Vector  Vector  (Last a)
deriving instance Unbox a => M.MVector MVector (Last a)
instance Unbox a => Unbox (Last a)

newtype instance MVector s (WrappedMonoid a) = MV_WrappedMonoid (MVector s a)
newtype instance Vector    (WrappedMonoid a) = V_WrappedMonoid  (Vector a)
deriving instance Unbox a => G.Vector  Vector  (WrappedMonoid a)
deriving instance Unbox a => M.MVector MVector (WrappedMonoid a)
instance Unbox a => Unbox (WrappedMonoid a)

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
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_Arg v)                  = M.basicLength v
  basicUnsafeSlice i n (MV_Arg v)         = MV_Arg $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_Arg v1) (MV_Arg v2)   = M.basicOverlaps v1 v2
  basicUnsafeNew n                        = MV_Arg `liftM` M.basicUnsafeNew n
  basicInitialize (MV_Arg v)              = M.basicInitialize v
  basicUnsafeReplicate n (Arg x y)        = MV_Arg `liftM` M.basicUnsafeReplicate n (x,y)
  basicUnsafeRead (MV_Arg v) i            = uncurry Arg `liftM` M.basicUnsafeRead v i
  basicUnsafeWrite (MV_Arg v) i (Arg x y) = M.basicUnsafeWrite v i (x,y)
  basicClear (MV_Arg v)                   = M.basicClear v
  basicSet (MV_Arg v) (Arg x y)           = M.basicSet v (x,y)
  basicUnsafeCopy (MV_Arg v1) (MV_Arg v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_Arg v1) (MV_Arg v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_Arg v) n            = MV_Arg `liftM` M.basicUnsafeGrow v n

instance (Unbox a, Unbox b) => G.Vector Vector (Arg a b) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Arg v)   = V_Arg `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Arg v)      = MV_Arg `liftM` G.basicUnsafeThaw v
  basicLength (V_Arg v)          = G.basicLength v
  basicUnsafeSlice i n (V_Arg v) = V_Arg $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Arg v) i  = uncurry Arg `liftM` G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Arg mv) (V_Arg v)
                                 = G.basicUnsafeCopy mv v
  elemseq _ (Arg x y) z          = G.elemseq (undefined :: Vector a) x
                                 $ G.elemseq (undefined :: Vector b) y z

newtype instance MVector s Any = MV_Any (MVector s Bool)
newtype instance Vector    Any = V_Any  (Vector    Bool)
deriving instance G.Vector  Vector  Any
deriving instance M.MVector MVector Any
instance Unbox Any

newtype instance MVector s All = MV_All (MVector s Bool)
newtype instance Vector    All = V_All  (Vector    Bool)
deriving instance G.Vector  Vector  All
deriving instance M.MVector MVector All
instance Unbox All

-- -------
-- Const
-- -------

newtype instance MVector s (Const b a) = MV_Const (MVector s b)
newtype instance Vector    (Const b a) = V_Const  (Vector    b)
deriving instance Unbox b => G.Vector  Vector  (Const b a)
deriving instance Unbox b => M.MVector MVector (Const b a)
instance Unbox b => Unbox (Const b a)

-- ---
-- Alt
-- ---

newtype instance MVector s (Alt f a) = MV_Alt (MVector s (f a))
newtype instance Vector    (Alt f a) = V_Alt  (Vector    (f a))
deriving instance Unbox (f a) => G.Vector  Vector  (Alt f a)
deriving instance Unbox (f a) => M.MVector MVector (Alt f a)
instance Unbox (f a) => Unbox (Alt f a)

-- -------
-- Compose
-- -------

newtype instance MVector s (Compose f g a) = MV_Compose (MVector s (f (g a)))
newtype instance Vector    (Compose f g a) = V_Compose  (Vector    (f (g a)))
deriving instance Unbox (f (g a)) => G.Vector  Vector  (Compose f g a)
deriving instance Unbox (f (g a)) => M.MVector MVector (Compose f g a)
instance Unbox (f (g a)) => Unbox (Compose f g a)

-- ------
-- Tuples
-- ------

#define DEFINE_INSTANCES
#include "unbox-tuple-instances"
