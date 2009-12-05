{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
module Data.Vector.Unboxed.Unbox (
  MVector(..), Vector(..), Unbox
) where

import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M

import qualified Data.Vector.Primitive as P

import Control.Monad.ST ( runST )
import Control.Monad

import Data.Word ( Word, Word8, Word16, Word32, Word64 )
import Data.Int  ( Int8, Int16, Int32, Int64 )

#include "vector.h"

data family MVector s a
data family Vector    a

type instance G.Mutable Vector = MVector

class (G.Vector Vector a, M.MVector MVector a) => Unbox a

-- ----
-- Unit
-- ----

newtype instance MVector s () = MV_Unit Int
newtype instance Vector    () = V_Unit Int

instance Unbox ()

instance M.MVector MVector () where
  {-# INLINE length #-}
  {-# INLINE unsafeSlice #-}
  {-# INLINE overlaps #-}
  {-# INLINE unsafeNew #-}
  {-# INLINE unsafeRead #-}
  {-# INLINE unsafeWrite #-}
  {-# INLINE clear #-}
  {-# INLINE set #-}
  {-# INLINE unsafeCopy #-}
  {-# INLINE unsafeGrow #-}

  length (MV_Unit n) = n

  unsafeSlice (MV_Unit n) i m
    = UNSAFE_CHECK(checkSlice) "unsafeSlice" i m n
    $ MV_Unit m

  overlaps _ _ = False

  unsafeNew n = UNSAFE_CHECK(checkLength) "unsafeNew" n
              $ return (MV_Unit n)

  unsafeRead (MV_Unit n) i = UNSAFE_CHECK(checkIndex) "unsafeRead" i n
                           $ return ()

  unsafeWrite (MV_Unit n) i () = UNSAFE_CHECK(checkIndex) "unsafeWrite" i n
                               $ return ()

  clear _ = return ()

  set (MV_Unit _) () = return ()

  unsafeCopy (MV_Unit _) (MV_Unit _) = return ()

  unsafeGrow (MV_Unit n) k = return $ MV_Unit (n+k)

instance G.Vector Vector () where
  {-# INLINE unsafeFreeze #-}
  unsafeFreeze (MV_Unit n) = return $ V_Unit n

  {-# INLINE basicLength #-}
  basicLength (V_Unit n) = n

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice (V_Unit n) i m = V_Unit m

  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (V_Unit _) i = return ()


-- ---------------
-- Primitive types
-- ---------------

#define primMVector(ty,con)                                             \
instance M.MVector MVector ty where {                                   \
  {-# INLINE length #-}                                                 \
; {-# INLINE unsafeSlice #-}                                            \
; {-# INLINE overlaps #-}                                               \
; {-# INLINE unsafeNew #-}                                              \
; {-# INLINE unsafeNewWith #-}                                          \
; {-# INLINE unsafeRead #-}                                             \
; {-# INLINE unsafeWrite #-}                                            \
; {-# INLINE clear #-}                                                  \
; {-# INLINE set #-}                                                    \
; {-# INLINE unsafeCopy #-}                                             \
; {-# INLINE unsafeGrow #-}                                             \
; length (con v) = M.length v                                           \
; unsafeSlice (con v) i n = con $ M.unsafeSlice v i n                   \
; overlaps (con v1) (con v2) = M.overlaps v1 v2                         \
; unsafeNew n = con `liftM` M.unsafeNew n                               \
; unsafeNewWith n x = con `liftM` M.unsafeNewWith n x                   \
; unsafeRead (con v) i = M.unsafeRead v i                               \
; unsafeWrite (con v) i x = M.unsafeWrite v i x                         \
; clear (con v) = M.clear v                                             \
; set (con v) x = M.set v x                                             \
; unsafeCopy (con v1) (con v2) = M.unsafeCopy v1 v2                     \
; unsafeGrow (con v) n = con `liftM` M.unsafeGrow v n }

#define primVector(ty,con,mcon)                                         \
instance G.Vector Vector ty where {                                     \
  {-# INLINE unsafeFreeze #-}                                           \
; {-# INLINE basicLength #-}                                            \
; {-# INLINE basicUnsafeSlice #-}                                       \
; {-# INLINE basicUnsafeIndexM #-}                                      \
; unsafeFreeze (mcon v) = con `liftM` G.unsafeFreeze v                  \
; basicLength (con v) = G.basicLength v                                 \
; basicUnsafeSlice (con v) i n = con $ G.basicUnsafeSlice v i n         \
; basicUnsafeIndexM (con v) i = G.basicUnsafeIndexM v i }

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

-- ------
-- Tuples
-- ------

data instance MVector s (a,b) = MV_2 {-# UNPACK #-} !Int
                                                    (MVector s a)
                                                    (MVector s b)
data instance Vector    (a,b) = V_2  {-# UNPACK #-} !Int
                                                    (Vector a)
                                                    (Vector b)

instance (Unbox a, Unbox b) => Unbox (a,b)

instance (Unbox a, Unbox b) => M.MVector MVector (a,b) where
  {-# INLINE length #-}
  {-# INLINE unsafeSlice #-}
  {-# INLINE overlaps #-}
  {-# INLINE unsafeNew #-}
  {-# INLINE unsafeNewWith #-}
  {-# INLINE unsafeRead #-}
  {-# INLINE unsafeWrite #-}
  {-# INLINE clear #-}
  {-# INLINE set #-}
  {-# INLINE unsafeCopy #-}
  {-# INLINE unsafeGrow #-}

  length (MV_2 n as bs) = n

  unsafeSlice (MV_2 n as bs) i m
    = UNSAFE_CHECK(checkSlice) "unsafeSlice" i m n
    $ MV_2 m (M.unsafeSlice as i m)
             (M.unsafeSlice bs i m)

  overlaps (MV_2 _ as1 bs1) (MV_2 _ as2 bs2)
    = M.overlaps as1 as2 || M.overlaps bs1 bs2

  unsafeNew n = liftM2 (MV_2 n) (M.unsafeNew n) (M.unsafeNew n)
  unsafeNewWith n (a,b) = liftM2 (MV_2 n) (M.unsafeNewWith n a)
                                          (M.unsafeNewWith n b)

  unsafeRead (MV_2 _ as bs) i = liftM2 (,) (M.unsafeRead as i)
                                           (M.unsafeRead bs i)

  unsafeWrite (MV_2 _ as bs) i (a,b)
    = do { M.unsafeWrite as i a ; M.unsafeWrite bs i b }

  clear (MV_2 _ as bs) = do { M.clear as ; M.clear bs }

  set (MV_2 _ as bs) (a,b) = do { M.set as a ; M.set bs b }

  unsafeCopy (MV_2 _ as1 bs1) (MV_2 _ as2 bs2)
    = do { M.unsafeCopy as1 as2 ; M.unsafeCopy bs1 bs2 }

  unsafeGrow (MV_2 n as bs) m
    = liftM2 (MV_2 $ m+n) (M.unsafeGrow as m) (M.unsafeGrow bs m)

instance (Unbox a, Unbox b) => G.Vector Vector (a,b) where
  {-# INLINE unsafeFreeze #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}

  unsafeFreeze (MV_2 n as bs)
    = liftM2 (V_2 n) (G.unsafeFreeze as) (G.unsafeFreeze bs)

  basicLength (V_2 n _ _) = n

  basicUnsafeSlice (V_2 _ as bs) i n
    = V_2 n (G.basicUnsafeSlice as i n)
            (G.basicUnsafeSlice bs i n)

  basicUnsafeIndexM (V_2 _ as bs) i
    = liftM2 (,) (G.basicUnsafeIndexM as i)
                 (G.basicUnsafeIndexM bs i)

