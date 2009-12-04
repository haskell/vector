{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
module Data.Vector.Unboxed.Unbox (
  MVector(..), Vector(..), Unbox
) where

import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M

import qualified Data.Vector.Primitive as P

import Control.Monad.ST ( runST )
import Control.Monad

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

instance M.MVector MVector () where
  length (MV_Unit n) = n

  unsafeSlice (MV_Unit n) i m
    = UNSAFE_CHECK(checkSlice) "unsafeSlice" i m n
    $ MV_Unit m

  {-# INLINE overlaps #-}
  overlaps _ _ = False

  {-# INLINE unsafeNew #-}
  unsafeNew n = UNSAFE_CHECK(checkLength) "unsafeNew" n
              $ return (MV_Unit n)

  {-# INLINE unsafeRead #-}
  unsafeRead (MV_Unit n) i = UNSAFE_CHECK(checkIndex) "unsafeRead" i n
                           $ return ()

  {-# INLINE unsafeWrite #-}
  unsafeWrite (MV_Unit n) i () = UNSAFE_CHECK(checkIndex) "unsafeWrite" i n
                               $ return ()

  {-# INLINE clear #-}
  clear _ = return ()

  {-# INLINE set #-}
  set (MV_Unit _) () = return ()

  {-# INLINE unsafeCopy #-}
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

instance Unbox ()


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
newtype instance MVector s Float = MV_Float (P.MVector s Float)
newtype instance Vector    Float = V_Float  (P.Vector    Float)

primMVector(Int, MV_Int)
primMVector(Float, MV_Float)

primVector(Int, V_Int, MV_Int)

-- ------
-- Tuples
-- ------

data instance MVector s (a,b) = MV_2 {-# UNPACK #-} !Int
                                                    (MVector s a)
                                                    (MVector s b)
data instance Vector    (a,b) = V_2  {-# UNPACK #-} !Int
                                                    (Vector a)
                                                    (Vector b)

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

