{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
module Data.Vector.Primitive.Unsafe
  ( -- * Mutable vector
    Vector(..)
  , unsafeCoerceVector
  , unsafeCast
  ) where

import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Fusion.Bundle   as Bundle
import           Data.Data
import           Data.Semigroup (Semigroup(..))
import           Data.Monoid (Monoid(..))
import           Data.Primitive.ByteArray
import           Data.Primitive ( Prim, sizeOf )
import           Control.Monad ( liftM )
import           Text.Read ( Read(..), readListPrecDefault )
import qualified GHC.Exts as Exts
import           GHC.Stack (HasCallStack)

import Control.DeepSeq ( NFData(rnf), NFData1(liftRnf) )

import Prelude
  ( Ord, Int, Ordering(..), Monad(..), Eq(..), Ord(..)
  , undefined, Show(..), seq, otherwise, error
  , (+), (*), ($!))

import Data.Coerce
import Unsafe.Coerce

import Data.Vector.Primitive.Mutable.Unsafe (MVector(..))
----------------------------------------------------------------
-- Immutable
----------------------------------------------------------------

type role Vector nominal

-- | Unboxed vectors of primitive types.
data Vector a = Vector {-# UNPACK #-} !Int       -- ^ offset
                       {-# UNPACK #-} !Int       -- ^ length
                       {-# UNPACK #-} !ByteArray -- ^ underlying byte array

type instance G.Mutable Vector = MVector

-- | /O(1)/ Unsafely coerce an immutable vector from one element type to another,
-- representationally equal type. The operation just changes the type of the
-- underlying pointer and does not modify the elements.
--
-- This is marginally safer than 'unsafeCast', since this function imposes an
-- extra 'Coercible' constraint. The constraint guarantees that the element types
-- are representationally equal. It however cannot guarantee
-- that their respective 'Prim' instances are compatible.
unsafeCoerceVector :: Coercible a b => Vector a -> Vector b
unsafeCoerceVector = unsafeCoerce

-- | /O(1)/ Unsafely cast a vector from one element type to another.
-- This operation just changes the type of the vector and does not
-- modify the elements.
--
-- This function will throw an error if elements are of mismatching sizes.
--
-- | @since 0.13.0.0
unsafeCast :: forall a b. (HasCallStack, Prim a, Prim b) => Vector a -> Vector b
{-# INLINE unsafeCast #-}
unsafeCast (Vector o n ba)
  | sizeOf (undefined :: a) == sizeOf (undefined :: b) = Vector o n ba
  | otherwise = error "Element size mismatch"


instance Prim a => G.Vector Vector a where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze (MVector i n marr)
    = Vector i n `liftM` unsafeFreezeByteArray marr

  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw (Vector i n arr)
    = MVector i n `liftM` unsafeThawByteArray arr

  {-# INLINE basicLength #-}
  basicLength (Vector _ n _) = n

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice j n (Vector i _ arr) = Vector (i+j) n arr

  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (Vector i _ arr) j = return $! indexByteArray arr (i+j)

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MVector i n dst) (Vector j _ src)
    = copyByteArray dst (i*sz) src (j*sz) (n*sz)
    where
      sz = sizeOf (undefined :: a)

  {-# INLINE elemseq #-}
  elemseq _ = seq


instance NFData (Vector a) where
  rnf (Vector _ _ _) = ()

-- | @since 0.12.1.0
instance NFData1 Vector where
  liftRnf _ (Vector _ _ _) = ()

instance (Show a, Prim a) => Show (Vector a) where
  showsPrec = G.showsPrec

instance (Read a, Prim a) => Read (Vector a) where
  readPrec = G.readPrec
  readListPrec = readListPrecDefault

instance (Data a, Prim a) => Data (Vector a) where
  gfoldl       = G.gfoldl
  toConstr _   = G.mkVecConstr "Data.Vector.Primitive.Vector"
  gunfold      = G.gunfold
  dataTypeOf _ = G.mkVecType "Data.Vector.Primitive.Vector"
  dataCast1    = G.dataCast


-- See http://trac.haskell.org/vector/ticket/12
instance (Prim a, Eq a) => Eq (Vector a) where
  {-# INLINE (==) #-}
  xs == ys = Bundle.eq (G.stream xs) (G.stream ys)

-- See http://trac.haskell.org/vector/ticket/12
instance (Prim a, Ord a) => Ord (Vector a) where
  {-# INLINE compare #-}
  compare xs ys = Bundle.cmp (G.stream xs) (G.stream ys)

  {-# INLINE (<) #-}
  xs < ys = Bundle.cmp (G.stream xs) (G.stream ys) == LT

  {-# INLINE (<=) #-}
  xs <= ys = Bundle.cmp (G.stream xs) (G.stream ys) /= GT

  {-# INLINE (>) #-}
  xs > ys = Bundle.cmp (G.stream xs) (G.stream ys) == GT

  {-# INLINE (>=) #-}
  xs >= ys = Bundle.cmp (G.stream xs) (G.stream ys) /= LT

instance Prim a => Semigroup (Vector a) where
  {-# INLINE (<>) #-}
  (<>) = (G.++)

  {-# INLINE sconcat #-}
  sconcat = G.concatNE

instance Prim a => Monoid (Vector a) where
  {-# INLINE mempty #-}
  mempty = G.empty

  {-# INLINE mappend #-}
  mappend = (<>)

  {-# INLINE mconcat #-}
  mconcat = G.concat

instance Prim a => Exts.IsList (Vector a) where
  type Item (Vector a) = a
  fromList = G.fromList
  fromListN = G.fromListN
  toList = G.toList
