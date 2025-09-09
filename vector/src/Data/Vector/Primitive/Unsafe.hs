{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
module Data.Vector.Primitive.Unsafe
  ( -- * Mutable vector
    MVector(..)
  , IOVector
  , STVector
  , unsafeCoerceMVector
  , unsafeCast
    -- * Immutable vector
  , Vector(..)
  , unsafeCoerceVector
  ) where

import qualified Data.Vector.Generic.Mutable as MG
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Fusion.Bundle   as Bundle
import           Data.Data
import           Data.Semigroup (Semigroup(..))
import           Data.Monoid (Monoid(..))
import           Data.Primitive.ByteArray
import           Data.Primitive ( Prim, sizeOf )
import           Data.Word ( Word8 )
import           Control.Monad.Primitive
import           Control.Monad ( liftM )
import           Text.Read ( Read(..), readListPrecDefault )
import qualified GHC.Exts as Exts
import           GHC.Stack (HasCallStack)

import Control.DeepSeq ( NFData(rnf)
#if MIN_VERSION_deepseq(1,4,3)
                       , NFData1(liftRnf)
#endif
                       )

import Prelude
  ( Ord, Int, Ordering(..), Monad(..), Eq(..), Ord(..)
  , otherwise, error, undefined, div, Show(..),  maxBound, seq
  , (+), (*), (&&), (||), ($), ($!), (++) )

import Data.Coerce
import Unsafe.Coerce


----------------------------------------------------------------
-- Mutable
----------------------------------------------------------------

type role MVector nominal nominal

-- | Mutable vectors of primitive types.
data MVector s a = MVector {-# UNPACK #-} !Int                  -- ^ offset
                           {-# UNPACK #-} !Int                  -- ^ length
                           {-# UNPACK #-} !(MutableByteArray s) -- ^ underlying mutable byte array

type IOVector = MVector RealWorld
type STVector s = MVector s

-- | /O(1)/ Unsafely coerce a mutable vector from one element type to another,
-- representationally equal type. The operation just changes the type of the
-- underlying pointer and does not modify the elements.
--
-- Note that this function is unsafe. The @Coercible@ constraint guarantees
-- that the element types are representationally equal. It however cannot
-- guarantee that their respective 'Prim' instances are compatible.
unsafeCoerceMVector :: Coercible a b => MVector s a -> MVector s b
unsafeCoerceMVector = unsafeCoerce

-- | /O(1)/ Unsafely cast a vector from one element type to another.
-- This operation just changes the type of the vector and does not
-- modify the elements.
--
-- This function will throw an error if elements are of mismatching sizes.
--
-- | @since 0.13.0.0
unsafeCast :: forall a b s. (HasCallStack, Prim a, Prim b) => MVector s a -> MVector s b
{-# INLINE unsafeCast #-}
unsafeCast (MVector o n ba)
  | sizeOf (undefined :: a) == sizeOf (undefined :: b) = MVector o n ba
  | otherwise = error "Element size mismatch"



instance NFData (MVector s a) where
  rnf (MVector _ _ _) = ()

#if MIN_VERSION_deepseq(1,4,3)
instance NFData1 (MVector s) where
  liftRnf _ (MVector _ _ _) = ()
#endif

instance Prim a => MG.MVector MVector a where
  basicLength (MVector _ n _) = n
  basicUnsafeSlice j m (MVector i _ arr)
    = MVector (i+j) m arr

  {-# INLINE basicOverlaps #-}
  basicOverlaps (MVector i m arr1) (MVector j n arr2)
    = sameMutableByteArray arr1 arr2
      && (between i j (j+n) || between j i (i+m))
    where
      between x y z = x >= y && x < z

  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew n
    | n < 0 = error $ "Primitive.basicUnsafeNew: negative length: " ++ show n
    | n > mx = error $ "Primitive.basicUnsafeNew: length too large: " ++ show n
    | otherwise = MVector 0 n `liftM` newByteArray (n * size)
    where
      size = sizeOf (undefined :: a)
      mx = maxBound `div` size :: Int

  {-# INLINE basicInitialize #-}
  basicInitialize (MVector off n v) =
      setByteArray v (off * size) (n * size) (0 :: Word8)
    where
      size = sizeOf (undefined :: a)


  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (MVector i _ arr) j = readByteArray arr (i+j)

  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (MVector i _ arr) j x = writeByteArray arr (i+j) x

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MVector i n dst) (MVector j _ src)
    = copyMutableByteArray dst (i*sz) src (j*sz) (n*sz)
    where
      sz = sizeOf (undefined :: a)

  {-# INLINE basicUnsafeMove #-}
  basicUnsafeMove (MVector i n dst) (MVector j _ src)
    = moveByteArray dst (i*sz) src (j*sz) (n * sz)
    where
      sz = sizeOf (undefined :: a)

  {-# INLINE basicSet #-}
  basicSet (MVector i n arr) x = setByteArray arr i n x



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

#if MIN_VERSION_deepseq(1,4,3)
-- | @since 0.12.1.0
instance NFData1 Vector where
  liftRnf _ (Vector _ _ _) = ()
#endif

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
