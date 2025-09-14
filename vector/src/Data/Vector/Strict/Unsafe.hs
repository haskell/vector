{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
module Data.Vector.Strict.Unsafe
  ( Vector(..)
    -- * Vector conversions
  , toLazy
  , fromLazy
    -- * Array conversions
  , toArray
  , fromArray
  , toArraySlice
  , unsafeFromArraySlice
  ) where


import Data.Coerce
import Data.Vector.Strict.Mutable  ( MVector(..) )
import Data.Primitive.Array
import qualified Data.Vector.Generic as G
import Data.Vector.Generic ((!))
import qualified Data.Vector as V

import Control.DeepSeq ( NFData(rnf), NFData1(liftRnf))

import Control.Monad ( MonadPlus(..), ap )
import Control.Monad.ST ( runST )
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix ( MonadFix (mfix) )
import Control.Monad.Zip
import Data.Function ( fix )

import Prelude
  ( Eq(..), Ord(..), Monoid, Functor, Monad, Show, Int
  , return, showsPrec, fmap, otherwise, flip, const
  , (>>=), (+), (-), (.), ($), seq)

import Data.Functor.Classes (Eq1 (..), Ord1 (..), Read1 (..), Show1 (..))
import Data.Data      ( Data(..) )
import Text.Read      ( Read(..), readListPrecDefault )
import Data.Semigroup ( Semigroup(..) )

import qualified Control.Applicative as Applicative
import qualified Data.Foldable as Foldable
import qualified Data.Traversable as Traversable

import qualified GHC.Exts as Exts (IsList(..))

-- | Strict boxed vectors, supporting efficient slicing.
newtype Vector a = Vector (V.Vector a)
  deriving (Foldable.Foldable, Semigroup, Monoid)

-- NOTE: [GND for strict vector]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Strict boxed vectors (both mutable an immutable) are newtypes over
-- lazy ones. This makes it possible to use GND to derive instances.
-- However one must take care to preserve strictness since Vector
-- instance for lazy vectors would be used.
--
-- In general it's OK to derive instances where vectors are passed as
-- parameters (e.g. Eq, Ord) and not OK to derive ones where new
-- vector is created (e.g. Read, Functor)

liftRnfV :: (a -> ()) -> Vector a -> ()
liftRnfV elemRnf = G.foldl' (\_ -> elemRnf) ()

instance NFData a => NFData (Vector a) where
  rnf = liftRnfV rnf
  {-# INLINEABLE rnf #-}

-- | @since 0.13.2.0
instance NFData1 Vector where
  liftRnf = liftRnfV
  {-# INLINEABLE liftRnf #-}

instance Show a => Show (Vector a) where
  showsPrec = G.showsPrec

instance Read a => Read (Vector a) where
  readPrec = G.readPrec
  readListPrec = readListPrecDefault

instance Show1 Vector where
  liftShowsPrec = G.liftShowsPrec

instance Read1 Vector where
  liftReadsPrec = G.liftReadsPrec

instance Exts.IsList (Vector a) where
  type Item (Vector a) = a
  fromList = G.fromList
  fromListN = G.fromListN
  toList = G.toList

instance Data a => Data (Vector a) where
  gfoldl       = G.gfoldl
  toConstr _   = G.mkVecConstr "Data.Vector.Strict.Vector"
  gunfold      = G.gunfold
  dataTypeOf _ = G.mkVecType "Data.Vector.Strict.Vector"
  dataCast1    = G.dataCast

type instance G.Mutable Vector = MVector

instance G.Vector Vector a where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze = coerce (G.basicUnsafeFreeze @V.Vector @a)
  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw = coerce (G.basicUnsafeThaw @V.Vector @a)
  {-# INLINE basicLength #-}
  basicLength = coerce (G.basicLength @V.Vector @a)
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice = coerce (G.basicUnsafeSlice @V.Vector @a)
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM = coerce (G.basicUnsafeIndexM @V.Vector @a)
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy = coerce (G.basicUnsafeCopy @V.Vector @a)
  {-# INLINE elemseq #-}
  elemseq _ = seq

-- See NOTE: [GND for strict vector]
--
-- Deriving strategies are only available since 8.2. So we can't use
-- deriving newtype until we drop support for 8.0
instance Eq a => Eq (Vector a) where
  {-# INLINE (==) #-}
  (==) = coerce ((==) @(V.Vector a))

-- See NOTE: [GND for strict vector]
instance Ord a => Ord (Vector a) where
  {-# INLINE compare #-}
  compare = coerce (compare @(V.Vector a))
  {-# INLINE (<) #-}
  (<)  = coerce ((<)  @(V.Vector a))
  {-# INLINE (<=) #-}
  (<=) = coerce ((<=) @(V.Vector a))
  {-# INLINE (>) #-}
  (>)  = coerce ((>)  @(V.Vector a))
  {-# INLINE (>=) #-}
  (>=) = coerce ((>=) @(V.Vector a))

instance Eq1 Vector where
  {-# INLINE liftEq #-}
  liftEq = G.eqBy

instance Ord1 Vector where
  {-# INLINE liftCompare #-}
  liftCompare = G.cmpBy

instance Functor Vector where
  {-# INLINE fmap #-}
  fmap = G.map

  {-# INLINE (<$) #-}
  (<$) = G.map . const

instance Monad Vector where
  {-# INLINE return #-}
  return = Applicative.pure

  {-# INLINE (>>=) #-}
  (>>=) = flip G.concatMap

-- | @since 0.13.2.0
instance Fail.MonadFail Vector where
  {-# INLINE fail #-}
  fail _ = G.empty

instance MonadPlus Vector where
  {-# INLINE mzero #-}
  mzero = G.empty

  {-# INLINE mplus #-}
  mplus = (G.++)

instance MonadZip Vector where
  {-# INLINE mzip #-}
  mzip = G.zip

  {-# INLINE mzipWith #-}
  mzipWith = G.zipWith

  {-# INLINE munzip #-}
  munzip = G.unzip

-- | This instance has the same semantics as the one for lists.
--
--  @since 0.13.2.0
instance MonadFix Vector where
  -- We take care to dispose of v0 as soon as possible (see headM docs).
  --
  -- It's perfectly safe to use non-monadic indexing within generate
  -- call since intermediate vector won't be created until result's
  -- value is demanded.
  {-# INLINE mfix #-}
  mfix f
    | G.null v0 = G.empty
    -- We take first element of resulting vector from v0 and create
    -- rest using generate. Note that cons should fuse with generate
    | otherwise = runST $ do
        h <- G.headM v0
        return $ G.cons h $
          G.generate (lv0 - 1) $
            \i -> fix (\a -> f a ! (i + 1))
    where
      -- Used to calculate size of resulting vector
      v0 = fix (f . G.head)
      !lv0 = G.length v0

instance Applicative.Applicative Vector where
  {-# INLINE pure #-}
  pure = G.singleton

  {-# INLINE (<*>) #-}
  (<*>) = ap

instance Applicative.Alternative Vector where
  {-# INLINE empty #-}
  empty = G.empty

  {-# INLINE (<|>) #-}
  (<|>) = (G.++)

instance Traversable.Traversable Vector where
  {-# INLINE traverse #-}
  traverse = G.traverse

  {-# INLINE mapM #-}
  mapM = G.mapM

  {-# INLINE sequence #-}
  sequence = G.sequence


-- Conversions - Lazy vectors
-- -----------------------------

-- | /O(1)/ Convert strict array to lazy array
toLazy :: Vector a -> V.Vector a
toLazy (Vector v) = v

-- | /O(n)/ Convert lazy array to strict array. This function reduces
-- each element of vector to WHNF.
fromLazy :: V.Vector a -> Vector a
fromLazy vec = liftRnf (`seq` ()) v `seq` v where v = Vector vec


-- Conversions - Arrays
-- -----------------------------

-- | /O(n)/ Convert an array to a vector and reduce each element to WHNF.
--
-- @since 0.13.2.0
fromArray :: Array a -> Vector a
{-# INLINE fromArray #-}
fromArray arr = liftRnf (`seq` ()) vec `seq` vec
  where
    vec = Vector $ V.fromArray arr

-- | /O(n)/ Convert a vector to an array.
--
-- @since 0.13.2.0
toArray :: Vector a -> Array a
{-# INLINE toArray #-}
toArray (Vector v) = V.toArray v

-- | /O(1)/ Extract the underlying `Array`, offset where vector starts and the
-- total number of elements in the vector. Below property always holds:
--
-- > let (array, offset, len) = toArraySlice v
-- > v === unsafeFromArraySlice len offset array
--
-- @since 0.13.2.0
toArraySlice :: Vector a -> (Array a, Int, Int)
{-# INLINE toArraySlice #-}
toArraySlice (Vector v) = V.toArraySlice v


-- | /O(n)/ Convert an array slice to a vector and reduce each element to WHNF.
--
-- This function is very unsafe, because constructing an invalid
-- vector can yield almost all other safe functions in this module
-- unsafe. These are equivalent:
--
-- > unsafeFromArraySlice len offset === unsafeTake len . unsafeDrop offset . fromArray
--
-- @since 0.13.2.0
unsafeFromArraySlice ::
     Array a -- ^ Immutable boxed array.
  -> Int -- ^ Offset
  -> Int -- ^ Length
  -> Vector a
{-# INLINE unsafeFromArraySlice #-}
unsafeFromArraySlice arr offset len = liftRnf (`seq` ()) vec `seq` vec
  where vec = Vector (V.unsafeFromArraySlice arr offset len)
