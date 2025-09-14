{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
-- |
module Data.Vector.Unsafe
  ( Vector(..)
    -- * Array conversions
  , toArray, fromArray
  , toArraySlice, unsafeFromArraySlice
  ) where

import Data.Vector.Mutable.Unsafe ( MVector(..) )
import Data.Primitive.Array
import qualified Data.Vector.Fusion.Bundle as Bundle
import qualified Data.Vector.Generic as G
import Data.Vector.Generic ((!))

import Control.DeepSeq ( NFData(rnf), NFData1(liftRnf) )

import Control.Monad ( MonadPlus(..), liftM, ap )
import Control.Monad.ST ( runST )
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix ( MonadFix (mfix) )
import Control.Monad.Zip
import Data.Function ( fix )

import Prelude
  ( Eq, Ord, Monoid, Functor, Monad, Show, Ordering(..), Int
  , compare, mempty, mappend, mconcat, return, showsPrec, fmap, otherwise, flip, const
  , (>>=), (+), (-), (<), (<=), (>), (>=), (==), (/=), (&&), (.), ($) )

import Data.Functor.Classes (Eq1 (..), Ord1 (..), Read1 (..), Show1 (..))
import Data.Data      ( Data(..) )
import Text.Read      ( Read(..), readListPrecDefault )
import Data.Semigroup ( Semigroup(..) )

import qualified Control.Applicative as Applicative
import qualified Data.Foldable as Foldable
import qualified Data.Traversable as Traversable

import qualified GHC.Exts as Exts (IsList(..))


-- | Boxed vectors, supporting efficient slicing.
data Vector a = Vector {-# UNPACK #-} !Int
                       {-# UNPACK #-} !Int
                       {-# UNPACK #-} !(Array a)

liftRnfV :: (a -> ()) -> Vector a -> ()
liftRnfV elemRnf = G.foldl' (\_ -> elemRnf) ()

instance NFData a => NFData (Vector a) where
  rnf = liftRnfV rnf
  {-# INLINEABLE rnf #-}

-- | @since 0.12.1.0
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
  toConstr _   = G.mkVecConstr "Data.Vector.Vector"
  gunfold      = G.gunfold
  dataTypeOf _ = G.mkVecType "Data.Vector.Vector"
  dataCast1    = G.dataCast

type instance G.Mutable Vector = MVector

instance G.Vector Vector a where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze (MVector i n marr)
    = Vector i n `liftM` unsafeFreezeArray marr

  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw (Vector i n arr)
    = MVector i n `liftM` unsafeThawArray arr

  {-# INLINE basicLength #-}
  basicLength (Vector _ n _) = n

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice j n (Vector i _ arr) = Vector (i+j) n arr

  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (Vector i _ arr) j = indexArrayM arr (i+j)

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MVector i n dst) (Vector j _ src)
    = copyArray dst i src j n

-- See http://trac.haskell.org/vector/ticket/12
instance Eq a => Eq (Vector a) where
  {-# INLINE (==) #-}
  xs == ys = Bundle.eq (G.stream xs) (G.stream ys)

-- See http://trac.haskell.org/vector/ticket/12
instance Ord a => Ord (Vector a) where
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

instance Eq1 Vector where
  {-# INLINE liftEq #-}
  liftEq = G.eqBy

instance Ord1 Vector where
  {-# INLINE liftCompare #-}
  liftCompare = G.cmpBy

instance Semigroup (Vector a) where
  {-# INLINE (<>) #-}
  (<>) = (G.++)

  {-# INLINE sconcat #-}
  sconcat = G.concatNE

instance Monoid (Vector a) where
  {-# INLINE mempty #-}
  mempty = G.empty

  {-# INLINE mappend #-}
  mappend = (<>)

  {-# INLINE mconcat #-}
  mconcat = G.concat

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


-- | @since 0.12.1.0
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
--  @since 0.12.2.0
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

instance Foldable.Foldable Vector where
  {-# INLINE foldr #-}
  foldr = G.foldr

  {-# INLINE foldl #-}
  foldl = G.foldl

  {-# INLINE foldr1 #-}
  foldr1 = G.foldr1

  {-# INLINE foldl1 #-}
  foldl1 = G.foldl1

  {-# INLINE foldr' #-}
  foldr' = G.foldr'

  {-# INLINE foldl' #-}
  foldl' = G.foldl'

  {-# INLINE toList #-}
  toList = G.toList

  {-# INLINE length #-}
  length = G.length

  {-# INLINE null #-}
  null = G.null

  {-# INLINE elem #-}
  elem = G.elem

  {-# INLINE maximum #-}
  maximum = G.maximum

  {-# INLINE minimum #-}
  minimum = G.minimum

  {-# INLINE sum #-}
  sum = G.sum

  {-# INLINE product #-}
  product = G.product

instance Traversable.Traversable Vector where
  {-# INLINE traverse #-}
  traverse = G.traverse
  
  {-# INLINE mapM #-}
  mapM = G.mapM
  
  {-# INLINE sequence #-}
  sequence = G.sequence


-- Conversions - Arrays
-- -----------------------------

-- | /O(1)/ Convert an array to a vector.
--
-- @since 0.12.2.0
fromArray :: Array a -> Vector a
{-# INLINE fromArray #-}
fromArray arr = Vector 0 (sizeofArray arr) arr

-- | /O(n)/ Convert a vector to an array.
--
-- @since 0.12.2.0
toArray :: Vector a -> Array a
{-# INLINE toArray #-}
toArray (Vector offset len arr)
  | offset == 0 && len == sizeofArray arr = arr
  | otherwise = cloneArray arr offset len

-- | /O(1)/ Extract the underlying `Array`, offset where vector starts and the
-- total number of elements in the vector. Below property always holds:
--
-- > let (array, offset, len) = toArraySlice v
-- > v === unsafeFromArraySlice len offset array
--
-- @since 0.13.0.0
toArraySlice :: Vector a -> (Array a, Int, Int)
{-# INLINE toArraySlice #-}
toArraySlice (Vector offset len arr) = (arr, offset, len)


-- | /O(1)/ Convert an array slice to a vector. This function is very unsafe,
-- because constructing an invalid vector can yield almost all other safe
-- functions in this module unsafe. These are equivalent:
--
-- > unsafeFromArraySlice len offset === unsafeTake len . unsafeDrop offset . fromArray
--
-- @since 0.13.0.0
unsafeFromArraySlice ::
     Array a -- ^ Immutable boxed array.
  -> Int -- ^ Offset
  -> Int -- ^ Length
  -> Vector a
{-# INLINE unsafeFromArraySlice #-}
unsafeFromArraySlice arr offset len = Vector offset len arr
