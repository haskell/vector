{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module      : Data.Stream.Monadic
-- Copyright   : (c) Roman Leshchinskiy 2008-2010
--                   Alexey Kuleshevich 2020-2022
--                   Aleksey Khudyakov 2020-2022
--                   Andrew Lelechenko 2020-2022
-- License     : BSD-style
--
-- Maintainer  : Haskell Libraries Team <libraries@haskell.org>
-- Stability   : experimental
-- Portability : non-portable
--
-- Monadic stream combinators.
--

module Data.Stream.Monadic (
  -- * Box monad
  Box(..), liftBox,
  -- * Stream
  Stream(..), Step(..), SPEC(..),

  -- ** Length
  length, null,

  -- ** Construction
  empty, singleton, cons, snoc, replicate, replicateM, generate, generateM, (++),

  -- ** Accessing elements
  head, last, (!!), (!?),

  -- ** Substreams
  slice, init, tail, take, drop,

  -- ** Mapping
  map, mapM, mapM_, trans, unbox, concatMap, flatten,

  -- ** Zipping
  indexed, indexedR, zipWithM_,
  zipWithM, zipWith3M, zipWith4M, zipWith5M, zipWith6M,
  zipWith, zipWith3, zipWith4, zipWith5, zipWith6,
  zip, zip3, zip4, zip5, zip6,

  -- ** Comparisons
  eqBy, cmpBy,

  -- ** Filtering
  filter, filterM, uniq, mapMaybe, mapMaybeM, catMaybes, takeWhile, takeWhileM, dropWhile, dropWhileM,

  -- ** Searching
  elem, notElem, find, findM, findIndex, findIndexM,

  -- ** Folding
  foldl, foldlM, foldl1, foldl1M, foldM, fold1M,
  foldl', foldlM', foldl1', foldl1M', foldM', fold1M',
  foldr, foldrM, foldr1, foldr1M,

  -- ** Specialised folds
  and, or, concatMapM,

  -- ** Unfolding
  unfoldr, unfoldrM,
  unfoldrN, unfoldrNM,
  unfoldrExactN, unfoldrExactNM,
  iterateN, iterateNM,

  -- ** Scans
  prescanl, prescanlM, prescanl', prescanlM',
  postscanl, postscanlM, postscanl', postscanlM',
  scanl, scanlM, scanl', scanlM',
  scanl1, scanl1M, scanl1', scanl1M',

  -- ** Enumerations
  enumFromStepN, enumFromTo, enumFromThenTo,

  -- ** Conversions
  toList, fromList, fromListN
) where

import Data.Char      ( ord )
import GHC.Base       ( unsafeChr )
import Control.Monad  ( liftM )
import qualified Prelude
import Prelude
  ( Functor, Applicative, Monad, Char, String, Int, Word, Integer, Float, Double
  , Bool(..), Ordering(..), Maybe(..), Either(..), Eq, Ord, Enum, Num, Integral
  , RealFrac, return, pure, otherwise, seq, error, not, id, show, const, fmap
  , (==), (<), (<=), (>), (+), (-), (/), ($), (.), (=<<), (>>=) )

import Data.Int  ( Int8, Int16, Int32, Int64 )
import Data.Word ( Word8, Word16, Word32, Word64 )

import GHC.Stack (HasCallStack)
import GHC.Types ( SPEC(..) )

#define INLINE_FUSED INLINE [1]
#define INLINE_INNER INLINE [0]



-- | Box monad
data Box a = Box { unBox :: a }

instance Functor Box where
  fmap f (Box x) = Box (f x)

instance Applicative Box where
  pure = Box
  Box f <*> Box x = Box (f x)

instance Monad Box where
  return = pure
  Box x >>= f = f x

liftBox :: Monad m => Box a -> m a
liftBox (Box a) = return a
{-# INLINE liftBox #-}


emptyStream :: String
{-# NOINLINE emptyStream #-}
emptyStream = "empty stream"


-- | Result of taking a single step in a stream
data Step s a where
  Yield :: a -> s -> Step s a
  Done  :: Step s a

instance Functor (Step s) where
  {-# INLINE fmap #-}
  fmap f (Yield x s) = Yield (f x) s
  fmap _ Done = Done
  {-# INLINE (<$) #-}
  (<$) = fmap . const

-- | Monadic streams
data Stream m a = forall s. Stream (s -> m (Step s a)) s

-- Length
-- ------

-- | Length of a 'Stream'
length :: Monad m => Stream m a -> m Int
{-# INLINE_FUSED length #-}
length = foldl' (\n _ -> n+1) 0

-- | Check if a 'Stream' is empty
null :: Monad m => Stream m a -> m Bool
{-# INLINE_FUSED null #-}
null (Stream step t) = null_loop t
  where
    null_loop s = do
      r <- step s
      case r of
        Yield _ _ -> return False
        Done      -> return True

-- Construction
-- ------------

-- | Empty 'Stream'
empty :: Monad m => Stream m a
{-# INLINE_FUSED empty #-}
empty = Stream (const (return Done)) ()

-- | Singleton 'Stream'
singleton :: Monad m => a -> Stream m a
{-# INLINE_FUSED singleton #-}
singleton x = Stream (return . step) True
  where
    {-# INLINE_INNER step #-}
    step True  = Yield x False
    step False = Done

-- | Replicate a value to a given length
replicate :: Monad m => Int -> a -> Stream m a
{-# INLINE_FUSED replicate #-}
replicate n x = replicateM n (return x)

-- | Yield a 'Stream' of values obtained by performing the monadic action the
-- given number of times
replicateM :: Monad m => Int -> m a -> Stream m a
{-# INLINE_FUSED replicateM #-}
replicateM n p = Stream step n
  where
    {-# INLINE_INNER step #-}
    step i | i <= 0    = return Done
           | otherwise = do { x <- p; return $ Yield x (i-1) }

generate :: Monad m => Int -> (Int -> a) -> Stream m a
{-# INLINE generate #-}
generate n f = generateM n (return . f)

-- | Generate a stream from its indices
generateM :: Monad m => Int -> (Int -> m a) -> Stream m a
{-# INLINE_FUSED generateM #-}
generateM n f = n `seq` Stream step 0
  where
    {-# INLINE_INNER step #-}
    step i | i < n     = do
                           x <- f i
                           return $ Yield x (i+1)
           | otherwise = return Done

-- | Prepend an element
cons :: Monad m => a -> Stream m a -> Stream m a
{-# INLINE cons #-}
cons x s = singleton x ++ s

-- | Append an element
snoc :: Monad m => Stream m a -> a -> Stream m a
{-# INLINE snoc #-}
snoc s x = s ++ singleton x

infixr 5 ++
-- | Concatenate two 'Stream's
(++) :: Monad m => Stream m a -> Stream m a -> Stream m a
{-# INLINE_FUSED (++) #-}
Stream stepa ta ++ Stream stepb tb = Stream step (Left ta)
  where
    {-# INLINE_INNER step #-}
    step (Left  sa) = do
      r <- stepa sa
      case r of
        Yield x sa' -> return $ Yield x (Left  sa')
        Done        -> step' tb
    step (Right sb) = step' sb

    {-# INLINE_INNER step' #-}
    step' s = do
      r <- stepb s
      case r of
        Yield x s' -> return $ Yield x (Right s')
        Done       -> return $ Done

-- Accessing elements
-- ------------------

-- | First element of the 'Stream' or error if empty
head :: (HasCallStack, Monad m) => Stream m a -> m a
{-# INLINE_FUSED head #-}
head (Stream step t) = head_loop SPEC t
  where
    head_loop !_ s
      = do
          r <- step s
          case r of
            Yield x _  -> return x
            Done       -> error emptyStream



-- | Last element of the 'Stream' or error if empty
last :: (HasCallStack, Monad m) => Stream m a -> m a
{-# INLINE_FUSED last #-}
last (Stream step t) = last_loop0 SPEC t
  where
    last_loop0 !_ s
      = do
          r <- step s
          case r of
            Yield x s' -> last_loop1 SPEC x s'
            Done       -> error emptyStream

    last_loop1 !_ x s
      = do
          r <- step s
          case r of
            Yield y s' -> last_loop1 SPEC y s'
            Done       -> return x

infixl 9 !!
-- | Element at the given position
(!!) :: (HasCallStack, Monad m) => Stream m a -> Int -> m a
{-# INLINE (!!) #-}
Stream step t !! j | j < 0     = error $ "negative index (" Prelude.++ show j Prelude.++ ")"
                   | otherwise = index_loop SPEC t j
  where
    index_loop !_ s i
      = i `seq`
        do
          r <- step s
          case r of
            Yield x s' | i == 0    -> return x
                       | otherwise -> index_loop SPEC s' (i-1)
            Done                   -> error emptyStream

infixl 9 !?
-- | Element at the given position or 'Nothing' if out of bounds
(!?) :: Monad m => Stream m a -> Int -> m (Maybe a)
{-# INLINE (!?) #-}
Stream step t !? j = index_loop SPEC t j
  where
    index_loop !_ s i
      = i `seq`
        do
          r <- step s
          case r of
            Yield x s' | i == 0    -> return (Just x)
                       | otherwise -> index_loop SPEC s' (i-1)
            Done                   -> return Nothing

-- Substreams
-- ----------

-- | Extract a substream of the given length starting at the given position.
slice :: Monad m => Int   -- ^ starting index
                 -> Int   -- ^ length
                 -> Stream m a
                 -> Stream m a
{-# INLINE slice #-}
slice i n s = take n (drop i s)

-- | All but the last element
init :: (HasCallStack, Monad m) => Stream m a -> Stream m a
{-# INLINE_FUSED init #-}
init (Stream step t) = Stream step' (Nothing, t)
  where
    {-# INLINE_INNER step' #-}
    step' (Nothing, s) = do
                           r <- step s
                           case r of
                             Yield x s' -> step'' x s'
                             Done       -> return (error emptyStream)

    step' (Just x,  s) = step'' x s

    {-# INLINE_INNER step'' #-}
    step'' x s = liftM (\r ->
                         case r of
                           Yield y s' -> Yield x (Just y, s')
                           Done       -> Done
                       ) (step s)

-- | All but the first element
tail :: (HasCallStack, Monad m) => Stream m a -> Stream m a
{-# INLINE_FUSED tail #-}
tail (Stream step t) = Stream step' (Left t)
  where
    {-# INLINE_INNER step' #-}
    step' (Left  s) = do
                        r <- step s
                        case r of
                          Yield _ s' -> step'' s'
                          Done       -> return (error emptyStream)
    step' (Right s) = step'' s

    {-# INLINE_INNER step'' #-}
    step'' s = liftM (\r ->
                       case r of
                         Yield x s' -> Yield x (Right s')
                         Done       -> Done
                     ) (step s)

-- | The first @n@ elements
take :: Monad m => Int -> Stream m a -> Stream m a
{-# INLINE_FUSED take #-}
take n (Stream step t) = n `seq` Stream step' (t, 0)
  where
    {-# INLINE_INNER step' #-}
    step' (s, i) | i < n = liftM (\r ->
                             case r of
                               Yield x s' -> Yield x (s', i+1)
                               Done       -> Done
                           ) (step s)
    step' (_, _) = return Done

-- | All but the first @n@ elements
drop :: Monad m => Int -> Stream m a -> Stream m a
{-# INLINE_FUSED drop #-}
drop n (Stream step t) = Stream step' (t, n)
  where
    {-# INLINE_INNER step' #-}
    step' (s, i) | i > 0 = go s i
    step' (s, _) = step'' s

    -- go is a recursive join point
    {-# INLINABLE go #-}
    go s i | i > 0 = do
                       r <- step s
                       case r of
                         Yield _ s' -> go s' (i-1)
                         Done -> return Done
           | otherwise = step'' s


    {-# INLINE_INNER step'' #-}
    step'' s = liftM (\r ->
                       case r of
                         Yield x s' -> Yield x (s', 0)
                         Done       -> Done
                       ) (step s)

-- Mapping
-- -------

instance Monad m => Functor (Stream m) where
  {-# INLINE fmap #-}
  fmap = map

-- | Map a function over a 'Stream'
map :: Monad m => (a -> b) -> Stream m a -> Stream m b
{-# INLINE map #-}
map f = mapM (return . f)


-- | Map a monadic function over a 'Stream'
mapM :: Monad m => (a -> m b) -> Stream m a -> Stream m b
{-# INLINE_FUSED mapM #-}
mapM f (Stream step t) = Stream step' t
  where
    {-# INLINE_INNER step' #-}
    step' s = do
                r <- step s
                case r of
                  Yield x s' -> liftM  (`Yield` s') (f x)
                  Done       -> return Done

consume :: Monad m => Stream m a -> m ()
{-# INLINE_FUSED consume #-}
consume (Stream step t) = consume_loop SPEC t
  where
    consume_loop !_ s
      = do
          r <- step s
          case r of
            Yield _ s' -> consume_loop SPEC s'
            Done       -> return ()

-- | Execute a monadic action for each element of the 'Stream'
mapM_ :: Monad m => (a -> m b) -> Stream m a -> m ()
{-# INLINE_FUSED mapM_ #-}
mapM_ m = consume . mapM m

-- | Transform a 'Stream' to use a different monad
trans :: (Monad m, Monad m')
      => (forall z. m z -> m' z) -> Stream m a -> Stream m' a
{-# INLINE_FUSED trans #-}
trans f (Stream step s) = Stream (f . step) s

unbox :: Monad m => Stream m (Box a) -> Stream m a
{-# INLINE_FUSED unbox #-}
unbox (Stream step t) = Stream step' t
  where
    {-# INLINE_INNER step' #-}
    step' s = do
                r <- step s
                case r of
                  Yield (Box x) s' -> return $ Yield x s'
                  Done             -> return Done

-- Zipping
-- -------

-- | Pair each element in a 'Stream' with its index
indexed :: Monad m => Stream m a -> Stream m (Int,a)
{-# INLINE_FUSED indexed #-}
indexed (Stream step t) = Stream step' (t,0)
  where
    {-# INLINE_INNER step' #-}
    step' (s,i) = i `seq`
                  do
                    r <- step s
                    case r of
                      Yield x s' -> return $ Yield (i,x) (s', i+1)
                      Done       -> return Done

-- | Pair each element in a 'Stream' with its index, starting from the right
-- and counting down
indexedR :: Monad m => Int -> Stream m a -> Stream m (Int,a)
{-# INLINE_FUSED indexedR #-}
indexedR m (Stream step t) = Stream step' (t,m)
  where
    {-# INLINE_INNER step' #-}
    step' (s,i) = i `seq`
                  do
                    r <- step s
                    case r of
                      Yield x s' -> let i' = i-1
                                    in
                                    return $ Yield (i',x) (s', i')
                      Done       -> return Done

-- | Zip two 'Stream's with the given monadic function
zipWithM :: Monad m => (a -> b -> m c) -> Stream m a -> Stream m b -> Stream m c
{-# INLINE_FUSED zipWithM #-}
zipWithM f (Stream stepa ta) (Stream stepb tb) = Stream step (ta, tb, Nothing)
  where
    {-# INLINE_INNER step #-}
    step (sa, sb, Nothing) = do 
                               r <- stepa sa
                               case r of
                                 Yield x sa' -> step' sa' sb x
                                 Done        -> return Done
    step (sa, sb, Just x) = step' sa sb x

    {-# INLINE_INNER step' #-}
    step' sa sb x  = do
                       r <- stepb sb
                       case r of
                         Yield y sb' ->
                           do
                             z <- f x y
                             return $ Yield z (sa, sb', Nothing)
                         Done        -> return Done

zipWithM_ :: Monad m => (a -> b -> m c) -> Stream m a -> Stream m b -> m ()
{-# INLINE zipWithM_ #-}
zipWithM_ f sa sb = consume (zipWithM f sa sb)

zipWith3M :: Monad m => (a -> b -> c -> m d) -> Stream m a -> Stream m b -> Stream m c -> Stream m d
{-# INLINE_FUSED zipWith3M #-}
zipWith3M f (Stream stepa ta)
            (Stream stepb tb)
            (Stream stepc tc) = Stream step (ta, tb, tc, Nothing)
  where
    {-# INLINE_INNER step #-}
    step (sa, sb, sc, Nothing) = do
        r <- stepa sa
        case r of
            Yield x sa' -> step' sa' sb sc x
            Done        -> return Done
    step (sa, sb, sc, Just (x, Nothing)) = step' sa sb sc x
    step (sa, sb, sc, Just (x, Just y)) = step'' sa sb sc x y

    {-# INLINE_INNER step' #-}
    step' sa sb sc x = do
        r <- stepb sb
        case r of
            Yield y sb' -> step'' sa sb' sc x y
            Done        -> return Done

    {-# INLINE_INNER step'' #-}
    step'' sa sb sc x y = do
        r <- stepc sc
        case r of
            Yield z sc' -> f x y z >>= (\res -> return $ Yield res (sa, sb, sc', Nothing))
            Done        -> return $ Done

zipWith4M :: Monad m => (a -> b -> c -> d -> m e)
                     -> Stream m a -> Stream m b -> Stream m c -> Stream m d
                     -> Stream m e
{-# INLINE zipWith4M #-}
zipWith4M f sa sb sc sd
  = zipWithM (\(a,b) (c,d) -> f a b c d) (zip sa sb) (zip sc sd)

zipWith5M :: Monad m => (a -> b -> c -> d -> e -> m f)
                     -> Stream m a -> Stream m b -> Stream m c -> Stream m d
                     -> Stream m e -> Stream m f
{-# INLINE zipWith5M #-}
zipWith5M f sa sb sc sd se
  = zipWithM (\(a,b,c) (d,e) -> f a b c d e) (zip3 sa sb sc) (zip sd se)

zipWith6M :: Monad m => (a -> b -> c -> d -> e -> f -> m g)
                     -> Stream m a -> Stream m b -> Stream m c -> Stream m d
                     -> Stream m e -> Stream m f -> Stream m g
{-# INLINE zipWith6M #-}
zipWith6M fn sa sb sc sd se sf
  = zipWithM (\(a,b,c) (d,e,f) -> fn a b c d e f) (zip3 sa sb sc)
                                                  (zip3 sd se sf)

zipWith :: Monad m => (a -> b -> c) -> Stream m a -> Stream m b -> Stream m c
{-# INLINE zipWith #-}
zipWith f = zipWithM (\a b -> return (f a b))

zipWith3 :: Monad m => (a -> b -> c -> d)
                    -> Stream m a -> Stream m b -> Stream m c -> Stream m d
{-# INLINE zipWith3 #-}
zipWith3 f = zipWith3M (\a b c -> return (f a b c))

zipWith4 :: Monad m => (a -> b -> c -> d -> e)
                    -> Stream m a -> Stream m b -> Stream m c -> Stream m d
                    -> Stream m e
{-# INLINE zipWith4 #-}
zipWith4 f = zipWith4M (\a b c d -> return (f a b c d))

zipWith5 :: Monad m => (a -> b -> c -> d -> e -> f)
                    -> Stream m a -> Stream m b -> Stream m c -> Stream m d
                    -> Stream m e -> Stream m f
{-# INLINE zipWith5 #-}
zipWith5 f = zipWith5M (\a b c d e -> return (f a b c d e))

zipWith6 :: Monad m => (a -> b -> c -> d -> e -> f -> g)
                    -> Stream m a -> Stream m b -> Stream m c -> Stream m d
                    -> Stream m e -> Stream m f -> Stream m g
{-# INLINE zipWith6 #-}
zipWith6 fn = zipWith6M (\a b c d e f -> return (fn a b c d e f))

zip :: Monad m => Stream m a -> Stream m b -> Stream m (a,b)
{-# INLINE zip #-}
zip = zipWith (,)

zip3 :: Monad m => Stream m a -> Stream m b -> Stream m c -> Stream m (a,b,c)
{-# INLINE zip3 #-}
zip3 = zipWith3 (,,)

zip4 :: Monad m => Stream m a -> Stream m b -> Stream m c -> Stream m d
                -> Stream m (a,b,c,d)
{-# INLINE zip4 #-}
zip4 = zipWith4 (,,,)

zip5 :: Monad m => Stream m a -> Stream m b -> Stream m c -> Stream m d
                -> Stream m e -> Stream m (a,b,c,d,e)
{-# INLINE zip5 #-}
zip5 = zipWith5 (,,,,)

zip6 :: Monad m => Stream m a -> Stream m b -> Stream m c -> Stream m d
                -> Stream m e -> Stream m f -> Stream m (a,b,c,d,e,f)
{-# INLINE zip6 #-}
zip6 = zipWith6 (,,,,,)

-- Comparisons
-- -----------

-- | Check if two 'Stream's are equal
eqBy :: (Monad m) => (a -> b -> Bool) -> Stream m a -> Stream m b -> m Bool
{-# INLINE_FUSED eqBy #-}
eqBy eq (Stream step1 t1) (Stream step2 t2) = eq_loop0 SPEC t1 t2
  where
    eq_loop0 !_ s1 s2 = do
      r <- step1 s1
      case r of
        Yield x s1' -> eq_loop1 SPEC x s1' s2
        Done        -> eq_null s2

    eq_loop1 !_ x s1 s2 = do
      r <- step2 s2
      case r of
        Yield y s2'
          | eq x y    -> eq_loop0 SPEC   s1 s2'
          | otherwise -> return False
        Done          -> return False

    eq_null s2 = do
      r <- step2 s2
      case r of
        Yield _ _ -> return False
        Done      -> return True

-- | Lexicographically compare two 'Stream's
cmpBy :: (Monad m) => (a -> b -> Ordering) -> Stream m a -> Stream m b -> m Ordering
{-# INLINE_FUSED cmpBy #-}
cmpBy cmp (Stream step1 t1) (Stream step2 t2) = cmp_loop0 SPEC t1 t2
  where
    cmp_loop0 !_ s1 s2 = do
      r <- step1 s1
      case r of
        Yield x s1' -> cmp_loop1 SPEC x s1' s2
        Done        -> cmp_null s2

    cmp_loop1 !_ x s1 s2 = do
      r <- step2 s2
      case r of
        Yield y s2' -> case x `cmp` y of
                         EQ -> cmp_loop0 SPEC s1 s2'
                         c  -> return c
        Done        -> return GT

    cmp_null s2 = do
      r <- step2 s2
      case r of
        Yield _ _ -> return LT
        Done      -> return EQ

-- Filtering
-- ---------

-- | Drop elements which do not satisfy the predicate
filter :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
{-# INLINE filter #-}
filter f = filterM (return . f)

mapMaybe :: Monad m => (a -> Maybe b) -> Stream m a -> Stream m b
{-# INLINE_FUSED mapMaybe #-}
mapMaybe f (Stream step t) = Stream step' t
  where
    {-# INLINE_INNER step' #-}
    step' s0 =
      let
        -- go is a recursive join point
        go s = do
                 r <- step s
                 case r of
                   Yield x s' -> case f x of
                                   Nothing -> go s'
                                   Just b' -> return $ Yield b' s'
                   Done       -> return $ Done
      in go s0

catMaybes :: Monad m => Stream m (Maybe a) -> Stream m a
catMaybes = mapMaybe id

-- | Drop elements which do not satisfy the monadic predicate
filterM :: Monad m => (a -> m Bool) -> Stream m a -> Stream m a
{-# INLINE_FUSED filterM #-}
filterM f (Stream step t) = Stream step' t
  where
    {-# INLINE_INNER step' #-}
    step' s0 =
      let
        -- go is a join point
        go s = do
                 r <- step s
                 case r of
                   Yield x s' -> do
                                   b <- f x
                                   if b then return $ Yield x s'
                                        else go s'
                   Done       -> return $ Done
      in go s0

-- | Apply monadic function to each element and drop all Nothings
--
-- @since 0.12.2.0
mapMaybeM :: Monad m => (a -> m (Maybe b)) -> Stream m a -> Stream m b
{-# INLINE_FUSED mapMaybeM #-}
mapMaybeM f (Stream step t) = Stream step' t
  where
    {-# INLINE_INNER step' #-}
    step' s0 =
      let
        -- go is a join point
        go s = do
                 r <- step s
                 case r of
                   Yield x s' -> do
                                   fx <- f x
                                   case fx of
                                     Nothing -> go s'
                                     Just b  -> return $ Yield b s'
                   Done       -> return $ Done
      in go s0

-- | Drop repeated adjacent elements.
uniq :: (Eq a, Monad m) => Stream m a -> Stream m a
{-# INLINE_FUSED uniq #-}
uniq (Stream step st) = Stream step' (Nothing,st)
  where
    {-# INLINE_INNER step' #-}
    step' (Nothing, s) = do r <- step s
                            case r of
                              Yield x s' -> return $ Yield x (Just x , s')
                              Done       -> return   Done
    step' (Just x, s) = go x s

    -- go is a recursive join point
    {-# INLINABLE go #-}
    go x0 s = do r <- step s
                 case r of
                   Yield x s' | x == x0   -> go x0 s'
                              | otherwise -> return $ Yield x (Just x , s')
                   Done                   -> return   Done

-- | Longest prefix of elements that satisfy the predicate
takeWhile :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
{-# INLINE takeWhile #-}
takeWhile f = takeWhileM (return . f)

-- | Longest prefix of elements that satisfy the monadic predicate
takeWhileM :: Monad m => (a -> m Bool) -> Stream m a -> Stream m a
{-# INLINE_FUSED takeWhileM #-}
takeWhileM f (Stream step t) = Stream step' t
  where
    {-# INLINE_INNER step' #-}
    step' s = do
                r <- step s
                case r of
                  Yield x s' -> do
                                  b <- f x
                                  return $ if b then Yield x s' else Done
                  Done       -> return $ Done

-- | Drop the longest prefix of elements that satisfy the predicate
dropWhile :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
{-# INLINE dropWhile #-}
dropWhile f = dropWhileM (return . f)

data DropWhile s a = DropWhile_Drop s | DropWhile_Yield a s | DropWhile_Next s

-- | Drop the longest prefix of elements that satisfy the monadic predicate
dropWhileM :: Monad m => (a -> m Bool) -> Stream m a -> Stream m a
{-# INLINE_FUSED dropWhileM #-}
dropWhileM f (Stream step t) = Stream step' (DropWhile_Drop t)
  where
    -- NOTE: we jump through hoops here to have only one Yield; local data
    -- declarations would be nice!

    {-# INLINE_INNER step' #-}
    step' s0 =
      let
        -- go is a join point
        go (DropWhile_Drop s)
          = do
              r <- step s
              case r of
                Yield x s' -> do
                                b <- f x
                                if b then go (DropWhile_Drop    s')
                                     else go (DropWhile_Yield x s')
                Done       -> return $ Done

        go (DropWhile_Yield x s) = return $ Yield x (DropWhile_Next s)

        go (DropWhile_Next s)
          = do
              r <- step s
              case r of
                Yield x s' -> go (DropWhile_Yield x s')
                Done       -> return Done
      in go s0

-- Searching
-- ---------

infix 4 `elem`
-- | Check whether the 'Stream' contains an element
elem :: (Monad m, Eq a) => a -> Stream m a -> m Bool
{-# INLINE_FUSED elem #-}
elem x (Stream step t) = elem_loop SPEC t
  where
    elem_loop !_ s
      = do
          r <- step s
          case r of
            Yield y s' | x == y    -> return True
                       | otherwise -> elem_loop SPEC s'
            Done                   -> return False

infix 4 `notElem`
-- | Inverse of `elem`
notElem :: (Monad m, Eq a) => a -> Stream m a -> m Bool
{-# INLINE notElem #-}
notElem x s = liftM not (elem x s)

-- | Yield 'Just' the first element that satisfies the predicate or 'Nothing'
-- if no such element exists.
find :: Monad m => (a -> Bool) -> Stream m a -> m (Maybe a)
{-# INLINE find #-}
find f = findM (return . f)

-- | Yield 'Just' the first element that satisfies the monadic predicate or
-- 'Nothing' if no such element exists.
findM :: Monad m => (a -> m Bool) -> Stream m a -> m (Maybe a)
{-# INLINE_FUSED findM #-}
findM f (Stream step t) = find_loop SPEC t
  where
    find_loop !_ s
      = do
          r <- step s
          case r of
            Yield x s' -> do
                            b <- f x
                            if b then return $ Just x
                                 else find_loop SPEC s'
            Done       -> return Nothing

-- | Yield 'Just' the index of the first element that satisfies the predicate
-- or 'Nothing' if no such element exists.
findIndex :: Monad m => (a -> Bool) -> Stream m a -> m (Maybe Int)
{-# INLINE_FUSED findIndex #-}
findIndex f = findIndexM (return . f)

-- | Yield 'Just' the index of the first element that satisfies the monadic
-- predicate or 'Nothing' if no such element exists.
findIndexM :: Monad m => (a -> m Bool) -> Stream m a -> m (Maybe Int)
{-# INLINE_FUSED findIndexM #-}
findIndexM f (Stream step t) = findIndex_loop SPEC t 0
  where
    findIndex_loop !_ s i
      = do
          r <- step s
          case r of
            Yield x s' -> do
                            b <- f x
                            if b then return $ Just i
                                 else findIndex_loop SPEC s' (i+1)
            Done       -> return Nothing

-- Folding
-- -------

-- | Left fold
foldl :: Monad m => (a -> b -> a) -> a -> Stream m b -> m a
{-# INLINE foldl #-}
foldl f = foldlM (\a b -> return (f a b))

-- | Left fold with a monadic operator
foldlM :: Monad m => (a -> b -> m a) -> a -> Stream m b -> m a
{-# INLINE_FUSED foldlM #-}
foldlM m w (Stream step t) = foldlM_loop SPEC w t
  where
    foldlM_loop !_ z s
      = do
          r <- step s
          case r of
            Yield x s' -> do { z' <- m z x; foldlM_loop SPEC z' s' }
            Done       -> return z

-- | Same as 'foldlM'
foldM :: Monad m => (a -> b -> m a) -> a -> Stream m b -> m a
{-# INLINE foldM #-}
foldM = foldlM

-- | Left fold over a non-empty 'Stream'
foldl1 :: Monad m => (a -> a -> a) -> Stream m a -> m a
{-# INLINE foldl1 #-}
foldl1 f = foldl1M (\a b -> return (f a b))

-- | Left fold over a non-empty 'Stream' with a monadic operator
foldl1M :: (HasCallStack, Monad m) => (a -> a -> m a) -> Stream m a -> m a
{-# INLINE_FUSED foldl1M #-}
foldl1M f (Stream step t) = foldl1M_loop SPEC t
  where
    foldl1M_loop !_ s
      = do
          r <- step s
          case r of
            Yield x s' -> foldlM f x (Stream step s')
            Done       -> error emptyStream

-- | Same as 'foldl1M'
fold1M :: Monad m => (a -> a -> m a) -> Stream m a -> m a
{-# INLINE fold1M #-}
fold1M = foldl1M

-- | Left fold with a strict accumulator
foldl' :: Monad m => (a -> b -> a) -> a -> Stream m b -> m a
{-# INLINE foldl' #-}
foldl' f = foldlM' (\a b -> return (f a b))

-- | Left fold with a strict accumulator and a monadic operator
foldlM' :: Monad m => (a -> b -> m a) -> a -> Stream m b -> m a
{-# INLINE_FUSED foldlM' #-}
foldlM' m w (Stream step t) = foldlM'_loop SPEC w t
  where
    foldlM'_loop !_ z s
      = z `seq`
        do
          r <- step s
          case r of
            Yield x s' -> do { z' <- m z x; foldlM'_loop SPEC z' s' }
            Done       -> return z

-- | Same as 'foldlM''
foldM' :: Monad m => (a -> b -> m a) -> a -> Stream m b -> m a
{-# INLINE foldM' #-}
foldM' = foldlM'

-- | Left fold over a non-empty 'Stream' with a strict accumulator
foldl1' :: Monad m => (a -> a -> a) -> Stream m a -> m a
{-# INLINE foldl1' #-}
foldl1' f = foldl1M' (\a b -> return (f a b))

-- | Left fold over a non-empty 'Stream' with a strict accumulator and a
-- monadic operator
foldl1M' :: (HasCallStack, Monad m) => (a -> a -> m a) -> Stream m a -> m a
{-# INLINE_FUSED foldl1M' #-}
foldl1M' f (Stream step t) = foldl1M'_loop SPEC t
  where
    foldl1M'_loop !_ s
      = do
          r <- step s
          case r of
            Yield x s' -> foldlM' f x (Stream step s')
            Done       -> error emptyStream

-- | Same as 'foldl1M''
fold1M' :: Monad m => (a -> a -> m a) -> Stream m a -> m a
{-# INLINE fold1M' #-}
fold1M' = foldl1M'

-- | Right fold
foldr :: Monad m => (a -> b -> b) -> b -> Stream m a -> m b
{-# INLINE foldr #-}
foldr f = foldrM (\a b -> return (f a b))

-- | Right fold with a monadic operator
foldrM :: Monad m => (a -> b -> m b) -> b -> Stream m a -> m b
{-# INLINE_FUSED foldrM #-}
foldrM f z (Stream step t) = foldrM_loop SPEC t
  where
    foldrM_loop !_ s
      = do
          r <- step s
          case r of
            Yield x s' -> f x =<< foldrM_loop SPEC s'
            Done       -> return z

-- | Right fold over a non-empty stream
foldr1 :: Monad m => (a -> a -> a) -> Stream m a -> m a
{-# INLINE foldr1 #-}
foldr1 f = foldr1M (\a b -> return (f a b))

-- | Right fold over a non-empty stream with a monadic operator
foldr1M :: (HasCallStack, Monad m) => (a -> a -> m a) -> Stream m a -> m a
{-# INLINE_FUSED foldr1M #-}
foldr1M f (Stream step t) = foldr1M_loop0 SPEC t
  where
    foldr1M_loop0 !_ s
      = do
          r <- step s
          case r of
            Yield x s' -> foldr1M_loop1 SPEC x s'
            Done       -> error emptyStream

    foldr1M_loop1 !_ x s
      = do
          r <- step s
          case r of
            Yield y s' -> f x =<< foldr1M_loop1 SPEC y s'
            Done       -> return x

-- Specialised folds
-- -----------------

and :: Monad m => Stream m Bool -> m Bool
{-# INLINE_FUSED and #-}
and (Stream step t) = and_loop SPEC t
  where
    and_loop !_ s
      = do
          r <- step s
          case r of
            Yield False _  -> return False
            Yield True  s' -> and_loop SPEC s'
            Done           -> return True

or :: Monad m => Stream m Bool -> m Bool
{-# INLINE_FUSED or #-}
or (Stream step t) = or_loop SPEC t
  where
    or_loop !_ s
      = do
          r <- step s
          case r of
            Yield False s' -> or_loop SPEC s'
            Yield True  _  -> return True
            Done           -> return False

concatMap :: Monad m => (a -> Stream m b) -> Stream m a -> Stream m b
{-# INLINE concatMap #-}
concatMap f = concatMapM (return . f)

concatMapM :: Monad m => (a -> m (Stream m b)) -> Stream m a -> Stream m b
{-# INLINE_FUSED concatMapM #-}
concatMapM f (Stream step t) = Stream step' (Left t)
  where
    {-# INLINE_INNER step' #-}
    step' s0 =
      let
        -- go is a join point
        go (Left s) = do
          r <- step s
          case r of
              Yield a s' -> do
                  b_stream <- f a
                  go (Right (b_stream, s'))
              Done       -> return Done
        go (Right (Stream inner_step inner_s, s)) = do
          r <- inner_step inner_s
          case r of
              Yield b inner_s' -> return $ Yield b (Right (Stream inner_step inner_s', s))
              Done             -> go (Left s)
      in go s0

-- | Create a 'Stream' of values from a 'Stream' of streamable things
flatten :: Monad m => (a -> m s) -> (s -> m (Step s b)) -> Stream m a -> Stream m b
{-# INLINE_FUSED flatten #-}
flatten mk istep (Stream ostep u) = Stream step (Left u)
  where
    {-# INLINE_INNER step #-}
    step s0 =
      let
        -- go is a join point
        go (Left t) = do
                        r <- ostep t
                        case r of
                          Yield a t' -> do
                                          s <- mk a
                                          s `seq` go (Right (s,t'))
                          Done       -> return $ Done


        go (Right (s,t)) = do
                             r <- istep s
                             case r of
                               Yield x s' -> return $ Yield x (Right (s',t))
                               Done       -> go (Left t)
      in go s0

-- Unfolding
-- ---------

-- | Unfold
unfoldr :: Monad m => (s -> Maybe (a, s)) -> s -> Stream m a
{-# INLINE_FUSED unfoldr #-}
unfoldr f = unfoldrM (return . f)

-- | Unfold with a monadic function
unfoldrM :: Monad m => (s -> m (Maybe (a, s))) -> s -> Stream m a
{-# INLINE_FUSED unfoldrM #-}
unfoldrM f t = Stream step t
  where
    {-# INLINE_INNER step #-}
    step s = liftM (\r ->
               case r of
                 Just (x, s') -> Yield x s'
                 Nothing      -> Done
             ) (f s)

unfoldrN :: Monad m => Int -> (s -> Maybe (a, s)) -> s -> Stream m a
{-# INLINE_FUSED unfoldrN #-}
unfoldrN n f = unfoldrNM n (return . f)

-- | Unfold at most @n@ elements with a monadic function.
unfoldrNM :: Monad m => Int -> (s -> m (Maybe (a, s))) -> s -> Stream m a
{-# INLINE_FUSED unfoldrNM #-}
unfoldrNM m f t = Stream step (t,m)
  where
    {-# INLINE_INNER step #-}
    step (s,n) | n <= 0    = return Done
               | otherwise = liftM (\r ->
                               case r of
                                 Just (x,s') -> Yield x (s',n-1)
                                 Nothing     -> Done
                             ) (f s)

-- | Unfold exactly @n@ elements
--
-- @since 0.12.2.0
unfoldrExactN :: Monad m => Int -> (s -> (a, s)) -> s -> Stream m a
{-# INLINE_FUSED unfoldrExactN #-}
unfoldrExactN n f = unfoldrExactNM n (return . f)

-- | Unfold exactly @n@ elements with a monadic function.
--
-- @since 0.12.2.0
unfoldrExactNM :: Monad m => Int -> (s -> m (a, s)) -> s -> Stream m a
{-# INLINE_FUSED unfoldrExactNM #-}
unfoldrExactNM m f t = Stream step (t,m)
  where
    {-# INLINE_INNER step #-}
    step (s,n) | n <= 0    = return Done
               | otherwise = do (x,s') <- f s
                                return $ Yield x (s',n-1)

-- | /O(n)/ Apply monadic function \(\max(n - 1, 0)\) times to an initial value,
-- producing a stream of \(\max(n, 0)\) values.
iterateNM :: Monad m => Int -> (a -> m a) -> a -> Stream m a
{-# INLINE_FUSED iterateNM #-}
iterateNM n f x0 = Stream step (x0,n)
  where
    {-# INLINE_INNER step #-}
    step (x,i) | i <= 0    = return Done
               | i == n    = return $ Yield x (x,i-1)
               | otherwise = do a <- f x
                                return $ Yield a (a,i-1)

-- | /O(n)/ Apply function \(\max(n - 1, 0)\) times to an initial value,
-- producing a stream of \(\max(n, 0)\) values.
iterateN :: Monad m => Int -> (a -> a) -> a -> Stream m a
{-# INLINE_FUSED iterateN #-}
iterateN n f x0 = iterateNM n (return . f) x0

-- Scans
-- -----

-- | Prefix scan
prescanl :: Monad m => (a -> b -> a) -> a -> Stream m b -> Stream m a
{-# INLINE prescanl #-}
prescanl f = prescanlM (\a b -> return (f a b))

-- | Prefix scan with a monadic operator
prescanlM :: Monad m => (a -> b -> m a) -> a -> Stream m b -> Stream m a
{-# INLINE_FUSED prescanlM #-}
prescanlM f w (Stream step t) = Stream step' (t,w)
  where
    {-# INLINE_INNER step' #-}
    step' (s,x) = do
                    r <- step s
                    case r of
                      Yield y s' -> do
                                      z <- f x y
                                      return $ Yield x (s', z)
                      Done       -> return Done

-- | Prefix scan with strict accumulator
prescanl' :: Monad m => (a -> b -> a) -> a -> Stream m b -> Stream m a
{-# INLINE prescanl' #-}
prescanl' f = prescanlM' (\a b -> return (f a b))

-- | Prefix scan with strict accumulator and a monadic operator
prescanlM' :: Monad m => (a -> b -> m a) -> a -> Stream m b -> Stream m a
{-# INLINE_FUSED prescanlM' #-}
prescanlM' f w (Stream step t) = Stream step' (t,w)
  where
    {-# INLINE_INNER step' #-}
    step' (s,x) = x `seq`
                  do
                    r <- step s
                    case r of
                      Yield y s' -> do
                                      z <- f x y
                                      return $ Yield x (s', z)
                      Done       -> return Done

-- | Suffix scan
postscanl :: Monad m => (a -> b -> a) -> a -> Stream m b -> Stream m a
{-# INLINE postscanl #-}
postscanl f = postscanlM (\a b -> return (f a b))

-- | Suffix scan with a monadic operator
postscanlM :: Monad m => (a -> b -> m a) -> a -> Stream m b -> Stream m a
{-# INLINE_FUSED postscanlM #-}
postscanlM f w (Stream step t) = Stream step' (t,w)
  where
    {-# INLINE_INNER step' #-}
    step' (s,x) = do
                    r <- step s
                    case r of
                      Yield y s' -> do
                                      z <- f x y
                                      return $ Yield z (s',z)
                      Done       -> return Done

-- | Suffix scan with strict accumulator
postscanl' :: Monad m => (a -> b -> a) -> a -> Stream m b -> Stream m a
{-# INLINE postscanl' #-}
postscanl' f = postscanlM' (\a b -> return (f a b))

-- | Suffix scan with strict acccumulator and a monadic operator
postscanlM' :: Monad m => (a -> b -> m a) -> a -> Stream m b -> Stream m a
{-# INLINE_FUSED postscanlM' #-}
postscanlM' f w (Stream step t) = w `seq` Stream step' (t,w)
  where
    {-# INLINE_INNER step' #-}
    step' (s,x) = x `seq`
                  do
                    r <- step s
                    case r of
                      Yield y s' -> do
                                      z <- f x y
                                      z `seq` return (Yield z (s',z))
                      Done       -> return Done

-- | Haskell-style scan
scanl :: Monad m => (a -> b -> a) -> a -> Stream m b -> Stream m a
{-# INLINE scanl #-}
scanl f = scanlM (\a b -> return (f a b))

-- | Haskell-style scan with a monadic operator
scanlM :: Monad m => (a -> b -> m a) -> a -> Stream m b -> Stream m a
{-# INLINE scanlM #-}
scanlM f z s = z `cons` postscanlM f z s

-- | Haskell-style scan with strict accumulator
scanl' :: Monad m => (a -> b -> a) -> a -> Stream m b -> Stream m a
{-# INLINE scanl' #-}
scanl' f = scanlM' (\a b -> return (f a b))

-- | Haskell-style scan with strict accumulator and a monadic operator
scanlM' :: Monad m => (a -> b -> m a) -> a -> Stream m b -> Stream m a
{-# INLINE scanlM' #-}
scanlM' f z s = z `seq` (z `cons` postscanlM f z s)

-- | Initial-value free scan over a 'Stream'
scanl1 :: Monad m => (a -> a -> a) -> Stream m a -> Stream m a
{-# INLINE scanl1 #-}
scanl1 f = scanl1M (\x y -> return (f x y))

-- | Initial-value free scan over a 'Stream' with a monadic operator
scanl1M :: Monad m => (a -> a -> m a) -> Stream m a -> Stream m a
{-# INLINE_FUSED scanl1M #-}
scanl1M f (Stream step t) = Stream step' (t, Nothing)
  where
    {-# INLINE_INNER step' #-}
    step' (s, Nothing) = do
                           r <- step s
                           case r of
                             Yield x s' -> return $ Yield x (s', Just x)
                             Done       -> return Done

    step' (s, Just x) = do
                          r <- step s
                          case r of
                            Yield y s' -> do
                                            z <- f x y
                                            return $ Yield z (s', Just z)
                            Done       -> return Done

-- | Initial-value free scan over a 'Stream' with a strict accumulator
scanl1' :: Monad m => (a -> a -> a) -> Stream m a -> Stream m a
{-# INLINE scanl1' #-}
scanl1' f = scanl1M' (\x y -> return (f x y))

-- | Initial-value free scan over a 'Stream' with a strict accumulator
-- and a monadic operator
scanl1M' :: Monad m => (a -> a -> m a) -> Stream m a -> Stream m a
{-# INLINE_FUSED scanl1M' #-}
scanl1M' f (Stream step t) = Stream step' (t, Nothing)
  where
    {-# INLINE_INNER step' #-}
    step' (s, Nothing) = do
                           r <- step s
                           case r of
                             Yield x s' -> x `seq` return (Yield x (s', Just x))
                             Done       -> return Done

    step' (s, Just x) = x `seq`
                        do
                          r <- step s
                          case r of
                            Yield y s' -> do
                                            z <- f x y
                                            z `seq` return (Yield z (s', Just z))
                            Done       -> return Done

-- Enumerations
-- ------------

-- The Enum class is broken for this, there just doesn't seem to be a
-- way to implement this generically. We have to specialise for as many types
-- as we can but this doesn't help in polymorphic loops.

-- | Yield a 'Stream' of the given length containing the values @x@, @x+y@,
-- @x+y+y@ etc.
enumFromStepN :: (Num a, Monad m) => a -> a -> Int -> Stream m a
{-# INLINE_FUSED enumFromStepN #-}
enumFromStepN x y n = x `seq` y `seq` n `seq` Stream step (x,n)
  where
    {-# INLINE_INNER step #-}
    step (w,m) | m > 0     = return $ Yield w (w+y,m-1)
               | otherwise = return $ Done

-- | Enumerate values
--
-- /WARNING:/ This operation can be very inefficient. If at all possible, use
-- 'enumFromStepN' instead.
enumFromTo :: (Enum a, Monad m) => a -> a -> Stream m a
{-# INLINE_FUSED enumFromTo #-}
enumFromTo x y = fromList [x .. y]


enumFromTo_integral :: (Integral a, Monad m) => a -> a -> Stream m a
{-# INLINE_FUSED enumFromTo_integral #-}
enumFromTo_integral !x !y = Stream step (Just x)
  where
    -- NOTE: We use (x+1) instead of (succ x) below because the latter
    --       checks for overflow which can't happen here.
    {-# INLINE_INNER step #-}
    step Nothing              = return $ Done
    step (Just z) | z == y    = return $ Yield z Nothing
                  | z <  y    = return $ Yield z (Just (z+1))
                  | otherwise = return $ Done

enumFromTo_char :: Monad m => Char -> Char -> Stream m Char
{-# INLINE_FUSED enumFromTo_char #-}
enumFromTo_char !x !y = Stream step xn
  where
    xn = ord x
    yn = ord y

    {-# INLINE_INNER step #-}
    step zn | zn <= yn  = return $ Yield (unsafeChr zn) (zn+1)
            | otherwise = return $ Done


enumFromTo_double :: (Monad m, Ord a, RealFrac a) => a -> a -> Stream m a
{-# INLINE_FUSED enumFromTo_double #-}
enumFromTo_double !n !m = Stream step ini
  where
    lim = m + 1/2 -- important to float out
    ini = 0
    step x | x' <= lim = return $ Yield x' (x+1)
           | otherwise = return $ Done
           where
             x' = x + n


{-# RULES

"Stream.enumFromTo[Int]"     enumFromTo @Int     = enumFromTo_integral
"Stream.enumFromTo[Int8]"    enumFromTo @Int8    = enumFromTo_integral
"Stream.enumFromTo[Int16]"   enumFromTo @Int16   = enumFromTo_integral
"Stream.enumFromTo[Int32]"   enumFromTo @Int32   = enumFromTo_integral
"Stream.enumFromTo[Int64]"   enumFromTo @Int64   = enumFromTo_integral
"Stream.enumFromTo[Word]"    enumFromTo @Word    = enumFromTo_integral
"Stream.enumFromTo[Word8]"   enumFromTo @Word8   = enumFromTo_integral
"Stream.enumFromTo[Word16]"  enumFromTo @Word16  = enumFromTo_integral
"Stream.enumFromTo[Word32]"  enumFromTo @Word32  = enumFromTo_integral
"Stream.enumFromTo[Word64]"  enumFromTo @Word64  = enumFromTo_integral
"Stream.enumFromTo[Integer]" enumFromTo @Integer = enumFromTo_integral

"Stream.enumFromTo[Float]"   enumFromTo @Float   = enumFromTo_double
"Stream.enumFromTo[Double]"  enumFromTo @Double  = enumFromTo_double

"Stream.enumFromTo[Char]"    enumFromTo @Char    = enumFromTo_char
  #-}



------------------------------------------------------------------------

-- | Enumerate values with a given step.
--
-- /WARNING:/ This operation is very inefficient. If at all possible, use
-- 'enumFromStepN' instead.
enumFromThenTo :: (Enum a, Monad m) => a -> a -> a -> Stream m a
{-# INLINE_FUSED enumFromThenTo #-}
enumFromThenTo x y z = fromList [x, y .. z]

-- FIXME: Specialise enumFromThenTo.

-- Conversions
-- -----------

-- | Convert a 'Stream' to a list
toList :: Monad m => Stream m a -> m [a]
{-# INLINE toList #-}
toList = foldr (:) []

-- | Convert a list to a 'Stream'
fromList :: Monad m => [a] -> Stream m a
{-# INLINE fromList #-}
fromList zs = Stream step zs
  where
    step (x:xs) = return (Yield x xs)
    step []     = return Done

-- | Convert the first @n@ elements of a list to a 'Bundle'
fromListN :: Monad m => Int -> [a] -> Stream m a
{-# INLINE_FUSED fromListN #-}
fromListN m zs = Stream step (zs,m)
  where
    {-# INLINE_INNER step #-}
    step (_, n) | n <= 0 = return Done
    step (x:xs,n)        = return (Yield x (xs,n-1))
    step ([],_)          = return Done

{-
fromVector :: (Monad m, Vector v a) => v a -> Stream m a
{-# INLINE_FUSED fromVector #-}
fromVector v = v `seq` n `seq` Stream (Unf step 0)
                                      (Unf vstep True)
                                      (Just v)
                                      (Exact n)
  where
    n = basicLength v

    {-# INLINE step #-}
    step i | i >= n = return Done
           | otherwise = case basicUnsafeIndexM v i of
                           Box x -> return $ Yield x (i+1)


    {-# INLINE vstep #-}
    vstep True  = return (Yield (Chunk (basicLength v) (\mv -> basicUnsafeCopy mv v)) False)
    vstep False = return Done

fromVectors :: forall m a. (Monad m, Vector v a) => [v a] -> Stream m a
{-# INLINE_FUSED fromVectors #-}
fromVectors vs = Stream (Unf pstep (Left vs))
                        (Unf vstep vs)
                        Nothing
                        (Exact n)
  where
    n = List.foldl' (\k v -> k + basicLength v) 0 vs

    pstep (Left []) = return Done
    pstep (Left (v:vs)) = basicLength v `seq` return (_ (Right (v,0,vs)))

    pstep (Right (v,i,vs))
      | i >= basicLength v = return $ _ (Left vs)
      | otherwise          = case basicUnsafeIndexM v i of
                               Box x -> return $ Yield x (Right (v,i+1,vs))

    -- FIXME: work around bug in GHC 7.6.1
    vstep :: [v a] -> m (Step [v a] (Chunk v a))
    vstep [] = return Done
    vstep (v:vs) = return $ Yield (Chunk (basicLength v)
                                         (\mv -> INTERNAL_CHECK(check) "concatVectors" "length mismatch"
                                                                       (M.basicLength mv == basicLength v)
                                                 $ basicUnsafeCopy mv v)) vs


concatVectors :: (Monad m, Vector v a) => Stream m (v a) -> Stream m a
{-# INLINE_FUSED concatVectors #-}
concatVectors (Stream step s}
  = Stream (Unf pstep (Left s))
           (Unf vstep s)
           Nothing
           Unknown
  where
    pstep (Left s) = do
      r <- step s
      case r of
        Yield v s' -> basicLength v `seq` return (_ (Right (v,0,s')))
        Done       -> return Done

    pstep (Right (v,i,s))
      | i >= basicLength v = return (_ (Left s))
      | otherwise          = case basicUnsafeIndexM v i of
                               Box x -> return (Yield x (Right (v,i+1,s)))


    vstep s = do
      r <- step s
      case r of
        Yield v s' -> return (Yield (Chunk (basicLength v)
                                           (\mv -> INTERNAL_CHECK(check) "concatVectors" "length mismatch"
                                                                          (M.basicLength mv == basicLength v)
                                                   $ basicUnsafeCopy mv v)) s')
        Done       -> return Done

reVector :: Monad m => Stream m a -> Stream m a
{-# INLINE_FUSED reVector #-}
reVector (Stream step s, sSize = n} = Stream step s n

{-# RULES

"reVector [Vector]"
  reVector = id

"reVector/reVector [Vector]" forall s.
  reVector (reVector s) = s   #-}


-}
