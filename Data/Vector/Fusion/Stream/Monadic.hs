{-# LANGUAGE ExistentialQuantification, Rank2Types #-}

-- |
-- Module      : Data.Vector.Fusion.Stream.Monadic
-- Copyright   : (c) Roman Leshchinskiy 2008-2009
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
--
-- Monadic streams
--

module Data.Vector.Fusion.Stream.Monadic (
  Stream(..), Step(..),

  -- * Size hints
  size, sized,

  -- * Length
  length, null,

  -- * Construction
  empty, singleton, cons, snoc, replicate, (++),

  -- * Accessing elements
  head, last, (!!),

  -- * Substreams
  extract, init, tail, take, drop,

  -- * Mapping
  map, mapM, mapM_, trans, unbox, concatMap,
  
  -- * Zipping
  zipWith, zipWithM, zipWith3, zipWith3M,

  -- * Filtering
  filter, filterM, takeWhile, takeWhileM, dropWhile, dropWhileM,

  -- * Searching
  elem, notElem, find, findM, findIndex, findIndexM,

  -- * Folding
  foldl, foldlM, foldl1, foldl1M, foldM, fold1M,
  foldl', foldlM', foldl1', foldl1M', foldM', fold1M',
  foldr, foldrM, foldr1, foldr1M,

  -- * Specialised folds
  and, or, concatMapM,

  -- * Unfolding
  unfoldr, unfoldrM,

  -- * Scans
  prescanl, prescanlM, prescanl', prescanlM',
  postscanl, postscanlM, postscanl', postscanlM',
  scanl, scanlM, scanl', scanlM',
  scanl1, scanl1M, scanl1', scanl1M',

  -- * Enumerations
  enumFromTo, enumFromThenTo,

  -- * Conversions
  toList, fromList
) where

import Data.Vector.Fusion.Stream.Size
import Data.Vector.Fusion.Util ( Box(..) )

import Control.Monad  ( liftM )
import Prelude hiding ( length, null,
                        replicate, (++),
                        head, last, (!!),
                        init, tail, take, drop,
                        map, mapM, mapM_, concatMap,
                        zipWith, zipWith3,
                        filter, takeWhile, dropWhile,
                        elem, notElem,
                        foldl, foldl1, foldr, foldr1,
                        and, or,
                        scanl, scanl1,
                        enumFromTo, enumFromThenTo )
import qualified Prelude

import Data.Int  ( Int8, Int16, Int32, Int64 )
import Data.Word ( Word8, Word16, Word32, Word, Word64 )

#include "vector.h"

-- | Result of taking a single step in a stream
data Step s a = Yield a s  -- ^ a new element and a new seed
              | Skip    s  -- ^ just a new seed
              | Done       -- ^ end of stream

-- | Monadic streams
data Stream m a = forall s. Stream (s -> m (Step s a)) s Size

-- | 'Size' hint of a 'Stream'
size :: Stream m a -> Size
{-# INLINE size #-}
size (Stream _ _ sz) = sz

-- | Attach a 'Size' hint to a 'Stream'
sized :: Stream m a -> Size -> Stream m a
{-# INLINE_STREAM sized #-}
sized (Stream step s _) sz = Stream step s sz

-- Length
-- ------

-- | Length of a 'Stream'
length :: Monad m => Stream m a -> m Int
{-# INLINE_STREAM length #-}
length s = foldl' (\n _ -> n+1) 0 s

-- | Check if a 'Stream' is empty
null :: Monad m => Stream m a -> m Bool
{-# INLINE_STREAM null #-}
null s = foldr (\_ _ -> False) True s


-- Construction
-- ------------

-- | Empty 'Stream'
empty :: Monad m => Stream m a
{-# INLINE_STREAM empty #-}
empty = Stream (const (return Done)) () (Exact 0)

-- | Singleton 'Stream'
singleton :: Monad m => a -> Stream m a
{-# INLINE_STREAM singleton #-}
singleton x = Stream (return . step) True (Exact 1)
  where
    {-# INLINE_INNER step #-}
    step True  = Yield x False
    step False = Done

-- | Replicate a value to a given length
replicate :: Monad m => Int -> a -> Stream m a
{-# INLINE_STREAM replicate #-}
replicate n x = Stream (return . step) n (Exact (max n 0))
  where
    {-# INLINE_INNER step #-}
    step i | i > 0     = Yield x (i-1)
           | otherwise = Done

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
{-# INLINE_STREAM (++) #-}
Stream stepa sa na ++ Stream stepb sb nb = Stream step (Left sa) (na + nb)
  where
    {-# INLINE_INNER step #-}
    step (Left  sa) = do
                        r <- stepa sa
                        case r of
                          Yield x sa' -> return $ Yield x (Left  sa')
                          Skip    sa' -> return $ Skip    (Left  sa')
                          Done        -> return $ Skip    (Right sb)
    step (Right sb) = do
                        r <- stepb sb
                        case r of
                          Yield x sb' -> return $ Yield x (Right sb')
                          Skip    sb' -> return $ Skip    (Right sb')
                          Done        -> return $ Done

-- Accessing elements
-- ------------------

-- | First element of the 'Stream' or error if empty
head :: Monad m => Stream m a -> m a
{-# INLINE_STREAM head #-}
head (Stream step s _) = head_loop s
  where
    head_loop s
      = do
          r <- step s
          case r of
            Yield x _  -> return x
            Skip    s' -> head_loop s'
            Done       -> BOUNDS_ERROR(emptyStream) "head"

-- | Last element of the 'Stream' or error if empty
last :: Monad m => Stream m a -> m a
{-# INLINE_STREAM last #-}
last (Stream step s _) = last_loop0 s
  where
    last_loop0 s
      = do
          r <- step s
          case r of
            Yield x s' -> last_loop1 x s'
            Skip    s' -> last_loop0   s'
            Done       -> BOUNDS_ERROR(emptyStream) "last"

    last_loop1 x s
      = do
          r <- step s
          case r of
            Yield y s' -> last_loop1 y s'
            Skip    s' -> last_loop1 x s'
            Done       -> return x

-- | Element at the given position
(!!) :: Monad m => Stream m a -> Int -> m a
{-# INLINE (!!) #-}
Stream step s _ !! i | i < 0     = BOUNDS_ERROR(error) "!!" "negative index"
                     | otherwise = loop s i
  where
    loop s i
      = i `seq`
        do
          r <- step s
          case r of
            Yield x s' | i == 0    -> return x
                       | otherwise -> loop s' (i-1)
            Skip    s'             -> loop s' i
            Done                   -> BOUNDS_ERROR(emptyStream) "!!"

-- Substreams
-- ----------

-- | Extract a substream of the given length starting at the given position.
extract :: Monad m => Stream m a -> Int   -- ^ starting index
                                 -> Int   -- ^ length
                                 -> Stream m a
{-# INLINE extract #-}
extract s i n = take n (drop i s)

-- | All but the last element
init :: Monad m => Stream m a -> Stream m a
{-# INLINE_STREAM init #-}
init (Stream step s sz) = Stream step' (Nothing, s) (sz - 1)
  where
    {-# INLINE_INNER step' #-}
    step' (Nothing, s) = liftM (\r ->
                           case r of
                             Yield x s' -> Skip (Just x,  s')
                             Skip    s' -> Skip (Nothing, s')
                             Done       -> BOUNDS_ERROR(emptyStream) "init"
                         ) (step s)

    step' (Just x,  s) = liftM (\r -> 
                           case r of
                             Yield y s' -> Yield x (Just y, s')
                             Skip    s' -> Skip    (Just x, s')
                             Done       -> Done
                         ) (step s)

-- | All but the first element
tail :: Monad m => Stream m a -> Stream m a
{-# INLINE_STREAM tail #-}
tail (Stream step s sz) = Stream step' (Left s) (sz - 1)
  where
    {-# INLINE_INNER step' #-}
    step' (Left  s) = liftM (\r ->
                        case r of
                          Yield x s' -> Skip (Right s')
                          Skip    s' -> Skip (Left  s')
                          Done       -> BOUNDS_ERROR(emptyStream) "tail"
                      ) (step s)

    step' (Right s) = liftM (\r ->
                        case r of
                          Yield x s' -> Yield x (Right s')
                          Skip    s' -> Skip    (Right s')
                          Done       -> Done
                      ) (step s)

-- | The first @n@ elements
take :: Monad m => Int -> Stream m a -> Stream m a
{-# INLINE_STREAM take #-}
take n (Stream step s sz) = Stream step' (s, 0) (smaller (Exact n) sz)
  where
    {-# INLINE_INNER step' #-}
    step' (s, i) | i < n = liftM (\r ->
                             case r of
                               Yield x s' -> Yield x (s', i+1)
                               Skip    s' -> Skip    (s', i)
                               Done       -> Done
                           ) (step s)
    step' (s, i) = return Done

-- | All but the first @n@ elements
drop :: Monad m => Int -> Stream m a -> Stream m a
{-# INLINE_STREAM drop #-}
drop n (Stream step s sz) = Stream step' (s, Just n) (sz - Exact n)
  where
    {-# INLINE_INNER step' #-}
    step' (s, Just i) | i > 0 = liftM (\r ->
                                case r of
                                   Yield x s' -> Skip (s', Just (i-1))
                                   Skip    s' -> Skip (s', Just i)
                                   Done       -> Done
                                ) (step s)
                      | otherwise = return $ Skip (s, Nothing)

    step' (s, Nothing) = liftM (\r ->
                           case r of
                             Yield x s' -> Yield x (s', Nothing)
                             Skip    s' -> Skip    (s', Nothing)
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
{-# INLINE_STREAM mapM #-}
mapM f (Stream step s n) = Stream step' s n
  where
    {-# INLINE_INNER step' #-}
    step' s = do
                r <- step s
                case r of
                  Yield x s' -> liftM  (`Yield` s') (f x)
                  Skip    s' -> return (Skip    s')
                  Done       -> return Done

-- | Execute a monadic action for each element of the 'Stream'
mapM_ :: Monad m => (a -> m b) -> Stream m a -> m ()
{-# INLINE_STREAM mapM_ #-}
mapM_ m (Stream step s _) = mapM_loop s
  where
    mapM_loop s
      = do
          r <- step s
          case r of
            Yield x s' -> do { m x; mapM_loop s' }
            Skip    s' -> mapM_loop s'
            Done       -> return ()

-- | Transform a 'Stream' to use a different monad
trans :: (Monad m, Monad m') => (forall a. m a -> m' a)
                             -> Stream m a -> Stream m' a
{-# INLINE_STREAM trans #-}
trans f (Stream step s n) = Stream (f . step) s n

unbox :: Monad m => Stream m (Box a) -> Stream m a
{-# INLINE_STREAM unbox #-}
unbox (Stream step s n) = Stream step' s n
  where
    {-# INLINE_INNER step' #-}
    step' s = do
                r <- step s
                case r of
                  Yield (Box x) s' -> return $ Yield x s'
                  Skip          s' -> return $ Skip    s'
                  Done             -> return $ Done

-- Zipping
-- -------

-- | Zip two 'Stream's with the given function
zipWith :: Monad m => (a -> b -> c) -> Stream m a -> Stream m b -> Stream m c
{-# INLINE zipWith #-}
zipWith f = zipWithM (\a b -> return (f a b))

-- | Zip two 'Stream's with the given monadic function
zipWithM :: Monad m => (a -> b -> m c) -> Stream m a -> Stream m b -> Stream m c
{-# INLINE_STREAM zipWithM #-}
zipWithM f (Stream stepa sa na) (Stream stepb sb nb)
  = Stream step (sa, sb, Nothing) (smaller na nb)
  where
    {-# INLINE_INNER step #-}
    step (sa, sb, Nothing) = liftM (\r ->
                               case r of
                                 Yield x sa' -> Skip (sa', sb, Just x)
                                 Skip    sa' -> Skip (sa', sb, Nothing)
                                 Done        -> Done
                             ) (stepa sa)

    step (sa, sb, Just x)  = do
                               r <- stepb sb
                               case r of
                                 Yield y sb' ->
                                   do
                                     z <- f x y
                                     return $ Yield z (sa, sb', Nothing)
                                 Skip    sb' -> return $ Skip (sa, sb', Just x)
                                 Done        -> return $ Done

-- FIXME: This might expose an opportunity for inplace execution.
{-# RULES

"zipWithM xs xs [Vector.Stream]" forall f xs.
  zipWithM f xs xs = mapM (\x -> f x x) xs

  #-}

-- | Zip three 'Stream's with the given function
zipWith3 :: Monad m => (a -> b -> c -> d) -> Stream m a -> Stream m b -> Stream m c -> Stream m d
{-# INLINE zipWith3 #-}
zipWith3 f = zipWith3M (\a b c -> return (f a b c))

-- | Zip three 'Stream's with the given monadic function
zipWith3M :: Monad m => (a -> b -> c -> m d) -> Stream m a -> Stream m b -> Stream m c -> Stream m d
{-# INLINE_STREAM zipWith3M #-}
zipWith3M f (Stream stepa sa na) (Stream stepb sb nb) (Stream stepc sc nc)
  = Stream step (sa, sb, sc, Nothing) (smaller na (smaller nb nc))
  where
    {-# INLINE_INNER step #-}
    step (sa, sb, sc, Nothing) = do
        r <- stepa sa
        return $ case r of
            Yield x sa' -> Skip (sa', sb, sc, Just (x, Nothing))
            Skip    sa' -> Skip (sa', sb, sc, Nothing)
            Done        -> Done

    step (sa, sb, sc, Just (x, Nothing)) = do
        r <- stepb sb
        return $ case r of
            Yield y sb' -> Skip (sa, sb', sc, Just (x, Just y))
            Skip    sb' -> Skip (sa, sb', sc, Just (x, Nothing))
            Done        -> Done

    step (sa, sb, sc, Just (x, Just y)) = do
        r <- stepc sc
        case r of
            Yield z sc' -> f x y z >>= (\res -> return $ Yield res (sa, sb, sc', Nothing))
            Skip    sc' -> return $ Skip (sa, sb, sc', Just (x, Just y))
            Done        -> return $ Done

-- Filtering
-- ---------

-- | Drop elements which do not satisfy the predicate
filter :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
{-# INLINE filter #-}
filter f = filterM (return . f)

-- | Drop elements which do not satisfy the monadic predicate
filterM :: Monad m => (a -> m Bool) -> Stream m a -> Stream m a
{-# INLINE_STREAM filterM #-}
filterM f (Stream step s n) = Stream step' s (toMax n)
  where
    {-# INLINE_INNER step' #-}
    step' s = do
                r <- step s
                case r of
                  Yield x s' -> do
                                  b <- f x
                                  return $ if b then Yield x s'
                                                else Skip    s'
                  Skip    s' -> return $ Skip s'
                  Done       -> return $ Done

-- | Longest prefix of elements that satisfy the predicate
takeWhile :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
{-# INLINE takeWhile #-}
takeWhile f = takeWhileM (return . f)

-- | Longest prefix of elements that satisfy the monadic predicate
takeWhileM :: Monad m => (a -> m Bool) -> Stream m a -> Stream m a
{-# INLINE_STREAM takeWhileM #-}
takeWhileM f (Stream step s n) = Stream step' s (toMax n)
  where
    {-# INLINE_INNER step' #-}
    step' s = do
                r <- step s
                case r of
                  Yield x s' -> do
                                  b <- f x
                                  return $ if b then Yield x s' else Done
                  Skip    s' -> return $ Skip s'
                  Done       -> return $ Done

-- | Drop the longest prefix of elements that satisfy the predicate
dropWhile :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
{-# INLINE dropWhile #-}
dropWhile f = dropWhileM (return . f)

data DropWhile s a = DropWhile_Drop s | DropWhile_Yield a s | DropWhile_Next s

-- | Drop the longest prefix of elements that satisfy the monadic predicate
dropWhileM :: Monad m => (a -> m Bool) -> Stream m a -> Stream m a
{-# INLINE_STREAM dropWhileM #-}
dropWhileM f (Stream step s n) = Stream step' (DropWhile_Drop s) (toMax n)
  where
    -- NOTE: we jump through hoops here to have only one Yield; local data
    -- declarations would be nice!

    {-# INLINE_INNER step' #-}
    step' (DropWhile_Drop s)
      = do
          r <- step s
          case r of
            Yield x s' -> do
                            b <- f x
                            return $ if b then Skip (DropWhile_Drop    s')
                                          else Skip (DropWhile_Yield x s')
            Skip    s' -> return $ Skip (DropWhile_Drop    s')
            Done       -> return $ Done

    step' (DropWhile_Yield x s) = return $ Yield x (DropWhile_Next s)

    step' (DropWhile_Next s)
      = liftM (\r ->
          case r of
            Yield x s' -> Skip    (DropWhile_Yield x s')
            Skip    s' -> Skip    (DropWhile_Next    s')
            Done       -> Done
        ) (step s)

-- Searching
-- ---------

infix 4 `elem`
-- | Check whether the 'Stream' contains an element
elem :: (Monad m, Eq a) => a -> Stream m a -> m Bool
{-# INLINE_STREAM elem #-}
elem x (Stream step s _) = elem_loop s
  where
    elem_loop s
      = do
          r <- step s
          case r of
            Yield y s' | x == y    -> return True
                       | otherwise -> elem_loop s'
            Skip    s'             -> elem_loop s'
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
{-# INLINE_STREAM findM #-}
findM f (Stream step s _) = find_loop s
  where
    find_loop s
      = do
          r <- step s
          case r of
            Yield x s' -> do
                            b <- f x
                            if b then return $ Just x
                                 else find_loop s'
            Skip    s' -> find_loop s'
            Done       -> return Nothing

-- | Yield 'Just' the index of the first element that satisfies the predicate
-- or 'Nothing' if no such element exists.
findIndex :: Monad m => (a -> Bool) -> Stream m a -> m (Maybe Int)
{-# INLINE_STREAM findIndex #-}
findIndex f = findIndexM (return . f)

-- | Yield 'Just' the index of the first element that satisfies the monadic
-- predicate or 'Nothing' if no such element exists.
findIndexM :: Monad m => (a -> m Bool) -> Stream m a -> m (Maybe Int)
{-# INLINE_STREAM findIndexM #-}
findIndexM f (Stream step s _) = findIndex_loop s 0
  where
    findIndex_loop s i
      = do
          r <- step s
          case r of
            Yield x s' -> do
                            b <- f x
                            if b then return $ Just i
                                 else findIndex_loop s' (i+1)
            Skip    s' -> findIndex_loop s' i
            Done       -> return Nothing

-- Folding
-- -------

-- | Left fold
foldl :: Monad m => (a -> b -> a) -> a -> Stream m b -> m a
{-# INLINE foldl #-}
foldl f = foldlM (\a b -> return (f a b))

-- | Left fold with a monadic operator
foldlM :: Monad m => (a -> b -> m a) -> a -> Stream m b -> m a
{-# INLINE_STREAM foldlM #-}
foldlM m z (Stream step s _) = foldlM_loop z s
  where
    foldlM_loop z s
      = do
          r <- step s
          case r of
            Yield x s' -> do { z' <- m z x; foldlM_loop z' s' }
            Skip    s' -> foldlM_loop z s'
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
foldl1M :: Monad m => (a -> a -> m a) -> Stream m a -> m a
{-# INLINE_STREAM foldl1M #-}
foldl1M f (Stream step s sz) = foldl1M_loop s
  where
    foldl1M_loop s
      = do
          r <- step s
          case r of
            Yield x s' -> foldlM f x (Stream step s' (sz - 1))
            Skip    s' -> foldl1M_loop s'
            Done       -> BOUNDS_ERROR(emptyStream) "foldl1M"

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
{-# INLINE_STREAM foldlM' #-}
foldlM' m z (Stream step s _) = foldlM'_loop z s
  where
    foldlM'_loop z s
      = z `seq`
        do
          r <- step s
          case r of
            Yield x s' -> do { z' <- m z x; foldlM'_loop z' s' }
            Skip    s' -> foldlM'_loop z s'
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
foldl1M' :: Monad m => (a -> a -> m a) -> Stream m a -> m a
{-# INLINE_STREAM foldl1M' #-}
foldl1M' f (Stream step s sz) = foldl1M'_loop s
  where
    foldl1M'_loop s
      = do
          r <- step s
          case r of
            Yield x s' -> foldlM' f x (Stream step s' (sz - 1))
            Skip    s' -> foldl1M'_loop s'
            Done       -> BOUNDS_ERROR(emptyStream) "foldl1M'"

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
{-# INLINE_STREAM foldrM #-}
foldrM f z (Stream step s _) = foldrM_loop s
  where
    foldrM_loop s
      = do
          r <- step s
          case r of
            Yield x s' -> f x =<< foldrM_loop s'
            Skip    s' -> foldrM_loop s'
            Done       -> return z

-- | Right fold over a non-empty stream
foldr1 :: Monad m => (a -> a -> a) -> Stream m a -> m a
{-# INLINE foldr1 #-}
foldr1 f = foldr1M (\a b -> return (f a b))

-- | Right fold over a non-empty stream with a monadic operator
foldr1M :: Monad m => (a -> a -> m a) -> Stream m a -> m a
{-# INLINE_STREAM foldr1M #-}
foldr1M f (Stream step s _) = foldr1M_loop0 s
  where
    foldr1M_loop0 s
      = do
          r <- step s
          case r of
            Yield x s' -> foldr1M_loop1 x s'
            Skip    s' -> foldr1M_loop0   s'
            Done       -> BOUNDS_ERROR(emptyStream) "foldr1M"

    foldr1M_loop1 x s
      = do
          r <- step s
          case r of
            Yield y s' -> f x =<< foldr1M_loop1 y s'
            Skip    s' -> foldr1M_loop1 x s'
            Done       -> return x

-- Specialised folds
-- -----------------

and :: Monad m => Stream m Bool -> m Bool
{-# INLINE_STREAM and #-}
and (Stream step s _) = and_loop s
  where
    and_loop s
      = do
          r <- step s
          case r of
            Yield False _  -> return False
            Yield True  s' -> and_loop s'
            Skip        s' -> and_loop s'
            Done           -> return True

or :: Monad m => Stream m Bool -> m Bool
{-# INLINE_STREAM or #-}
or (Stream step s _) = or_loop s
  where
    or_loop s
      = do
          r <- step s
          case r of
            Yield False s' -> or_loop s'
            Yield True  _  -> return True
            Skip        s' -> or_loop s'
            Done           -> return False

concatMap :: Monad m => (a -> Stream m b) -> Stream m a -> Stream m b
{-# INLINE concatMap #-}
concatMap f = concatMapM (return . f)

concatMapM :: Monad m => (a -> m (Stream m b)) -> Stream m a -> Stream m b
{-# INLINE_STREAM concatMapM #-}
concatMapM f (Stream step s _) = Stream concatMap_go (Left s) Unknown
  where
    concatMap_go (Left s) = do
        r <- step s
        case r of
            Yield a s' -> do
                b_stream <- f a
                return $ Skip (Right (b_stream, s'))
            Skip    s' -> return $ Skip (Left s')
            Done       -> return Done
    concatMap_go (Right (Stream inner_step inner_s sz, s)) = do
        r <- inner_step inner_s
        case r of
            Yield b inner_s' -> return $ Yield b (Right (Stream inner_step inner_s' sz, s))
            Skip    inner_s' -> return $ Skip (Right (Stream inner_step inner_s' sz, s))
            Done             -> return $ Skip (Left s)

-- Unfolding
-- ---------

-- | Unfold
unfoldr :: Monad m => (s -> Maybe (a, s)) -> s -> Stream m a
{-# INLINE_STREAM unfoldr #-}
unfoldr f = unfoldrM (return . f)

-- | Unfold with a monadic function
unfoldrM :: Monad m => (s -> m (Maybe (a, s))) -> s -> Stream m a
{-# INLINE_STREAM unfoldrM #-}
unfoldrM f s = Stream step s Unknown
  where
    {-# INLINE_INNER step #-}
    step s = liftM (\r ->
               case r of
                 Just (x, s') -> Yield x s'
                 Nothing      -> Done
             ) (f s)

-- Scans
-- -----

-- | Prefix scan
prescanl :: Monad m => (a -> b -> a) -> a -> Stream m b -> Stream m a
{-# INLINE prescanl #-}
prescanl f = prescanlM (\a b -> return (f a b))

-- | Prefix scan with a monadic operator
prescanlM :: Monad m => (a -> b -> m a) -> a -> Stream m b -> Stream m a
{-# INLINE_STREAM prescanlM #-}
prescanlM f z (Stream step s sz) = Stream step' (s,z) sz
  where
    {-# INLINE_INNER step' #-}
    step' (s,x) = do
                    r <- step s
                    case r of
                      Yield y s' -> do
                                      z <- f x y
                                      return $ Yield x (s', z)
                      Skip    s' -> return $ Skip (s', x)
                      Done       -> return Done

-- | Prefix scan with strict accumulator
prescanl' :: Monad m => (a -> b -> a) -> a -> Stream m b -> Stream m a
{-# INLINE prescanl' #-}
prescanl' f = prescanlM' (\a b -> return (f a b))

-- | Prefix scan with strict accumulator and a monadic operator
prescanlM' :: Monad m => (a -> b -> m a) -> a -> Stream m b -> Stream m a
{-# INLINE_STREAM prescanlM' #-}
prescanlM' f z (Stream step s sz) = Stream step' (s,z) sz
  where
    {-# INLINE_INNER step' #-}
    step' (s,x) = x `seq`
                  do
                    r <- step s
                    case r of
                      Yield y s' -> do
                                      z <- f x y
                                      return $ Yield x (s', z)
                      Skip    s' -> return $ Skip (s', x)
                      Done       -> return Done

-- | Suffix scan
postscanl :: Monad m => (a -> b -> a) -> a -> Stream m b -> Stream m a
{-# INLINE postscanl #-}
postscanl f = postscanlM (\a b -> return (f a b))

-- | Suffix scan with a monadic operator
postscanlM :: Monad m => (a -> b -> m a) -> a -> Stream m b -> Stream m a
{-# INLINE_STREAM postscanlM #-}
postscanlM f z (Stream step s sz) = Stream step' (s,z) sz
  where
    {-# INLINE_INNER step' #-}
    step' (s,x) = do
                    r <- step s
                    case r of
                      Yield y s' -> do
                                      z <- f x y
                                      return $ Yield z (s',z)
                      Skip    s' -> return $ Skip (s',x)
                      Done       -> return Done

-- | Suffix scan with strict accumulator
postscanl' :: Monad m => (a -> b -> a) -> a -> Stream m b -> Stream m a
{-# INLINE postscanl' #-}
postscanl' f = postscanlM' (\a b -> return (f a b))

-- | Suffix scan with strict acccumulator and a monadic operator
postscanlM' :: Monad m => (a -> b -> m a) -> a -> Stream m b -> Stream m a
{-# INLINE_STREAM postscanlM' #-}
postscanlM' f z (Stream step s sz) = z `seq` Stream step' (s,z) sz
  where
    {-# INLINE_INNER step' #-}
    step' (s,x) = x `seq`
                  do
                    r <- step s
                    case r of
                      Yield y s' -> do
                                      z <- f x y
                                      z `seq` return (Yield z (s',z))
                      Skip    s' -> return $ Skip (s',x)
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

-- | Scan over a non-empty 'Stream'
scanl1 :: Monad m => (a -> a -> a) -> Stream m a -> Stream m a
{-# INLINE scanl1 #-}
scanl1 f = scanl1M (\x y -> return (f x y))

-- | Scan over a non-empty 'Stream' with a monadic operator
scanl1M :: Monad m => (a -> a -> m a) -> Stream m a -> Stream m a
{-# INLINE_STREAM scanl1M #-}
scanl1M f (Stream step s sz) = Stream step' (s, Nothing) sz
  where
    {-# INLINE_INNER step' #-}
    step' (s, Nothing) = do
                           r <- step s
                           case r of
                             Yield x s' -> return $ Yield x (s', Just x)
                             Skip    s' -> return $ Skip (s', Nothing)
                             Done       -> BOUNDS_ERROR(emptyStream) "scanl1M"

    step' (s, Just x) = do
                          r <- step s
                          case r of
                            Yield y s' -> do
                                            z <- f x y
                                            return $ Yield z (s', Just z)
                            Skip    s' -> return $ Skip (s', Just x)
                            Done       -> return Done

-- | Scan over a non-empty 'Stream' with a strict accumulator
scanl1' :: Monad m => (a -> a -> a) -> Stream m a -> Stream m a
{-# INLINE scanl1' #-}
scanl1' f = scanl1M' (\x y -> return (f x y))

-- | Scan over a non-empty 'Stream' with a strict accumulator and a monadic
-- operator
scanl1M' :: Monad m => (a -> a -> m a) -> Stream m a -> Stream m a
{-# INLINE_STREAM scanl1M' #-}
scanl1M' f (Stream step s sz) = Stream step' (s, Nothing) sz
  where
    {-# INLINE_INNER step' #-}
    step' (s, Nothing) = do
                           r <- step s
                           case r of
                             Yield x s' -> x `seq` return (Yield x (s', Just x))
                             Skip    s' -> return $ Skip (s', Nothing)
                             Done       -> BOUNDS_ERROR(emptyStream) "scanl1M"

    step' (s, Just x) = x `seq`
                        do
                          r <- step s
                          case r of
                            Yield y s' -> do
                                            z <- f x y
                                            z `seq` return (Yield z (s', Just z))
                            Skip    s' -> return $ Skip (s', Just x)
                            Done       -> return Done

-- Enumerations
-- ------------

-- FIXME: The Enum class is broken for this, there just doesn't seem to be a
-- way to implement this generically. We have to either specialise (which
-- doesn't help in polymorphic loops) or define a new Enum-like class with a
-- proper interface.

-- | Enumerate values from @x@ to @y@
enumFromTo :: (Enum a, Monad m) => a -> a -> Stream m a
{-# INLINE_STREAM enumFromTo #-}
enumFromTo x y = fromList [x .. y]

enumFromTo_small :: (Enum a, Ord a, Monad m) => a -> a -> Stream m a
{-# INLINE_STREAM enumFromTo_small #-}
enumFromTo_small x y = Stream step x (Exact n)
  where
    n = max (fromEnum y - fromEnum x + 1) 0

    {-# INLINE_INNER step #-}
    step x | x <= y    = return $ Yield x (succ x)
           | otherwise = return $ Done

{-# RULES

"enumFromTo<Int> [Stream]"
  enumFromTo = enumFromTo_small :: Monad m => Int -> Int -> Stream m Int

"enumFromTo<Char> [Stream]"
  enumFromTo = enumFromTo_small :: Monad m => Char -> Char -> Stream m Char

"enumFromTo<Int8> [Stream]"
  enumFromTo = enumFromTo_small :: Monad m => Int8 -> Int8 -> Stream m Int8

"enumFromTo<Int16> [Stream]"
  enumFromTo = enumFromTo_small :: Monad m => Int16 -> Int16 -> Stream m Int16

"enumFromTo<Int32> [Stream]"
  enumFromTo = enumFromTo_small :: Monad m => Int32 -> Int32 -> Stream m Int32

"enumFromTo<Word8> [Stream]"
  enumFromTo = enumFromTo_small :: Monad m => Word8 -> Word8 -> Stream m Word8

"enumFromTo<Word16> [Stream]"
  enumFromTo = enumFromTo_small :: Monad m => Word16 -> Word16 -> Stream m Word16

  #-}

enumFromTo_big :: (Enum a, Integral a, Monad m) => a -> a -> Stream m a
{-# INLINE_STREAM enumFromTo_big #-}
enumFromTo_big x y = Stream step x (Exact n)
  where
    n | x > y = 0
      | y - x < fromIntegral (maxBound :: Int) = fromIntegral (y-x+1)
      | otherwise = error $ "vector.enumFromTo_big: Array too large"

    {-# INLINE_INNER step #-}
    step x | x <= y    = return $ Yield x (succ x)
           | otherwise = return $ Done

{-# RULES


"enumFromTo<Int64> [Stream]"
  enumFromTo = enumFromTo_big :: Monad m => Int64 -> Int64 -> Stream m Int64

"enumFromTo<Word32> [Stream]"
  enumFromTo = enumFromTo_big :: Monad m => Word32 -> Word32 -> Stream m Word32

"enumFromTo<Word64> [Stream]"
  enumFromTo = enumFromTo_big :: Monad m => Word64 -> Word64 -> Stream m Word64

"enumFromTo<Integer> [Stream]"
  enumFromTo = enumFromTo_big :: Monad m => Integer -> Integer -> Stream m Integer

  #-}


-- | Enumerate values from @x@ to @y@
enumFromThenTo :: (Enum a, Monad m) => a -> a -> a -> Stream m a
{-# INLINE_STREAM enumFromThenTo #-}
enumFromThenTo x y z = fromList [x, y .. y]

-- Conversions
-- -----------

-- | Convert a 'Stream' to a list
toList :: Monad m => Stream m a -> m [a]
{-# INLINE toList #-}
toList = foldr (:) []

-- | Convert a list to a 'Stream'
fromList :: Monad m => [a] -> Stream m a
{-# INLINE_STREAM fromList #-}
fromList xs = Stream step xs Unknown
  where
    step (x:xs) = return (Yield x xs)
    step []     = return Done

