{-# LANGUAGE ExistentialQuantification #-}

#include "phases.h"

module Data.Vector.Fusion.Stream.Monadic (
  Stream(..),

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

  -- * Mapping and zipping
  map, mapM, mapM_, zipWith, zipWithM,

  -- * Filtering
  filter, filterM, takeWhile, takeWhileM, dropWhile, dropWhileM,

  -- * Searching
  elem, notElem, find, findM, findIndex, findIndexM,

  -- * Folding
  foldl, foldlM, foldM, foldl1, foldl1M,
  foldl', foldlM', foldl1', foldl1M',
  foldr, foldrM, foldr1, foldr1M,

  -- * Unfolding
  unfold, unfoldM,

  -- * Scans
  prescanl, prescanlM, prescanl', prescanlM',

  toList, fromList
) where

import Data.Vector.Fusion.Stream.Step
import Data.Vector.Fusion.Stream.Size

import Control.Monad  ( liftM )
import Prelude hiding ( length, null,
                        replicate, (++),
                        head, last, (!!),
                        init, tail, take, drop,
                        map, mapM, mapM_, zipWith,
                        filter, takeWhile, dropWhile,
                        elem, notElem,
                        foldl, foldl1, foldr, foldr1 )
import qualified Prelude

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
    {-# INLINE step #-}
    step True  = Yield x False
    step False = Done

-- | Replicate a value to a given length
replicate :: Monad m => Int -> a -> Stream m a
{-# INLINE_STREAM replicate #-}
replicate n x = Stream (return . step) n (Exact (max n 0))
  where
    {-# INLINE step #-}
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
    head_loop s = do
                    r <- step s
                    case r of
                      Yield x _  -> return x
                      Skip    s' -> head_loop s'
                      Done       -> errorEmptyStream "head"

-- | Last element of the 'Stream' or error if empty
last :: Monad m => Stream m a -> m a
{-# INLINE_STREAM last #-}
last (Stream step s _) = last_loop0 s
  where
    last_loop0 s = do
                     r <- step s
                     case r of
                       Yield x s' -> last_loop1 x s'
                       Skip    s' -> last_loop0   s'
                       Done       -> errorEmptyStream "last"

    last_loop1 x s = do
                       r <- step s
                       case r of
                         Yield y s' -> last_loop1 y s'
                         Skip    s' -> last_loop1 x s'
                         Done       -> return x

-- | Element at the given position
(!!) :: Monad m => Stream m a -> Int -> m a
{-# INLINE (!!) #-}
s !! i = head (drop i s)

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
    {-# INLINE step' #-}
    step' (Nothing, s) = liftM (\r ->
                           case r of
                             Yield x s' -> Skip (Just x,  s')
                             Skip    s' -> Skip (Nothing, s')
                             Done       -> Done  -- FIXME: should be an error
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
    {-# INLINE step' #-}
    step' (Left  s) = liftM (\r ->
                        case r of
                          Yield x s' -> Skip (Right s')
                          Skip    s' -> Skip (Left  s')
                          Done       -> Done    -- FIXME: should be error?
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
    {-# INLINE step' #-}
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
    {-# INLINE step' #-}
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
                     

-- Mapping/zipping
-- ---------------

instance Monad m => Functor (Stream m) where
  {-# INLINE fmap #-}
  fmap = map

map :: Monad m => (a -> b) -> Stream m a -> Stream m b
{-# INLINE map #-}
map f = mapM (return . f)

mapM :: Monad m => (a -> m b) -> Stream m a -> Stream m b
{-# INLINE_STREAM mapM #-}
mapM f (Stream step s n) = Stream step' s n
  where
    {-# INLINE step' #-}
    step' s = do
                r <- step s
                case r of
                  Yield x s' -> liftM  (`Yield` s') (f x)
                  Skip    s' -> return (Skip    s')
                  Done       -> return Done

mapM_ :: Monad m => (a -> m b) -> Stream m a -> m ()
{-# INLINE_STREAM mapM_ #-}
mapM_ m (Stream step s _) = mapM_go s
  where
    mapM_go s = do
                  r <- step s
                  case r of
                    Yield x s' -> do { m x; mapM_go s' }
                    Skip    s' -> mapM_go s'
                    Done       -> return ()

-- | Zip two 'Stream's with the given function
zipWith :: Monad m => (a -> b -> c) -> Stream m a -> Stream m b -> Stream m c
{-# INLINE zipWith #-}
zipWith f = zipWithM (\a b -> return (f a b))

-- | Zip two 'Stream's with the given function
zipWithM :: Monad m => (a -> b -> m c) -> Stream m a -> Stream m b -> Stream m c
{-# INLINE_STREAM zipWithM #-}
zipWithM f (Stream stepa sa na) (Stream stepb sb nb)
  = Stream step (sa, sb, Nothing) (smaller na nb)
  where
    {-# INLINE step #-}
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

-- Filtering
-- ---------

-- | Drop elements which do not satisfy the predicate
filter :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
{-# INLINE filter #-}
filter f = filterM (return . f)

-- | Drop elements which do not satisfy the predicate
filterM :: Monad m => (a -> m Bool) -> Stream m a -> Stream m a
{-# INLINE_STREAM filterM #-}
filterM f (Stream step s n) = Stream step' s (toMax n)
  where
    {-# INLINE step' #-}
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

-- | Longest prefix of elements that satisfy the predicate
takeWhileM :: Monad m => (a -> m Bool) -> Stream m a -> Stream m a
{-# INLINE_STREAM takeWhileM #-}
takeWhileM f (Stream step s n) = Stream step' s (toMax n)
  where
    {-# INLINE step' #-}
    step' s = do
                r <- step s
                case r of
                  Yield x s' -> do
                                  b <- f x
                                  return $ if b then Yield x s' else Done
                  Skip    s' -> return $ Skip s'
                  Done       -> return $ Done


dropWhile :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
{-# INLINE dropWhile #-}
dropWhile f = dropWhileM (return . f)

data DropWhile s a = DropWhile_Drop s | DropWhile_Yield a s | DropWhile_Next s

-- | Drop the longest prefix of elements that satisfy the predicate
dropWhileM :: Monad m => (a -> m Bool) -> Stream m a -> Stream m a
{-# INLINE_STREAM dropWhileM #-}
dropWhileM f (Stream step s n) = Stream step' (DropWhile_Drop s) (toMax n)
  where
    -- NOTE: we jump through hoops here to have only one Yield; local data
    -- declarations would be nice!

    {-# INLINE step' #-}
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
    elem_loop s = do
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

find :: Monad m => (a -> Bool) -> Stream m a -> m (Maybe a)
{-# INLINE find #-}
find f = findM (return . f)

-- | Yield 'Just' the first element matching the predicate or 'Nothing' if no
-- such element exists.
findM :: Monad m => (a -> m Bool) -> Stream m a -> m (Maybe a)
{-# INLINE_STREAM findM #-}
findM f (Stream step s _) = find_loop s
  where
    find_loop s = do
                    r <- step s
                    case r of
                      Yield x s' -> do
                                      b <- f x
                                      if b then return $ Just x
                                           else find_loop s'
                      Skip    s' -> find_loop s'
                      Done       -> return Nothing

findIndex :: Monad m => (a -> Bool) -> Stream m a -> m (Maybe Int)
{-# INLINE_STREAM findIndex #-}
findIndex f = findIndexM (return . f)

-- | Yield 'Just' the index of the first element matching the predicate or
-- 'Nothing' if no such element exists.
findIndexM :: Monad m => (a -> m Bool) -> Stream m a -> m (Maybe Int)
{-# INLINE_STREAM findIndexM #-}
findIndexM f (Stream step s _) = findIndex_loop s 0
  where
    findIndex_loop s i = do
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

foldl :: Monad m => (a -> b -> a) -> a -> Stream m b -> m a
{-# INLINE foldl #-}
foldl f = foldlM (\a b -> return (f a b))

foldlM :: Monad m => (a -> b -> m a) -> a -> Stream m b -> m a
{-# INLINE_STREAM foldlM #-}
foldlM m z (Stream step s _) = foldlM_go z s
  where
    foldlM_go z s = do
                      r <- step s
                      case r of
                        Yield x s' -> do { z' <- m z x; foldlM_go z' s' }
                        Skip    s' -> foldlM_go z s'
                        Done       -> return z

foldM :: Monad m => (a -> b -> m a) -> a -> Stream m b -> m a
{-# INLINE foldM #-}
foldM = foldlM

foldl1 :: Monad m => (a -> a -> a) -> Stream m a -> m a
{-# INLINE foldl1 #-}
foldl1 f = foldl1M (\a b -> return (f a b))

foldl1M :: Monad m => (a -> a -> m a) -> Stream m a -> m a
{-# INLINE_STREAM foldl1M #-}
foldl1M f (Stream step s sz) = foldl1M_go s
  where
    foldl1M_go s = do
                     r <- step s
                     case r of
                       Yield x s' -> foldlM f x (Stream step s' (sz - 1))
                       Skip    s' -> foldl1M_go s'
                       Done       -> errorEmptyStream "foldl1M"

foldl' :: Monad m => (a -> b -> a) -> a -> Stream m b -> m a
{-# INLINE foldl' #-}
foldl' f = foldlM' (\a b -> return (f a b))

foldlM' :: Monad m => (a -> b -> m a) -> a -> Stream m b -> m a
{-# INLINE_STREAM foldlM' #-}
foldlM' m z (Stream step s _) = foldlM'_go z s
  where
    foldlM'_go z s = z `seq`
                     do
                       r <- step s
                       case r of
                         Yield x s' -> do { z' <- m z x; foldlM'_go z' s' }
                         Skip    s' -> foldlM'_go z s'
                         Done       -> return z

foldl1' :: Monad m => (a -> a -> a) -> Stream m a -> m a
{-# INLINE foldl1' #-}
foldl1' f = foldl1M' (\a b -> return (f a b))

foldl1M' :: Monad m => (a -> a -> m a) -> Stream m a -> m a
{-# INLINE_STREAM foldl1M' #-}
foldl1M' f (Stream step s sz) = foldl1M'_go s
  where
    foldl1M'_go s = do
                      r <- step s
                      case r of
                        Yield x s' -> foldlM' f x (Stream step s' (sz - 1))
                        Skip    s' -> foldl1M'_go s'
                        Done       -> errorEmptyStream "foldl1M'"

foldr :: Monad m => (a -> b -> b) -> b -> Stream m a -> m b
{-# INLINE foldr #-}
foldr f = foldrM (\a b -> return (f a b))

foldrM :: Monad m => (a -> b -> m b) -> b -> Stream m a -> m b
{-# INLINE_STREAM foldrM #-}
foldrM f z (Stream step s _) = foldrM_go s
  where
    foldrM_go s = do
                    r <- step s
                    case r of
                      Yield x s' -> f x =<< foldrM_go s'
                      Skip    s' -> foldrM_go s'
                      Done       -> return z

foldr1 :: Monad m => (a -> a -> a) -> Stream m a -> m a
{-# INLINE foldr1 #-}
foldr1 f = foldr1M (\a b -> return (f a b))

foldr1M :: Monad m => (a -> a -> m a) -> Stream m a -> m a
{-# INLINE_STREAM foldr1M #-}
foldr1M f (Stream step s _) = foldr1M_go0 s
  where
    foldr1M_go0 s = do
                      r <- step s
                      case r of
                        Yield x s' -> foldr1M_go1 x s'
                        Skip    s' -> foldr1M_go0   s'
                        Done       -> errorEmptyStream "foldr1M"

    foldr1M_go1 x s = do
                        r <- step s
                        case r of
                          Yield y s' -> f x =<< foldr1M_go1 y s'
                          Skip    s' -> foldr1M_go1 x s'
                          Done       -> return x

-- Unfolding
-- ---------

-- | Unfold
unfold :: Monad m => (s -> Maybe (a, s)) -> s -> Stream m a
{-# INLINE_STREAM unfold #-}
unfold f = unfoldM (return . f)

unfoldM :: Monad m => (s -> m (Maybe (a, s))) -> s -> Stream m a
{-# INLINE_STREAM unfoldM #-}
unfoldM f s = Stream step s Unknown
  where
    {-# INLINE step #-}
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

-- | Prefix scan
prescanlM :: Monad m => (a -> b -> m a) -> a -> Stream m b -> Stream m a
{-# INLINE_STREAM prescanlM #-}
prescanlM f z (Stream step s sz) = Stream step' (s,z) sz
  where
    {-# INLINE step' #-}
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

-- | Prefix scan with strict accumulator
prescanlM' :: Monad m => (a -> b -> m a) -> a -> Stream m b -> Stream m a
{-# INLINE_STREAM prescanlM' #-}
prescanlM' f z (Stream step s sz) = Stream step' (s,z) sz
  where
    {-# INLINE step' #-}
    step' (s,x) = x `seq`
                  do
                    r <- step s
                    case r of
                      Yield y s' -> do
                                      z <- f x y
                                      return $ Yield x (s', z)
                      Skip    s' -> return $ Skip (s', x)
                      Done       -> return Done

-- Conversions
-- -----------

toList :: Monad m => Stream m a -> m [a]
{-# INLINE toList #-}
toList = foldr (:) []

fromList :: Monad m => [a] -> Stream m a
{-# INLINE_STREAM fromList #-}
fromList xs = Stream step xs Unknown
  where
    step (x:xs) = return (Yield x xs)
    step []     = return Done


errorEmptyStream :: String -> a
errorEmptyStream s = error $ "Data.Vector.Fusion.Stream.Monadic."
                        Prelude.++ s Prelude.++ ": empty stream"

