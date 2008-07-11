{-# LANGUAGE ExistentialQuantification, BangPatterns, CPP #-}

#include "phases.h"

module Data.Vector.Stream (
  Step(..), Stream(..),

  size, sized, unfold, toList, fromList,
  empty, singleton, replicate, (++),
  map, filter, zipWith,
  foldr, foldl, foldl',
  mapM_, foldM
) where

import Data.Vector.Stream.Size

import Prelude hiding ( replicate, (++), map, filter, zipWith,
                        foldr, foldl,
                        mapM_ )

data Step s a = Yield a s
              | Skip    s
              | Done

data Stream a = forall s. Stream (s -> Step s a) s Size

size :: Stream a -> Size
{-# INLINE size #-}
size (Stream _ _ sz) = sz

sized :: Stream a -> Size -> Stream a
{-# INLINE_STREAM sized #-}
sized (Stream step s _) sz = Stream step s sz

unfold :: (s -> Maybe (a, s)) -> s -> Stream a
{-# INLINE_STREAM unfold #-}
unfold f s = Stream step s Unknown
  where
    {-# INLINE step #-}
    step s = case f s of
               Just (x, s') -> Yield x s'
               Nothing      -> Done

toList :: Stream a -> [a]
{-# INLINE toList #-}
toList s = foldr (:) [] s

fromList :: [a] -> Stream a
{-# INLINE_STREAM fromList #-}
fromList xs = Stream step xs Unknown
  where
    step (x:xs) = Yield x xs
    step []     = Done

empty :: Stream a
{-# INLINE_STREAM empty #-}
empty = Stream (const Done) () (Exact 0)

singleton :: a -> Stream a
{-# INLINE_STREAM singleton #-}
singleton x = Stream step True (Exact 1)
  where
    {-# INLINE step #-}
    step True  = Yield x False
    step False = Done

replicate :: Int -> a -> Stream a
{-# INLINE_STREAM replicate #-}
replicate n x = Stream step n (Exact (max n 0))
  where
    {-# INLINE step #-}
    step i | i > 0     = Yield x (i-1)
           | otherwise = Done

infixr ++
(++) :: Stream a -> Stream a -> Stream a
{-# INLINE_STREAM (++) #-}
Stream stepa sa na ++ Stream stepb sb nb = Stream step (Left sa) (na + nb)
  where
    step (Left  sa) = case stepa sa of
                        Yield x sa' -> Yield x (Left  sa')
                        Skip    sa' -> Skip    (Left  sa')
                        Done        -> Skip    (Right sb)
    step (Right sb) = case stepb sb of
                        Yield x sb' -> Yield x (Right sb')
                        Skip    sb' -> Skip    (Right sb')
                        Done        -> Done

map :: (a -> b) -> Stream a -> Stream b
{-# INLINE_STREAM map #-}
map f (Stream step s n) = Stream step' s n
  where
    {-# INLINE step' #-}
    step' s = case step s of
                Yield x s' -> Yield (f x) s'
                Skip    s' -> Skip        s'
                Done       -> Done

filter :: (a -> Bool) -> Stream a -> Stream a
{-# INLINE_STREAM filter #-}
filter f (Stream step s n) = Stream step' s (toMax n)
  where
    {-# INLINE step' #-}
    step' s = case step s of
                Yield x s' | f x       -> Yield x s'
                           | otherwise -> Skip    s'
                Skip    s'             -> Skip    s'
                Done                   -> Done

zipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
{-# INLINE_STREAM zipWith #-}
zipWith f (Stream stepa sa na) (Stream stepb sb nb)
  = Stream step (sa, sb, Nothing) (smaller na nb)
  where
    {-# INLINE step #-}
    step (sa, sb, Nothing) = case stepa sa of
                               Yield x sa' -> Skip (sa', sb, Just x)
                               Skip    sa' -> Skip (sa', sb, Nothing)
                               Done        -> Done

    step (sa, sb, Just x)  = case stepb sb of
                               Yield y sb' -> Yield (f x y) (sa, sb', Nothing)
                               Skip    sb' -> Skip          (sa, sb', Just x)
                               Done        -> Done

foldl :: (a -> b -> a) -> a -> Stream b -> a
{-# INLINE_STREAM foldl #-}
foldl f z (Stream step s _) = foldl_go z s
  where
    foldl_go z s = case step s of
                     Yield x s' -> foldl_go (f z x) s'
                     Skip    s' -> foldl_go z       s'
                     Done       -> z

foldl' :: (a -> b -> a) -> a -> Stream b -> a
{-# INLINE_STREAM foldl' #-}
foldl' f !z (Stream step s _) = foldl_go z s
  where
    foldl_go !z s = case step s of
                      Yield x s' -> foldl_go (f z x) s'
                      Skip    s' -> foldl_go z       s'
                      Done       -> z

foldr :: (a -> b -> b) -> b -> Stream a -> b
{-# INLINE_STREAM foldr #-}
foldr f z (Stream step s _) = foldr_go s
  where
    foldr_go s = case step s of
                   Yield x s' -> f x (foldr_go s')
                   Skip    s' -> foldr_go s'
                   Done       -> z

mapM_ :: Monad m => (a -> m ()) -> Stream a -> m ()
{-# INLINE_STREAM mapM_ #-}
mapM_ m (Stream step s _) = mapM_go s
   where
     mapM_go s = case step s of
                   Yield x s' -> do { m x; mapM_go s' }
                   Skip    s' -> mapM_go s'
                   Done       -> return ()

foldM :: Monad m => (a -> b -> m a) -> a -> Stream b -> m a
{-# INLINE_STREAM foldM #-}
foldM m z (Stream step s _) = foldM_go z s
  where
    foldM_go z s = case step s of
                     Yield x s' -> do { z' <- m z x; foldM_go z' s' }
                     Skip    s' -> foldM_go z s'
                     Done       -> return z

