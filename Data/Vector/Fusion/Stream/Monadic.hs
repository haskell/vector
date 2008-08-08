{-# LANGUAGE ExistentialQuantification #-}

#include "phases.h"

module Data.Vector.Fusion.Stream.Monadic (
  Stream,

  sized,

  unfoldM, foldM,

  map, mapM, filter, filterM
) where

import Data.Vector.Fusion.Stream.Step
import Data.Vector.Fusion.Stream.Size

import Control.Monad  ( liftM )
import Prelude hiding ( map, mapM, filter )

data Stream m a = forall s. Stream (s -> m (Step s a)) s Size

sized :: Stream m a -> Size -> Stream m a
{-# INLINE_STREAM sized #-}
sized (Stream step s _) sz = Stream step s sz

unfoldM :: Monad m => (s -> m (Maybe (a, s))) -> s -> Stream m a
{-# INLINE_STREAM unfoldM #-}
unfoldM f s = Stream step s Unknown
  where
    {-# INLINE step #-}
    step s = do
               r <- f s
               case r of
                 Just (x, s') -> return $ Yield x s'
                 Nothing      -> return $ Done

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

filter :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
{-# INLINE filter #-}
filter f = filterM (return . f)

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
                                  if b then return $ Yield x s'
                                       else return $ Skip    s'
                  Skip    s' -> return $ Skip s'
                  Done       -> return $ Done

foldl :: Monad m => (a -> b -> a) -> a -> Stream m b -> m a
{-# INLINE foldl #-}
foldl f = foldM (\a b -> return (f a b))

foldr :: Monad m => (a -> b -> b) -> b -> Stream m a -> m b
{-# INLINE foldr #-}
foldr f = foldrM (\a b -> return (f a b))

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

foldM :: Monad m => (a -> b -> m a) -> a -> Stream m b -> m a
{-# INLINE foldM #-}
foldM = foldlM

foldM' :: Monad m => (a -> b -> m a) -> a -> Stream m b -> m a
{-# INLINE foldM' #-}
foldM' = foldlM'

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

