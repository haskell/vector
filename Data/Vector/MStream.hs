{-# LANGUAGE ExistentialQuantification #-}

#include "phases.h"

module Data.Vector.MStream (
  MStream,

  sized,

  unfoldM, foldM,

  map, mapM, filter, filterM
) where

import Data.Vector.Stream ( Step(..) )
import Data.Vector.Stream.Size

import Control.Monad  ( liftM )
import Prelude hiding ( map, mapM, filter )

data MStream m a = forall s. MStream (s -> m (Step s a)) s Size

sized :: MStream m a -> Size -> MStream m a
{-# INLINE_STREAM sized #-}
sized (MStream step s _) sz = MStream step s sz

unfoldM :: Monad m => (s -> m (Maybe (a, s))) -> s -> MStream m a
{-# INLINE_STREAM unfoldM #-}
unfoldM f s = MStream step s Unknown
  where
    {-# INLINE step #-}
    step s = do
               r <- f s
               case r of
                 Just (x, s') -> return $ Yield x s'
                 Nothing      -> return $ Done

map :: Monad m => (a -> b) -> MStream m a -> MStream m b
{-# INLINE map #-}
map f = mapM (return . f)

mapM :: Monad m => (a -> m b) -> MStream m a -> MStream m b
{-# INLINE_STREAM mapM #-}
mapM f (MStream step s n) = MStream step' s n
  where
    {-# INLINE step' #-}
    step' s = do
                r <- step s
                case r of
                  Yield x s' -> liftM  (`Yield` s') (f x)
                  Skip    s' -> return (Skip    s')
                  Done       -> return Done

filter :: Monad m => (a -> Bool) -> MStream m a -> MStream m a
{-# INLINE filter #-}
filter f = filterM (return . f)

filterM :: Monad m => (a -> m Bool) -> MStream m a -> MStream m a
{-# INLINE_STREAM filterM #-}
filterM f (MStream step s n) = MStream step' s (toMax n)
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

foldM :: Monad m => (a -> b -> m a) -> a -> MStream m b -> m a
{-# INLINE_STREAM foldM #-}
foldM m z (MStream step s _) = foldM_go z s
  where
    foldM_go z s = do
                     r <- step s
                     case r of
                       Yield x s' -> do { z' <- m z x; foldM_go z' s' }
                       Skip    s' -> foldM_go z s'
                       Done       -> return z

