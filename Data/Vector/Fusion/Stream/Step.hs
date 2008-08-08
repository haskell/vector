module Data.Vector.Fusion.Stream.Step (
  Step(..)
) where

data Step s a = Yield a s
              | Skip    s
              | Done

