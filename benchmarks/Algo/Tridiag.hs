module Algo.Tridiag ( tridiag ) where

import Data.Vector.Unboxed as V

tridiag :: (Vector Double, Vector Double, Vector Double, Vector Double)
            -> Vector Double
{-# NOINLINE tridiag #-}
tridiag (as,bs,cs,ds) = xs
    where
      (cs',ds') = V.unzip
                $ V.prescanl' modify (0,0)
                $ V.zip (V.zip as bs) (V.zip cs ds)

      modify (c',d') ((a,b),(c,d)) = 
                   let id = 1 / (b - c'*a)
                   in
                   id `seq` (c*id, (d-d'*a)*id)

      xs = V.prescanr' (\(c,d) x' -> d - c*x') 0 (V.zip cs' ds')

