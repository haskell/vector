module Data.Vector.Stream.Size (
  Size(..), smaller, larger, toMax, upperBound
) where

data Size = Exact Int
          | Max   Int
          | Unknown
        deriving( Eq, Show )

instance Num Size where
  Exact m + Exact n = Exact (m+n)
  Exact m + Max   n = Max   (m+n)

  Max   m + Exact n = Max   (m+n)
  Max   m + Max   n = Max   (m+n)

  _       + _       = Unknown


  Exact m - Exact n = Exact (m-n)
  Exact m - Max   n = Max   m

  Max   m - Exact n = Max   (m-n)
  Max   m - Max   n = Max   m
  Max   m - Unknown = Max   m

  _       - _       = Unknown


  fromInteger n     = Exact (fromInteger n)


smaller :: Size -> Size -> Size
smaller (Exact m) (Exact n) = Exact (m `min` n)
smaller (Exact m) (Max   n) = Max   (m `min` n)
smaller (Exact m) Unknown   = Max   m
smaller (Max   m) (Exact n) = Max   (m `min` n)
smaller (Max   m) (Max   n) = Max   (m `min` n)
smaller (Max   m) Unknown   = Max   m
smaller Unknown   (Exact n) = Max   n
smaller Unknown   (Max   n) = Max   n
smaller Unknown   Unknown   = Unknown

larger :: Size -> Size -> Size
larger (Exact m) (Exact n)             = Exact (m `max` n)
larger (Exact m) (Max   n) | m >= n    = Exact m
                           | otherwise = Max   n
larger (Max   m) (Exact n) | n >= m    = Exact n
                           | otherwise = Max   m
larger (Max   m) (Max   n)             = Max   (m `max` n)
larger _         _                     = Unknown

toMax :: Size -> Size
toMax (Exact n) = Max n
toMax (Max   n) = Max n
toMax Unknown   = Unknown

lowerBound :: Size -> Int
lowerBound (Exact n) = n
lowerBound _         = 0

upperBound :: Size -> Maybe Int
upperBound (Exact n) = Just n
upperBound (Max   n) = Just n
upperBound Unknown   = Nothing

