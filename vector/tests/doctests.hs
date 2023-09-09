import Test.DocTest (doctest)

-- Doctests are weirdly fragile. For example running tests for module
-- A (D.V.Unboxed.Base) could cause tests in unrelated woudle B
-- (D.V.Storable) to start failing with weird errors.
--
-- In order to avoid this one would want to run doctests with
-- per-module granularity but this cause another sort of problems!
-- When we load only single module and use import doctests then some
-- data types may come from built library and some from ghci session.
--
-- This could be remedied by running doctests for groups of modules.
-- This _is_ convoluted setup but doctests now works for GHC9.4
main :: IO ()
main = mapM_ run modGroups
  where
    run mods = do
      mapM_ putStrLn mods
      doctest $ ["-Iinclude", "-Iinternal", "-XHaskell2010"] ++ mods
    --
    modGroups =
      [ [ "src/Data/Vector/Storable/Mutable.hs"
        , "src/Data/Vector/Storable.hs"
        ]
      , [ "src/Data/Vector.hs"
        , "src/Data/Vector/Mutable.hs"
        ]
      , [ "src/Data/Vector/Generic.hs"
        , "src/Data/Vector/Generic/Mutable.hs"
        ]
      , [ "src/Data/Vector/Primitive.hs"
        , "src/Data/Vector/Primitive/Mutable.hs"
        ]
      , [ "src/Data/Vector/Unboxed.hs"
        , "src/Data/Vector/Unboxed/Mutable.hs"
        , "src/Data/Vector/Unboxed/Base.hs"
        ]
      ]
