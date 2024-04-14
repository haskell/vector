module Boilerplater where

import Data.List (stripPrefix)
import Test.Tasty.QuickCheck

import Language.Haskell.TH


testProperties :: [Name] -> Q Exp
testProperties nms = fmap ListE $ sequence [[| testProperty $(stringE prop_name) $(varE nm) |]
                                           | nm <- nms
                                           , Just prop_name <- [stripPrefix "prop_" (nameBase nm)]]
