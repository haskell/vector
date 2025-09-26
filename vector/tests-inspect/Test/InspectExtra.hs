{-# LANGUAGE TemplateHaskell #-}
-- |
-- Helpers for fusion tests
module Test.InspectExtra
  ( noStream
  , inspectFusion
  , inspectClassyFusion
  , module Test.Tasty.Inspection
  ) where

import Language.Haskell.TH (Name,Q,Exp)
import Test.Tasty.Inspection

import qualified Data.Stream.Monadic   as S


noStream :: Name -> Obligation
noStream = (`doesNotUseAnyOf` ['S.Yield, 'S.Skip, 'S.Done])

inspectFusion :: Name -> Q Exp
inspectFusion = inspectObligations [ noStream
                                   , hasNoTypeClasses
                                   ]

inspectClassyFusion :: Name -> Q Exp
inspectClassyFusion = inspectObligations [ noStream ]
