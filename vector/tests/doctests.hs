{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.DocTest (mainFromCabal)
import System.Environment (getArgs)

main :: IO ()
main = mainFromCabal "vector" =<< getArgs
