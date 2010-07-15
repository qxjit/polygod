{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment

import qualified Tests as Tests
import qualified WebApp as WebApp

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("--test":rest) -> withArgs rest Tests.main
    _ -> WebApp.main

