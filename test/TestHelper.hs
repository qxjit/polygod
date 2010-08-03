module TestHelper
  ( module Test.Framework
  , module Test.Framework.Providers.QuickCheck2
  , module Test.Framework.Providers.HUnit
  , module Test.QuickCheck
  , module Test.HUnit
  , module ArbitraryInstances
  , module Debug.Trace
  , module Util
  , dbg
  )
  where

import Debug.Trace

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

import Test.QuickCheck hiding (Testable)
import Test.HUnit hiding (Test, Testable)
import ArbitraryInstances

import Util

dbg :: Show a => String -> a -> b -> b
dbg s a = trace (s ++ ": " ++ show a)
