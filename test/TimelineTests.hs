module TimelineTests where

import TestHelper

import Prelude hiding (catch)
import Control.Exception (catch, SomeException)

import Timeline

tests :: Test
tests = testGroup "Timeline" [
  "interfere throws exception immediately when World -> World throws exception" `testCase` do
    timeline <- newTimeline (1,1) (const $ const $ ())
    x <- (interfere (error "Caught It") timeline >> return "Didn't Catch It")
            `catch` (\e -> return $ show (e :: SomeException))
    x @?= "Caught It"
  ]
