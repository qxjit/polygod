module TimelineTests where

import TestHelper

import Prelude hiding (catch)
import Control.Monad (liftM)
import Control.Exception (catch, SomeException)
import Text.Printf

import Timeline

tests :: Test
tests = testGroup "Timeline" [
  "interfereAt throws exception immediately when World -> World throws exception" `testCase` do
    withTimeline (defaultConfig { tlTickDelay = 50 }) $ \timeline -> do
      sliceAfter 2 timeline
      x <- (interfereAt 1 (error "Caught It") timeline >> return "Didn't Catch It")
              `catch` (\e -> return $ show (e :: SomeException))
      x @?= "Caught It"

 ,"interefereAt returns the next tick that will include the change" `testCase` do
    withTimeline (defaultConfig { tlTickDelay = 10000 }) $ \timeline -> do
      sliceAfter 2 timeline
      interfereAt 1 id timeline >>= (@?= Just 4)

 ,"sliceAfter returns a projection following the given tick" `testCase` do
    withTimeline (defaultConfig {tlTickDelay = 50}) $ \timeline -> do
      let oldTick = 1
      projectedTick <- (liftM tick) (sliceAfter oldTick timeline)
      assertBool (printf "Should not have returned projection from before specified tick (%d > %d)" projectedTick oldTick)
                 (projectedTick `after` oldTick)
 ]

