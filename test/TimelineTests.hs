module TimelineTests where

import TestHelper

import Prelude hiding (catch)
import Control.Monad (liftM)
import Control.Exception (catch, SomeException)
import Data.Maybe
import Text.Printf

import Life
import Timeline
import Pattern

tests :: Test
tests = testGroup "Timeline" [
  "a tick that is far from another tick is always after it" `testProperty` \tick1 tick2 ->
    farApart tick1 tick2 ==> (tick1 `after` tick2) .&. (tick2 `after` tick1)

 ,"when ticks are close together, one is after the other" `testProperty` \(CloseTogether tick1 tick2) ->
    (tick1 `after` tick2) /= (tick2 `after` tick1)

 ,"t1 `close` t2 && t1 `after` t2 ==> t1 `after` (t2 + 1) || t1 == (t2 + 1)" `testProperty` \(CloseTogether tick1 tick2) ->
    tick1 `after` tick2 ==> tick1 `after` (tick2 + 1) || tick1 == (tick2 + 1)

 ,"t1 `close` t2 && t1 `after` t2 ==> t1 `after` (t2 - 1) || farApart t1 (t2 - 1)" `testProperty` \(CloseTogether tick1 tick2) ->
    tick1 `after` tick2 ==> tick1 `after` (tick2 - 1) || farApart tick1 (tick2 - 1)

 ,"a tick is not after itself" `testProperty` \t-> (t`after` t) == False

 ,"nextSlice increments tick by one" `testProperty` \slice (Blind projector) ->
      let _ = (slice, projector) :: (Slice (), Projector ())
      in (tick $ nextSlice projector slice) == (tick slice) + 1

 ,"nextSlice projects new world using projector" `testProperty` \slice (Blind projector) ->
      let _ = (slice, projector) :: (Slice (), Projector Int)
          slice' = nextSlice projector slice
      in projection slice' == projector (world slice') (tick slice')

 ,"nextSlice evolves new world from current one" `testProperty` \slice (Blind projector) ->
      let _ = (slice, projector) :: (Slice (), Projector ())
          slice' = nextSlice projector slice
      in world slice' == evolve (world slice)

 ,"nextSlice evolves from userInput if present" `testProperty` \slice (Blind projector) input ->
      let _ = (slice, projector) :: (Slice (), Projector ())
          Just sliceWithInput = addUserInput (tick slice) (drawPatternAt (0,0) input) slice
          slice' = nextSlice projector sliceWithInput
      in world slice' == evolve (drawPatternAt (0,0) input $ world slice)

 ,"nextSlice evolves world from user historical input" `testProperty` \slice (Blind projector) input ->
      let _ = (slice, projector) :: (Slice (), Projector ())
          futureSlice = foldl (.) id (replicate 3 $ nextSlice projector) slice
          Just futureSliceWithInput = addUserInput (tick slice) (drawPatternAt (0,0) input) futureSlice
          slice' = nextSlice projector futureSliceWithInput
      in world slice' == foldl (.) id (replicate 4 $ evolve) (drawPatternAt (0,0) input $ world slice)

 ,"addUserInput returns Nothing when input tick is outside history" `testProperty` \slice (Blind projector) input ->
      let _ = (slice, projector) :: (Slice (), Projector ())
          futureSlice = trimHistory 1 $ nextSlice projector slice
      in isNothing (addUserInput (tick slice) (drawPatternAt (0,0) input) futureSlice) == True

 ,"interfereAt throws exception immediately when World -> World throws exception" `testCase` do
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

data CloseTogether = CloseTogether Tick Tick deriving (Show)

instance Arbitrary CloseTogether where
  arbitrary = do one <- arbitrary
                 op <- elements [(-), (+)]
                 diff <- elements [1..maxCloseTickDifference]
                 let two = (op one diff)
                 if farApart one two
                   then fail $ "CloseTogether generated ticks that are far apart: " ++ (show one) ++ ", " ++ (show two)
                   else return $ CloseTogether one two
