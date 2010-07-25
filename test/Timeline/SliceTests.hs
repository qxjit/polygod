module Timeline.SliceTests where

import TestHelper

import Data.Maybe

import Life
import Timeline.Slice
import Pattern

tests :: Test
tests = testGroup "Timeline.Slice" [
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
          worldWithInput = (drawPatternAt (0,0) input $ world slice)
      in world slice' == evolve worldWithInput &&
         world (nextSlice projector slice') == (evolve . evolve) worldWithInput

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
