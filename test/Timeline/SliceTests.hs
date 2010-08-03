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
      in world (nextSlice projector slice) == evolve (world slice)

 ,"nextSlice evolves from userInput if present" `testProperty` \slice (Blind projector) input ->
      let _ = (slice, projector) :: (Slice (), Projector ())

          Just sliceWithInput = addUserInput (tick slice) (drawPatternAt (0,0) input) slice
          slice' = nextSlice projector sliceWithInput
          expectedWorldWithInput = drawPatternAt (0,0) input $ world slice

      in world slice' == evolve expectedWorldWithInput &&
         world (nextSlice projector slice') == (evolve . evolve) expectedWorldWithInput

 ,"nextSlice unions multiple user inputs" `testProperty` \slice (Blind projector) input1 input2 ->
      let _ = (slice, projector) :: (Slice (), Projector ())

          Just sliceWithInput1 = addUserInput (tick slice) (drawPatternAt (0,0) input1) slice
          Just sliceWithInput2 = addUserInput (tick slice) (drawPatternAt (0,0) input2) sliceWithInput1

          expectedWorldWithInput1 = drawPatternAt (0,0) input1 (world slice)
          expectedWorldWithInput2 = drawPatternAt (0,0) input2 (world slice)

      in world (nextSlice projector sliceWithInput2) == evolve (merge expectedWorldWithInput1 expectedWorldWithInput2)

 ,"nextSlice evolves world from user historical input" `testProperty` \slice (Blind projector) input ->
      let _ = (slice, projector) :: (Slice (), Projector ())
          futureSlice = (3 `aps` nextSlice projector) slice
          Just futureSliceWithInput = addUserInput (tick slice) (drawPatternAt (0,0) input) futureSlice
          slice' = nextSlice projector futureSliceWithInput
      in world slice' == (4 `aps` evolve) (drawPatternAt (0,0) input $ world slice)

 ,"nextSlice unions multiple user inputs at multiple time points" `testProperty` \slice (Blind projector) input1 input2 ->
      let _ = (slice, projector) :: (Slice (), Projector ())
          input1Tick = tick slice + 1
          input2Tick = input1Tick - 1

          sliceAtInput1 = (2 `aps` nextSlice projector) slice
          Just sliceWithInput1 = addUserInput input1Tick (drawPatternAt (0,0) input1) sliceAtInput1

          sliceAtInput2 = nextSlice projector sliceWithInput1
          Just sliceWithInput2 = addUserInput input2Tick (drawPatternAt (0,0) input2) sliceAtInput2

          expectedWorldWithInput2 = drawPatternAt (0,0) input2 (world slice)
          expectedWorldWithInput1 = drawPatternAt (0,0) input1 (evolve expectedWorldWithInput2)

      in world (nextSlice projector sliceWithInput2) == (3 `aps` evolve) expectedWorldWithInput1


 ,"addUserInput returns Nothing when input tick is outside history" `testProperty` \slice (Blind projector) input ->
      let _ = (slice, projector) :: (Slice (), Projector ())
          futureSlice = trimHistory 1 $ nextSlice projector slice
      in isNothing (addUserInput (tick slice) (drawPatternAt (0,0) input) futureSlice) == True

 ,"trimHistory does not stop evolution" `testProperty` \slice (Blind projector) ->
      let _ = (slice, projector) :: (Slice (), Projector ())
          slice' = (4 `aps` nextSlice projector) slice
          slice'' = trimHistory 2 slice'
      in world slice' == world slice''
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
