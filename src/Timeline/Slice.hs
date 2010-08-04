module Timeline.Slice
  ( Slice, Tick, HistorySize, Projector
  , projection, world, tick
  , newSlice, nextSlice
  , maxCloseTickDifference, farApart, after
  , addUserInput, trimHistory
  )
  where

import Data.List
import Data.Word

import Life

type Tick = Word16
type HistorySize = Word8
type Projector a = World -> Tick -> a
type UserInputs = [World -> World]

data Slice a = Slice { projection :: a, slHistory :: HistoricalRecord }
data HistoricalRecord = Evolution { hrTick::Tick, hrInput::UserInputs, hrPrevious::HistoricalRecord }
                      | BaseState { hrTick::Tick, hrInput::UserInputs, baseWorld::World }

instance Show (Slice a) where
  show slice = "World size " ++ (show $ size (world slice)) ++
               " at tick " ++ (show $ tick slice) ++
               " with " ++ (show $ hrLength (slHistory slice)) ++ " historical records"

maxCloseTickDifference :: Word16
maxCloseTickDifference = 11

tick :: Slice a -> Tick
tick = hrTick . slHistory

world :: Slice a -> World
world = hrWorld . slHistory

hrIsBase :: HistoricalRecord -> Bool
hrIsBase (BaseState _ _ _) = True
hrIsBase _ = False

hrLength :: HistoricalRecord -> Int
hrLength hr | hrIsBase hr = 1
            | otherwise = 1 + hrLength (hrPrevious hr)

hrWorld :: HistoricalRecord -> World
hrWorld hr | null (hrInput hr) = hrWorld' hr
           | otherwise = foldl1' merge worldsWithUserInput

  where hrWorld' hr' | hrIsBase hr' = baseWorld hr'
                     | otherwise = evolve . hrWorld . hrPrevious $ hr'
        worldsWithUserInput = map ($ hrWorld' hr) (hrInput hr)

farApart :: Tick -> Tick -> Bool
farApart tick1 tick2 = tick2 - tick1 > maxCloseTickDifference && tick1 - tick2 > maxCloseTickDifference

after :: Tick -> Tick -> Bool
t1 `after` t2 | farApart t1 t2 = True
              | otherwise = (t1 - t2) < (t2 - t1)

newSlice :: Projector a -> World -> Tick -> Slice a
newSlice f w t = mkSlice f BaseState { hrTick = t
                                     , hrInput = []
                                     , baseWorld = w
                                     }

nextSlice :: Projector b -> Slice a -> Slice b
nextSlice f s = mkSlice f Evolution { hrTick = tick s + 1
                                    , hrInput = []
                                    , hrPrevious = slHistory s
                                    }

mkSlice :: Projector a -> HistoricalRecord -> Slice a
mkSlice f hr = s where s = Slice { projection = f (world s) (tick s), slHistory = hr }

trimHistory :: HistorySize -> Slice a -> Slice a
trimHistory n s = s { slHistory = trimHistory' n (slHistory s) }
  where trimHistory' n' hr | hrIsBase hr = hr
                           | n' == 1 = BaseState { hrTick = hrTick hr
                                                 , hrInput = hrInput hr
                                                 , baseWorld = evolve . hrWorld . hrPrevious $ hr
                                                 }
                           | otherwise = hr { hrPrevious = trimHistory' (n' - 1) (hrPrevious hr) }

addUserInput :: Tick -> (World -> World) -> Slice a -> Maybe (Slice a)
addUserInput inputTick f s = do newHistory <- addUserInput' (slHistory s)
                                return $ s { slHistory = newHistory }
  where addUserInput' hr | hrTick hr == inputTick = return $ hr { hrInput = f : hrInput hr }
                         | hrIsBase hr = Nothing
                         | otherwise = do newPrevious <- addUserInput' (hrPrevious hr)
                                          return $ hr { hrPrevious = newPrevious }

