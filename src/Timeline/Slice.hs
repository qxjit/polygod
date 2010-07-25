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
data Slice a = Slice { projection :: a, world :: !World, tick :: !Tick, slHistory :: [(Tick, World)], slUserWorlds :: [World] }

instance Show (Slice a) where
  show slice = "World size " ++ (show $ size (world slice)) ++
               " at tick " ++ (show $ tick slice) ++
               " with " ++ (show $ length (slHistory slice)) ++ " historical records"

maxCloseTickDifference :: Word16
maxCloseTickDifference = 11

farApart :: Tick -> Tick -> Bool
farApart tick1 tick2 = tick2 - tick1 > maxCloseTickDifference && tick1 - tick2 > maxCloseTickDifference

after :: Tick -> Tick -> Bool
t1 `after` t2 | farApart t1 t2 = True
              | otherwise = (t1 - t2) < (t2 - t1)

newSlice :: Projector a -> World -> Tick -> Slice a
newSlice f w t = Slice { world = w, tick = t, projection = f w t, slHistory = [(t,w)], slUserWorlds = [] }

nextSlice :: Projector b -> Slice a -> Slice b
nextSlice f s = let tick' = (tick s) + 1
                    world' = evolve (head $ slUserWorlds s ++ [world s])
                in Slice { tick = tick'
                         , projection = f world' tick'
                         , world = world'
                         , slHistory = (tick', world') : (slHistory s)
                         , slUserWorlds = []
                         }

trimHistory :: HistorySize -> Slice a -> Slice a
trimHistory n s = s { slHistory = genericTake n (slHistory s) }

addUserInput :: Tick -> (World -> World) -> Slice a -> Maybe (Slice a)
addUserInput inputTick f s = do worldAtTimeOfInput <- lookup inputTick (slHistory s)
                                -- evaluating strictly so any exceptions will be raised while request is still being handled
                                let !userWorld = evolveUpTo (f worldAtTimeOfInput) inputTick (tick s)
                                return $ s { slUserWorlds = [ userWorld ] }
  where evolveUpTo w t t' | t == t' = w
                          | otherwise = evolveUpTo (evolve w) (t + 1) t'

