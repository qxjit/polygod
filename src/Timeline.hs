module Timeline
  ( Timeline, tlConfig
  , Tick
  , TickDelay
  , Projector
  , Configuration(..)
  , Slice, projection, world, tick
  , defaultConfig
  , trimHistory
  , newTimeline, stopTimeline, withTimeline
  , newSlice, nextSlice,sliceNow, sliceAfter
  , maxCloseTickDifference, farApart, after
  , addUserInput, interfereAt
  )
  where

import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM

import Data.List
import Data.Word

import Life

type Tick = Word16
type TickDelay = Int
type HistorySize = Word8
type Projector a = World -> Tick -> a
data Timeline a = Timeline { tlTVar::(TVar (Slice a)),
                             tlThreadId::ThreadId,
                             tlConfig::Configuration a }

data Slice a = Slice { projection :: a, world :: !World, tick :: !Tick, slHistory :: [(Tick, World)], slUserWorlds :: [World] }

instance Show (Slice a) where
  show slice = "World size " ++ (show $ size (world slice)) ++ " at tick " ++ (show $ tick slice)

data Configuration a = Config { tlSize::Address, tlTickDelay::TickDelay, tlProjector::Projector a, tlHistorySize::HistorySize }

defaultConfig :: Configuration ()
defaultConfig = Config { tlSize = (50, 50), tlTickDelay = 1000 * 250, tlProjector = const $ const $ (), tlHistorySize = 9 }

newTimeline :: Configuration a -> IO (Timeline a)
newTimeline config = do
  let initialWorld = newWorld (tlSize config)
  tvar <- newTVarIO $ newSlice (tlProjector config) initialWorld 0

  threadId <- forkIO $ do
    forever $ do
      threadDelay (tlTickDelay config)
      atomically $ do
        slice <- readTVar tvar
        let !slice' = nextSlice (tlProjector config) slice
        writeTVar tvar (trimHistory (tlHistorySize config) slice')

  return $ Timeline { tlTVar = tvar, tlThreadId = threadId, tlConfig = config }

stopTimeline :: Timeline a -> IO ()
stopTimeline = killThread . tlThreadId

withTimeline :: Configuration a -> (Timeline a -> IO b) -> IO b
withTimeline config action = bracket (newTimeline config) stopTimeline action

sliceNow :: Timeline a -> IO (Slice a)
sliceNow timeline = readTVarIO (tlTVar timeline)

maxCloseTickDifference :: Word16
maxCloseTickDifference = 11

farApart :: Tick -> Tick -> Bool
farApart tick1 tick2 = tick2 - tick1 > maxCloseTickDifference && tick1 - tick2 > maxCloseTickDifference

after :: Tick -> Tick -> Bool
t1 `after` t2 | farApart t1 t2 = True
              | otherwise = (t1 - t2) < (t2 - t1)

sliceAfter :: Tick -> Timeline a -> IO (Slice a)
sliceAfter oldTick timeline = atomically $ do
  slice <- readTVar (tlTVar timeline)
  check (tick slice `after` oldTick)
  return slice

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

interfereAt :: Tick -> (World -> World) -> Timeline a -> IO (Maybe ())
interfereAt inputTick f timeline = atomically $ do
  slice <- readTVar (tlTVar timeline)
  case addUserInput inputTick f slice of
    Just slice' -> liftM Just $ writeTVar (tlTVar timeline) slice'
    _ -> return Nothing

