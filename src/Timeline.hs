module Timeline
  ( module Timeline.Slice
  , Timeline, tlConfig
  , TickDelay
  , Configuration(..)
  , defaultConfig
  , newTimeline, stopTimeline, withTimeline
  , sliceNow, sliceAfter
  , interfereAt
  )
  where

import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM

import Life
import Timeline.Slice

type TickDelay = Int
data Timeline a = Timeline { tlTVar::(TVar (Slice a)),
                             tlThreadId::ThreadId,
                             tlConfig::Configuration a }

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

sliceAfter :: Tick -> Timeline a -> IO (Slice a)
sliceAfter oldTick timeline = atomically $ do
  slice <- readTVar (tlTVar timeline)
  check (tick slice `after` oldTick)
  return slice

interfereAt :: Tick -> (World -> World) -> Timeline a -> IO (Maybe Tick)
interfereAt inputTick f timeline = atomically $ do
  slice <- readTVar (tlTVar timeline)
  case addUserInput inputTick f slice of
    Just slice' -> do writeTVar (tlTVar timeline) slice'
                      return $ Just (tick slice' + 1)
    _ -> return Nothing

