module Timeline
  ( Timeline
  , Tick
  , newTimeline
  , stopTimeline
  , now
  , worldAfter
  , interfere
  )
  where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

import Data.Word

import Life

type Tick = Word16
data Timeline a = Timeline { tlTVar::(TVar (World,Tick,a)),
                             tlSharedView::(World -> Tick -> a),
                             tlThreadId::ThreadId }

tickDelay :: Int
tickDelay = 1000 * 250

newTimeline :: Address -> (World -> Tick -> a) -> IO (Timeline a)
newTimeline addr f = do
  let world = newWorld addr
  tvar <- (newTVarIO (world, 0, f world 0))
  let timeline = Timeline { tlTVar = tvar, tlSharedView = f, tlThreadId = undefined }

  threadId <- forkIO $ do
    localThreadId <- myThreadId
    let timeline' = timeline { tlThreadId = localThreadId }
    forever $ do
      threadDelay tickDelay
      interfere evolve timeline'

  return $ timeline { tlThreadId = threadId }

stopTimeline :: Timeline a -> IO ()
stopTimeline = killThread . tlThreadId

now :: Timeline a -> IO (World, Tick, a)
now = readTVarIO . tlTVar

worldAfter :: Tick -> Timeline a -> IO (World, Tick, a)
worldAfter oldTick timeline = atomically $ do
  (world, newTick, x) <- readTVar (tlTVar timeline)
  check (oldTick /= newTick)
  return (world, newTick, x)

interfere :: (World -> World) -> Timeline a -> IO ()
interfere f timeline = atomically $ do
  (!world, !tick, _) <- readTVar (tlTVar timeline)
  let !world' = f world -- force evaluation of the world now in case f raises any exceptions
      tick' = tick + 1
  writeTVar (tlTVar timeline) (world', tick', tlSharedView timeline world' tick')

