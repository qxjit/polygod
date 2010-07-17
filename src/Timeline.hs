module Timeline
  ( Timeline
  , Tick
  , newTimeline
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
newtype Timeline = Timeline (TVar (World,Tick))

tickDelay :: Int
tickDelay = 1000 * 250

newTimeline :: Address -> IO Timeline
newTimeline a = do
  timeline <- liftM Timeline (newTVarIO (newWorld a, 0))
  forkIO $ do
    forever $ do
      threadDelay tickDelay
      interfere evolve timeline
  return timeline

now :: Timeline -> IO (World, Tick)
now (Timeline tvar) = readTVarIO tvar

worldAfter :: Tick -> Timeline -> IO (World, Tick)
worldAfter oldTick (Timeline tvar) = atomically $ do
  (world, newTick) <- readTVar tvar
  check (oldTick /= newTick)
  return (world, newTick)

interfere :: (World -> World) -> Timeline -> IO ()
interfere f (Timeline tvar) = atomically $ do
  (world, tick) <- readTVar tvar
  writeTVar tvar ((f world), tick + 1)
