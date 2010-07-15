module Timeline
  ( Timeline
  , newTimeline
  , now
  , interfere
  )
  where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

import Life

newtype Timeline = Timeline (TVar World)

oneSecond :: Int
oneSecond = 1000 * 1000

newTimeline :: Address -> IO Timeline
newTimeline a = do
  timeline <- liftM Timeline (newTVarIO (newWorld a))
  forkIO $ do
    forever $ do
      threadDelay oneSecond
      interfere evolve timeline
  return timeline

now :: Timeline -> IO World
now (Timeline tvar) = readTVarIO tvar

interfere :: (World -> World) -> Timeline -> IO ()
interfere f (Timeline tvar) = atomically $ do
  world <- readTVar tvar
  writeTVar tvar (f world)
