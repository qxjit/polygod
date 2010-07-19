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
data Timeline a = Timeline (TVar (World,Tick,a)) (World -> Tick -> a)

tickDelay :: Int
tickDelay = 1000 * 250

newTimeline :: Address -> (World -> Tick -> a) -> IO (Timeline a)
newTimeline addr f = do
  let world = newWorld addr
  tvar <- (newTVarIO (world, 0, f world 0))
  let timeline = Timeline tvar f
  forkIO $ do
    forever $ do
      threadDelay tickDelay
      interfere evolve timeline
  return timeline

now :: Timeline a -> IO (World, Tick, a)
now (Timeline tvar _) = readTVarIO tvar

worldAfter :: Tick -> Timeline a -> IO (World, Tick, a)
worldAfter oldTick (Timeline tvar _) = atomically $ do
  (world, newTick, x) <- readTVar tvar
  check (oldTick /= newTick)
  return (world, newTick, x)

interfere :: (World -> World) -> Timeline a -> IO ()
interfere f (Timeline tvar g) = atomically $ do
  (!world, !tick, _) <- readTVar tvar
  let !world' = f world -- force evaluation of the world now in case f raises any exceptions
      tick' = tick + 1
  writeTVar tvar (world', tick', g world' tick')

