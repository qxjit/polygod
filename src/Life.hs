module Life
  ( Cell(..)
  , Dimension
  , Address
  , World
  , newWorld
  , size
  , evolve
  , interfere
  , cellAt
  )
  where

import Data.Array

data Cell = Alive | Dead
type Dimension = Int
type Address = (Dimension, Dimension)

newtype World = World (Array Address Cell)

newWorld :: Address -> World
newWorld worldSize = World $ listArray ((0,0), worldSize) (repeat Dead)

size :: World -> Address
size (World ary) = snd $ bounds ary

evolve :: World -> World
evolve = id

interfere :: World -> Address -> Cell -> World
interfere w _ _ = w

cellAt :: World -> Address -> Cell
cellAt (World ary) ix = ary ! ix
