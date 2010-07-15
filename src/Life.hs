module Life
  ( Cell(..)
  , Dimension
  , Address
  , World
  , newWorld
  , size
  , evolve
  , setCellAt
  , cellAt
  )
  where

import Data.Array.IArray

data Cell = Alive | Dead
type Dimension = Int
type Address = (Dimension, Dimension)

newtype World = World (Array Address Cell)

newWorld :: Address -> World
newWorld worldSize = World $ listArray ((0,0), worldSize) (cycle [Dead, Alive])

size :: World -> Address
size (World ary) = snd $ bounds ary

evolve :: World -> World
evolve (World ary) = World $ amap flipCell ary
  where flipCell Alive = Dead
        flipCell Dead = Alive

setCellAt :: World -> Address -> Cell -> World
setCellAt w _ _ = w

cellAt :: World -> Address -> Cell
cellAt (World ary) ix = ary ! ix
