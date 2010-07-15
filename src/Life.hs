module Life
  ( Cell(..)
  , Dimension
  , Address
  , World
  , newWorld
  , newWorldWithCells
  , size
  , evolve
  , setCellAt
  , cellAt
  )
  where

import Data.Array.IArray

data Cell = Alive | Dead deriving (Show)
type Dimension = Int
type Address = (Dimension, Dimension)

newtype World = World (Array Address Cell) deriving (Show)

newWorld :: Address -> World
newWorld worldSize = newWorldWithCells worldSize (cycle [Dead, Alive])

newWorldWithCells :: Address -> [Cell] -> World
newWorldWithCells worldSize cells = World $ listArray ((0,0), worldSize) cells

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
