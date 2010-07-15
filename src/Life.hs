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
  , cells
  , neighboringAddresses
  , fate
  )
  where

import Data.Array.IArray

data Cell = Alive | Dead deriving (Show, Eq)
type Dimension = Int
type Address = (Dimension, Dimension)

newtype World = World (Array Address Cell)

instance Show World where
  show world = "World " ++ (show $ size world)

newWorld :: Address -> World
newWorld worldSize = newWorldWithCells worldSize (cycle [Dead, Alive])

newWorldWithCells :: Address -> [Cell] -> World
newWorldWithCells (width, height) cells = World $ listArray ((0,0), (width-1, height-1)) cells

size :: World -> Address
size (World ary) = (maxX + 1, maxY + 1)
  where (maxX, maxY) = snd $ bounds ary

evolve :: World -> World
evolve (World ary) = World $ amap flipCell ary
  where flipCell Alive = Dead
        flipCell Dead = Alive

setCellAt :: World -> Address -> Cell -> World
setCellAt w _ _ = w

cellAt :: World -> Address -> Cell
cellAt (World ary) ix = ary ! ix

cells :: World -> [Cell]
cells (World ary) = elems ary

fate :: Cell -> [Cell] -> Cell
fate Dead neighbors = if (length (filter (==Alive) neighbors)) == 3 then Alive else Dead
fate Alive neighbors = if (length (filter (==Alive) neighbors)) `elem` [2,3] then Alive else Dead

neighboringAddresses :: World -> Address -> [Address]
neighboringAddresses world (x, y) = map offset cases
  where (width, height) = size world
        cases = [(dX, dY) | dX <- [-1..1], dY <- [-1..1], not (dX == 0 && dY == 0)]
        offset (dX, dY) = ((x + dX) `mod` width, (y + dY) `mod` height)
