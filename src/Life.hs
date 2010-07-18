module Life
  ( Cell(..)
  , Dimension
  , Address
  , World
  , newWorld
  , newWorldWithCells
  , size
  , evolve
  , updateCells
  , cellAt
  , cells
  , neighboringAddresses
  , fate
  )
  where

import Data.Array.IArray
import Data.Array.Unboxed

data Cell = Alive | Dead deriving (Show, Eq)
type Dimension = Int
type Address = (Dimension, Dimension)

newtype World = World (UArray Address Bool)

instance Show World where
  show world = "World " ++ (show $ size world)

newWorld :: Address -> World
newWorld worldSize = newWorldWithCells worldSize (cycle [Dead])

toCell :: Bool -> Cell
toCell True = Alive
toCell False = Dead

fromCell :: Cell -> Bool
fromCell Alive = True
fromCell Dead = False

newWorldWithCells :: Address -> [Cell] -> World
newWorldWithCells (width, height) cells = World $ listArray ((0,0), (width-1, height-1)) (map fromCell cells)

size :: World -> Address
size (World ary) = (maxX + 1, maxY + 1)
  where (maxX, maxY) = snd $ bounds ary

evolve :: World -> World
evolve world@(World ary) = World $ array (bounds ary) (map newCellAt $ indices ary)
  where newCellAt ix = (ix, fromCell $ fate (cellAt world ix) (map (cellAt world) (neighboringAddresses world ix)))

updateCells:: World -> [(Address, Cell)] -> World
updateCells (World ary) updates = World $ (ary // boolUpdates) where
  boolUpdates = map (\(address, cell) -> (address, fromCell cell)) updates

cellAt :: World -> Address -> Cell
cellAt (World ary) ix = toCell (ary ! ix)

cells :: World -> [Cell]
cells (World ary) = map toCell (elems ary)

fate :: Cell -> [Cell] -> Cell
fate Dead neighbors = if (length (filter (==Alive) neighbors)) == 3 then Alive else Dead
fate Alive neighbors = if (length (filter (==Alive) neighbors)) `elem` [2,3] then Alive else Dead

neighboringAddresses :: World -> Address -> [Address]
neighboringAddresses world (x, y) = map offset cases
  where (width, height) = size world
        cases = [(dX, dY) | dX <- [-1..1], dY <- [-1..1], not (dX == 0 && dY == 0)]
        offset (dX, dY) = ((x + dX) `mod` width, (y + dY) `mod` height)
