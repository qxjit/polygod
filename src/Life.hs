module Life
  ( Cell(..)
  , Dimension
  , Address
  , World
  , newWorld
  , newWorldWithCells
  , merge
  , size
  , addresses
  , evolve
  , updateCells
  , cellAt
  , safeCellAt
  , cells
  , neighboringAddresses
  , fate
  )
  where

import           Data.Array.IArray
import           Data.Array.Unboxed

data Cell = Alive | Dead deriving (Show, Eq)
type Dimension = Int
type Address = (Dimension, Dimension)

newtype World = World (UArray Address Bool) deriving (Eq)

instance Show World where
  show world = "World " ++ (show $ size world)

newWorld :: Address -> World
newWorld worldSize = newWorldWithCells worldSize (cycle [Dead])

merge :: World -> World -> World
merge w1@(World ary1) w2@(World ary2) = World $ array
                                                ((0,0), (maxX, maxY))
                                                [ ((x,y), boolAt (x,y) ary1 || boolAt (x,y) ary2) | x <- [0..maxX], y <- [0..maxY] ]

  where (width1, height1) = size w1
        (width2, height2) = size w2
        boolAt ix ary | inRange (bounds ary) ix = ary ! ix
                      | otherwise = False
        (maxX, maxY) = (max width1 width2 - 1, max height1 height2 - 1)

toCell :: Bool -> Cell
toCell True = Alive
toCell False = Dead

fromCell :: Cell -> Bool
fromCell Alive = True
fromCell Dead = False

newWorldWithCells :: Address -> [Cell] -> World
newWorldWithCells (width, height) initialCells = World $ listArray ((0,0), (width-1, height-1)) (map fromCell initialCells)

size :: World -> Address
size (World ary) = (maxX + 1, maxY + 1)
  where (maxX, maxY) = snd $ bounds ary

addresses :: World -> [Address]
addresses (World ary) = indices ary

evolve :: World -> World
evolve world@(World ary) = World $ {-# SCC "evolve-array" #-} array (bounds ary) (map newCellAt $ indices ary)
  where newCellAt ix = (ix, fromCell $ fate (cellAt world ix) (map (cellAt world) (neighboringAddresses world ix)))

updateCells:: [(Address, Cell)] -> World -> World
updateCells updates (World ary) = World $ (ary // boolUpdates) where
  boolUpdates = map (\(address, cell) -> (address, fromCell cell)) updates

cellAt :: World -> Address -> Cell
cellAt (World ary) ix = toCell (ary ! ix)

safeCellAt :: World -> Address -> Cell
safeCellAt w@(World ary) ix | inRange (bounds ary) ix = cellAt w ix
                            | otherwise = Dead

cells :: World -> [Cell]
cells (World ary) = map toCell (elems ary)

fate :: Cell -> [Cell] -> Cell
fate Dead neighbors = if (length (filter (==Alive) neighbors)) == 3 then Alive else Dead
fate Alive neighbors = if (length (filter (==Alive) neighbors)) `elem` [2,3] then Alive else Dead

neighboringAddresses :: World -> Address -> [Address]
neighboringAddresses world (x, y) = [ (x |- 1, y $- 1), (x, y $- 1), (x |+ 1, y $- 1),
                                      (x |- 1, y     ),              (x |+ 1, y     ),
                                      (x |- 1, y $+ 1), (x, y $+ 1), (x |+ 1, y $+ 1) ]
  where (width, height) = size world
        x' |- n = (x' - n) `mod` width
        x' |+ n = (x' + n) `mod` width
        y' $- n = (y' - n) `mod` height
        y' $+ n = (y' + n) `mod` height
        {-# INLINE (|-) #-}
        {-# INLINE (|+) #-}
        {-# INLINE ($-) #-}
        {-# INLINE ($+) #-}


