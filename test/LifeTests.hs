module LifeTests where

import Life
import Data.List
import TestHelper

tests :: Test
tests = testGroup "Life"
  [ "evolve maintains world size" `testProperty` \world -> (size world) == (size $ evolve world)
  , "world has correct cell count for size" `testProperty` \world ->
      let (width, height) = size world in genericLength (cells world) == width * height

  , "new world has correct size and cells" `testProperty` \(width', height') someCells -> not (null someCells) ==>
      let worldSize@(width, height) = ((width' `mod` 100) + 1, (height' `mod` 100) + 1)
          worldCells = (genericTake (width * height) (cycle someCells))
          world = newWorldWithCells worldSize worldCells
      in (size world) == worldSize && (cells world) == worldCells

  , "there are 8 neighbors" `testProperty` \(AddressInWorld world address) -> length (neighboringAddresses world address) == 8
  , "neighbors are unique" `testProperty` \(AddressInWorld world address) -> let (width, height) = size world in height >= 3 && width >= 3 ==> 
      nub (neighboringAddresses world address) == (neighboringAddresses world address)

  , "distance of neighbors should be less than 2" `testProperty` \(AddressInWorld world address) ->
      let (width, height) = size world
          toroidalDifference coord1 coord2 ceil = min (abs (coord1 - coord2)) (ceil - abs (coord1 - coord2))
          distance (x1, y1) (x2, y2) = (toroidalDifference x1 x2 width)^(2::Int) + (toroidalDifference y1 y2 height)^(2::Int)
      in  maximum (map (distance address) (neighboringAddresses world address)) <= 2

  , "neighbors should be within bounds" `testProperty` \(AddressInWorld world address) ->
      let (width, height) = size world
          addressWithinBounds (neighborX, neighborY)= neighborX < width && neighborY < height
      in  all addressWithinBounds (neighboringAddresses world address)

  , "a Dead cell is birthed by exactly 3 live neighbors" `testProperty` \(NeighborList neighbors) ->
      (fate Dead neighbors == Alive) == (length (filter (==Alive) neighbors) == 3)
  , "an Alive cell with 2 or 3 live neighbors lives" `testProperty` \(NeighborList neighbors) ->
      (fate Alive neighbors == Alive) == (length (filter (==Alive) neighbors) `elem` [2,3])
  ]
