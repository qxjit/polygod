module LifeTests where

import Life
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Data.List

import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary

tests :: Test
tests = testGroup "Life"
  [ "evolve maintains world size" `testProperty` \world -> (size world) == (size $ evolve world)
  , "world has correct cell count for size" `testProperty` \world ->
      let (width, height) = size world in length (cells world) == width * height

  , "new world has correct size and cells" `testProperty` \(width', height') someCells -> not (null someCells) ==>
      let worldSize@(width, height) = ((width' `mod` 100) + 1, (height' `mod` 100) + 1)
          worldCells = (take (width * height) (cycle someCells))
          world = newWorldWithCells worldSize worldCells
      in (size world) == worldSize && (cells world) == worldCells

  , "there are 8 neighbors" `testProperty` \(AddressInWorld world address) -> length (neighboringAddresses world address) == 8
  , "neighbors are unique" `testProperty` \(AddressInWorld world address) -> let (width, height) = size world in height >= 3 && width >= 3 ==> 
      nub (neighboringAddresses world address) == (neighboringAddresses world address)

  , "distance of neighbors should be less than 2" `testProperty` \(AddressInWorld world address) ->
      let (width, height) = size world
          toroidalDifference coord1 coord2 ceiling = min (abs (coord1 - coord2)) (ceiling - abs (coord1 - coord2))
          distance (x1, y1) (x2, y2) = (toroidalDifference x1 x2 width)^2 + (toroidalDifference y1 y2 height)^2
      in  maximum (map (distance address) (neighboringAddresses world address)) <= 2

  , "neighbors should be within bounds" `testProperty` \(AddressInWorld world address) ->
      let (width, height) = size world
          addressWithinBounds (neighborX, neighborY)= neighborX < width && neighborY < height
      in  all addressWithinBounds (neighboringAddresses world address)
  ]

data AddressInWorld = AddressInWorld World Address deriving (Show)

instance Arbitrary World where
  arbitrary = do
    width  <- choose (1, 100)
    height <- choose (1, 100)
    cells <- vector ((width) * (height))
    return $ newWorldWithCells (width, height) cells

instance Arbitrary Cell where
  arbitrary = elements [Alive, Dead]

instance Arbitrary AddressInWorld where
  arbitrary = do
    world <- arbitrary
    let (width, height) = size world
    x <- choose (0, width - 1)
    y <- choose (0, height - 1)
    return $ AddressInWorld world (x, y)
