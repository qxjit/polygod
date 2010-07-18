module ArbitraryInstances where

import Life
import Control.Monad

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary

data AddressInWorld = AddressInWorld World Address deriving (Show)
data NeighborList = NeighborList [Cell] deriving (Show)

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

instance Arbitrary NeighborList where
  arbitrary = liftM NeighborList (vector 8)
