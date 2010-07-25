module ArbitraryInstances where

import Control.Monad
import Data.Word

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary

import Life
import Timeline
import Pattern

data AddressInWorld = AddressInWorld World Address deriving (Show)
data NeighborList = NeighborList [Cell] deriving (Show)

instance Arbitrary World where
  arbitrary = do
    width  <- liftM fromInteger $ choose (1, 100) :: Gen Dimension
    height <- liftM fromInteger $ choose (1, 100)
    cells <- vector $ fromEnum ((width) * (height))
    return $ newWorldWithCells (width, height) cells

instance CoArbitrary World where
  -- limit to 50 cells so tests will run faster
  coarbitrary = coarbitrary . take 50 . cells

instance Arbitrary Cell where
  arbitrary = elements [Alive, Dead]

instance CoArbitrary Cell where
  coarbitrary cell = coarbitrary (cell == Alive)

instance Arbitrary AddressInWorld where
  arbitrary = do
    world <- arbitrary
    let (width, height) = size world
    x <- liftM fromInteger $ choose (0, toInteger $ width - 1)
    y <- liftM fromInteger $ choose (0, toInteger $ height - 1)
    return $ AddressInWorld world (x, y)

instance Arbitrary NeighborList where
  arbitrary = liftM NeighborList (vector 8)

instance Arbitrary Word16 where
  arbitrary = liftM fromInteger arbitrary

instance CoArbitrary Word16 where
  coarbitrary = variant

instance Arbitrary a => Arbitrary (Slice a) where
  arbitrary = liftM3 newSlice arbitrary arbitrary arbitrary

instance Arbitrary Pattern where
  arbitrary = liftM Pattern arbitrary
