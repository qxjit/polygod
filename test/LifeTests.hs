module LifeTests where

import Life
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary

tests :: Test
tests = testGroup "Life"
  [ "evolve maintains world size" `testProperty` \world -> (size world) == (size $ evolve world)
  ]

instance Arbitrary World where
  arbitrary = do
    width  <- choose (0, 100)
    height <- choose (0, 100)
    cells <- vector (width * height)
    return $ newWorldWithCells (width, height) cells

instance Arbitrary Cell where
  arbitrary = elements [Alive, Dead]
