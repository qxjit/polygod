module Tests where

import Test.Framework (defaultMain)

import qualified LifeTests as LifeTests
import qualified Life.JSONTests as JSONTests

main :: IO ()
main = defaultMain [ LifeTests.tests, JSONTests.tests ]


