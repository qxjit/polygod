module Tests where

import Test.Framework (defaultMain)

import qualified LifeTests as LifeTests
import qualified Life.JSONTests as JSONTests
import qualified TimelineTests as TimelineTests

main :: IO ()
main = defaultMain [ TimelineTests.tests, LifeTests.tests, JSONTests.tests ]


