module Tests where

import Test.Framework (defaultMain)

import qualified LifeTests as LifeTests
import qualified Life.JSONTests as JSONTests
import qualified TimelineTests as TimelineTests
import qualified WebAppTests as WebAppTests
import qualified ConcurrentUsersTests as ConcurrentUsersTests

main :: IO ()
main = defaultMain [ TimelineTests.tests, ConcurrentUsersTests.tests, WebAppTests.tests, LifeTests.tests, JSONTests.tests ]


