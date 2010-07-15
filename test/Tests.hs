module Tests where

import Test.Framework (defaultMain)

import qualified LifeTests as LifeTests

main :: IO ()
main = defaultMain [ LifeTests.tests ]

