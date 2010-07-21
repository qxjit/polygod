module ConcurrentUsersTests where

import TestHelper

import           Control.Concurrent
import           Data.Char
import           Data.ByteString (ByteString)
import qualified Data.ByteString as Strict
import           Data.Time.Clock

import ConcurrentUsers

tests :: Test
tests = testGroup "ConcurrentUsers" [
  "newUserSet starts with 0 users" `testCase` do
    users <- newUserSet
    count <- userCount users
    count @?= 0

 ,"trackNewUser increases set of concurrent users" `testCase` do
    users <- newUserSet
    trackNewUser users
    (userCount users >>=) (@?= 1)
    trackNewUser users
    (userCount users >>=) (@?= 2)

 ,"trackNewUser returns a unique tracker" `testCase` do
    [users1, users2] <- sequence [newUserSet, newUserSet]
    [Just token1, Just token2, Just token3] <- mapM trackNewUser [users1, users1, users2]
    assertBool "Expected tokens to be different" (token1 /= token2)
    assertBool "Expected tokens to be different" (token1 /= token3)

 ,"untrackUser decrements the count iff token is still tracked" `testCase` do
    users <- newUserSet
    [Just token, _] <- mapM trackNewUser [users, users]
    (userCount users >>=) (@?= 2)
    untrackUser token users
    (userCount users >>=) (@?= 1)
    untrackUser token users
    (userCount users >>=) (@?= 1)

  ,"token can be serialized / de-serialized to ByteString" `testCase` do
    Just token <- (trackNewUser =<< newUserSet)
    let serialized = (tokenToString token) :: ByteString
        Just unserialized = (tokenFromString serialized)
    assertBool "Expected token to match deserialized vakel" (token == unserialized)

  ,"token serializes to alphanumeric" `testCase` do
    Just token <- (trackNewUser =<< newUserSet)
    let isOk o = let c = (chr $ fromIntegral o) in isAlphaNum c || c == '-'
    assertBool "Expected all characters to be alphanumeric" (Strict.all isOk (tokenToString token))

  ,"isUserTracked returns True iff user is still tracked" `testCase` do
    users <- newUserSet
    Just token <- trackNewUser users
    (isUserTracked token users >>=) (@?= True)
    untrackUser token users
    (isUserTracked token users >>=) (@?= False)

  ,"reapStaleUsers removes a user if user is older specified period" `testCase` do
    users <- newUserSet
    Just oldUser <- trackNewUser users
    threadDelay 100000
    Just newUser <- trackNewUser users
    reapStaleUsers (picosecondsToDiffTime 100000000) users
    (isUserTracked oldUser users >>=) (@?= False)
    (isUserTracked newUser users >>=) (@?= True)

  ,"trackUser updates age of user, preventing it from being reaped" `testCase` do
    users <- newUserSet
    Just user <- trackNewUser users
    threadDelay 100000
    trackUser user users
    reapStaleUsers (picosecondsToDiffTime 100000000) users
    (isUserTracked user users >>=) (@?= True)
 ]

