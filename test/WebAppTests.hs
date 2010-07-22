module WebAppTests where

import           TestHelper

import           Control.Concurrent
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Map as Map
import           Data.List
import           Data.IORef
import           Data.Time.Clock


import           Text.JSONb
import           Snap.Iteratee
import           Snap.Types
import           Snap.Internal.Http.Types

import           ConcurrentUsers
import           WebApp

tests :: Test
tests = testGroup "WebApp" [
  "renders valid json" `testCase` do
    withAppTimeline $ \timeline -> do
      users <- newUserSet
      body <- request (site timeline users) GET "/world/next.json" [("tick", "1")]
      assertBool "Expected response to be parseable json" (either (const False) (const True) $ decode body)

 ,"a significant percentage of the work is shared between concurrent requests" `testCase` do
    let concurrentThreads = [1,2,5,10,20]
        timeSite n = withAppTimeline $ \timeline -> do
                        !users <- newUserSet
                        timeConcurrent (request (site timeline users) GET "/world/next.json" [("tick", "1")]) n

    averageTimes <- averageOverTrials 10 (mapM timeSite concurrentThreads)

    let floatingThreads = map (fromRational . toRational) concurrentThreads :: [Float]
        floatingTimes = map (fromRational . toRational) averageTimes
        (oneThreadCoef:multipleThreadCoefs) = zipWith (/) (map (10*) floatingTimes) floatingThreads
        estimatedSharedWork = (oneThreadCoef - (maximum multipleThreadCoefs)) / oneThreadCoef

    assertBool ("Expected at least 30% of work to be shared between requests, but only " ++ (show $ estimatedSharedWork * 100) ++ "% was.") 
               (estimatedSharedWork > 0.3)
 ]

averageOverTrials :: Fractional a => Int -> IO [a] -> IO [a]
averageOverTrials trials action = do
  !results <- sequence (replicate trials action)
  return (map (\vs -> (Prelude.sum vs) / (fromRational $ toRational trials)) (transpose results))

timeConcurrent :: IO a -> Int -> IO NominalDiffTime
timeConcurrent action n = do
  finished <- newEmptyMVar
  startedTime <- getCurrentTime
  sequence_ (replicate n $ forkIO (action >> putMVar finished ()))
  sequence_ (replicate n $ takeMVar finished)
  finishedTime <- getCurrentTime
  return (diffUTCTime finishedTime startedTime)


request :: Snap() -> Method -> String -> [(Strict.ByteString, Strict.ByteString)] -> IO Strict.ByteString
request testSite testMethod uri@('/':pathInfo) params = do
    bodyRef <- newIORef (SomeEnumerator $ enumBS "")
    let testRequest = Request { rqServerName = "localhost",
                                rqServerPort = 0,
                                rqRemoteAddr = "127.0.0.1",
                                rqRemotePort = 0,
                                rqLocalAddr = "127.0.0.1",
                                rqLocalPort = 0,
                                rqLocalHostname = "localhost",
                                rqIsSecure = False,
                                rqHeaders = Map.empty,
                                rqBody = bodyRef,
                                rqContentLength = Nothing,
                                rqMethod = testMethod,
                                rqVersion = (1, 1),
                                rqCookies = [],
                                rqSnapletPath = "",
                                rqPathInfo = Char8.pack pathInfo,
                                rqContextPath = "/",
                                rqURI = Char8.pack uri,
                                rqQueryString = (Strict.intercalate "&" $ map (\(k,v) -> k `Strict.append` "=" `Strict.append` v) params),
                                rqParams = Map.fromList (map (\(k,v) -> (k, replicate 1 v)) params)
                              }
    (_, testResponse) <- run (runSnap testSite (const $ return ()) testRequest)
    bodyIteratee <- (rspBodyToEnum $ rspBody testResponse) stream2list
    !body <- run bodyIteratee
    return $ Strict.pack body

request _ _ uri _ = assertFailure ("Malformed uri " ++ uri ++ ". Please use the absolute url routed to your handler under test.") >> undefined

