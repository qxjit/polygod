module WebAppTests where

import           TestHelper

import qualified Data.Map as Map
import qualified Data.ByteString as Strict
import           Data.IORef

import           Text.JSONb
import           Snap.Iteratee
import           Snap.Types
import           Snap.Internal.Http.Types

import           ConcurrentUsers
import           WebApp

tests :: Test
tests = testGroup "WebApp" [
  "renders valid json" `testCase` do
    timeline <- newAppTimeline
    users <- newUserSet
    bodyRef <- newIORef (SomeEnumerator $ enumBS "")
    let request = Request { rqServerName = "localhost",
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
                            rqMethod = GET,
                            rqVersion = (1, 1),
                            rqCookies = [],
                            rqSnapletPath = "",
                            rqPathInfo = "world/next.json",
                            rqContextPath = "/",
                            rqURI = "/world/next.json",
                            rqQueryString = "tick=1",
                            rqParams = Map.singleton "tick" ["1"]
                          }
    (_, testResponse) <- run (runSnap (site timeline users) (const $ return ()) request)
    bodyIteratee <- (rspBodyToEnum $ rspBody testResponse) stream2list
    !body <- run bodyIteratee
    assertBool "Expected response to be parseable json" (either (const False) (const True) $ decode (Strict.pack body))
  ]
