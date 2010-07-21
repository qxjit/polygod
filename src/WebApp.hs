module WebApp where

import           Control.Applicative

import           Snap.Types
import           Snap.Util.FileServe

import           Server

import           Life
import           Timeline
import           Pattern
import           ConcurrentUsers

import           RootHandlers
import           WorldHandlers
import           WorldView

worldWidth, worldHeight :: Dimension
(worldWidth, worldHeight) = (100, 50)

main :: IO ()
main = do
  timeline <- newAppTimeline
  users <- newUserSet

  gliderGunPattern <- loadPattern "gospersGliderGun.txt"
  interfere (drawPatternAt (0, 0) gliderGunPattern) timeline
  quickServer $ site timeline users

newAppTimeline :: IO (Timeline SharedTimelineView)
newAppTimeline = newTimeline (worldWidth, worldHeight) worldView

site :: Timeline SharedTimelineView -> UserSet -> Snap ()
site timeline users = ifTop (rootHandler timeline) <|>
                      noCache (route [ ("world/current.json", worldHandler users timeline)
                                     , ("world/next.json", nextWorldHandler users timeline)
                                     , ("world", updateWorldHandler timeline)
                                     ]) <|>
                      fileServe "public"

noCache :: Snap () -> Snap ()
noCache handler = do
  modifyResponse (setHeader "Pragma" "no-cache")
  modifyResponse (setHeader "Cache-Control" "no-cache")
  modifyResponse (setHeader "Expires" "-1")
  handler

