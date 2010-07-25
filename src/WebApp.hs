module WebApp where

import           Control.Applicative
import           Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Char8 as Char8

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
  withAppTimeline $ \timeline -> do
    users <- newUserSet

    gliderGunPattern <- loadPattern "gospersGliderGun.txt"
    interfereAt 0 (drawPatternAt (0, 0) gliderGunPattern) timeline
    quickServer $ site timeline users

withAppTimeline :: (Timeline SharedTimelineView -> IO a) -> IO a
withAppTimeline = withTimeline (defaultConfig {tlSize = (worldWidth, worldHeight), tlProjector = sharedWorldView})

site :: Timeline SharedTimelineView -> UserSet -> Snap ()
site timeline users = ifTop (rootHandler timeline) <|>
                      (do route [ ("world/current.json", worldHandler users timeline)
                                , ("world/next.json", nextWorldHandler users timeline)
                                , ("world", updateWorldHandler timeline)
                                ]
                          noCache
                          addConcurrentUsersHeader users) <|>
                      fileServe "public"

addConcurrentUsersHeader :: UserSet -> Snap ()
addConcurrentUsersHeader userSet = do
  count <- liftIO (userCount userSet)
  modifyResponse (setHeader "X-Polygod-ConcurrentUsers" (Char8.pack $ show count))

noCache :: Snap ()
noCache = do
  modifyResponse (setHeader "Pragma" "no-cache")
  modifyResponse (setHeader "Cache-Control" "no-cache")
  modifyResponse (setHeader "Expires" "-1")

