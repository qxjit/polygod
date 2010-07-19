module WebApp where

import           Control.Applicative

import           Snap.Types
import           Snap.Util.FileServe

import           Server

import           Text.JSONb

import           Life
import           Life.JSON
import           Timeline
import           Pattern

import           RootHandlers
import           WorldHandlers

worldWidth, worldHeight :: Dimension
(worldWidth, worldHeight) = (100, 50)

main :: IO ()
main = do
  timeline <- newTimeline (worldWidth, worldHeight) (\world tick -> jsonTemplate (worldView world tick))
  gliderGunPattern <- loadPattern "gospersGliderGun.txt"
  interfere (drawPatternAt (0, 0) gliderGunPattern) timeline
  quickServer $
        ifTop (rootHandler timeline) <|>
        noCache (route [ ("world/current.json", worldHandler timeline)
                       , ("world/next.json", nextWorldHandler timeline)
                       , ("world", updateWorldHandler timeline)
                       ]) <|>
        fileServe "public"

noCache :: Snap () -> Snap ()
noCache handler = do
  modifyResponse (setHeader "Pragma" "no-cache")
  modifyResponse (setHeader "Cache-Control" "no-cache")
  modifyResponse (setHeader "Expires" "-1")
  handler

jsonTemplate :: JSON -> Snap ()
jsonTemplate = writeBS . encode Compact

worldView :: World -> Tick -> JSON
worldView world tick = let (Object trie) = worldToJson world
                       in Object $ add trie "tick" (Number $ fromIntegral tick)


