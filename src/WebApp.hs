module WebApp where

import           Prelude
import qualified Prelude as P
import           Control.Monad.Trans
import           Control.Applicative

import           Snap.Types
import qualified Snap.Types as T
import           Snap.Util.FileServe

import           Server

import           Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A

import           Text.JSONb
import qualified Text.JSONb as Json
import           Data.Char
import           Data.ByteString
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import           Data.String

import           Numeric

import           Life
import qualified Life as Life
import           Life.JSON
import           Timeline
import           Pattern

worldWidth, worldHeight :: Dimension
worldWidth = 100
worldHeight = 50

main :: IO ()
main = do
  timeline <- newTimeline (worldWidth, worldHeight) (\world tick -> jsonTemplate (worldView world tick))
  pattern <- loadPattern "gospersGliderGun.txt"
  interfere (drawPatternAt (0, 0) pattern) timeline
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

blazeTemplate :: Html a -> Snap ()
blazeTemplate = writeLBS . renderHtml

jsonTemplate :: JSON -> Snap ()
jsonTemplate = writeBS . encode Compact

badPost :: ByteString -> Snap ()
badPost message = do
        putResponse $
          setResponseStatus 400 "Bad Request" emptyResponse
        writeBS message
        r <- getResponse
        finishWith r


rootHandler :: Timeline a -> Snap ()
rootHandler timeline = do
  (world, _, _) <- liftIO (now timeline)
  let (wWidth, wHeight) = Life.size world
  blazeTemplate $ html $ do
    H.head $ do
      H.title "Polygod -- A Multiplayer Game Of Life"
      (script ! type_ "text/javascript" ! src "http://code.jquery.com/jquery-1.4.2.min.js") ""
      (script ! type_ "text/javascript" ! src "javascripts/application.js") ""
      (link ! rel "stylesheet" ! type_ "text/css" ! href "stylesheets/application.css")
    body $ do
      h1 "Welcome to Polygod"
      (canvas ! A.id "game-canvas"
              ! dataAttribute "width" (fromString $ show wWidth)
              ! dataAttribute "height" (fromString $ show wHeight)) ""

updateWorldHandler :: Timeline a -> Snap ()
updateWorldHandler timeline = do
  body <- getRequestBody
  case (Json.decode (Strict.concat . Lazy.toChunks $ body)) of
    Right json -> case (jsonToCells json) of
                  Just cells -> do liftIO $ interfere (updateCells cells) timeline
                                   writeBS "Divine intervention successful.  Happy godding!\n"
                  _ -> badPost "The JSON you posted didn't include any properly structured cell data.\n"
    Left _ -> badPost "The post data was not valid JSON.  Please use JSON format to post updates to the world.\n"

worldHandler :: Timeline (Snap ()) -> Snap ()
worldHandler timeline =  do
  (_, _, renderedJSON) <- liftIO (now timeline)
  renderedJSON

nextWorldHandler :: Timeline (Snap ()) -> Snap ()
nextWorldHandler timeline = do
  tickParam <- getParam "tick"
  maybe pass (\tickString ->
                  case readDec (P.map (chr . fromIntegral) (unpack tickString)) of
                    [(tick, [])] -> do (_, _, renderedJSON) <- liftIO (worldAfter tick timeline)
                                       renderedJSON
                    _ -> pass)
        tickParam

worldView :: World -> Tick -> JSON
worldView world tick = let (Object trie) = worldToJson world
                       in Object $ add trie "tick" (Number $ fromIntegral tick)


