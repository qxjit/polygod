{-# LANGUAGE OverloadedStrings #-}
module WebApp where

import           Prelude
import qualified Prelude as P
import           Control.Monad
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
import           Data.Trie
import           Data.Char
import           Data.ByteString
import           Data.String

import           Numeric

import           Life
import qualified Life as Life
import           Timeline

worldWidth, worldHeight :: Dimension
worldWidth = 100
worldHeight = 50

main :: IO ()
main = do
  timeline <- newTimeline (worldWidth, worldWidth)
  quickServer $
        ifTop rootHandler <|>
        noCache (route [ ("world/current.json", worldHandler timeline)
                       , ("world/next.json", nextWorldHandler timeline)
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
jsonTemplate = writeLBS . encode Compact

rootHandler :: Snap ()
rootHandler = blazeTemplate $ html $ do
  H.head $ do
    H.title "Polygod -- A Multiplayer Game Of Life"
    (script ! type_ "text/javascript" ! src "http://code.jquery.com/jquery-1.4.2.min.js") ""
    (script ! type_ "text/javascript" ! src "javascripts/application.js") ""
    (link ! rel "stylesheet" ! type_ "text/css" ! href "stylesheets/application.css")
  body $ do
    h1 "Welcome to Polygod"
    (canvas ! A.id "game-canvas"
            ! dataAttribute "width" (fromString $ show worldWidth)
            ! dataAttribute "height" (fromString $ show worldHeight)) ""

worldHandler :: Timeline -> Snap ()
worldHandler timeline =  do
  (world, tick) <- liftIO (now timeline)
  jsonTemplate $ worldView world tick

nextWorldHandler :: Timeline -> Snap ()
nextWorldHandler timeline = do
  tickParam <- getParam "tick"
  maybe pass (\tickString ->
                  case readDec (P.map (chr . fromIntegral) (unpack tickString)) of
                    [(tick, [])] -> do (world, tick') <- liftIO (worldAfter tick timeline)
                                       jsonTemplate $ worldView world tick'
                    _ -> pass)
        tickParam

worldView :: World -> Tick -> JSON
worldView w tick = Object $ fromList [("tick", Number $ fromIntegral tick),
                                      ("cells", Array $ [cellJson (x, y) | x <- [0..width - 1], y <- [0..height - 1]])]
  where (width, height) = Life.size w
        cellJson (x, y) = Object $ fromList [("point", Array [Number$ fromIntegral x, Number $ fromIntegral y]),
                                             ("alive", Boolean $ isAlive (cellAt w (x, y)))]
        isAlive Alive = True
        isAlive _ = False

