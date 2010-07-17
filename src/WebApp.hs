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

import           Numeric

import           Life
import qualified Life as Life
import           Timeline

main :: IO ()
main = do
  timeline <- newTimeline (30, 30)
  quickServer $
        ifTop rootHandler <|>
        route [ ("world.json", worldHandler timeline)
              , ("world/:tick/next.json", nextWorldHandler timeline)
              ] <|>
        fileServe "public"

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
    (canvas ! A.id "game-canvas") ""

worldHandler :: Timeline -> Snap ()
worldHandler timeline =  do
  (world, tick) <- liftIO (now timeline)
  jsonTemplate $ worldView world tick

nextWorldHandler :: Timeline -> Snap ()
nextWorldHandler timeline = do
  -- todo error handling
  Just tickString <- getParam "tick"
  let [(tick, [])] = readDec (P.map (chr . fromIntegral) (unpack tickString))
  (world, tick') <- liftIO (worldAfter tick timeline)
  jsonTemplate $ worldView world tick'

worldView :: World -> Tick-> JSON
worldView w tick = Object $ fromList [("tick", Number $ fromIntegral tick),
                                      ("cells", Array $ [cellJson (x, y) | x <- [0..width - 1], y <- [0..width - 1]])]
  where (width, height) = Life.size w
        cellJson (x, y) = Object $ fromList [("point", Array [Number$ fromIntegral x, Number $ fromIntegral y]),
                                             ("alive", Boolean $ isAlive (cellAt w (x, y)))]
        isAlive Alive = True
        isAlive _ = False

