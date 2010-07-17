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

import           Life
import qualified Life as Life
import           Timeline

main :: IO ()
main = do
  timeline <- newTimeline (30, 30)
  quickServer $
        ifTop rootHandler <|>
        route [ ("world", worldHandler timeline)
              ] <|>
        fileServe "public"

blazeTemplate :: Html a -> Snap ()
blazeTemplate = writeLBS . renderHtml

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
  world <- liftIO (now timeline)
  blazeTemplate $ worldView world

worldView :: World -> Html a
worldView w = html $ do
  H.head $ do
      H.title "World"
  body $ do
      let (width, height) = Life.size w
      forM_ [0..height - 1] $ \y -> do
        H.div $ do
          forM [0..width - 1] $ \x -> do
            case (cellAt w (x, y)) of
              Alive -> "* "
              Dead -> "- "

