{-# LANGUAGE OverloadedStrings #-}
module WebApp where

import           Prelude
import qualified Prelude as P
import           Control.Monad
import           Control.Monad.Trans
import           Control.Applicative
import           Data.ByteString

import           Snap.Types
import qualified Snap.Types as T
import           Snap.Util.FileServe

import           Server

import           Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H

import           Life
import           Timeline

main :: IO ()
main = do
  timeline <- newTimeline (30, 30)
  quickServer $
        ifTop (writeBS "hello world") <|>
        route [ ("echo/:echoparam", echoHandler)
              , ("world", worldHandler timeline)
              ] <|>
        T.dir "static" (fileServe ".")

blazeTemplate :: Html a -> Snap ()
blazeTemplate = writeLBS . renderHtml

worldHandler :: Timeline -> Snap ()
worldHandler timeline =  do
  world <- liftIO (now timeline)
  blazeTemplate $ worldView world

echoHandler :: Snap ()
echoHandler = do
    echoparam <- getParam "echoparam"
    blazeTemplate $ echoView echoparam

worldView :: World -> Html a
worldView w = html $ do
    H.head $ do
        H.title "World"
    body $ do
        let (width, height) = size w
        forM_ [0..height - 1] $ \y -> do
          H.div $ do
            forM [0..width - 1] $ \x -> do
              case (cellAt w (x, y)) of
                Alive -> "* "
                Dead -> "- "

echoView :: Maybe ByteString -> Html a
echoView s = html $ do
     H.head $ do
         H.title "Echo"
     body $
        p . showHtml $
          maybe ("must specify echo/param in URL") P.id s

