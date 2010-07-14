{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Prelude
import qualified Prelude as P
import           Control.Monad
import           Control.Applicative
import           Data.ByteString

import           Snap.Types
import qualified Snap.Types as T
import           Snap.Util.FileServe

import           Server

import           Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H

import           Life

main :: IO ()
main = quickServer $
        ifTop (writeBS "hello world") <|>
        route [ ("echo/:echoparam", echoHandler)
              , ("world", worldHandler)
              ] <|>
        T.dir "static" (fileServe ".")

blazeTemplate :: Html a -> Snap ()
blazeTemplate = writeLBS . renderHtml

worldHandler :: Snap ()
worldHandler = blazeTemplate $ worldView (newWorld (30, 30))

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
        forM_ [0..height] $ \y -> do
          H.div $ do
            forM [0..width] $ \x -> do
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
