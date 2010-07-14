{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Prelude
import qualified Prelude as P
import           Control.Applicative
import           Data.ByteString

import           Snap.Types
import qualified Snap.Types as T
import           Snap.Util.FileServe
import           Text.Templating.Heist
import           Text.Templating.Heist.TemplateDirectory

import           Glue
import           Server

import           Text.Blaze.Html5
import           Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

main :: IO ()
main = quickServer $ 
        ifTop (writeBS "hello world") <|>
        route [ ("echo/:echoparam", echoHandler)
              ] <|>
        T.dir "static" (fileServe ".")


echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    writeLBS $ renderHtml (echoView param)

echoView :: Maybe ByteString -> Html a
echoView s = html $ do
     H.head $ do
         H.title "Echo"
     body $
        p . showHtml $
          maybe ("must specify echo/param in URL") P.id s
