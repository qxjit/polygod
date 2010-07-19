module RootHandlers where

import           Control.Monad.Trans (liftIO)
import           Data.String (fromString)

import           Snap.Types
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A


import qualified Life as Life
import           Timeline

rootHandler :: Timeline a -> Snap ()
rootHandler timeline = do
  (world, _, _) <- liftIO (now timeline)
  let (wWidth, wHeight) = Life.size world

  blazeTemplate $ html $ do
    H.head $ do
      H.title "Polygod -- A Multiplayer Game Of Life"
      script ! type_ "text/javascript" ! src "http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js" $ ""
      script ! type_ "text/javascript" ! src "http://ajax.googleapis.com/ajax/libs/jqueryui/1.8.2/jquery-ui.min.js" $ ""
      script ! type_ "text/javascript" ! src "javascripts/application.js" $ ""
      link ! rel "stylesheet" ! type_ "text/css" ! href "stylesheets/ui-darkness/jquery-ui-1.8.2.custom.css"
      link ! rel "stylesheet" ! type_ "text/css" ! href "stylesheets/application.css"

    body $ do
      h1 "Welcome to Polygod"
      H.div ! class_ "toolbar" $ do
        input ! type_ "radio" ! name "tool" ! A.id "resurrect" ! checked "checked" ! value "resurrect"
        H.label ! for "resurrect" $ "Resurrect"

        input ! type_ "radio" ! name "tool" ! A.id "smite" ! value "smite"
        H.label ! for "smite" $ "Smite"

      H.div ! A.id "pattern-box" $ ""

      canvas ! A.id "game-canvas"
             ! dataAttribute "width" (fromString $ show wWidth)
             ! dataAttribute "height" (fromString $ show wHeight) $ ""

blazeTemplate :: Html a -> Snap ()
blazeTemplate = writeLBS . renderHtml

