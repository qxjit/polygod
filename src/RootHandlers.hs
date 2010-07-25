module RootHandlers where

import           Data.String (fromString)

import           Snap.Types
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A

import           Timeline

rootHandler :: Timeline a -> Snap ()
rootHandler timeline = do
  let (wWidth, wHeight) = tlSize $ tlConfig timeline

  blazeTemplate $ html $ do
    H.head $ do
      H.title "Polygod -- A Multiplayer Game Of Life"
      script ! type_ "text/javascript" ! src "http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js" $ ""
      script ! type_ "text/javascript" ! src "http://ajax.googleapis.com/ajax/libs/jqueryui/1.8.2/jquery-ui.min.js" $ ""
      script ! type_ "text/javascript" ! src "javascripts/jLog.min.js" $ ""
      script ! type_ "text/javascript" ! src "javascripts/application.js" $ ""
      link ! rel "stylesheet" ! type_ "text/css" ! href "stylesheets/custom-theme/jquery-ui-1.8.2.custom.css"
      link ! rel "stylesheet" ! type_ "text/css" ! href "stylesheets/application.css"

    body $ do
      H.div ! class_ "app-content" $ do
        H.div ! class_ "ui-widget-content ui-corner-all app-container" $ do
          H.div ! A.id "error-dialog" ! A.title "An error occured" $ do
            p ! A.id "error-message" $ ""
          h1 ! class_ "ui-widget-header ui-corner-all" $ "Polygod"
          canvas ! A.id "game-canvas"
                 ! dataAttribute "startUrl" "/world/current.json"
                 ! dataAttribute "width" (fromString $ show wWidth)
                 ! dataAttribute "height" (fromString $ show wHeight) $ ""

        H.div ! class_ "ui-widget-content ui-corner-all" $ do
          H.div ! class_ "toolbar" $ do
            input ! type_ "radio" ! name "tool" ! A.id "resurrect" ! checked "checked" ! value "resurrect"
            H.label ! for "resurrect" $ "Resurrect"

            input ! type_ "radio" ! name "tool" ! A.id "smite" ! value "smite"
            H.label ! for "smite" $ "Smite"

          H.div ! A.id "pattern-box" $ ""
          H.div $ do
            "Concurrent Users: "
            H.span ! class_ "concurrentUsersCount" $ ""
            " Last Tick From Server: "
            H.span ! class_ "lastServerTick" $ ""
            " Predictive Lookahead: "
            H.span ! class_ "predictedTicks" $ ""

blazeTemplate :: Html a -> Snap ()
blazeTemplate = writeLBS . renderHtml

