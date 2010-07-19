module WorldHandlers where

import           Control.Monad.Trans (liftIO)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import           Data.Char
import           Numeric

import qualified Text.JSONb as Json
import           Snap.Types

import           Life
import           Life.JSON
import           Timeline

updateWorldHandler :: Timeline a -> Snap ()
updateWorldHandler timeline = do
  body <- getRequestBody
  either (\_ -> badPost "The post data was not valid JSON.  Please use JSON format to post updates to the world.\n")
         (\json -> maybe (badPost "The JSON you posted didn't include any properly structured cell data.\n")
                         (\newCells -> do liftIO $ interfere (updateCells newCells) timeline
                                          writeBS "Divine intervention successful.  Happy godding!\n")
                         (jsonToCells json))
         (Json.decode (Strict.concat . Lazy.toChunks $ body))

worldHandler :: Timeline (Snap ()) -> Snap ()
worldHandler timeline =  do
  (_, _, renderedJSON) <- liftIO (now timeline)
  renderedJSON

nextWorldHandler :: Timeline (Snap ()) -> Snap ()
nextWorldHandler timeline = do
  tickParam <- getParam "tick"
  maybe pass (\tickString ->
                  case readDec (map (chr . fromIntegral) (Strict.unpack tickString)) of
                    [(tick, [])] -> do (_, _, renderedJSON) <- liftIO (worldAfter tick timeline)
                                       renderedJSON
                    _ -> pass)
        tickParam

badPost :: Strict.ByteString -> Snap ()
badPost message = do
        putResponse $
          setResponseStatus 400 "Bad Request" emptyResponse
        writeBS message
        r <- getResponse
        finishWith r

