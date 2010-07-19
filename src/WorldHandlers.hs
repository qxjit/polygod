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
  processInput (Json.decode (Strict.concat . Lazy.toChunks $ body))
               "The post data was not valid JSON.  Please use JSON format to post updates to the world.\n" $

    \json -> processInput (jsonToCells json) "The JSON you posted didn't include any properly structured cell data.\n" $

      \newCells -> do liftIO $ interfere (updateCells newCells) timeline
                      writeBS "Divine intervention successful.  Happy godding!\n"

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

badRequest :: Strict.ByteString -> Snap ()
badRequest message = do
        putResponse $
          setResponseStatus 400 "Bad Request" emptyResponse
        writeBS message
        r <- getResponse
        finishWith r

processInput :: PossibleBadInput m => m b -> Strict.ByteString -> (b -> Snap()) -> Snap ()
processInput input message action = useGoodInput input action (badRequest message)

class PossibleBadInput m where
  useGoodInput :: m a -> (a -> b) -> b -> b

instance PossibleBadInput Maybe where
  useGoodInput (Just a) f _ = f a
  useGoodInput _ _ b = b

instance PossibleBadInput (Either a) where
  useGoodInput (Right a) f _ = f a
  useGoodInput _ _ b = b
