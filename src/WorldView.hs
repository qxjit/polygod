module WorldView
  ( sharedWorldView
  , SharedTimelineView
  , worldTemplate
  )
  where

import           Data.ByteString.Char8 (ByteString, pack, append)

import           Snap.Types
import           Text.JSONb

import           ConcurrentUsers
import           Life.JSON
import           Timeline

data SharedTimelineView = STV ByteString Tick

sharedWorldView :: Projector SharedTimelineView
sharedWorldView world tick = STV (encode Compact $ worldToJson world tick) tick

nextWorldUrl :: Tick -> UserToken -> ByteString
nextWorldUrl tick userToken = "/world/next.json?tick=" `append` (pack $ show tick) `append` "&u=" `append` (tokenToString userToken)

worldTemplate :: SharedTimelineView -> UserToken -> Snap ()
worldTemplate (STV jsonByteString tick) userToken = do
  modifyResponse (setHeader "Location" $ nextWorldUrl tick userToken)
  writeBS jsonByteString
