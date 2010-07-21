module WorldView
  (worldView)
  where

import           Control.Monad.Trans (liftIO)
import           Data.ByteString.Char8 (pack, append)

import           Snap.Types
import           Text.JSONb

import           ConcurrentUsers
import           Life
import           Life.JSON
import           Timeline

worldView :: World -> Tick -> UserToken -> UserSet -> Snap ()
worldView world tick userToken userSet = do
  count <- liftIO (userCount userSet)
  jsonTemplate $ worldViewJson world tick userToken count

worldViewJson :: World -> Tick -> UserToken -> UserCount -> JSON
worldViewJson world tick userToken count =
  let (Object trie) = worldToJson world
      trieWithCount = add trie "userCount" (String (pack $ show count))
  in Object (add trieWithCount "nextUrl" (String $ "/world/next.json?tick=" `append` (pack $ show tick)
                                                                            `append` "&u="
                                                                            `append` (tokenToString userToken)))

jsonTemplate :: JSON -> Snap ()
jsonTemplate = writeBS . encode Compact
