module Util where

import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy

strict :: Lazy.ByteString -> Strict.ByteString
strict = Strict.concat . Lazy.toChunks

lazy :: Strict.ByteString -> Lazy.ByteString
lazy s = Lazy.fromChunks [s]

