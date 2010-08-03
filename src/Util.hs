module Util where

import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy

import           Text.JSONb
import qualified Data.Trie as Trie

strict :: Lazy.ByteString -> Strict.ByteString
strict = Strict.concat . Lazy.toChunks

lazy :: Strict.ByteString -> Lazy.ByteString
lazy s = Lazy.fromChunks [s]

jsonProperty :: Strict.ByteString -> JSON -> Maybe JSON
jsonProperty prop (Object trie) = Trie.lookup prop trie
jsonProperty _ _ = Nothing

jsonNumber :: JSON -> Maybe Rational
jsonNumber (Number r) = Just r
jsonNumber _ = Nothing

aps :: Num n => n -> (a -> a) -> a -> a
aps 0 _ a = a
aps n f a = aps (n - 1) f (f a)
