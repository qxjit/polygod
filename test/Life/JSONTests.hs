module Life.JSONTests where

import Text.JSONb
import qualified Data.Trie as T

import Life
import Life.JSON

import TestHelper

tests :: Test
tests = testGroup "Life.JSON" [
  "json representation has same number of cells as world" `testProperty` \world ->
    let (Object trie) = worldToJson world 0
        Just (Array jsonCells) = T.lookup "cells" trie
    in (length jsonCells) == (length $ cells world)

 ,"json representation can be parsed to a list of addressed cells" `testProperty` \world ->
    let Just convertedCells = jsonToCells (worldToJson world 0)
    in (length convertedCells) == (length $ cells world)
  ]
