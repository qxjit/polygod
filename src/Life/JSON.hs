module Life.JSON
  ( worldToJson
  , jsonToCells
  , add
  )
  where

import           Control.Monad

import           Text.JSONb
import           Data.Trie
import qualified Data.Trie as Trie

import           Life

worldToJson :: World -> JSON
worldToJson w = Object $ Trie.singleton "cells" (Array $ map cellJson (addresses w))
  where (wWidth, wHeight) = Life.size w
        cellJson (x, y) = Object $ add (Trie.singleton "point" (Array [Number $ fromIntegral x, Number $ fromIntegral y]))
                                                       "alive" (Boolean $ isAlive (cellAt w (x, y)))
        isAlive Alive = True
        isAlive _ = False

add :: Trie JSON -> KeyString -> JSON -> Trie JSON
add t k j = alterBy (\_ new _ -> Just new) k j t

jsonToCells :: JSON -> Maybe [(Address, Cell)]
jsonToCells (Object trie) = case Trie.lookup "cells" trie of
                            Just (Array cells) -> mapM jsonToCell cells
                            _ -> Nothing
  where jsonToCell (Object cellTrie) = liftM2 (,) (pointToAddress =<< Trie.lookup "point" cellTrie)
                                                  (boolToCell =<< Trie.lookup "alive" cellTrie)
        jsonToCell _ = Nothing

        pointToAddress (Array [Number x,Number y]) = Just (truncate x, truncate y)
        pointToAddress _ = Nothing

        boolToCell (Boolean q) = Just (if q then Alive else Dead)
        boolToCell _ = Nothing
