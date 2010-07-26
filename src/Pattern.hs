module Pattern
  ( Pattern(..)
  , loadPattern
  , drawPatternAt
  )
  where

import Data.List
import Life

data Pattern = Pattern { patternCells :: [(Address, Cell)] } deriving (Show)

loadPattern :: FilePath -> IO Pattern
loadPattern filePath = do
  fileContents <- readFile filePath
  let parseChar '.' = Dead
      parseChar ' ' = Alive
      mapCell y x cell = (x+1, ((x,y), parseChar cell))
      mapRow y row = (y+1, snd $ mapAccumL (mapCell y) 0 row)
      !patternCells = snd $ mapAccumL mapRow 0 (lines fileContents)
  return $ Pattern (concat patternCells)

drawPatternAt :: Address -> Pattern -> World -> World
drawPatternAt (xOff, yOff) (Pattern updates) world = updateCells (map offset updates) world
  where (width, height) = size world
        offset ((x, y), cell) = ( ( (x + xOff) `mod` width, (y + yOff) `mod` height), cell)

