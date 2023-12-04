-- |
module PartOne (partOne) where

import Common

partOne :: String -> String
partOne field = show $ sum $ map chunkValue $ filter hasSymbolNeighbors $ chunkRuns ps
  where
    ps = positions $ lines field
    hasSymbolNeighbors = any (/= '.') . fmap fromPositioned . chunkNeighbors ps
