-- |
module PartTwo (partTwo) where

import Common

import Data.List (intersect)

listsOverlap :: Eq a => [a] -> [a] -> Bool
listsOverlap as = not . null . intersect as

partTwo :: String -> String
partTwo field = show $ sum $ fmap product valueOfChunks
  where
    ps = positions $ lines field
    gears = filter (testValue (== '*')) ps
    chunks = chunkRuns ps
    gearNeighbors = fmap (positionNeighbors ps) gears
    chunksWhichNeighborGears = fmap chunksWhichNeighborAGear gearNeighbors
    chunksWhichNeighborAGear gearNs = filter (listsOverlap gearNs) chunks
    gearsWith2ChunkNeighbors = filter ((== 2) . length) chunksWhichNeighborGears
    valueOfChunks = (fmap . fmap) chunkValue gearsWith2ChunkNeighbors
