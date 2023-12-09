-- |
module PartTwo (partTwo) where

import Control.Arrow ((&&&))

partTwo :: String -> String
partTwo = go . fmap parseLine . lines
  where
    go :: [[Int]] -> String
    go = show . sum . fmap (`calc` 0)

parseLine :: String -> [Int]
parseLine = fmap read . words

diffList :: [Int] -> [Int]
diffList = fmap (uncurry (-)) . uncurry zip . (&&&) (drop 1) id

calc :: [Int] -> Int -> Int
calc list@(x : _) m
    | not (all (== 0) list) = x * (if odd m then -1 else 1) + calc (diffList list) (m + 1)
    | otherwise = 0
calc [] _ = error "Not expecting the list to be empty :("
