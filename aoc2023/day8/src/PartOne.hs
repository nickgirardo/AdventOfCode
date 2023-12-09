-- |
module PartOne (partOne) where

import Control.Arrow ((&&&))

partOne :: String -> String
partOne = go . fmap parseLine . lines
  where
    go :: [[Int]] -> String
    go = show . sum . fmap (\xs -> calc xs (length xs) 0)

parseLine :: String -> [Int]
parseLine = fmap read . words

diffList :: [Int] -> [Int]
diffList = fmap (uncurry (-)) . uncurry zip . (&&&) (drop 1) id

calc :: [Int] -> Int -> Int -> Int
calc list@(x:_) n m | not (all (== 0) list) = x * binom n m + calc (diffList list) n (m+1)
                    | otherwise = 0
calc [] _ _ = error "Not expecting the list to be empty :("

-- From https://stackoverflow.com/a/48454265
-- Didn't want to import the combinatorics package and this looks slightly faster than a totally naive approach
-- Although I'm not instrumenting it
binom :: Int -> Int -> Int
binom n k = product [max (k+1) (n-k+1) .. n] `div` product [1 .. min k (n-k)]
