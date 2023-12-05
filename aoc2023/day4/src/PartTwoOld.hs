-- |
module PartTwoOld (partTwoOld) where

import Parser (parseLine)

import Data.List (intersect)
import Text.Parsec (parse)

countMatches :: (Int, [Int], [Int]) -> (Int, Int)
countMatches (cardId, winners, pool) = (cardId, length $ intersect winners pool)

scoreBook :: [(Int, [Int], [Int])] -> Int
scoreBook book = scoreCards baseScores 0 (length baseScores)
  where
    baseScores = fmap countMatches book

scoreCards :: [(Int, Int)] -> Int -> Int -> Int
scoreCards baseScores start count = sum $ fmap (scoreCard baseScores) [start .. start + count]

-- NOTE the recursion here leads to recomputing values a ton
-- The performance really isn't great but it's not *too* bad
-- I might come back and try caching the intermediate results for fun
scoreCard :: [(Int, Int)] -> Int -> Int
scoreCard baseScores cardId =
    case lookup cardId baseScores of
        Nothing -> 0
        Just n ->
            if n == 0
                then 1
                else 1 + scoreCards baseScores (cardId + 1) (n - 1)

partTwoOld :: String -> String
partTwoOld = show . fmap scoreBook . traverse (parse parseLine "") . lines
