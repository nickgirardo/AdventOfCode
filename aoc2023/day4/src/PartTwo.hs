-- |
module PartTwo (partTwo) where

import Parser (parseLine)

import Data.List (intersect)
import Data.Maybe (mapMaybe)
import Text.Parsec (parse)

countMatches :: (Int, [Int], [Int]) -> (Int, Int)
countMatches (cardId, winners, pool) = (cardId, length $ intersect winners pool)

scoreBook :: [(Int, [Int], [Int])] -> Int
scoreBook book = scoreCards baseScores [] (length baseScores)
  where
    baseScores = fmap countMatches book

scoreCards :: [(Int, Int)] -> [(Int, Int)] -> Int -> Int
scoreCards _baseScores _scored cardId | cardId == -1 = 0
scoreCards baseScores scored cardId | otherwise = cardScore + scoreCards baseScores ((cardId, cardScore) : scored) (cardId - 1)
  where
    cardScore = scoreCard baseScores scored cardId

scoreCard :: [(Int, Int)] -> [(Int, Int)] -> Int -> Int
scoreCard baseScores scoredCards cardId =
    case lookup cardId baseScores of
        Nothing -> 0
        Just n ->
            if n == 0
                then 1
                else 1 + scoreOfOtherCards n
  where
    scoreOfOtherCards n = sum $ mapMaybe (`lookup` scoredCards) [cardId + 1 .. cardId + n]

partTwo :: String -> String
partTwo = show . fmap scoreBook . traverse (parse parseLine "") . lines
