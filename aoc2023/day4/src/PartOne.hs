-- |
module PartOne (partOne) where

import Parser (parseLine)

import Data.List (intersect)
import Text.Parsec (parse)

countMatches :: (Int, [Int], [Int]) -> Int
countMatches (_, winners, pool) = length $ intersect winners pool

scoreCard :: (Int, [Int], [Int]) -> Int
scoreCard card = if matches == 0 then 0 else 2 ^ (matches - 1)
  where
    matches = countMatches card

partOne :: String -> String
partOne = withParsed . traverse (parse parseLine "") . lines
  where
    withParsed (Left err) = show err
    withParsed (Right a) = show $ sum $ fmap scoreCard a
