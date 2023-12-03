-- |
module PartTwo (partTwo) where

import Parser (Game (..), parseGame)

import Data.Bifunctor (second)
import Text.Parsec (parse)

partTwo :: String -> String
partTwo = go . traverse (parse parseGame "") . lines
  where
    go (Left err) = show err
    go (Right games) = show $ sum $ map power games
    power :: Game -> Int
    power game = maxReds game * maxBlues game * maxGreens game
