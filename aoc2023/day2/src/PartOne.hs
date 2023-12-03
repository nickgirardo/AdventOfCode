-- |
module PartOne (partOne) where

import Parser (Game (..), parseGame)

import Data.Bifunctor (second)
import Text.Parsec (parse)

partOne :: String -> String
partOne = go . traverse (parse parseGame "") . lines
  where
    go (Left err) = show err
    go (Right games) = show $ sumIds $ filter fewEnough games
    fewEnough games =
        maxReds games <= 12
            && maxBlues games <= 14
            && maxGreens games <= 13
    sumIds = foldl (curry $ uncurry (+) . second gameId) 0
