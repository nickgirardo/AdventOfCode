-- |
module PartOne (partOne) where

import Common (Map)
import Parser (parseBasicAlmanac)

import Data.Foldable (find)
import Text.Parsec (parse)

partOne :: String -> String
partOne = either show getBest . parse parseBasicAlmanac ""
  where
    getBest = show . minimum . uncurry destinations
    destinations seeds maps = flip (foldl evalMap) maps <$> seeds

evalMap :: Int -> Map -> Int
evalMap n = evalMapEntries . snd
  where
    evalMapEntries ranges = maybe n getDestination (find isInRange ranges)
    isInRange (_, start, cnt) = n >= start && n < start + cnt
    getDestination (destStart, srcStart, _) = destStart + n - srcStart
