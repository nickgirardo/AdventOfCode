-- |
module PartOne (partOne) where

import Control.Applicative (liftA3)
import Data.List (intersect)
import Text.Parsec (Parsec, parse, many1, spaces, char, digit, string, sepEndBy1)

type Parser a = Parsec String () a

parseNumber :: Parser Int
parseNumber = spaces *> p
  where
    p = read <$> many1 digit

parseNumbers :: Parser [Int]
parseNumbers = sepEndBy1 parseNumber spaces

parsePrefix :: Parser Int
parsePrefix = string "Card" *> parseNumber <* char ':'

parseLine :: Parser (Int, [Int], [Int])
parseLine = liftA3 (,,) parsePrefix parseNumbers (char '|' *> parseNumbers)


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
