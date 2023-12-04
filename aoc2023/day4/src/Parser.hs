-- |
module Parser (parseLine) where

import Control.Applicative (liftA3)
import Text.Parsec (Parsec, char, digit, many1, sepEndBy1, spaces, string)

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
