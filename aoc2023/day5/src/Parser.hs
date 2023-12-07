-- |
module Parser (parseBasicAlmanac, parseFancyAlmanac) where

import Common (Map, MapEntry)

import Control.Applicative (liftA2, liftA3, (<|>))
import Data.Functor (void)
import Text.Parsec (Parsec, char, digit, letter, many1, manyTill, optional, sepEndBy1, spaces, string)

type Parser = Parsec String ()

newline :: Parser ()
newline = void $ char '\n'

parseDashedWord :: Parser String
parseDashedWord = many1 $ letter <|> char '-'

parseNumbers :: Parser [Int]
parseNumbers = manyTill (parseNumber <* optional (char ' ')) newline

parseNumberPairs :: Parser [(Int, Int)]
parseNumberPairs = manyTill (p <* optional (char ' ')) newline
  where
    p = liftA2 (,) parseNumber (char ' ' *> parseNumber)

parseNumber :: Parser Int
parseNumber = read <$> many1 digit

-- | Parse an almanac where the seed line represent (startId, range) pairs
parseFancyAlmanac :: Parser ([(Int, Int)], [Map])
parseFancyAlmanac = liftA2 (,) parseSeedPairs parseMaps

-- | Parse an almanac where the seed line represents a simple list of seed ids
parseBasicAlmanac :: Parser ([Int], [Map])
parseBasicAlmanac = liftA2 (,) parseSeeds parseMaps

parseSeedPairs :: Parser [(Int, Int)]
parseSeedPairs = string "seeds: " *> parseNumberPairs <* newline

parseSeeds :: Parser [Int]
parseSeeds = string "seeds: " *> parseNumbers <* newline

parseMaps :: Parser [Map]
parseMaps = sepEndBy1 parseMap newline

parseMap :: Parser Map
parseMap = liftA2 (,) parseDashedWord (string " map:\n" *> sepEndBy1 parseMapEntry newline)

parseMapEntry :: Parser MapEntry
parseMapEntry = liftA3 (,,) parseNumber (spaces *> parseNumber) (spaces *> parseNumber)
