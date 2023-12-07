-- |
module Parser (parseAlmanac) where

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

parseNumber :: Parser Int
parseNumber = read <$> many1 digit

parseAlmanac :: Parser ([Int], [Map])
parseAlmanac = liftA2 (,) parseSeeds parseMaps

parseSeeds :: Parser [Int]
parseSeeds = string "seeds: " *> parseNumbers <* newline

parseMaps :: Parser [Map]
parseMaps = sepEndBy1 parseMap newline

parseMap :: Parser Map
parseMap = liftA2 (,) parseDashedWord (string " map:\n" *> sepEndBy1 parseMapEntry newline)

parseMapEntry :: Parser MapEntry
parseMapEntry = liftA3 (,,) parseNumber (spaces *> parseNumber) (spaces *> parseNumber)
