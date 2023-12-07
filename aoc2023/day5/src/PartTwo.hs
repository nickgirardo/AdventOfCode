-- |
module PartTwo (partTwo) where

import Parser (parseFancyAlmanac)

import Text.Parsec (parse)

partTwo :: String -> String
partTwo = (warning <>) . show . parse parseFancyAlmanac ""
  where
    warning = "Part two incomplete, this is just for testing the updated parser\n"
