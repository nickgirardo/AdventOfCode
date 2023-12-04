module Main where

import Data.Char (isDigit, digitToInt, toLower)
import Control.Arrow ((&&&))
import System.Environment (getArgs)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.List (tails, isPrefixOf)

main :: IO ()
main = getArgs >>= \arg -> if listToMaybe arg == Just "--part-one" then go partOne else go partTwo
    where
      go :: (String -> IO ()) -> IO ()
      go = (>>=) $ readFile "./input.txt"

-- NOTE only makes sense if the input Ints are single digits
joinDigits :: Int -> Int -> Int
joinDigits a b = a*10 + b

partOne :: String -> IO ()
partOne = print . processFile
    where
        processFile :: String -> Int
        processFile = sum . map processLine . lines
        processLine :: String -> Int
        processLine = uncurry joinDigits . (&&&) head last . map digitToInt . filter isDigit
        _processLine' :: String -> Int
        _processLine' = read . uncurry (:) . (&&&) head ((: []) . last) . filter isDigit

partTwo :: String -> IO ()
partTwo = print . processFile

processFile :: String -> Int
processFile = sum . map processLine . lines

processLine :: String -> Int
processLine = uncurry joinDigits . (&&&) head last . mapMaybe valueOfTail . tails . map toLower

valueOfTail :: String -> Maybe Int
valueOfTail = magicalLookup (flip isPrefixOf) values
  where
        magicalLookup :: (a -> b -> Bool) -> [(b,c)] -> a -> Maybe c
        magicalLookup _f [] _key = Nothing
        magicalLookup f ((x,y):xys) key = if f key x then Just y else magicalLookup f xys key
        values = [("1", 1),
                  ("2", 2),
                  ("3", 3),
                  ("4", 4),
                  ("5", 5),
                  ("6", 6),
                  ("7", 7),
                  ("8", 8),
                  ("9", 9),
                  ("0", 0),
                  ("one",   1),
                  ("two",   2),
                  ("three", 3),
                  ("four",  4),
                  ("five",  5),
                  ("six",   6),
                  ("seven", 7),
                  ("eight", 8),
                  ("nine",  9),
                  ("zero",  0)]
