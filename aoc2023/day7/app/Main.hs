module Main where

import PartOne (partOne)
import PartTwo (partTwo)

import System.Environment (getArgs)
import Data.Maybe (listToMaybe)

main :: IO ()
main = getArgs >>= go . listToMaybe
  where
    go (Just "--part-one") = readFile "input.txt" >>= putStrLn . partOne
    go _ = readFile "input.txt" >>= putStrLn . partTwo
