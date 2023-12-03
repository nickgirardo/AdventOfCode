module Main where

import PartOne (partOne)
import PartTwo (partTwo)

import Data.Maybe (listToMaybe)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= go . listToMaybe
  where
    go (Just "--partOne") = readFile "input.txt" >>= print . partOne
    go _ = readFile "input.txt" >>= print . partTwo
