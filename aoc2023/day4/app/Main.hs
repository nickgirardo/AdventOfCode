module Main where

import PartOne (partOne)
import PartTwo (partTwo)
import PartTwoOld (partTwoOld)

import Data.Maybe (listToMaybe)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= go . listToMaybe
  where
    go (Just "--part-one") = readFile "input.txt" >>= print . partOne
    go (Just "--part-two-old") = readFile "input.txt" >>= print . partTwoOld
    go _ = readFile "input.txt" >>= print . partTwo
