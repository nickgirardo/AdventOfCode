module Main where

import PartOne (partOne)

import Data.Maybe (listToMaybe)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= go . listToMaybe
  where
    go _ = readFile "input.txt" >>= print . partOne
