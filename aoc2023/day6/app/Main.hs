module Main where

import Data.Char (isDigit)
import System.Environment (getArgs)
import Data.Bifunctor (bimap, second)
import Data.Maybe (listToMaybe)

main :: IO ()
main = getArgs >>= \arg -> if listToMaybe arg == Just "--part-one" then go partOne else go partTwo
  where
    go :: (String -> IO ()) -> IO ()
    go = (>>=) $ readFile "./input.txt"

partOne :: String -> IO ()
partOne = print . product . fmap (distance . uncurry (quadratic (-1)) . second negate) . parseFile . lines
  where
    parseFile :: [String] -> [(Double, Double)]
    parseFile (times:distances:_) = zip (readLine times) (readLine distances)
    parseFile _ = error "Bad input"
    readLine = fmap read . words . dropWhile (not . isDigit)

partTwo :: String -> IO ()
partTwo = print . distance . uncurry (quadratic (-1)) . second negate . parseFile . lines
  where
    parseFile :: [String] -> (Double, Double)
    parseFile (times:distances:_) = (,) (readLine times) (readLine distances)
    parseFile _ = error "Bad input"
    readLine = read . filter isDigit

distance :: (Double, Double) -> Int
distance = uncurry (flip (-)) . bimap floor floor

quadratic :: Double -> Double -> Double -> (Double, Double)
quadratic a b c = ((-b + rad)/2 * a, (-b - rad)/ 2 * a)
  where
    rad = sqrt $ (b * b) - (4 * a * c)
