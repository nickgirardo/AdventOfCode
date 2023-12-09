-- |
module PartOne (partOne) where

import Common (
    Card (..),
    Hand (..),
    HandType (..),
    rank,
    sortedCounts,
 )

import Data.List (sortOn)

partOne :: String -> String
partOne = go . parseFile
  where
    go (Left err) = show err
    go (Right stuff) = show $ sum $ fmap score $ rank $ sortOn fst stuff
    score ((_hand, bid), rank') = bid * rank'

parseFile :: String -> Either String [(Hand, Int)]
parseFile = traverse parseLine . lines

parseLine :: String -> Either String (Hand, Int)
parseLine = go . words
  where
    go [hand, bid] = Right (parseHand hand, read bid)
    go _ = Left "Parse Error!"

parseHand :: String -> Hand
parseHand s = Hand (handTypeFrom $ sortedCounts s) (Card <$> s)
  where
    handTypeFrom [_] = FiveOAK
    handTypeFrom [_, (_, lesserQty)]
        | lesserQty == 1 = FourOAK
        | otherwise = FullHouse
    handTypeFrom [(_, qty), _, _]
        | qty == 3 = ThreeOAK
        | otherwise = TwoPair
    handTypeFrom [_, _, _, _] = OnePair
    handTypeFrom _ = HighCard
