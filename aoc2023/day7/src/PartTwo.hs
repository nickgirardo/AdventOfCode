-- |
module PartTwo (partTwo) where

import Common (
    HandType (..),
    JokerCard (..),
    JokerHand (..),
    rank,
    sortedCounts,
 )

import Data.Bifunctor (first, second)
import Data.List (find, partition, sortOn)
import Data.Maybe (listToMaybe)

partTwo :: String -> String
partTwo = go . parseFile
  where
    go (Left err) = show err
    go (Right stuff) = show $ sum $ fmap score $ rank $ sortOn fst stuff
    score ((_hand, bid), rank') = bid * rank'

parseFile :: String -> Either String [(JokerHand, Int)]
parseFile = traverse parseLine . lines

parseLine :: String -> Either String (JokerHand, Int)
parseLine = go . words
  where
    go [hand, bid] = Right (parseHand hand, read bid)
    go _ = Left "Parse Error!"

parseHand :: String -> JokerHand
parseHand s = JokerHand (handTypeFrom $ jokerMerge $ sortedCounts s) (JokerCard <$> s)
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

jokerMerge :: [(Char, Int)] -> [(Char, Int)]
jokerMerge cards = case joker of
    Nothing -> cards
    (Just (_, jokerQty)) -> second (+ jokerQty) bestNonJoker : nonBestNonJokers
  where
    (joker, nonJokers) = first listToMaybe $ partition ((==) 'J' . fst) cards
    nonBestNonJokers = drop 1 nonJokers
    bestNonJoker = case listToMaybe nonJokers of
        Nothing -> ('J', 0)
        (Just best) -> best
