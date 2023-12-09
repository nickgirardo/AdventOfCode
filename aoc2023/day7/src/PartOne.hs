-- |
module PartOne (partOne) where

import Control.Arrow ((&&&))
import Data.List (group, sort, sortOn)
import Data.Ord (Down (..))

data Hand = Hand HandType [Card]
    deriving (Eq, Show)

newtype Card = Card Char
    deriving (Eq, Show)

instance Enum Card where
    toEnum n
        | n == 0 = Card '2'
        | n == 1 = Card '3'
        | n == 2 = Card '4'
        | n == 3 = Card '5'
        | n == 4 = Card '6'
        | n == 5 = Card '7'
        | n == 6 = Card '8'
        | n == 7 = Card '9'
        | n == 8 = Card 'T'
        | n == 9 = Card 'J'
        | n == 10 = Card 'Q'
        | n == 11 = Card 'K'
        | n == 12 = Card 'A'
        | otherwise = error "Bad Arg"

    fromEnum (Card c)
        | c == '2' = 0
        | c == '3' = 1
        | c == '4' = 2
        | c == '5' = 3
        | c == '6' = 4
        | c == '7' = 5
        | c == '8' = 6
        | c == '9' = 7
        | c == 'T' = 8
        | c == 'J' = 9
        | c == 'Q' = 10
        | c == 'K' = 11
        | c == 'A' = 12
        | otherwise = error "Bad Arg"

instance Ord Card where
    (<=) a b = fromEnum a <= fromEnum b

data HandType
    = FiveOAK
    | FourOAK
    | FullHouse
    | ThreeOAK
    | TwoPair
    | OnePair
    | HighCard
    deriving (Eq, Show)

instance Enum HandType where
    toEnum n
        | n == 0 = HighCard
        | n == 1 = OnePair
        | n == 2 = TwoPair
        | n == 3 = ThreeOAK
        | n == 4 = FullHouse
        | n == 5 = FourOAK
        | n == 6 = FiveOAK
        | otherwise = error "Bad Arg"

    fromEnum HighCard = 0
    fromEnum OnePair = 1
    fromEnum TwoPair = 2
    fromEnum ThreeOAK = 3
    fromEnum FullHouse = 4
    fromEnum FourOAK = 5
    fromEnum FiveOAK = 6

instance Ord HandType where
    (<=) htypeA htypeB = fromEnum htypeA <= fromEnum htypeB

instance Ord Hand where
    (<=) (Hand htypeA cardsA) (Hand htypeB cardsB)
        | htypeA == htypeB = cardsA <= cardsB
        | otherwise = htypeA <= htypeB

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

rank :: [a] -> [(a, Int)]
rank = flip zip [1 ..]

sortedCounts :: Ord a => [a] -> [(a, Int)]
sortedCounts = sortOn (Down . snd) . counts

counts :: Ord a => [a] -> [(a, Int)]
counts = fmap ((&&&) head length) . group . sort
