-- |
module Common where

import Control.Arrow ((&&&))
import Data.List (group, sort, sortOn)
import Data.Ord (Down (..))

data Hand = Hand HandType [Card]
    deriving (Eq, Show)

instance Ord Hand where
    (<=) (Hand htypeA cardsA) (Hand htypeB cardsB)
        | htypeA == htypeB = cardsA <= cardsB
        | otherwise = htypeA <= htypeB

data JokerHand = JokerHand HandType [JokerCard]
    deriving (Eq, Show)

instance Ord JokerHand where
    (<=) (JokerHand htypeA cardsA) (JokerHand htypeB cardsB)
        | htypeA == htypeB = cardsA <= cardsB
        | otherwise = htypeA <= htypeB

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

newtype JokerCard = JokerCard Char
    deriving (Eq, Show)

instance Enum JokerCard where
    toEnum n
        | n == 0 = JokerCard 'J'
        | n == 1 = JokerCard '2'
        | n == 2 = JokerCard '3'
        | n == 3 = JokerCard '4'
        | n == 4 = JokerCard '5'
        | n == 5 = JokerCard '6'
        | n == 6 = JokerCard '7'
        | n == 7 = JokerCard '8'
        | n == 8 = JokerCard '9'
        | n == 9 = JokerCard 'T'
        | n == 10 = JokerCard 'Q'
        | n == 11 = JokerCard 'K'
        | n == 12 = JokerCard 'A'
        | otherwise = error "Bad Arg"

    fromEnum (JokerCard c)
        | c == 'J' = 0
        | c == '2' = 1
        | c == '3' = 2
        | c == '4' = 3
        | c == '5' = 4
        | c == '6' = 5
        | c == '7' = 6
        | c == '8' = 7
        | c == '9' = 8
        | c == 'T' = 9
        | c == 'Q' = 10
        | c == 'K' = 11
        | c == 'A' = 12
        | otherwise = error "Bad Arg"

instance Ord JokerCard where
    (<=) a b = fromEnum a <= fromEnum b

rank :: [a] -> [(a, Int)]
rank = flip zip [1 ..]

sortedCounts :: Ord a => [a] -> [(a, Int)]
sortedCounts = sortOn (Down . snd) . counts

counts :: Ord a => [a] -> [(a, Int)]
counts = fmap ((&&&) head length) . group . sort
