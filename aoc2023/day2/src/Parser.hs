-- |
module Parser (Game (..), parseGame) where

import Control.Applicative (Alternative ((<|>)), liftA2)
import Data.Bifunctor (bimap)
import Text.Parsec (Parsec, char, digit, many1, sepBy1, space, string)

type Parser a = Parsec String () a

data Color = Red | Blue | Green
    deriving (Show)

data Pull = Pull
    { reds :: Int
    , blues :: Int
    , greens :: Int
    }
    deriving (Show)

instance Semigroup Pull where
    (<>) p1 p2 =
        Pull
            { reds = reds p1 + reds p2
            , blues = blues p1 + blues p2
            , greens = greens p1 + greens p2
            }

instance Monoid Pull where
    mempty = Pull{reds = 0, blues = 0, greens = 0}

data Game = Game
    { gameId :: Int
    , maxReds :: Int
    , maxBlues :: Int
    , maxGreens :: Int
    }
    deriving (Show)

parseNumber :: Parser Int
parseNumber = read <$> many1 digit

parseColor :: Parser Color
parseColor =
    (string "red" *> pure Red)
        <|> (string "blue" *> pure Blue)
        <|> (string "green" *> pure Green)

parsePrefix :: Parser Int
parsePrefix = string "Game " *> parseNumber <* char ':'

parsePullPart :: Parser Pull
parsePullPart = makePull <$> (space *> parseNumber) <*> (space *> parseColor)
  where
    makePull qty Red = Pull{reds = qty, blues = 0, greens = 0}
    makePull qty Blue = Pull{reds = 0, blues = qty, greens = 0}
    makePull qty Green = Pull{reds = 0, blues = 0, greens = qty}

parsePull :: Parser Pull
parsePull = mconcat <$> sepBy1 parsePullPart (char ',')

parseGame :: Parser Game
parseGame = mkGame <$> liftA2 (,) parsePrefix (sepBy1 parsePull (char ';'))
  where
    mkGame :: (Int, [Pull]) -> Game
    mkGame (gameId, pulls) =
        Game
            { gameId = gameId
            , maxReds = foldl (maxWith id reds) 0 pulls
            , maxBlues = foldl (maxWith id blues) 0 pulls
            , maxGreens = foldl (maxWith id greens) 0 pulls
            }

    maxWith f g = curry $ uncurry max . bimap f g
