{-# LANGUAGE TupleSections #-}

-- |
module PartOne (partOne) where

import Data.Char (isDigit)
import Data.Foldable (find)
import Data.List (nub)
import Data.Maybe (isJust, mapMaybe)

newtype Positioned a = Positioned (Int, Int, a)
    deriving (Show, Eq)

getPosition :: Positioned a -> (Int, Int)
getPosition (Positioned (x, y, _)) = (x, y)

fromPositioned :: Positioned a -> a
fromPositioned (Positioned (_, _, a)) = a

isAt :: (Int, Int) -> Positioned a -> Bool
isAt (x, y) (Positioned (x', y', _)) = x == x' && y == y'

hasAt :: (Int, Int) -> [Positioned a] -> Bool
hasAt pos = isJust . findPositionedAt pos

findPositionedAt :: (Int, Int) -> [Positioned a] -> Maybe (Positioned a)
findPositionedAt pos = find $ isAt pos

testValue :: (a -> Bool) -> Positioned a -> Bool
testValue p = p . fromPositioned

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

spread :: (a, [b]) -> [(a, b)]
spread (x, ys) = fmap (x,) ys

-- | Returns all positions neighboring a given positioned element
neighbors :: Positioned a -> [(Int, Int)]
neighbors (Positioned (x, y, _)) =
    [ (x + 1, y)
    , (x -1, y)
    , (x, y + 1)
    , (x, y -1)
    , (x + 1, y + 1)
    , (x + 1, y -1)
    , (x -1, y + 1)
    , (x -1, y -1)
    ]

-- | Returns all positioned elements neighboring a given positioned element
positionNeighbors :: [Positioned a] -> Positioned a -> [Positioned a]
positionNeighbors ps = mapMaybe (`findPositionedAt` ps) . neighbors

{- | Returns all positioned elements neighboring a given chunk of positioned elements
 Note that the elements of the chunk themselves are not returned
-}
chunkNeighbors :: Eq a => [Positioned a] -> [Positioned a] -> [Positioned a]
chunkNeighbors ps chunk = filter (isInChunk chunk) $ nub $ positionNeighbors ps =<< chunk
  where
    isInChunk :: [Positioned a] -> Positioned a -> Bool
    isInChunk chunk' = not . (`hasAt` chunk') . getPosition

-- | Groups all chunks of digit runs, the meat of this challenge
chunkRuns :: [Positioned Char] -> [[Positioned Char]]
chunkRuns allPs = map (makeChunk 0 digitPs) (starters digitPs)
  where
    digitPs = filter (testValue isDigit) allPs
    -- The digit elements at the start of a chunk
    -- (all digit elements that do not have a digit at the preceding location)
    starters allPos = filter (not . hasPreceding allPos) allPos
    hasPreceding ps (Positioned (x, y, _)) = hasAt (x - 1, y) ps
    -- Recursively add elements to the chunk, continuing until a non digit is reached
    -- `n` is the offset from the starter, incremented on each recursion
    makeChunk :: Int -> [Positioned a] -> Positioned a -> [Positioned a]
    makeChunk n ps starter@(Positioned (x, y, _)) =
        case findPositionedAt (x + n, y) ps of
            Just p -> p : makeChunk (n + 1) ps starter
            Nothing -> []

-- | Returns the integer value of a chunk
chunkValue :: [Positioned Char] -> Int
chunkValue = read . fmap fromPositioned

-- | Transforms a two dimensional field into a list of positioned elements
positions :: [[a]] -> [Positioned a]
positions = fmap (Positioned . swizzle . flatten3) . concatMap spread . enumerate . fmap enumerate
  where
    -- NOTE a bit of a hack since it's most convenient to make a list of (y,x) pairs
    -- but much easier to think about (x,y) pairs
    swizzle (y, x, z) = (x, y, z)
    -- See `Data.Profunctor.Product.Flatten`
    -- Didn't feel like adding it as a dependency for this one simple fn
    flatten3 (x, (y, z)) = (x, y, z)

partOne :: String -> String
partOne field = show $ sum $ map chunkValue $ filter hasSymbolNeighbors $ chunkRuns ps
  where
    ps = positions $ lines field
    hasSymbolNeighbors = any (/= '.') . fmap fromPositioned . chunkNeighbors ps
