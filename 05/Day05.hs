module Day05 where

import qualified Data.Map as Map

data Point = Point Int Int deriving (Show)
data Line = Line Point Point deriving (Show)

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

parseInput :: String -> [Line]
parseInput c = map parseLine (lines c)
  where
    parsePoint :: String -> Point
    parsePoint point = let [a, b] = map (read) (wordsWhen (==',') point) in Point a b
    parseLine :: String -> Line
    parseLine line = Line (parsePoint a) (parsePoint b)
      where
        [a,_,b] = words line

isHorizontal :: Line -> Bool
isHorizontal (Line (Point x1 y1) (Point x2 y2)) = y1 == y2

isVertical :: Line -> Bool
isVertical (Line (Point x1 y1) (Point x2 y2)) = x1 == x2

isDiagonal :: Line -> Bool
isDiagonal (Line (Point x1 y1) (Point x2 y2)) = ((max x1 x2) - (min x1 x2)) == ((max y1 y2) - (min y1 y2))

enumerateLine :: Line -> [(Int, Int)]
enumerateLine (Line (Point x1 y1) (Point x2 y2)) | y1 == y2  = zip [min x1 x2..max x1 x2] (repeat y1)
                                                 | x1 == x2 = zip (repeat x1) [min y1 y2..max y1 y2]
                                                 | (x1 < x2) && (y1 < y2) = zip [x1..x2] [y1..y2]
                                                 | (x1 < x2) && (y1 > y2) = zip [x1..x2] (reverse [y2..y1])
                                                 | (x1 > x2) && (y1 < y2) = zip (reverse [x2..x1]) [y1..y2]
                                                 | otherwise = zip (reverse [x2..x1]) (reverse [y2..y1])


data Board = Board (Map.Map (Int, Int) Int) deriving (Show)

newBoard :: Board
newBoard = Board (Map.fromList [])


markOnBoard :: Board -> (Int, Int) -> Board
markOnBoard (Board m) k = Board (Map.alter alterF k m)
  where
    alterF :: Maybe Int -> Maybe Int
    alterF Nothing = Just 1
    alterF (Just x) = Just (x+1)

part1 :: String -> Int
part1 c = answer $ foldl step newBoard lines
  where
    lines = parseInput c

    answer :: Board -> Int
    answer (Board m) = length $ filter (>=2) $ Map.elems m

    step :: Board -> Line -> Board
    step board line | isHorizontal line || isVertical line = foldl markOnBoard board (enumerateLine line)
                    | otherwise = board

part2 :: String -> Int
part2 c = answer $ foldl step newBoard lines
  where
    lines = parseInput c

    answer :: Board -> Int
    answer (Board m) = length $ filter (>=2) $ Map.elems m

    step :: Board -> Line -> Board
    step board line | isDiagonal line || isHorizontal line || isVertical line = foldl markOnBoard board (enumerateLine line)
                    | otherwise = board

solve :: String -> IO ()
solve filename = do
  c <- readFile filename
  print $ part1 c
  print $ part2 c
