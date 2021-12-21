module Day13 where

import qualified Data.Set as Set

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

type Point = (Int, Int)
data Grid = Grid (Set.Set Point) deriving Show
data Fold = X Int | Y Int deriving Show

parseInput :: String -> (Grid, [Fold])
parseInput c = (grid, folds)
  where
    grid = Grid (Set.fromList $ map (\[a, b] -> (read a, read b)) $ map (wordsWhen (==',')) $ takeWhile (/="") $ lines c)
    folds = map parseFold $ map (!!2) $ map words $ tail $ dropWhile (/="") $ lines c

parseFold :: String -> Fold
parseFold fold = case wordsWhen (=='=') fold of ["x", x] -> X (read x)
                                                ["y", y] -> Y (read y)

visible :: Grid -> Int
visible (Grid grid) = Set.size grid

foldPoint :: Fold -> Point -> Point
foldPoint (X foldX) (x, y) | x < foldX = (x, y)
                           | otherwise = (foldX + (foldX - x), y)

foldPoint (Y foldY) (x, y) | y < foldY = (x, y)
                           | otherwise = (x, foldY + (foldY - y))


part1 :: String -> Int
part1 c = visible $ foldl doFold startGrid (take 1 folds)
  where
    (startGrid, folds) = parseInput c

    doFold :: Grid -> Fold -> Grid
    doFold (Grid grid) fold = Grid (Set.map (foldPoint fold) grid)

part2 :: String -> IO ()
part2 c = printGrid $ foldl doFold startGrid folds
  where
    (startGrid, folds) = parseInput c

    doFold :: Grid -> Fold -> Grid
    doFold (Grid grid) fold = Grid (Set.map (foldPoint fold) grid)

    printGrid :: Grid -> IO ()
    printGrid (Grid grid) = mapM_ print [[if Set.member (i, j) grid then '*' else '.'| i <- [0..maxX]] | j <- [0..maxY+1]]
      where
        (maxX, maxY) = maximum $ Set.elems grid


solve :: String -> IO ()
solve filename = do
  c <- readFile filename
  print $ part1 c
  part2 c

main = solve "input.txt"
