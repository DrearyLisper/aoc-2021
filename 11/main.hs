module Main where

import qualified Data.Array as Array

data Grid = Grid (Array.Array (Int, Int) Int) deriving Show

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n l | n > 0 = (take n l) : (chunks n (drop n l))
           | otherwise = error "Negative or zero n"

newGrid :: Grid
newGrid = Grid (Array.array ((0, 0), (9, 9)) [((i,j), 0) | i <- [0..9], j <- [0..9]] )

parseInput :: String -> Grid
parseInput c = Grid (Array.array ((0, 0), (height-1,width-1)) [((i,j), read [e])| (i, line) <- zip [0..] (lines c), (j, e) <- zip [0..] line])
  where
    height = 10
    width = 10


candidates :: (Int, Int) -> [(Int, Int)]
candidates (i, j) = [(i+i', j+j') | i' <- [-1..1], j' <- [-1..1],
                                    i+i' >= 0, j+j' >= 0,
                                    i+i' < 10, j+j' < 10,
                                    (abs i') + (abs j') > 0]


addOne :: Grid -> (Int, Int) -> Grid
addOne (Grid grid) point = Grid (grid Array.// [(point, (grid Array.! point) + 1)])

setMinusOne :: Grid -> (Int, Int) -> Grid
setMinusOne (Grid grid) point = Grid (grid Array.// [(point, -1)])

value :: Grid -> (Int, Int) -> Int
value (Grid grid) point = grid Array.! point

setValue :: Grid -> (Int, Int) -> Int -> Grid
setValue (Grid grid) point v = Grid (grid Array.// [(point, v)])

update :: Grid -> (Int, Int) -> Grid
update grid point | (value grid point) == -1 = grid
                  | otherwise = addOne grid point

cell :: (Grid, Bool) -> (Int, Int) -> (Grid, Bool)
cell (grid, hadFlash) point | (value grid point) == 9 = (setMinusOne (foldl update grid (candidates point)) point, True)
                            | otherwise = (grid, hadFlash)

clamp :: (Grid, Bool) -> (Int, Int) -> (Grid, Bool)
clamp (grid, hadFlash) point | (value grid point) >= 9 = (setValue grid point 9, True)
                             | otherwise = (grid, hadFlash)

bounds :: [(Int, Int)]
bounds = [(i, j) | i <- [0..9], j <- [0..9]]

flash :: (Grid, Bool) -> (Grid, Bool)
flash (grid, _) = foldl clamp (foldl cell (grid, False) bounds) bounds

step :: Grid -> Grid
step grid = foldl addOne (fst $ head $ dropWhile snd (tail (iterate flash (grid, False)))) bounds

part1 :: String -> Int
part1 c = foldl countZeros 0 (take 101 $ (iterate step startGrid))
  where
    countZeros :: Int -> Grid -> Int
    countZeros previous (Grid grid) = previous + (length $ filter (==0) (Array.elems grid))

    startGrid :: Grid
    startGrid = parseInput c

part2 :: String -> Int
part2 c = snd $ head $ dropWhile (not.allZeros.fst) $ zip (iterate step startGrid) [0..]
  where
    allZeros :: Grid -> Bool
    allZeros (Grid grid) = all (==0) $ Array.elems grid

    startGrid :: Grid
    startGrid = parseInput c

solve :: String -> IO ()
solve filename = do
  c <- readFile filename
  print $ part1 c
  print $ part2 c

main :: IO ()
main = solve "input.txt"
