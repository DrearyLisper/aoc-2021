module Day09 where

import qualified Data.Array as Array
import Data.List

data FloorMap = FloorMap {
  height :: Int,
  width :: Int,
  heightMap :: (Array.Array (Int, Int) Int),
  components :: (Array.Array (Int, Int) Int)} deriving Show

parseInput :: String -> FloorMap
parseInput c = FloorMap height width
                -- Height map from input
                (Array.array ((0, 0), (height-1, width-1)) [((i, j), e) | (i, row) <- zip [0..] listMap, (j, e) <- zip [0..] row])
                -- Empty components map, will be filled in the second part
                (Array.array ((0, 0), (height-1, width-1)) [((i, j), -1) | i <- [0..height-1], j <- [0..width-1]])
  where
    listMap :: [[Int]]
    listMap = map (map (\char -> read [char])) (lines c)

    height :: Int
    height = length $ listMap

    width :: Int
    width = length $ head $ listMap

part1 :: String -> Int
part1 c = sum                -- Sum all local minimum heights
          $ map ((+1).snd)   -- Take height from the tuple
          $ filter fst       -- Filter only local minimum heights
          $ [(isLocal (i, j), (heightMap fm) Array.! (i, j)) | j <- [0..(width fm)-1], i <- [0..(height fm)-1]]
  where
    fm = parseInput c

    allLess :: (Int, Int) -> [(Int, Int)] -> Bool
    allLess i list = all (heightMap fm Array.! i<) $ map (heightMap fm Array.!) list

    candidates :: (Int, Int) -> [(Int, Int)]
    candidates (i, j) = [(i+i', j+j') | i' <- [-1..1], -- Generate offsets for i and j
                                        j' <- [-1..1],
                                        ((abs i') + (abs j')) == 1, -- We want only horizontal or vertical moves
                                        i+i' >= 0, -- Check borders
                                        j+j' >= 0,
                                        i+i' < height fm,
                                        j+j' < width fm]

    isLocal :: (Int, Int) -> Bool
    isLocal (i, j) = allLess (i, j) (candidates (i, j))

part2 :: String -> Int
part2 c = product           -- And get their product
          $ take 3          -- Take only 3 of them
          $ reverse         -- Get largest ones first
          $ sort            -- Sort them
          $ map length      -- Get sizes of groups
          $ group           -- Group components
          $ sort            -- Sort for grouping
          $ filter (/=(-1)) -- Filter out unreachable cells
          $ (Array.elems $ components $ labelledFloor) -- Take filled components values
  where
    alreadyVisited :: FloorMap -> (Int, Int) -> Bool
    alreadyVisited fm point = (heightMap fm) Array.! point == 9 || (components fm) Array.! point /= -1

    foldlF :: FloorMap -> (Int, Int) -> FloorMap
    foldlF fm (i, j) | alreadyVisited fm (i, j) = fm
                     | otherwise = bfs (i * (width fm) + j) fm [(i,j)]

    labelledFloor :: FloorMap
    labelledFloor = foldl foldlF fm [(i, j) | i <- [0..(height fm)-1], j <- [0..(width fm)-1]]
      where
        fm = parseInput c

    candidates :: (Int, Int) -> FloorMap -> [(Int, Int)]
    candidates (i, j) fm = [(i+i', j+j') | i' <- [-1..1], -- Generate offsets
                                           j' <- [-1..1],
                                           ((abs i') + (abs j')) == 1, -- Take only horizontal or vertical directions
                                           i+i' >= 0, -- Check borders
                                           j+j' >= 0,
                                           i+i' < height fm,
                                           j+j' < width fm,
                                           not $ alreadyVisited fm (i+i',j+j')]

    bfs :: Int -> FloorMap -> [(Int, Int)] -> FloorMap
    bfs _ floorMap [] = floorMap
    bfs label fm (point:queue) = bfs label fm { components = components fm Array.// [(point, label)]} ((candidates point fm) ++ queue)



solve :: String -> IO ()
solve filename = do
  c <- readFile filename
  print $ part1 c
  print $ part2 c

main = solve "input.txt"
