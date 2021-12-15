module Main where

import Data.List

import qualified Data.Heap as Heap
import qualified Data.Array as Array
import Debug.Trace

type Point = (Int, Int)
data Grid e = Grid {height :: Int,  width :: Int, gridImpl :: (Array.Array Point e)} deriving Show

parseInput :: String -> Grid Int
parseInput c = Grid height
                    width
                    (Array.array ((0, 0), (height-1, width-1)) [((i, j), read [e]) | (row, i) <- zip (lines c) [0..], (e, j) <- zip row [0..]])
  where
    height = length $ lines c
    width = length $ head $ lines c

parseInput2 :: String -> Grid Int
parseInput2 c = Grid (5*height)
                     (5*width)
                     (Array.array ((0, 0), (5*height-1, 5*width-1)) [((si * height + i, sj * width  + j), let v = (read [e] + si + sj) in if v >= 10 then 1 + (v `mod` 10) else v)
                                                                | si <- [0..4],
                                                                  sj <- [0..4],
                                                                  (row, i) <- zip (lines c) [0..],
                                                                  (e, j) <- zip row [0..]]
                                                                  )
  where
    height = length $ lines c
    width = length $ head $ lines c


newRiskGrid :: Grid Int -> Grid Int
newRiskGrid grid = setValue
                    (Grid
                      (height grid)
                      (width grid)
                      (Array.array ((0, 0), (height grid - 1, width grid - 1)) [((i, j), maxBound) | i <- [0..height grid - 1], j <- [0..width grid - 1]]))
                    (0, 0) 0


newSeenGrid :: Grid Int -> Grid Bool
newSeenGrid grid = Grid (height grid)
                        (width grid)
                        (Array.array ((0, 0), (height grid - 1, width grid- 1)) [((i, j), False) | i <- [0..height grid - 1], j <- [0..width grid - 1]])


getValue :: Grid e -> Point -> e
getValue (Grid _ _ grid) point = grid Array.! point

setValue :: Grid e -> Point -> e -> Grid e
setValue grid point value = Grid (height grid) (width grid) (gridImpl grid Array.// [(point, value)])

part1 :: String -> Int
part1 c = bfs inputGrid (newRiskGrid inputGrid) (newSeenGrid inputGrid) (Heap.fromList [(0, (0, 0))])
  where
    inputGrid = parseInput c

    neighbours :: Grid a -> Point -> [Point]
    neighbours grid (i, j) = [(i+i',j+j') | i' <- [-1..1],
                                            j' <- [-1..1],
                                            abs i'+ abs j' == 1,
                                            i+i' >= 0,
                                            j+j' >= 0,
                                            i+i' < height grid,
                                            j+j' < width grid]

    updateRiskGrid :: Grid Int -> Point -> Int -> Grid Int
    updateRiskGrid grid point value = if point /= (0, 0) && prevValue > value then (setValue grid point value) else grid
      where
        prevValue = getValue grid point

    viewHeap heap = case Heap.view heap of Nothing -> error "Empty heap"
                                           Just (i, h) -> (i, h)

    bfs :: Grid Int -> Grid Int -> Grid Bool -> (Heap.MinHeap (Int, Point)) -> Int
    bfs grid riskGrid seenGrid queue | Heap.size queue == 0 || point == (height riskGrid - 1, width riskGrid - 1) = getValue riskGrid (height riskGrid - 1, width riskGrid - 1)
                                     | otherwise = bfs grid newRiskGrid newSeen newQueue
                                       where
                                         ((pointValue, point), queueTail) = viewHeap queue

                                         pointNeighbours :: [Point]
                                         pointNeighbours = neighbours grid point

                                         unseenNeighbours :: [Point]
                                         unseenNeighbours = map snd $ filter (not.fst) $ zip (map (getValue seenGrid) pointNeighbours) pointNeighbours

                                         newSeen = foldl (\g p -> setValue g p True) (setValue seenGrid point True) unseenNeighbours

                                         newRiskGrid = (foldl foldF riskGrid unseenNeighbours)
                                         foldF :: Grid Int -> Point -> Grid Int
                                         foldF foldlRiskGrid foldlPoint = updateRiskGrid foldlRiskGrid foldlPoint (pointValue + (getValue grid foldlPoint))

                                         newQueue = (foldl (\h i -> Heap.insert i h) queueTail (zip (map (getValue newRiskGrid) unseenNeighbours) unseenNeighbours))

--part2 :: String -> Int
part2 c = bfs inputGrid (newRiskGrid inputGrid) (newSeenGrid inputGrid) (Heap.fromList [(0, (0, 0))])
  where
    inputGrid = parseInput2 c

    neighbours :: Grid a -> Point -> [Point]
    neighbours grid (i, j) = [(i+i',j+j') | i' <- [-1..1],
                                            j' <- [-1..1],
                                            abs i'+ abs j' == 1,
                                            i+i' >= 0,
                                            j+j' >= 0,
                                            i+i' < height grid,
                                            j+j' < width grid]

    updateRiskGrid :: Grid Int -> Point -> Int -> Grid Int
    updateRiskGrid grid point value = if point /= (0, 0) && prevValue > value then (setValue grid point value) else grid
      where
        prevValue = getValue grid point

    viewHeap heap = case Heap.view heap of Nothing -> error "Empty heap"
                                           Just (i, h) -> (i, h)

    bfs :: Grid Int -> Grid Int -> Grid Bool -> (Heap.MinHeap (Int, Point)) -> Int
    bfs grid riskGrid seenGrid queue | Heap.size queue == 0 || point == (height riskGrid - 1, width riskGrid - 1) = getValue riskGrid (height riskGrid - 1, width riskGrid - 1)
                                     | otherwise = trace (show (pointValue, point)) $ bfs grid newRiskGrid newSeen newQueue
                                       where
                                         ((pointValue, point), queueTail) = viewHeap queue

                                         pointNeighbours :: [Point]
                                         pointNeighbours = neighbours grid point

                                         unseenNeighbours :: [Point]
                                         unseenNeighbours = map snd $ filter (not.fst) $ zip (map (getValue seenGrid) pointNeighbours) pointNeighbours

                                         newSeen = foldl (\g p -> setValue g p True) (setValue seenGrid point True) unseenNeighbours

                                         newRiskGrid = (foldl foldF riskGrid unseenNeighbours)
                                         foldF :: Grid Int -> Point -> Grid Int
                                         foldF foldlRiskGrid foldlPoint = updateRiskGrid foldlRiskGrid foldlPoint (pointValue + (getValue grid foldlPoint))

                                         newQueue = (foldl (\h i -> Heap.insert i h) queueTail (zip (map (getValue newRiskGrid) unseenNeighbours) unseenNeighbours))

solve filename = do
  c <- readFile filename
  --print $ part1 c
  print $ part2 c

main :: IO ()
main = solve "input.txt"
