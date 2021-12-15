module Main where

import Data.List

import qualified Data.Heap as Heap
import qualified Data.Array as Array
import qualified Data.Set as Set

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

getValue :: Grid e -> Point -> e
getValue (Grid _ _ grid) point = grid Array.! point

neighbours :: Grid a -> Set.Set Point -> Point -> [Point]
neighbours grid seen (i, j) = [(i+i',j+j') | i' <- [-1..1],
                                             j' <- [-1..1],
                                             abs i'+ abs j' == 1,
                                             i+i' >= 0,
                                             j+j' >= 0,
                                             i+i' < height grid,
                                             j+j' < width grid,
                                             not $ Set.member (i+i', j+j') seen]

splitHeap :: Heap.MinHeap (Int, Point) ->  ((Int, Point), Heap.MinHeap (Int, Point))
splitHeap heap = case Heap.view heap of Nothing -> error "Empty heap"
                                        Just (i, h) -> (i, h)

bfs :: Grid Int -> Set.Set Point -> (Heap.MinHeap (Int, Point)) -> Int
bfs grid seen queue | Heap.size queue == 0 || point == (height grid - 1, width grid - 1) = pointValue
                    | otherwise = bfs grid newSeen newQueue
                      where
                        ((pointValue, point), queueTail) = splitHeap queue

                        candidates :: [Point]
                        candidates = neighbours grid seen point

                        newSeen = Set.union seen (Set.fromList candidates)
                        newQueue = (foldl (\h i -> Heap.insert i h) queueTail (zip (map ((+pointValue).(getValue grid)) candidates) candidates))


part1 :: String -> Int
part1 c = bfs inputGrid (Set.fromList [(0,0)]) (Heap.fromList [(0, (0, 0))])
  where
    inputGrid = parseInput c

part2 :: String -> Int
part2 c = bfs inputGrid (Set.fromList [(0,0)]) (Heap.fromList [(0, (0, 0))])
  where
    inputGrid = parseInput2 c

solve filename = do
  c <- readFile filename
  print $ part1 c
  print $ part2 c

main :: IO ()
main = solve "input.txt"
