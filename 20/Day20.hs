{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day20 where

import qualified Data.Map as Map
import Data.Maybe

type Point = (Int, Int)
data Image = Image Point Point (Map.Map Point Int) Int deriving Show

parseInput :: String -> ([Int], Image)
parseInput c = let l = lines c in (parsePixels $ head l,
                                   Image (-1, -1) (length (drop 2 l), length (l !! 2)) (parseImage $ drop 2 l) 0)
  where
    parseImage l = Map.fromList [((i, j), if e == '#' then 1 else 0) | (i, row) <- zip [0..] l, (j, e) <- zip [0..] row]
    parsePixels l = map (\e -> if e == '#' then 1 else 0) l

safeLookup :: Image -> Point -> Int
safeLookup (Image _ _ m dflt) point = fromMaybe dflt (Map.lookup point m)

grid :: Image -> [(Int, Int)]
grid (Image (x1, y1) (x2, y2) _ _) = [(i, j) | i <- [x1..x2], j <- [y1..y2]]

patches :: Image -> [(Point, [Int])]
patches image = [((i, j), [safeLookup image (i+i', j+j') | i' <- [-1..1], j' <- [-1..1]]) | (i, j) <- grid image]

bin2int :: [Int] -> Int
bin2int = foldl (\a b -> a * 2 + b) 0

pixels :: Image -> [Int]
pixels (Image _ _ m _) = Map.elems m

part1 :: String -> Int
part1 c = length $ filter (==1) $ pixels $ iterate step image !! 2
  where
    (outputPixels, image) = parseInput c

    step :: Image -> Image
    step (Image (x1, y1) (x2, y2) m dflt) = Image (x1-2, y1-2) (x2+2, y2+2) newM newDflt
      where
        newM = Map.fromList
          (map (\(point, pixels) -> (point, outputPixels !! bin2int pixels))
           $ patches
           $ Image (x1, y1) (x2, y2) m dflt)

        newDflt = if dflt == 0
          then outputPixels !! 0
          else outputPixels !! bin2int [1, 1, 1, 1, 1, 1, 1, 1, 1]

part2 :: String -> Int
part2 c = length $ filter (==1) $ pixels $ iterate step image !! 50
  where
    (outputPixels, image) = parseInput c

    step :: Image -> Image
    step (Image (x1, y1) (x2, y2) m dflt) = Image (x1-2, y1-2) (x2+2, y2+2) newM newDflt
      where
        newM = Map.fromList
          (map (\(point, pixels) -> (point, outputPixels !! bin2int pixels))
           $ patches
           $ Image (x1, y1) (x2, y2) m dflt)
        newDflt = if dflt == 0
          then outputPixels !! 0
          else outputPixels !! bin2int [1, 1, 1, 1, 1, 1, 1, 1, 1]

solve filename = do
  c <- readFile filename
  print $ part1 c
  print $ part2 c
