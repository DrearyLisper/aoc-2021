{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day25 where

import qualified Data.Map as Map
import Data.Maybe

data Grid = Grid Int Int (Map.Map (Int, Int) Char) deriving Show

parseGrid :: String -> Grid
parseGrid c = Grid (length rows) (length $ head rows) (Map.fromList [((i, j), e)
                                                                    | (row, i) <- zip rows [0..]
                                                                    , (e, j) <- zip row [0..]
                                                                    , e /= '.'] )
  where
    rows = lines c

get :: Grid -> (Int, Int) -> Char
get (Grid height width m) (x, y) = fromMaybe '.' (Map.lookup (x `mod` height, y `mod` width) m)

set :: Grid -> (Int, Int) -> Char -> Grid
set (Grid height width m) (x, y) e = Grid height width (Map.insert (x `mod` height, y `mod` width) e m)

delete :: Grid -> (Int, Int) -> Grid
delete (Grid height width m) (x, y) = Grid height width (Map.delete (x `mod` height, y `mod` width) m)

east :: (Grid, Bool) -> (Int, Int) -> (Grid, Bool)
east (grid, b) (x, y) | '>' == get grid (x, y) && '.' == get grid (x, y + 1) = (set (set grid (x, y) 'X') (x, y + 1) 'Z', True)
                      | otherwise = (grid, b)

south :: (Grid, Bool) -> (Int, Int) -> (Grid, Bool)
south (grid, b) (x, y) | 'v' == get grid (x, y) && '.' == get grid (x + 1, y) = (set (set grid (x, y) 'X') (x + 1, y) 'Z', True)
                       | otherwise = (grid, b)


moveEast :: Grid -> (Grid, Bool)
moveEast (Grid height width m) = let
                                  (Grid _ _ newM, updated) = foldl east (Grid height width m, False) [(i, j) | j <- reverse [0..width-1], i <- [0..height-1]]
                                 in (Grid height width (Map.map (\x -> case x of
                                                 '>' -> '>'
                                                 'Z' -> '>'
                                                 'X' -> '.'
                                                 'v' -> 'v'
                                                 '.' -> '.') newM), updated)

moveSouth :: Grid -> (Grid, Bool)
moveSouth (Grid height width m) = let
                                  (Grid _ _ newM, updated) = foldl south (Grid height width m, False) [(i, j) | j <- [0..width-1], i <- reverse [0..height-1]]
                                 in (Grid height width (Map.map (\x -> case x of
                                                 '>' -> '>'
                                                 'Z' -> 'v'
                                                 'X' -> '.'
                                                 'v' -> 'v'
                                                 '.' -> '.') newM), updated)


formatGrid :: Grid -> String
formatGrid (Grid height width m) = concat $ [[get (Grid height width m) (i, j) | j <- [0..width-1]] ++ "\n" | i <- [0..height-1]]

step :: (Grid, Bool) -> (Grid, Bool)
step (grid, _) = let
                  (first, firstMoved) = moveEast grid
                  (second, secondMoved) = moveSouth first
                 in (second, firstMoved || secondMoved)

part1 :: String -> Int
part1 c = length steps
  where
    steps = takeWhile snd $ iterate step (parseGrid c, True)

part2 :: String -> String
part2 c = "Merry Christmas and Happy New Year!"

solve filename = do
  c <- readFile filename
  print $ part1 c
  print $ part2 c

main = solve "input.txt"
