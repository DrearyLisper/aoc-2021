module Day06 where

import qualified Data.Map as Map
import qualified Data.List as List

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

parseInput :: String -> Map.Map Int Int
parseInput c = foldl step (Map.fromList []) $ map read (wordsWhen (==',') c)
  where
    alterF :: Maybe Int -> Maybe Int
    alterF Nothing = Just 1
    alterF (Just x) = Just (x+1)

    step :: Map.Map Int Int -> Int -> Map.Map Int Int
    step m k = Map.alter alterF k m

part1 :: String -> Int
part1 c = sum $ Map.elems $ ((iterate step (parseInput c)) !! 80)
  where
    nextDay :: (Int, Int) -> [(Int, Int)]
    nextDay (0, count) = [(6, count), (8, count)]
    nextDay (age, count) = [(age-1, count)]

    step :: Map.Map Int Int -> Map.Map Int Int
    step m = Map.fromAscListWith (\a b -> a + b) $ List.sort $ concat $ map nextDay (Map.assocs m)

part2 :: String -> Int
part2 c = sum $ Map.elems $ ((iterate step (parseInput c)) !! 256)
  where
    nextDay :: (Int, Int) -> [(Int, Int)]
    nextDay (0, count) = [(6, count), (8, count)]
    nextDay (age, count) = [(age-1, count)]

    step :: Map.Map Int Int -> Map.Map Int Int
    step m = Map.fromAscListWith (\a b -> a + b) $ List.sort $ concat $ map nextDay (Map.assocs m)

solve :: String -> IO ()
solve filename = do
  c <- readFile filename
  print $ part1 c
  print $ part2 c

main = solve "input.txt"
