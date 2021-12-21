{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day21 where


part1 :: String -> Int
part1 c = 1

part2 :: String -> Int
part2 c = 2

solve filename = do
  c <- readFile filename
  print $ part1 c
  print $ part2 c
