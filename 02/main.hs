module Main where

data Submarine = Submarine Int Int deriving (Show)

part1 :: String -> Int
part1 = answer . foldl step (Submarine 0 0) . map (words) . lines
  where
    answer :: Submarine -> Int
    answer (Submarine pos depth) = pos * depth

    step :: Submarine -> [String] -> Submarine
    step (Submarine pos depth) ["forward", distance] = Submarine (pos + (read distance :: Int)) depth
    step (Submarine pos depth) ["up", distance] = Submarine pos (depth - (read distance :: Int))
    step (Submarine pos depth) ["down", distance] = Submarine pos (depth + (read distance :: Int))


data SubmarineV2 = SubmarineV2 Int Int Int deriving (Show)

part2 :: String -> Int
part2 = answer . foldl step (SubmarineV2 0 0 0) . map (words) . lines
  where
    answer :: SubmarineV2 -> Int
    answer (SubmarineV2 pos depth aim) = pos * depth

    step :: SubmarineV2 -> [String] -> SubmarineV2
    step (SubmarineV2 pos depth aim) ["forward", distance] = SubmarineV2 (pos + (read distance :: Int)) (depth + (read distance :: Int) * aim) aim
    step (SubmarineV2 pos depth aim) ["up", distance] = SubmarineV2 pos depth (aim - (read distance :: Int))
    step (SubmarineV2 pos depth aim) ["down", distance] = SubmarineV2 pos depth (aim + (read distance :: Int))


solve :: String -> IO ()
solve filename = do
  c <- readFile filename
  print $ part1 c
  print $ part2 c

main = solve "input.txt"
