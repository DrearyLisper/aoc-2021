module Main where

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

parseInput :: String -> [Int]
parseInput = map read . wordsWhen (==',')

part1 :: String -> Int
part1 c = minimum $ map (cost crabs) [minimum crabs..maximum crabs]
  where
    crabs = parseInput c
    cost crabs p = sum $ map (\c -> abs (c - p)) crabs

part2 :: String -> Int
part2 c = minimum $ map (cost crabs) [minimum crabs..maximum crabs]
  where
    crabs = parseInput c
    cost crabs p = sum $ map (\c -> let n = abs (c - p) in (n * (n + 1)) `div` 2) crabs

solve :: String -> IO ()
solve filename = do
  c <- readFile filename
  print $ part1 c
  print $ part2 c

main :: IO ()
main = solve "input.txt"
