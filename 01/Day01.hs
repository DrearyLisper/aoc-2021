module Day01 where

part1 :: String -> Int
part1 = numberOfIncreases . map read . lines
  where
    numberOfIncreases :: [Int] -> Int
    numberOfIncreases xs | length xs < 2 = 0
                         | head xs < head (tail xs) = 1 + numberOfIncreases (tail xs)
                         | otherwise = numberOfIncreases (tail xs)


part2 :: String -> Int
part2 = numberOfIncreases . map read .lines
  where
    numberOfIncreases :: [Int] -> Int
    numberOfIncreases xs | length xs < 4 = 0
                         | sum (take 3 xs) < sum (take 3 $ tail xs) = 1 + numberOfIncreases (tail xs)
                         | otherwise = numberOfIncreases (tail xs)



solve :: String -> IO ()
solve filename = do
  c <- readFile filename
  print $ part1 c
  print $ part2 c
