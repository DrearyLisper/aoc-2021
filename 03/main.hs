module Main where

import qualified Data.Map as Map

convert :: [Char] -> Int
convert [] = 0
convert (x : xs) = ((read [x]) :: Int) + 2 * convert xs

part1 :: String -> Int
part1 = answer . foldl step (Map.fromList [])  . map (zip [0..]) . lines
  where
    --answer :: Map.Map (Char, Int) Int -> Int
    answer m = mostPopular m * leastPopular m
      where
        mostPopular = convert . reverse . queryMap (>)
        leastPopular = convert . reverse . queryMap (<)

        size :: Map.Map (Int, Char) Int -> Int
        size m = fst $ head $ reverse $ Map.keys m

        queryMap f m = map (popular f m) [0..size m]

        popular :: (Int -> Int -> Bool) -> Map.Map (Int, Char) Int -> Int -> Char
        popular f m index = if ((Just f) <*> (Map.lookup (index, '0') m) <*> (Map.lookup (index, '1') m)) == Just True
            then '1'
            else '0'

    step :: Map.Map (Int, Char) Int-> [(Int, Char)] -> Map.Map (Int, Char) Int
    step map bits = foldl addToMap map bits
      where
        alterF :: Maybe Int -> Maybe Int
        alterF (Just k) = Just (k+1)
        alterF Nothing = Just 1

        addToMap :: Map.Map (Int, Char) Int -> (Int, Char) -> Map.Map (Int, Char) Int
        addToMap map bit = Map.alter alterF bit map


part2 c = oxygen c * co2 c
  where
    oxygen = convert . reverse . sieve (>) 0 . numbers
    co2 = convert . reverse . sieve (<=) 0 . numbers

    numbers :: String -> [[(Int, Char)]]
    numbers = map (zip [0..]) . lines

    sieve :: (Int -> Int -> Bool) -> Int -> [[(Int, Char)]] -> [Char]
    sieve f i n | length n == 1 = map snd $ head n
                | otherwise = sieve f (i+1) $ filter (\x -> (snd (x !! i)) == bit) n
                    where
                      bit = popular f i $ foldl (step i) (Map.fromList[]) n

    popular :: (Int -> Int -> Bool) -> Int -> Map.Map (Int, Char) Int -> Char
    popular f index m = if ((Just f) <*> (Map.lookup (index, '0') m) <*> (Map.lookup (index, '1') m)) == Just True
        then '0'
        else '1'

    step :: Int -> Map.Map (Int, Char) Int-> [(Int, Char)] -> Map.Map (Int, Char) Int
    step i m bits = addToMap m (bits !! i)
      where
        alterF :: Maybe Int -> Maybe Int
        alterF (Just k) = Just (k+1)
        alterF Nothing = Just 1

        addToMap :: Map.Map (Int, Char) Int -> (Int, Char) -> Map.Map (Int, Char) Int
        addToMap m bit = Map.alter alterF bit m



solve :: String -> IO ()
solve filename = do
  c <- readFile filename
  print $ part1 c
  print $ part2 c

main = solve "input.txt"
