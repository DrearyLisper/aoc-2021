module Main where

import qualified Data.Map as Map

convert :: [Char] -> Int
convert [] = 0
convert (x : xs) = ((read [x]) :: Int) + 2 * convert xs

part1 :: String -> Int
part1 = answer . foldl step (Map.fromList [])  . map (zip [0..]) . lines
  where
    answer :: Map.Map (Int, Char) Int -> Int
    answer m = mostPopular * leastPopular
      where
        mostPopular :: Int
        mostPopular = convert $ reverse $ queryMap (<)

        leastPopular :: Int
        leastPopular = convert $ reverse $ queryMap (>)

        size :: Int
        size = fst $ head $ reverse $ Map.keys m

        queryMap :: (Int -> Int -> Bool) -> [Char]
        queryMap f = map (popular f) [0..size]

        popular :: (Int -> Int -> Bool) -> Int -> Char
        popular f index | ((Just f) <*> (Map.lookup (index, '0') m) <*> (Map.lookup (index, '1') m)) == Just True = '1'
                        | otherwise = '0'

    step :: Map.Map (Int, Char) Int-> [(Int, Char)] -> Map.Map (Int, Char) Int
    step m bits = foldl addToMap m bits
      where
        alterF :: Maybe Int -> Maybe Int
        alterF (Just k) = Just (k+1)
        alterF Nothing = Just 1

        addToMap :: Map.Map (Int, Char) Int -> (Int, Char) -> Map.Map (Int, Char) Int
        addToMap m bit = Map.alter alterF bit m

part2 :: String -> Int
part2 c = oxygen * co2
  where
    numbers :: [[(Int, Char)]]
    numbers = map (zip [0..]) $ lines c

    oxygen :: Int
    oxygen = convert $ reverse $ sieve (>) 0 $ numbers

    co2 :: Int
    co2 = convert $ reverse $ sieve (<=) 0 $ numbers

    sieve :: (Int -> Int -> Bool) -> Int -> [[(Int, Char)]] -> [Char]
    sieve f i n | length n == 1 = map snd $ head n
                | otherwise = sieve f (i+1) $ filter (\x -> (snd (x !! i)) == bit) n
                    where
                      bit = popular f i $ foldl (step i) (Map.fromList[]) n

    popular :: (Int -> Int -> Bool) -> Int -> Map.Map (Int, Char) Int -> Char
    popular f index m | ((Just f) <*> (Map.lookup (index, '0') m) <*> (Map.lookup (index, '1') m)) == Just True = '0'
                      | otherwise = '1'

    step :: Int -> Map.Map (Int, Char) Int-> [(Int, Char)] -> Map.Map (Int, Char) Int
    step i m bits = addToMap (bits !! i)
      where
        alterF :: Maybe Int -> Maybe Int
        alterF (Just k) = Just (k+1)
        alterF Nothing = Just 1

        addToMap :: (Int, Char) -> Map.Map (Int, Char) Int
        addToMap bit = Map.alter alterF bit m



solve :: String -> IO ()
solve filename = do
  c <- readFile filename
  print $ part1 c
  print $ part2 c

main :: IO ()
main = solve "input.txt"
