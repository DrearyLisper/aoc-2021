module Main where

import Data.List
import qualified Data.Map as Map

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

data Number = Number String deriving (Show, Ord, Eq)
data Example = Example [Number] [Number] deriving Show

parseInput :: String -> [Example]
parseInput = map parseExample . lines
  where
    parseExample :: String -> Example
    parseExample line = Example input output
      where
        input = map Number . map sort $ take 10 $ words line
        output = map Number . map sort $ take 4 $ drop 11 $ words line

part1 :: String -> Int
part1 c = sum $ map count examples
  where
    examples :: [Example]
    examples = parseInput c

    easy :: Number -> Bool
    easy (Number signal) = elem (length signal) [2, 3, 4, 7]

    count :: Example -> Int
    count (Example _ output) = length $ filter easy output

part2 :: String -> Maybe Int
part2 c = fmap sum $ sequence $ map buildAndDecode examples
  where

    buildAndDecode example = decode example (hard example (easy example))

    examples :: [Example]
    examples = parseInput c

    decode :: Example -> Map.Map Int Number -> Maybe Int
    decode (Example _ output) m = fmap (foldl (\a b -> a*10+b) 0) $ sequence $ map (decodeNumber m) output
      where
        decodeNumber :: Map.Map Int Number -> Number -> Maybe Int
        decodeNumber m (Number signal) = Map.lookup (Number signal) (Map.fromList $ map (\(a,b)->(b,a)) (Map.assocs m))

    isJust :: Maybe a -> Bool
    isJust (Just _) = True
    isJust _ = False

    byLength :: Number -> Maybe Int
    byLength (Number signal) | length signal == 2 = Just 1
                             | length signal == 3 = Just 7
                             | length signal == 4 = Just 4
                             | length signal == 7 = Just 8
                             | otherwise = Nothing

    decodeLength6 :: String -> Map.Map Int Number -> Maybe Int
    decodeLength6 signal m = do
      (Number signal1) <- Map.lookup 1 m
      (Number signal4) <- Map.lookup 4 m
      if length (intersect signal1 signal) == 1
        then return 6
        else if length (intersect signal4 signal) == 3
                then return 0
                else return 9

    decodeLength5 :: String -> Map.Map Int Number -> Maybe Int
    decodeLength5 signal m = do
      (Number signal1) <- Map.lookup 1 m
      (Number signal4) <- Map.lookup 4 m
      if length (intersect signal1 signal) == 2
        then return 3
        else if length (intersect signal4 signal) == 3
                then return 5
                else return 2

    byMap :: Map.Map Int Number -> Number -> Maybe Int
    byMap m (Number signal) | isJust $ byLength (Number signal) = byLength (Number signal)
                            | length signal == 6 = decodeLength6 signal m
                            | length signal == 5 = decodeLength5 signal m
                            | otherwise = Nothing

    easy :: Example -> Map.Map Int Number
    easy (Example input _) = Map.fromList $ map (\((Just i), s) -> (i, s)) $ filter (isJust.fst) $ zip (map byLength input) input

    hard :: Example -> Map.Map Int Number -> Map.Map Int Number
    hard (Example input _) m = Map.fromList $ map (\((Just i), s) -> (i, s)) $ filter (isJust.fst) $ zip (map (byMap m) input) input




solve :: String -> IO ()
solve filename = do
  c <- readFile filename
  print $ part1 c
  print $ part2 c

main :: IO ()
main = solve "input.txt"
