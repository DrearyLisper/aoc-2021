module Day10 where

import Data.List

isOpenBracket :: Char -> Bool
isOpenBracket x = x `elem` ['(', '[', '{', '<']

isCloseBracket :: Char -> Bool
isCloseBracket x = x `elem` [')', ']', '}', '>']

leftBracket :: Char -> Char
leftBracket ')' = '('
leftBracket ']' = '['
leftBracket '}' = '{'
leftBracket '>' = '<'

rightBracket :: Char -> Char
rightBracket '(' = ')'
rightBracket '[' = ']'
rightBracket '{' = '}'
rightBracket '<' = '>'

score :: Char -> Int
score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137
score _ = 0

score2 :: Char -> Int
score2 ')' = 1
score2 ']' = 2
score2 '}' = 3
score2 '>' = 4
score2 _ = 0


part1 :: String -> Int
part1 c = sum $ map (find []) (lines c)
  where
    find :: [Char] -> [Char] -> Int
    find _ [] = 0
    find [] (x:xs) = find [x] xs
    find (s:ss) (x:xs) | isCloseBracket x && s /= leftBracket x = score x
                       | isOpenBracket x = find (x:s:ss) xs
                       | otherwise = find ss xs

part2 :: String -> Int
part2 c = result $ map (scoreCompletion.(getCompletion [])) incomplete
  where
    input = lines c
    incomplete = map snd $ filter ((==0).fst) $ (zip (map (find []) input) input)

    result xs = sort xs !! ((length xs) `div` 2)

    scoreCompletion :: [Char] -> Int
    scoreCompletion completion = foldl (\a b -> 5 * a + score2 b) 0 completion

    getCompletion :: [Char] -> [Char] -> [Char]
    getCompletion ss [] = map rightBracket ss
    getCompletion [] (x:xs) = getCompletion [x] xs
    getCompletion (s:ss) (x:xs) | isOpenBracket x = getCompletion (x:s:ss) xs
                                | otherwise = getCompletion ss xs

    find :: [Char] -> [Char] -> Int
    find _ [] = 0
    find [] (x:xs) = find [x] xs
    find (s:ss) (x:xs) | isCloseBracket x && s /= leftBracket x = score x
                       | x `elem` ['(', '[', '{', '<'] = find (x:s:ss) xs
                       | otherwise = find ss xs


solve :: String -> IO ()
solve filename = do
  c <- readFile filename
  print $ part1 c
  print $ part2 c
