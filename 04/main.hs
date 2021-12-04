module Main where

import Data.List

data Board = Board [[(Int, Bool)]] deriving (Show)
data Bingo = Bingo [Int] [Board] deriving (Show)

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

parseBoard :: [String] -> Board
parseBoard l = Board (map (\row -> zip (map read (words row)) (repeat False)) l)

parseInput :: String -> Bingo
parseInput c = Bingo numbers (boards (tail l))
  where
    l = lines c

    numbers :: [Int]
    numbers = map read (wordsWhen (==',') $ head $ l)

    boards :: [String] -> [Board]
    boards [] = []
    boards rows = parseBoard (take 5 $ tail rows) : boards (drop 6 rows)

markOnBoard :: Board -> Int -> Board
markOnBoard (Board rows) number = Board (map (\row -> map (\(i, f) -> (i, i == number || f)) row) rows)

doesBoardWin :: Board -> Bool
doesBoardWin (Board rows) = checkRows rows || checkRows (transpose rows)
  where
    checkRows = any (\row -> all (\(_, f) -> f) row)

foldlBoard :: (a -> (Int, Bool) -> a) -> a -> Board -> a
foldlBoard f initValue (Board rows) = foldl f initValue (concat rows)

part1 :: String -> Int
part1 = play . game
  where
    game :: String -> Bingo
    game = parseInput

    play :: Bingo -> Int
    play (Bingo numbers boards) = play' numbers boards (-1)
      where
        play' :: [Int] -> [Board] -> Int -> Int
        play' (number:numbers) boards lastNumber | any doesBoardWin boards = lastNumber * sumUnmarked boards
                                                 | otherwise = play' numbers (map (\board -> markOnBoard board number) boards) number

        sumUnmarked :: [Board] -> Int
        sumUnmarked boards = sum $ map (\board -> let winner = doesBoardWin board in foldlBoard (\s (i, f) -> s + if not f && winner then i else 0) 0 board) boards

part2 :: String -> Int
part2 = play . game
  where
    game :: String -> Bingo
    game = parseInput

    play :: Bingo -> Int
    play (Bingo numbers boards) = play' numbers boards (-1) (map (\_ -> False) boards)
      where
        play' :: [Int] -> [Board] -> Int -> [Bool] -> Int
        play' (number:numbers) boards lastNumber lastWinners | all doesBoardWin boards = lastNumber * sumUnmarked boards (map not (zipWith (&&) lastWinners (map doesBoardWin boards)))
                                                             | otherwise = play' numbers (map (\board -> markOnBoard board number) boards) number (map doesBoardWin boards)

        sumUnmarked :: [Board] -> [Bool] -> Int
        sumUnmarked boards inds = sum $ map (\(board, ind) -> foldlBoard (\s (i, f) -> s + if not f && ind then i else 0) 0 board) (zip boards inds)


solve :: String -> IO ()
solve filename = do
  c <- readFile filename
  print $ part1 c
  print $ part2 c

main :: IO ()
main = solve "input.txt"
