{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day21 where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.List (group, sort)

data Dice = Dice Int Int deriving Show

newDice :: Dice
newDice = Dice 0 1

throwDice :: Dice -> (Int, Dice)
throwDice (Dice n 100) = (100, Dice (n+1) 1)
throwDice (Dice n next) = (next, Dice (n+1) (next+1))

data Player = Player Int Int deriving (Show, Ord, Eq)

parsePlayer :: String -> Player
parsePlayer l = Player 0 (read $ last $ words l)

advance :: Int -> Int -> Int
advance place moves = ((place + moves - 1) `mod` 10) + 1

data Game = Game Int Bool [Player] Dice deriving Show

newGame :: [Player] -> Game
newGame players = Game 0 False players newDice

part1 :: String -> Int
part1 c = finalScore $ play $ game
  where
    players = map parsePlayer $ lines c
    game = newGame players

    play :: Game -> Game
    play game = head $ dropWhile (\(Game _ finished _ _) -> not finished) $ iterate step game

    finalScore :: Game -> Int
    finalScore (Game nextPlayer _ players (Dice n _)) = playerScore * n
      where
        (Player playerScore _) = players !! nextPlayer

    step :: Game -> Game
    step (Game nextPlayer _ players dice) = Game (1 - nextPlayer) (playerNewScore >= 1000) replace newDice
      where
        (Player playerScore playerPlace) = players !! nextPlayer
        (moves, newDice) = iterate (\(p, d) -> let (ap, nd) = throwDice d in (p+ap, nd)) (0, dice) !! 3

        playerNewPlace = advance playerPlace moves
        playerNewScore = playerScore + playerNewPlace

        replace :: [Player]
        replace = replace' players nextPlayer (Player playerNewScore playerNewPlace)
          where
            replace' :: [Player] -> Int -> Player -> [Player]
            replace' (x:xs) 0 p = p : xs
            replace' (x:xs) n p = x : replace' xs (n-1) p

part2 :: String -> Int
part2 c = let
            (player1Ways, player2Ways) = waysToWin (players !! 0, players !! 1, True)
          in max player1Ways player2Ways
  where
    players = map parsePlayer $ lines c
    game = newGame players

    waysToWin :: (Player, Player, Bool) -> (Int, Int)
    waysToWin state = if who state
      then (howManyFinished player + waysToWin1, waysToWin2)
      else (waysToWin1, howManyFinished player + waysToWin2)
      where
        player = getPlayer state

        (waysToWin1, waysToWin2) = foldl (\(a, b) (c, d) -> (a + c, b + d))
                                   (0, 0)
                                   $ map
                                   (\(times, player) -> let (a, b) = waysToWin $ replacePlayer state player in (times * a, times * b))
                                   (nextUnfinished player)

    getPlayer :: (Player, Player, Bool) -> Player
    getPlayer (player1, player2, whoPlays) = (if whoPlays then fst else snd) (player1, player2)

    replacePlayer :: (Player, Player, Bool) -> Player -> (Player, Player, Bool)
    replacePlayer (player1, player2, whoPlays) player = if whoPlays
      then (player, player2, not whoPlays)
      else (player1, player, not whoPlays)

    who :: (Player, Player, Bool) -> Bool
    who (_, _, b) = b

    howManyFinished :: Player -> Int
    howManyFinished player = sum $ map (\(times, Player score _) -> if score >= 21 then times else 0) (nextPlayers player)

    nextUnfinished :: Player -> [(Int, Player)]
    nextUnfinished player = filter (\(times, Player score _) -> score < 21) (nextPlayers player)

    nextPlayers :: Player -> [(Int, Player)]
    nextPlayers (Player playerScore playerPlace) = map (\(times, moves) ->
                                                          let
                                                            playerNewPlace = advance playerPlace moves
                                                          in (times, Player (playerScore + playerNewPlace) playerNewPlace)) dices

    dices :: [(Int, Int)]
    dices = map (\x-> (length x, head x)) $ group $ sort $ [a + b + c | a <- [1..3], b <- [1..3], c <- [1..3]]

solve filename = do
  c <- readFile filename
  print $ part1 c
  print $ part2 c

main = solve "input.txt"
