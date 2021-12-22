{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day22 where

import qualified Data.Set as Set
import Control.Monad
import Data.Maybe
import Debug.Trace


wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'


type Point = (Int, Int, Int)
data Cuboid = Cuboid Point Point deriving (Show, Ord, Eq)
data Action = Action Bool Cuboid deriving Show

parseAction :: String -> Action
parseAction l = Action (w !! 0 == "on") (parseCuboid $ w !! 1)
  where
    w = words l

parseCuboid :: String -> Cuboid
parseCuboid s = let
                  [(x1, x2), (y1, y2) ,(z1, z2)] = map parseInterval $ wordsWhen (==',') s
                in Cuboid (x1, y1, z1) (x2, y2, z2)

parseInterval :: String -> (Int, Int)
parseInterval s = let
                    [l, r] = wordsWhen (=='.') (wordsWhen (=='=') s !! 1)
                  in (read l, read r)

volume :: Cuboid -> Int
volume (Cuboid (x1, y1, z1) (x2, y2, z2)) = (x2 - x1 + 1) * (y2 - y1 + 1) * (z2 - z1 + 1)

intersect :: Cuboid -> Cuboid -> Maybe Cuboid
intersect (Cuboid (x1, y1, z1) (x2, y2, z2)) (Cuboid (x3, y3, z3) (x4, y4, z4)) =
  case sequence [intersect' (x1, x2) (x3, x4),
                 intersect' (y1, y2) (y3, y4),
                 intersect' (z1, z2) (z3, z4)] of
    Nothing -> Nothing
    Just [(x5, x6), (y5, y6), (z5, z6)] -> Just $ Cuboid (x5, y5, z5) (x6, y6, z6)

  where
    intersect' :: (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
    intersect' (a1, a2) (a3, a4) = let
                                    (b1, b2) = min (a1, a2) (a3, a4)
                                    (b3, b4) = max (a1, a2) (a3, a4)
                                   in if b3 > b2
                                         then Nothing
                                         else Just (b3, min b2 b4)
remove :: Cuboid -> Cuboid -> [Cuboid]
remove (Cuboid (x1, y1, z1) (x2, y2, z2)) (Cuboid (x3, y3, z3) (x4, y4, z4)) =
  filter (/=Cuboid (x3, y3, z3) (x4, y4, z4))
  $ filter valid
  [Cuboid (x !! xi, y !! yi, z !! zi)
          ((x !! (xi + 1)) - 1, (y !! (yi + 1)) - 1, (z !! (zi + 1)) - 1)
  | xi <- [0..2]
  , yi <- [0..2]
  , zi <- [0..2]]
  where
    valid :: Cuboid -> Bool
    valid (Cuboid (a1, b1, c1) (a2, b2, c2)) = (a1 <= a2) && (b1 <= b2) && (c1 <= c2)
    x = [x1, x3, x4 + 1, x2 + 1]
    y = [y1, y3, y4 + 1, y2 + 1]
    z = [z1, z3, z4 + 1, z2 + 1]

diff :: [Cuboid] -> Cuboid -> [Cuboid]
diff as b = concat $ [maybe [a] (remove a) (a `intersect` b) | a <- as]

on :: [Cuboid] -> Cuboid -> [Cuboid]
on as b = b : diff as b

off :: [Cuboid] -> Cuboid -> [Cuboid]
off as b = concatMap (\a -> maybe [a] (remove a) (a `intersect` b)) as

step :: [Cuboid] -> Action -> [Cuboid]
step as (Action True b) = on as b
step as (Action False b) = off as b

part1 :: String -> Int
part1 c = sum $ map volume $ foldl step [] actions
  where
    actions = take 20 $ map parseAction $ lines c

part2 :: String -> Int
part2 c = sum $ map volume $ foldl step [] actions
  where
    actions = map parseAction $ lines c


solve filename = do
  c <- readFile filename
  print $ part1 c
  print $ part2 c

main = solve "input.txt"
