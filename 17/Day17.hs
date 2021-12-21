{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day17 where

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

type Point = (Int, Int)
data Area = Area Point Point deriving Show
data Probe = Probe { pos :: Point, vel :: Point } deriving Show

parseInput :: String -> Area
parseInput c = let
                [x , y] = map (map read.wordsWhen (==',')) $ words c
               in
                 Area (x !! 0, y !! 1) (x !! 1, y !! 0)

isProbeInArea :: Area -> Probe -> Bool
isProbeInArea
  (Area (topLeftX, topLeftY) (botRightX, botRightY))
  (Probe (x, y) _) = (x >= topLeftX)
                     && (x <= botRightX)
                     && (y >= botRightY)
                     && (y <= topLeftY)

canBeInArea :: Area -> Probe -> Bool
canBeInArea
  (Area (topLeftX, topLeftY) (botRightX, botRightY))
  (Probe (x, y) (vx, vy)) | x > botRightX && vx >= 0 = False
                          | y < botRightY = False
                          | x < topLeftX && vx <= 0 = False
                          | otherwise = True


trace :: Area -> Probe -> [(Point, Bool, Bool)]
trace area = takeWhile (\(_, _, c) -> c)
             . map (\probe  -> (pos probe, isProbeInArea area probe, canBeInArea area probe))
             . iterate probeStep

aggregate :: [(Point, Bool, Bool)] -> (Int, Bool)
aggregate t = (maximum $ map (\((_, y), _, _) -> y) t, any (\(_,b,_)->b) t)

newProbe :: Point -> Probe
newProbe = Probe (0, 0)

probeStep :: Probe -> Probe
probeStep (Probe (x, y) (vx, vy)) = Probe (newX, newY) (newVx, newVy)
  where
    newX = x + vx
    newY = y + vy
    newVx = case compare vx 0 of
      GT -> vx - 1
      LT -> vx + 1
      EQ -> 0
    newVy = vy - 1

part1 :: String -> Int
part1 c = maximum $ map fst $ filter snd $ bruteforce
  where
    area = parseInput c

    bruteforce = map aggregate
                 $ filter (not.null)
                 $ [trace area (newProbe (vx, vy)) | vx <- [0..550], vy <- [-300..300]]

part2 :: String -> Int
part2 c = length $ filter snd $ bruteforce
  where
    area = parseInput c

    bruteforce = map aggregate
                 $ filter (not.null)
                 $ [trace area (newProbe (vx, vy)) | vx <- [0..550], vy <- [-300..300]]


solve filename = do
  c <- readFile filename
  print $ part1 c
  print $ part2 c
