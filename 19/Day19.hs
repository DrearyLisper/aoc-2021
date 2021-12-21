{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day19 where

import Data.List

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'


type Point = (Int, Int, Int)
newtype Scanner = Scanner [Point] deriving Show

parseInput :: String -> [Scanner]
parseInput c = parse [] $ lines c
  where
    parse :: [Point] -> [String] -> [Scanner]
    parse points [] | not $ null points = [Scanner points]
                    | otherwise = []

    parse points (line:xs) | line == "" && not (null points) = Scanner points : parse [] xs
                           | take 3 line == "---" = parse [] xs
                           | otherwise = parse (parsePoint line : points) xs

    parsePoint :: String -> Point
    parsePoint line = let [x, y, z] = map read $ wordsWhen (==',') line in (x, y, z)

(<->) :: Point -> Point -> Point
(<->) (x1, y1, z1) (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)

(<+>) :: Point -> Point -> Point
(<+>) (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

(<-->) :: Point -> Point -> Int
(<-->) (x1, y1, z1) (x2, y2, z2) = abs (x1 - x2)
                                   + abs (y1 - y2)
                                   + abs (z1 - z2)

transform :: Int -> Point -> Point
transform n (x, y, z) = let
                        (a, b, c) = (n `mod` 2, (n `div` 2) `mod` 2, (n `div` 4) `mod` 2)
                        in (if a == 0 then x else -x,
                            if b == 0 then y else -y,
                            if c == 0 then z else -z)

pivot :: Int -> Point -> Point
pivot 0 (x, y, z) = (x, y, z)
pivot 1 (x, y, z) = (y, z, x)
pivot 2 (x, y, z) = (z, x, y)
pivot 3 (x, y, z) = (x, z, y)
pivot 4 (x, y, z) = (z, y, x)
pivot 5 (x, y, z) = (y, x, z)


data Connection = Connection Int Int (Int, Int, Point) deriving Show

findNode :: Eq a => a -> [(a, p)] -> p
findNode node mappings = case lookup node mappings of
                          Nothing -> error "Can't find scanner"
                          Just mapping -> mapping


part1 :: String -> Int
part1 c = length $ group $ sort rotate
  where
    scanners :: [Scanner]
    scanners = parseInput c

    offsets = fst $ dfs [] (0, (id, (0, 0, 0)))

    rotate :: [Point]
    rotate = concatMap
            (\(node, Scanner a) -> let (mapping, offset) = findNode node offsets in map (\o -> mapping o <+> offset) a)
            $ zip [0..] scanners

    pairs :: (Point->Point) -> Scanner -> Scanner -> [(Int, Int, Point)]
    pairs mapping (Scanner a) (Scanner b) = [(p, t, mapping i <-> pivot p (transform t (mapping j)))
                                            | i <- a, j <- b, p <- [0..5], t <- [0..7]]

    connection :: (Point->Point) -> Int -> Int -> [Connection]
    connection mapping i j = let
                               matched = filter ((>=12).length) $ group $ sort $ pairs mapping (scanners !! i) (scanners !! j)
                             in [Connection i j (head $ head matched) | not $ null matched]

    connections :: Int -> (Point -> Point) -> [Connection]
    connections from mapping = concat $ [connection mapping from j | j <- [0..length scanners - 1], from /= j]


    dfs :: [(Int, (Point -> Point, Point))]
      -> (Int, (Point -> Point, Point))
      -> ([(Int, (Point -> Point, Point))], [(Int, (Point -> Point, Point))])
    dfs history (node, (mapping, offset))
      | any (\(a, _) -> a == node) history = ([], history)
      | otherwise = foldl (\(found, nextHistory) (Connection _ to (p, t, o))
                          -> let
                              (new, nextHistory') = dfs nextHistory (to, (pivot p.transform t.mapping, o <+> offset))
                             in (found ++ new, nextHistory'))
                          ([(node, (mapping, offset))], (node, (mapping, offset)):history)
                          (connections node mapping)

part2 :: String -> Int
part2 c = maximum $ [a <--> b | a <- offsets, b <- offsets]
  where
    offsets :: [Point]
    offsets = map (\(_, (_, o)) -> o) $ fst $ dfs [] (0, (id, (0, 0, 0)))

    scanners :: [Scanner]
    scanners = parseInput c

    pairs :: (Point->Point) -> Scanner -> Scanner -> [(Int, Int, Point)]
    pairs mapping (Scanner a) (Scanner b) = [(p, t, mapping i <-> pivot p (transform t (mapping j)))
                                            | i <- a, j <- b, p <- [0..5], t <- [0..7]]

    connection :: (Point->Point) -> Int -> Int -> [Connection]
    connection mapping i j = let
                               matched = filter ((>=12).length) $ group $ sort $ pairs mapping (scanners !! i) (scanners !! j)
                             in [Connection i j (head $ head matched) | not $ null matched]

    connections :: Int -> (Point -> Point) -> [Connection]
    connections from mapping = concat $ [connection mapping from j | j <- [0..length scanners - 1], from /= j]

    dfs :: [(Int, (Point -> Point, Point))]
      -> (Int, (Point -> Point, Point))
      -> ([(Int, (Point -> Point, Point))], [(Int, (Point -> Point, Point))])
    dfs history (node, (mapping, offset))
      | any (\(a, _) -> a == node) history = ([], history)
      | otherwise = foldl (\(found, nextHistory) (Connection _ to (p, t, o))
                          -> let
                              (new, nextHistory') = dfs nextHistory (to, (pivot p.transform t.mapping, o <+> offset))
                             in (found ++ new, nextHistory'))
                          ([(node, (mapping, offset))], (node, (mapping, offset)):history)
                          (connections node mapping)

solve filename = do
  c <- readFile filename
  print $ part1 c
  print $ part2 c

main = solve "input.txt"
