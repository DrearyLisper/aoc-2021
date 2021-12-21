module Day12 where

import Data.List
import qualified Data.Char as Char
import qualified Data.Map as Map

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'


data Cave = Cave { name :: String, isBig :: Bool} deriving (Show, Ord, Eq)
data CaveMap = CaveMap (Map.Map Cave [Cave]) deriving Show

newCaveMap :: CaveMap
newCaveMap = CaveMap (Map.fromList [])

parseCave :: String -> Cave
parseCave caveName = Cave caveName (Char.isUpper $ head caveName)

parseInput :: String -> CaveMap
parseInput c = foldl foldlF newCaveMap pairs
  where
    pairs :: [[Cave]]
    pairs = map (map parseCave) $ map (wordsWhen (=='-')) (lines c)

    foldlF :: CaveMap -> [Cave] -> CaveMap
    foldlF (CaveMap caveMap) caves = CaveMap (Map.alter (alterF (caves !! 0)) (caves !! 1)
                                              (Map.alter (alterF (caves !! 1)) (caves !! 0) caveMap))
      where
        alterF :: Cave -> Maybe [Cave] -> Maybe [Cave]
        alterF cave Nothing = Just [cave]
        alterF cave (Just foundCaves) = Just (cave:foundCaves)

neighbours :: CaveMap -> Cave -> [Cave]
neighbours (CaveMap caveMap) cave = case Map.lookup cave caveMap of Nothing -> error "Absent cave in caveMap"
                                                                    (Just caves) -> caves
part1 :: String -> Int
part1 c = length $ dfs [] (parseCave "start")
  where
    caveMap = parseInput c

    dfs :: [Cave] -> Cave -> [[Cave]]
    dfs history cave | (not $ isBig cave) && elem cave history = []
                     | name cave == "end" = [(parseCave "end") : history]
                     | otherwise = concat $ map (dfs (cave:history)) (neighbours caveMap cave)

part2 :: String -> Int
part2 c = length $ dfs [] (parseCave "start")
  where
    caveMap = parseInput c

    dfs :: [Cave] -> Cave -> [[Cave]]
    dfs history cave | name cave == "end" = [(parseCave "end") : history]
                     | condition cave history = []
                     | otherwise = concat $ map (dfs (cave:history)) (neighbours caveMap cave)

    condition :: Cave -> [Cave] -> Bool
    condition cave history = ((name cave == "start") && (elem cave history))
                             || (length (filter (==2) (map length (group (sort (filter (not.isBig) (cave:history)))))) > 1)
                             || (length (filter (>2) (map length (group (sort (filter (not.isBig) (cave:history)))))) >= 1)

solve :: String -> IO ()
solve filename = do
  c <- readFile filename
  print $ part1 c
  print $ part2 c

main = solve "input.txt"
