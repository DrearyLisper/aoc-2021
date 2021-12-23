{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import Data.List
import qualified Data.Heap as Heap
import qualified Data.Set as Set

import Debug.Trace
import Data.Maybe (isJust, fromMaybe)


wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

data Amphipod = Occupied Char | Empty deriving (Show, Eq, Ord)

data Diagram = Diagram [Amphipod] [Amphipod] [Amphipod] deriving (Show, Eq, Ord)

weight :: Amphipod -> Int
weight (Occupied 'A') = 1
weight (Occupied 'B') = 10
weight (Occupied 'C') = 100
weight (Occupied 'D') = 1000
weight Empty = error "No weight for Empty"

position :: Amphipod -> Int
position (Occupied 'A') = 2
position (Occupied 'B') = 4
position (Occupied 'C') = 6
position (Occupied 'D') = 8
position Empty = error "No position for Empty"

slice :: Int -> Int -> [a] -> [a]
slice from to xs = slice' (min from to) (max from to) xs
  where
    slice' from to xs = take (to - from + 1) (drop from xs)


put :: Int -> a -> [a] -> [a]
put 0 element (x:xs) = element:xs
put index element (x:xs) = x:put (index-1) element xs

parseDiagram :: String -> Diagram
parseDiagram c = Diagram secondRow firstRow hallway
  where
    firstRow = [let a = (lines c !! 2) !! i in if a `elem` ['.', ' ', '#'] then Empty else Occupied a | i <- [1..11]]
    secondRow = [let a = (lines c !! 3) !! i in if a `elem` ['.', ' ', '#'] then Empty else Occupied a | i <- [1..11]]
    hallway = [let a = (lines c !! 1) !! i in if a `elem` ['.', ' ', '#'] then Empty else Occupied a | i <- [1..11]]

formatAmphipod :: Amphipod -> Char
formatAmphipod (Occupied c) = c
formatAmphipod Empty = '.'

formatDiagram :: Diagram -> String
formatDiagram (Diagram secondRow firstRow hallway) = map formatAmphipod hallway ++ "\n" ++
                                                     map formatAmphipod firstRow ++ "\n" ++
                                                     map formatAmphipod secondRow ++ "\n"

isFinal :: Diagram -> Bool
isFinal (Diagram secondRow firstRow _) = (secondRow == goodRow) && firstRow == goodRow
  where
    goodRow = [Empty, Empty, Occupied 'A', Empty, Occupied 'B', Empty, Occupied 'C', Empty, Occupied 'D', Empty, Empty]

columnIsGood :: Diagram -> Int -> Bool
columnIsGood (Diagram secondRow firstRow hallway) i = ((secondRow !! i) /= Empty && (firstRow !! i) == Empty && i == position (secondRow !! i))
                                                      || ((secondRow !! i) /= Empty && (firstRow !! i) /= Empty && i == position (secondRow !! i) && i == position (firstRow !! i))


possibleDiagrams :: Diagram -> [(Int, Diagram)]
possibleDiagrams diagram = [(weight, moveAmphipod diagram from to) | (from, to, weight) <- possibleMoves]
  where
    possibleMoves = possibleToExit diagram ++ possibleToEnter diagram

    possibleToExit :: Diagram -> [(Int, Int, Int)]
    possibleToExit (Diagram secondRow firstRow hallway) = [(from, to, weight) | from <- [2, 4, 6, 8]
                                                                              , firstRow !! from /= Empty
                                                                              , not $ columnIsGood (Diagram secondRow firstRow hallway) from
                                                                              , to <- [0..10]
                                                                              , hallway !! to == Empty
                                                                              , let (possible, weight) = possibleToExit' firstRow hallway from to 0
                                                                              , possible]
                                                          ++ [(from, to, weight) | from <- [2, 4, 6, 8]
                                                                                 , secondRow !! from /= Empty
                                                                                 , firstRow !! from == Empty
                                                                                 , not $ columnIsGood (Diagram secondRow firstRow hallway) from
                                                                                 , to <- [0..10]
                                                                                 , let (possible, weight) = possibleToExit' secondRow hallway from to 1
                                                                                 , possible]

    possibleToExit' :: [Amphipod] -> [Amphipod] -> Int -> Int -> Int -> (Bool, Int)
    possibleToExit' amphipod hallway from to additionalStep | any (/=Empty) (slice from to hallway) = (False, 0)
                                                            | to `elem` [2, 4, 6, 8] = (False, 0)
                                                            | otherwise = (True, (abs (to - from) + 1 + additionalStep) * weight (amphipod !! from))

    possibleToEnter :: Diagram -> [(Int, Int, Int)]
    possibleToEnter (Diagram secondRow firstRow hallway) = [(from, to, weight) | from <- [0..10]
                                                                               , hallway !! from /= Empty
                                                                               , to <- [2, 4, 6, 8]
                                                                               , firstRow !! to == Empty
                                                                               , ((secondRow !! to) /= Empty) && (to == position (secondRow !! to))
                                                                               , let (possible, weight) = possibleToEnter' hallway firstRow from to 0
                                                                               , possible]
                                                           ++ [(from, to, weight) | from <- [0..10]
                                                                                  , hallway !! from /= Empty
                                                                                  , to <- [2, 4, 6, 8]
                                                                                  , firstRow !! to == Empty
                                                                                  , secondRow !! to == Empty
                                                                                  , let (possible, weight) = possibleToEnter' hallway secondRow from to 1
                                                                                  , possible]




    possibleToEnter' :: [Amphipod] -> [Amphipod] -> Int -> Int -> Int -> (Bool, Int)
    possibleToEnter' hallway amphipod from to additionalStep | (to < from) && any (/=Empty) (slice (from-1) to hallway) = (False, 0)
                                                             | (to > from) && any (/=Empty) (slice (from+1) to hallway) = (False, 0)
                                                             | to `notElem` [2, 4, 6, 8] = (False, 0)
                                                             | to /= position (hallway !! from) =  (False, 0)
                                                             | otherwise = (True, (abs (to - from) + 1 + additionalStep) * weight (hallway !! from))



moveAmphipod :: Diagram -> Int -> Int -> Diagram
moveAmphipod (Diagram secondRow firstRow hallway) from to
  | from `elem` [2, 4, 6, 8] && (firstRow !! from) /= Empty = Diagram secondRow (put from Empty firstRow) (put to (firstRow !! from) hallway)
  | from `elem` [2, 4, 6, 8] && (secondRow !! from) /= Empty = Diagram (put from Empty secondRow) firstRow (put to (secondRow !! from) hallway)
  | (secondRow !! to) == Empty = Diagram (put to (hallway !! from) secondRow) firstRow (put from Empty hallway)
  | (firstRow !! to) == Empty = Diagram secondRow (put to (hallway !! from) firstRow) (put from Empty hallway)
  | otherwise = error "Can't make move"

splitHeap :: Heap.MinHeap (Int, Diagram) ->  ((Int, Diagram), Heap.MinHeap (Int, Diagram))
splitHeap heap = case Heap.view heap of Nothing -> error "Empty heap"
                                        Just (i, h) -> (i, h)

--part1 :: String -> Int
part1 c = search (Heap.fromList [(0, parseDiagram c)])  (Set.fromList [parseDiagram c])
  where
    search :: Heap.MinHeap (Int, Diagram) -> Set.Set Diagram -> Int
    search h s
      | Heap.size h == 0 = 0
      | otherwise = trace (show (weight, Heap.size h)) $ if isFinal diagram
                  then weight
                  else search (foldl (\cH (w, nD) -> addOrUpdate (weight+w, nD) cH) newH possible) (Set.insert diagram s)
      where
        ((weight, diagram), newH) = splitHeap h
        possible = filter (\(_, d) -> not $ Set.member d s) $ possibleDiagrams diagram

        addOrUpdate :: (Int, Diagram) -> Heap.MinHeap (Int, Diagram) -> Heap.MinHeap (Int, Diagram)
        addOrUpdate (i,d) h = if null found
                                then Heap.insert (i,d) h
                                else if i < fst (head found) then
                                       Heap.insert (i,d) (Heap.filter ((/=d).snd) h)
                                       else h
          where
            found = Heap.toList $ Heap.filter ((==d).snd) h

--part2 :: String -> Int
part2 c = 2 --map (\(x, _) -> trace (formatDiagram x) $ x) $ possibleDiagrams (parseDiagram c)

solve filename = do
  c <- readFile filename
  print $ part1 c
  print $ part2 c

main = solve "input.txt"
