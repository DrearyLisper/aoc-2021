module Day14 where

import qualified Data.Map as Map
import Data.List

type Template = String
type Insertions = Map.Map String String

parseInput :: String -> (Template, Insertions)
parseInput c = (template, insertions)
  where
    template = head $ lines c
    insertions = foldl parseInsertion (Map.empty) (tail $ tail $ lines c)

parseInsertion :: Insertions -> String -> Insertions
parseInsertion insertions c = let [from, _, to] = words c in Map.insert from to insertions

getInsertion :: Insertions -> String -> String
getInsertion insertions key = case (Map.lookup key insertions) of Just v -> v
                                                                  Nothing -> []

part1 :: String -> Int
part1 c = maximum sizes - minimum sizes
  where
    sizes = map length $ group $ sort $ ((iterate expand template) !! 10)

    (template, insertions) = parseInput c

    expand :: String -> String
    expand [] = []
    expand [x] = [x]
    expand (x:y:xs) = [x]
                      ++ (getInsertion insertions [x, y])
                      ++ expand (y:xs)

type TemplateV2 = Map.Map (Template, Bool) Int

convertTemplate :: Template -> TemplateV2
convertTemplate template = convert Map.empty template
  where
    alterF :: Maybe Int -> Maybe Int
    alterF Nothing = Just 1
    alterF (Just x) = Just (x + 1)

    convert :: TemplateV2 -> Template -> TemplateV2
    convert templatesV2 [] = templatesV2
    convert templatesV2 [_] = templatesV2
    convert templatesV2 [x,y] = Map.alter alterF ([x,y], True) templatesV2
    convert templatesV2 (x:y:xs) = convert (Map.alter alterF ([x,y], False) templatesV2) (y:xs)


part2 :: String -> Int
part2 c = maximum sizes - minimum sizes
  where
    (template, insertions) = parseInput c

    iteratedTemplate :: TemplateV2
    iteratedTemplate = (iterate expand (convertTemplate template)) !! 40

    sizes :: [Int]
    sizes = map snd
            $ Map.assocs
            $ Map.fromListWith (+)
            $ concat
            $ map (\((t, isFinal), count) -> if isFinal
                                             then [([t !! 0], count), ([t !! 1], count)]
                                             else [([t !! 0], count)]) (Map.assocs iteratedTemplate)

    expand :: TemplateV2 -> TemplateV2
    expand templateV2 = Map.fromListWith (+) $ expand' (Map.assocs templateV2)
      where
        expand' :: [((Template, Bool), Int)] -> [((Template, Bool), Int)]
        expand' [] = []
        expand' (((t, isFinal), count):xs) = case (Map.lookup t insertions)
                                              of Nothing -> ((t, isFinal), count) : expand' xs
                                                 Just insertion -> (([t !! 0, head insertion], False), count)
                                                                   : (([head insertion, t !! 1], isFinal), count)
                                                                   : expand' xs

solve :: String -> IO ()
solve filename = do
  c <- readFile filename
  print $ part1 c
  print $ part2 c
