{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

data Number = Value Int | Pair Number Number deriving Show

parseNumber :: String -> (Number, String)
parseNumber xs | head xs == '[' = let
                                   (firstNumber, firstTail) = parseNumber $ drop 1 xs
                                   (secondNumber, secondTail) = parseNumber $ drop 1 firstTail
                                  in (Pair firstNumber secondNumber, drop 1 secondTail)
               | otherwise      = (Value (read [head xs]), drop 1 xs)

formatNumber :: Number -> String
formatNumber (Value a) = show a
formatNumber (Pair a b) = "[" ++ formatNumber a ++ "," ++ formatNumber b ++ "]"

add :: Number -> Number -> Number
add = Pair

split :: Number -> (Number, Bool)
split (Value a) | a < 10  = (Value a, False)
                | a >= 10 = (Pair (Value (a `div` 2)) (Value (a - (a `div` 2))), True)
split (Pair a b) = let
                     (newA, leftSplitted) = split a
                     (newB, rightSplitted) = split b
                   in case (leftSplitted, rightSplitted) of
                        (False, False) -> (Pair a b, False)
                        (True, _) -> (Pair newA b, True)
                        (False, True) -> (Pair a newB, True)


explode :: Number -> (Number, Bool)
explode number = let (resultNumber, _, _, exploded) = explode' 0 number in (resultNumber, exploded)
  where
    explode' :: Int -> Number -> (Number, Int, Int, Bool)
    explode' n (Pair (Value a) (Value b)) | n >= 4    = (Value 0, a, b, True)
                                          | otherwise = (Pair (Value a) (Value b), 0, 0, False)
    explode' n (Pair (Value a) b) = let
                                      (newB, toLeft, toRight, exploded) = explode' (n+1) b
                                    in (Pair (Value (a + toLeft)) newB, 0, toRight, exploded)
    explode' n (Pair a (Value b)) = let
                                      (newA, toLeft, toRight, exploded) = explode' (n+1) a
                                    in (Pair newA (Value (b + toRight)), toLeft, 0, exploded)
    explode' n (Pair a b) = let
                              (newA, leftToLeft, leftToRight, leftExploded) = explode' (n+1) a
                              (newB, rightToLeft, rightToRight, rightExploded) = explode' (n+1) b
                            in case (leftExploded, rightExploded) of
                                 (False, False) -> (Pair a b, 0, 0, False)
                                 (True, _) -> (Pair newA (addLeft leftToRight b), leftToLeft, 0, True)
                                 (False, True) -> (Pair (addRight rightToLeft a) newB, 0, rightToRight, True)



    addLeft :: Int -> Number -> Number
    addLeft n (Pair (Value a) b) = Pair (Value (a+n)) b
    addLeft n (Pair a b) = Pair (addLeft n a) b

    addRight :: Int -> Number -> Number
    addRight n (Pair a (Value b)) = Pair a (Value (b+n))
    addRight n (Pair a b) = Pair a (addRight n b)

reduce :: Number -> Number
reduce number = fst $ head $ dropWhile snd $ iterate (\(n, _) -> step n) (number, True)
  where
    step :: Number -> (Number, Bool)
    step number = let
                    (a, exploded) = explode number
                    (b, splitted) = split number
                  in case (exploded, splitted) of
                       (False, False) -> (number, False)
                       (True, _) -> (a, True)
                       (False, _) -> (b, True)

magnitude :: Number -> Int
magnitude (Value a) = a
magnitude (Pair a b) = 3 * magnitude a + 2 * magnitude b

part1 :: String -> Int
part1 c = magnitude $ foldl1 (\a b -> reduce $ add a b) $ numbers
  where
    numbers = map (fst.parseNumber) $ lines c


part2 :: String -> Int
part2 c = maximum [magnitude $ reduce $ add a b | a <- numbers, b <- numbers]
 where
    numbers = map (fst.parseNumber) $ lines c


solve filename = do
  c <- readFile filename
  print $ part1 c
  print $ part2 c

main :: IO ()
main = solve "input.txt"
