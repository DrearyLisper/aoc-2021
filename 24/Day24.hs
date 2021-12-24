{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day24 where

import Debug.Trace

data Arg = RegisterW | RegisterX | RegisterY | RegisterZ | Value Int deriving Show
data Opcode = Inp Arg | Add Arg Arg | Mul Arg Arg | Div Arg Arg | Mod Arg Arg | Eql Arg Arg deriving Show
data ALU = ALU { w :: Int, x :: Int, y :: Int, z :: Int, inputs :: [Int], opcodes :: [Opcode]} deriving Show

parseArg :: String -> Arg
parseArg "w" = RegisterW
parseArg "x" = RegisterX
parseArg "y" = RegisterY
parseArg "z" = RegisterZ
parseArg x = Value (read x)

parseOpcode :: String -> Opcode
parseOpcode l = parseOpcode' $ words l
  where
    parseOpcode' ["inp", arg] = Inp (parseArg arg)
    parseOpcode' ["add", arg1, arg2] = Add (parseArg arg1) (parseArg arg2)
    parseOpcode' ["mul", arg1, arg2] = Mul (parseArg arg1) (parseArg arg2)
    parseOpcode' ["div", arg1, arg2] = Div (parseArg arg1) (parseArg arg2)
    parseOpcode' ["mod", arg1, arg2] = Mod (parseArg arg1) (parseArg arg2)
    parseOpcode' ["eql", arg1, arg2] = Eql (parseArg arg1) (parseArg arg2)

formatALU :: ALU -> String
formatALU alu = "w = " ++ show (w $ alu) ++ " " ++
                "x = " ++ show (x $ alu) ++ " " ++
                "y = " ++ show (y $ alu) ++ " " ++
                "z = " ++ show (z $ alu) ++ " " ++
                "opcode = " ++ show (head $ opcodes alu)

newALU :: [Opcode] -> [Int] -> ALU
newALU opcodes inputs = ALU
  { w = 0,
    x = 0,
    y = 0,
    z = 0,
    inputs = inputs,
    opcodes = opcodes
  }

set :: ALU -> Arg -> Int -> ALU
set alu RegisterW value = alu {w = value}
set alu RegisterX value = alu {x = value}
set alu RegisterY value = alu {y = value}
set alu RegisterZ value = alu {z = value}
set _ (Value _) _ = error "Can't use Value to set register"

get :: ALU -> Arg -> Int
get alu RegisterW = w $ alu
get alu RegisterX = x $ alu
get alu RegisterY = y $ alu
get alu RegisterZ = z $ alu
get _ (Value v) = v

isHalted :: ALU -> Bool
isHalted alu = null $ opcodes alu

step :: ALU -> ALU
step alu | null $ opcodes alu = alu
         | otherwise          = trace (formatALU alu) $ let newAlu = execute opcode in newAlu {opcodes = tail (opcodes alu)}
  where
    opcode = head (opcodes alu)

    execute :: Opcode -> ALU
    execute (Inp arg) =
      let
        newAlu = set alu arg (head $ inputs alu)
      in newAlu {inputs = tail $ inputs alu}

    execute (Add arg1 arg2) =
      let
        result = get alu arg1 + get alu arg2
        newAlu = set alu arg1 result
      in newAlu

    execute (Mul arg1 arg2) =
      let
        result = get alu arg1 * get alu arg2
        newAlu = set alu arg1 result
      in newAlu

    execute (Div arg1 arg2) =
      let
        result = get alu arg1 `div` get alu arg2
        newAlu = set alu arg1 result
      in newAlu

    execute (Mod arg1 arg2) =
      let
        result = get alu arg1 `mod` get alu arg2
        newAlu = set alu arg1 result
      in newAlu

    execute (Eql arg1 arg2) =
      let
        result = if get alu arg1 == get alu arg2 then 1 else 0
        newAlu = set alu arg1 result
      in newAlu


{-

x = z % 26 + 12
x = if w == x then 0 else 1
z = z * (x * 25 + 1) + x * (w + 6)

x = z % 26 + 10
x = if w == x then 0 else 1
z = z * (x * 25 + 1) + x * (w + 2)

x = z % 26 + 10
x = if w == x then 0 else 1
z = z * (x * 25 + 1) + x * (w + 13)

x = (z % 26) - 6
z = (z / 26)
x = if w == x then 0 else 1
z = z * (x * 25 + 1) + x * (w + 8)

x = z % 26 + 11
x = if w == x then 0 else 1
z = z * (x * 25 + 1) + x * (w + 13)

x = (z % 26) - 12
z = (z / 26)
x = if w == x then 0 else 1
z = z * (x * 25 + 1) + x * (w + 8)

x = z % 26 + 11
x = if w == x then 0 else 1
z = z * (x * 25 + 1) + x * (w + 3)

x = z % 26 + 12
x = if w == x then 0 else 1
z = z * (x * 25 + 1) + x * (w + 11)

x = z % 26 + 12
x = if w == x then 0 else 1
z = z * (x * 25 + 1) + x * (w + 10)


x = (z % 26) - 2
z = (z / 26)
x = if w == x then 0 else 1
z = z * (x * 25 + 1) + x * (w + 8)

x = (z % 26) - 5
z = (z / 26)
x = if w == x then 0 else 1
z = z * (x * 25 + 1) + x * (w + 14)

x = (z % 26) - 4
z = (z / 26)
x = if w == x then 0 else 1
z = z * (x * 25 + 1) + x * (w + 6)

x = (z % 26) - 4
z = (z / 26)
x = if w == x then 0 else 1
z = z * (x * 25 + 1) + x * (w + 8)

x = (z % 26) - 12
z = (z / 26)
x = if w == x then 0 else 1
z = z * (x * 25 + 1) + x * (w + 2)

push w + 6
push w + 2
push w + 13

top? w + 6
pop
push? w + 8

push w + 13

top? w + 12
pop
push? w + 8

push w + 3
push w + 11
push w + 10

top? w + 2
pop
push? w + 8

top? w + 5
pop
push? w + 14

top? w + 4
pop
push? w + 6

top? w + 4
pop
push? w + 8

top? w + 12
pop
push? w + 2

-}

run :: ALU -> Int -> ALU
run alu steps = iterate step alu !! steps

check i opcodes = z (run (alu i) (18 * length i)) `mod` 26
  where
    alu i = newALU opcodes i

largest = [9, 9, 2, 9, 1, 5, 9, 3, 1, 9, 9, 8, 9, 2]
smallest = [7, 3, 1, 8, 1, 2, 2, 1, 1, 9, 7, 1, 1, 1]

part1 :: String -> Int
part1 c = read (concatMap show largest)

part2 :: String -> Int
part2 c = read (concatMap show smallest)

solve filename = do
  c <- readFile filename
  print $ part1 c
  print $ part2 c

main = solve "input.txt"
