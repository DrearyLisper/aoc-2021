{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

data Packet = Value Int Int Int | Operator Int Int [Packet] deriving Show

hex2bin :: String -> String
hex2bin [] = []
hex2bin (x:xs) = hex2bin' x ++ hex2bin xs
  where
    hex2bin' '0' = "0000"
    hex2bin' '1' = "0001"
    hex2bin' '2' = "0010"
    hex2bin' '3' = "0011"
    hex2bin' '4' = "0100"
    hex2bin' '5' = "0101"
    hex2bin' '6' = "0110"
    hex2bin' '7' = "0111"
    hex2bin' '8' = "1000"
    hex2bin' '9' = "1001"
    hex2bin' 'A' = "1010"
    hex2bin' 'B' = "1011"
    hex2bin' 'C' = "1100"
    hex2bin' 'D' = "1101"
    hex2bin' 'E' = "1110"
    hex2bin' 'F' = "1111"

bin2int :: String -> Int
bin2int = foldl (\a b -> a * 2 + b) 0 . map (\x -> read [x])

parseInput c = parsePacket $ hex2bin c

parsePacketValue :: String -> (String, String)
parsePacketValue ('1':v0:v1:v2:v3:xs) = ([v0, v1, v2, v3] ++ (fst $ parsePacketValue xs), snd $ parsePacketValue xs)
parsePacketValue ('0':v0:v1:v2:v3:xs) = ([v0, v1, v2, v3], xs)


parsePacketOperator :: String -> ([Packet], String)
parsePacketOperator (lt:xs) | lt == '0' = parse15Bits xs
                            | otherwise = parse11Bits xs
  where
    parse15Bits xs = (parse' $ take dataLength $ drop 15 xs, drop (15 + dataLength) xs)
      where
        parse' :: String -> [Packet]
        parse' [] = []
        parse' xs = let (packet, tail) = parsePacket xs in packet:parse' tail

        dataLength = bin2int $ take 15 xs

    parse11Bits xs = parse' (numberOfPackets xs) (drop 11 xs)
      where
        parse' :: Int -> String -> ([Packet], String)
        parse' 0 xs = ([], xs)
        parse' n xs = let
          (packet, tail) = parsePacket xs
          (packets, tail') = parse' (n-1) tail
          in (packet:packets, tail')
        numberOfPackets xs = bin2int $ take 11 xs

parsePacket :: String -> (Packet, String)
parsePacket (v0:v1:v2:id0:id1:id2:xs) | id == 4   = let (binValue, tail) = parsePacketValue xs in (Value version id (bin2int binValue), tail)
                                      | otherwise = let (packets, tail) = parsePacketOperator xs in (Operator version id packets, tail)
  where
    version = bin2int [v0,v1,v2]
    id = bin2int [id0,id1,id2]

part1 :: String -> Int
part1 c = sum $ versions packet
  where
    (packet, _) = parseInput c

    versions :: Packet -> [Int]
    versions (Value version _ _) = [version]
    versions (Operator version _ packets) = concat $ [version]:map versions packets

part2 :: String -> Int
part2 c = eval packet
  where
    (packet, _) = parseInput c

    eval :: Packet -> Int
    eval (Value _ _ value) = value
    eval (Operator _ id packets) | id == 0 = sum $ map eval packets
                                 | id == 1 = product $ map eval packets
                                 | id == 2 = minimum $ map eval packets
                                 | id == 3 = maximum $ map eval packets
                                 | id == 5 = if eval (packets !! 0) > eval (packets !! 1) then 1 else 0
                                 | id == 6 = if eval (packets !! 0) < eval (packets !! 1) then 1 else 0
                                 | id == 7 = if eval (packets !! 0) == eval (packets !! 1) then 1 else 0


solve filename = do
  c <- readFile filename
  print $ part1 c
  print $ part2 c

main :: IO ()
main = solve "input.txt"
