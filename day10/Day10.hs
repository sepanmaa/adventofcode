import Data.Char (ord, isSpace)
import Data.Bits (xor)
import Numeric (showHex)

data Str = Str { numbers :: [Int]
               , skip :: Int
               , pos :: Int }

empty = Str [0..255] 0 0

rotate :: [Int] -> Int -> [Int]
rotate xs n = take (length xs) . drop n $ cycle xs

knot pos x xs = let xs' = rotate xs pos
                in rotate ((reverse $ take x xs') ++ drop x xs') (length xs - pos)

hash :: Str -> [Int] -> Str
hash str [] = str
hash (Str ns skip pos) (x:xs) = hash Str { numbers = knot pos x ns
                                         , skip = skip + 1
                                         , pos = (pos + x + skip) `mod` length ns
                                         } xs

dense :: [Int] -> [Int]
dense [] = []
dense xs = (foldr1 xor $ take 16 xs) : dense (drop 16 xs)

toHex :: Int -> String
toHex x = let hex = showHex x ""
          in if length hex == 1 then "0" ++ hex else hex

p1 :: [Int] -> Int
p1 = product . take 2 . numbers . hash empty

p2 :: [Int] -> String
p2 = concat . map toHex . dense . numbers . hash empty . concat . take 64 . repeat
                                                         
parseInput :: String -> [Int]
parseInput = map (\x -> read x :: Int) . words . map (\x -> if x == ',' then ' ' else x)
  
parseAsciiInput :: String -> [Int]
parseAsciiInput xs = (map ord $ filter (not . isSpace) xs) ++ [17, 31, 73, 47, 23]

main = do
  input <- readFile "input.txt"
  putStrLn . show . p1 $ parseInput input
  putStrLn . p2 $ parseAsciiInput input
