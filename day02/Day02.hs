import Data.List (sort)

toInts :: [String] -> [Int]
toInts = map (\x -> read x :: Int)

parseInput :: String -> [[Int]]
parseInput = map (toInts . words) . lines

checkSum :: [[Int]] -> Int
checkSum = foldl1 (+) . map (\row -> foldl1 max row - foldl1 min row)
 
evenlyDivisible :: [[Int]] -> Int
evenlyDivisible = foldl1 (+) . map (sum . go . reverse . sort)
  where go (x:xs) = (map fst . filter (((==) 0) . snd) $ map (quotRem x) xs) ++ go xs
        go [] = []

printResult :: ([[Int]] -> Int) -> String -> IO ()
printResult f = putStrLn . show . f . parseInput

main = do
  input <- readFile "input.txt"
  printResult checkSum input
  printResult evenlyDivisible input
