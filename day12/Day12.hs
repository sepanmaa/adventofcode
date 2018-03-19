import qualified Data.Map as M
import qualified Data.Set as S

parseInput :: String -> M.Map Int [Int]
parseInput s = 
  let readInt x = read x :: Int
      nodes (x:_:xs) = (readInt x, map readInt xs)
      parseNodes line = nodes . words $ filter ((/=) ',') line
  in M.fromList . map parseNodes $ lines s

groups :: M.Map Int [Int] -> [Int]
groups input | length input <= 0 = []
             | otherwise = let ks = S.fromList $ programs input [] (head $ M.keys input)
                           in (length ks) : groups (M.withoutKeys input ks)

programs :: M.Map Int [Int] -> [Int] -> Int -> [Int]
programs input visited current
  | current `elem` visited = []
  | otherwise = let nodes = M.findWithDefault [] current input
                in current : (concat $ map (\x -> programs input (current : visited) x) nodes)

main = do
  input <- readFile "input.txt"
  let result = groups (parseInput $ input)
  putStrLn . show $ head result
  putStrLn . show $ length result

