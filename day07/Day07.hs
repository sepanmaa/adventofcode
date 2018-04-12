import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.List (find, partition)
import Control.Monad (join)

data Tower = Tower String Int [Tower]

findRoot :: String -> String
findRoot s =
  let parseChildren s = case words s of
                          (_:_:_:xs) -> map (filter (/=',')) xs
                          _ -> []
      children = concat . map parseChildren $ lines s
  in head . filter (\x -> x `notElem` children) . map (head . words) $ lines s

buildTower :: String -> Tower
buildTower input = go (findRoot input)
  where parseWeight s = read (init $ tail s) :: Int
        programs = foldl (\m x -> M.insert (head x) x m) M.empty . map words . lines $ input
        go node = case M.lookup node programs of
                    Just (n:w:_:xs) -> Tower n (parseWeight w) (map go $ map (filter (/=',')) xs)
                    Just (n:w:_) -> Tower n (parseWeight w) []
                    Nothing -> Tower "" 0 []

towerWeight :: Tower -> Int
towerWeight (Tower name weight []) = weight
towerWeight (Tower name weight towers) = weight + (sum $ map towerWeight towers)

balanceTower :: Tower -> Maybe Int
balanceTower (Tower name weight []) = Nothing
balanceTower tower@(Tower _ w xs) =
  let ws = map (\x@(Tower _ weight _) -> (towerWeight x, weight)) xs
      diff = (fst $ maximum ws) - (fst $ minimum ws)
      (a, b) = partition ((==) (minimum ws)) ws
  in if diff /= 0
     then case join $ find isJust (map balanceTower xs) of
            Just x -> Just x
            Nothing -> Just $ if length a < length b
                              then (snd $ head b) - diff
                              else (snd $ head a) + diff
     else Nothing
    
main = do
  input <- readFile "input.txt"
  let root = findRoot input
  putStrLn . show $ root
  putStrLn . show . balanceTower $ buildTower input
