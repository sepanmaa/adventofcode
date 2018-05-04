{-# LANGUAGE Strict #-}

import qualified Data.Map as M
import Prelude hiding (Up, Down, Left, Right)

data Dir = Up | Right | Down | Left deriving (Eq, Enum, Bounded)

data NodeStatus = Clean | Weakened | Infected | Flagged deriving (Eq, Enum, Bounded)

data State = State { nodes :: M.Map (Int, Int) NodeStatus
                   , pos :: (Int, Int)
                   , dir :: Dir
                   , infections :: Int
                   }

parseNodes input =
  let len = (length $ lines input) `div` 2
      xs = zip (concat $ lines input) [ (x, y) | x <- [-len..len], y <- [-len..len] ]
  in map (\(_,(x,y)) -> ((y, x), Infected)) $ filter (\x -> fst x == '#') xs

parseInput input = State { nodes = M.fromList $ parseNodes input
                         , pos = (0, 0)
                         , dir = Up
                         , infections = 0 }

succ' :: (Eq a, Enum a, Bounded a) => a -> a
succ' x | x == maxBound = minBound
        | otherwise = succ x

pred' :: (Eq a, Enum a, Bounded a) => a -> a
pred' x | x == minBound = maxBound
        | otherwise = pred x

nodeStatus (State ns pos _ _) =
  case M.lookup pos ns of
    Just x -> x
    Nothing -> Clean

turn s = s { dir = case nodeStatus s of
                     Clean -> pred' $ dir s
                     Weakened -> dir s
                     Infected -> succ' $ dir s
                     Flagged -> succ' . succ' $ dir s }

infect s = s { infections = if succ' $ nodeStatus s == Infected
                            then infections s + 1
                            else infections s
             , nodes = M.insert (pos s) (succ' $ nodeStatus s) (nodes s) }


forward s = let (x, y) = pos s
            in s { pos = case dir s of
                           Up -> (x, y-1)
                           Down -> (x, y+1)
                           Left -> (x-1, y)
                           Right -> (x+1, y) }

virus :: State -> Int -> State
virus state limit = burst state 0
  where burst s i = if i < limit
                    then burst (forward . infect . turn $ s) (i + 1)
                    else s

main = do
  input <- readFile "input.txt"
  let s = parseInput input
  putStrLn . show . infections $ virus s 10000000
