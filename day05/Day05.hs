{-# LANGUAGE Strict #-}

import Data.Array
import qualified Data.Sequence as Seq

followJumps :: [Int] -> Int -> Int -> (Int -> Int) -> Int
followJumps xs p steps f =
  if p < 0 || p >= length xs
  then steps
  else let i = xs !! p
           xs' = (take p xs) ++ [(f i)] ++ (drop (p + 1) xs)
       in followJumps xs' (p + i) (steps + 1) f

followJumpsA :: Array Int Int -> Int -> Int -> (Int -> Int) -> Int
followJumpsA xs i steps f =
  if i < 0 || i >= length xs - 1
  then steps
  else let offset = xs ! i
           newVal = f offset
       in followJumpsA (xs // [(i, newVal)]) (i + offset) (steps + 1) f

followJumpsS xs i steps f =
  case Seq.lookup i xs of
    Nothing -> steps
    Just x -> let xs' = Seq.adjust f i xs
              in followJumpsS xs' (i + x) (steps + 1) f

main = do
  input <- readFile "input.txt"
  let xs = map (\x -> read x :: Int) $ lines input
--      arr = array (0, length xs) $ zip [0..(length xs)] xs
--      p1 = followJumpsA arr 0 0 (+1)
--      p2 = followJumpsA arr 0 0 (\x -> if x >= 3 then x - 1 else x + 1)
      p1 = followJumpsS (Seq.fromList xs) 0 0 succ
      p2 = followJumpsS (Seq.fromList xs) 0 0 (\x -> if x >= 3 then x - 1 else x + 1)
  putStrLn $ show p1
  putStrLn $ show p2

  
