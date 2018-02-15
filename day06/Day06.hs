import qualified Data.Sequence as S

type Memory = S.Seq Int

redistribute :: S.Seq Int -> Int -> Int -> S.Seq Int
redistribute xs i 0 = xs
redistribute xs i n =
  let i' = i `mod` length xs
  in redistribute (S.adjust (+1) i' xs) (i' + 1) (n - 1)

balance :: S.Seq Int -> S.Seq Int
balance xs =
  let m = maximum xs
  in case S.elemIndexL m xs of
       Nothing -> xs
       Just i -> redistribute (S.update i 0 xs) (i + 1) m  

reallocate :: [S.Seq Int] -> S.Seq Int -> [S.Seq Int]
reallocate configs blocks =
  if blocks `elem` configs
  then blocks : configs
  else reallocate (blocks : configs) (balance blocks)

redistCycles :: [S.Seq Int] -> Int
redistCycles = pred . length

loopCycles :: [S.Seq Int] -> Int
loopCycles (x:xs) = succ . length $ takeWhile (/=x) xs

main = do
  input <- readFile "input.txt"
  let readInt x = read x :: Int
      memory = reallocate [] . S.fromList . map readInt $ words input
      p1 = redistCycles memory
      p2 = loopCycles memory
  putStrLn . show $ p1
  putStrLn . show $ p2
  
