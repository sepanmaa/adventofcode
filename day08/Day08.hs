import qualified Data.Map as M

eq :: String -> (Int -> Int -> Bool)
eq s = case s of
         "<" -> (<)
         ">" -> (>)
         ">=" -> (>=)
         "<=" -> (<=)
         "!=" -> (/=)
         _ -> (==)

parseInput :: String -> (M.Map String Int, Int)
parseInput xs = foldl (\(m, maxVal) x -> parse (words x) m maxVal) (M.empty, 0) (lines xs)
  where parse (k1:op1:v1:_:k2:op2:v2:[]) m maxVal =
          if (eq op2) (M.findWithDefault 0 k2 m) (read v2 :: Int)
          then let x = M.findWithDefault 0 k1 m
                   f op = if op == "inc" then (+) else (-)
                   m' = M.insert k1 ((f op1) x (read v1 :: Int)) m
               in (m', max x maxVal)
          else (m, maxVal)                  

main = do
  input <- readFile "input.txt"
  let (p1, maxVal) = parseInput input
  putStrLn . show . maximum . M.elems $ p1
  putStrLn . show $ maxVal
                 
