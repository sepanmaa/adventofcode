readGarbage :: String -> Int -> (String, Int)
readGarbage [] n = ("", n)
readGarbage (x:xs) n =
  case x of
    '!' -> readGarbage (drop 1 xs) n
    '>' -> (xs, n)
    _ -> readGarbage xs (n+1)

readStream :: String -> Int -> Int -> Int -> (Int, Int)
readStream [] _ score g = (score, g)
readStream (x:xs) i score g =
  case x of
    '{' -> readStream xs (i+1) (score+i) g
    '}' -> readStream xs (i-1) score g
    '<' -> let (xs', garbage) = readGarbage xs 0
           in readStream xs' i score (g+garbage)
    _ -> readStream xs i score g

main = do
  input <- readFile "input.txt"
  putStrLn . show $ readStream input 1 0 0
