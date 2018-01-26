import Data.Char (digitToInt)

-- Part 1
inverseCaptcha :: String -> Int
inverseCaptcha (x:xs) =
  foldl (\s (x, y) -> if x == y then s + digitToInt x else s) 0 $ zip (x:xs) (xs ++ [x])

inverseCaptcha' :: String -> Int
inverseCaptcha' s@(x:xs) =
  foldl (+) 0 . map (digitToInt . fst) . filter (uncurry (==)) . zip s $ (xs ++ [x])

-- Part 2
inverseCaptcha2 :: String -> Int
inverseCaptcha2 s = matchDigits s $ length s `div` 2

matchDigits :: String -> Int -> Int
matchDigits s n =
  let numPairs = zip s $ rotateString s n
  in  foldl (\s (x, y) -> if x == y then s + digitToInt x else s) 0 numPairs

rotateString :: String -> Int -> String
rotateString s n = take (length s) . drop n $ cycle s
