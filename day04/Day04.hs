import Data.List (sort)

type Validation = String -> String -> Bool

validate :: Validation -> [String] -> Bool
validate _ [] = True
validate f (x:xs) = not (any (f x) xs) && validate f xs

validatePassphrases :: Validation -> String -> Int
validatePassphrases f = length . filter id . map (validate f) . map words . lines

anagram x y = sort x == sort y

main = do
  input <- readFile "input.txt"
  putStrLn . show $ validatePassphrases (==) input -- part 1
  putStrLn . show $ validatePassphrases anagram input -- part 2
