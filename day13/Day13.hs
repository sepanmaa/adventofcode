data Layer = Layer { range :: Int
                   , depth :: Int
                   , pos :: Int}

parseInput :: String -> [Layer]
parseInput = map (parse . words) . lines
  where parse (x:y:[]) = Layer { range = read y :: Int
                               , depth = read (init x) :: Int
                               , pos = 0 }

trip :: [Layer] -> Int
trip = sum . map (severity . scan)
  where severity (Layer r d p) = if p == 0 then r * d else 0
        scan layer@(Layer r d p) = layer { pos = d `mod` ((r - 1) * 2)}

delayedTrip :: [Layer] -> Int
delayedTrip input =
  let delayInput delay = map (\x@(Layer _ d _) -> x { depth = d + delay }) input
  in succ . length . takeWhile (\x -> x > 0) . map trip $ map delayInput [1..]

main = do
  input <- readFile "input.txt"
  let parsedInput = parseInput input
  putStrLn . show $ trip parsedInput
  putStrLn . show $ delayedTrip parsedInput

