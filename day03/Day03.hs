import qualified Data.Map as M

-- Part 1

spiralMemory :: Int -> Int
spiralMemory n =
  let layers = takeWhile (\(x, y) -> (y < n)) [(x, (x*x)) | x <- [1,3..]]
      (layerNum,size) = last layers
      midDist = length layers * 2
      mids = take 4 $ iterate (+midDist) $ size + (layerNum + 2) `div` 2
  in length layers + minimum (map (\x -> (abs $ x - n)) mids)


-- Part 2

type Square = (Int, Int)
type Dir = Square

type Grid = M.Map Square Int

data Result = Result { square :: Square, value :: Int }

sumNeighbors :: M.Map Square Int -> Square -> Int
sumNeighbors grid (x, y) =
  let neighbors = [(x, y) | x <- [-1..1], y <- [-1..1], (x,y) /= (0,0)]
      ns = map (\(i, j) -> (x+i, y+j)) neighbors
  in sum $ map (\x -> M.findWithDefault 0 x grid) ns

next :: Grid -> Square -> Dir -> (Square, Dir)
next grid square@(x, y) dir@(dx, dy) =
  let forward = (x+dx, y+dy)
      newDir@(dx', dy') = case dir of
                            (-1, 0) -> (0, -1)
                            (0, -1) -> (1,  0)
                            (1,  0) -> (0,  1)
                            _ -> (-1, 0)                            
      square' = (x+dx', y+dy')
  in if M.member square' grid then (forward, dir) else (square', newDir)

spiral :: Int -> (Grid -> Square -> Int) -> Result
spiral n f = go (M.fromList [((0,0), 1),((1,0),1)]) (1,0) (1, 0) 1
  where go grid square dir val =
          let (square', dir') = next grid square dir
              val' = f grid square'
              grid' = M.insert square' val' grid
          in if val' >= n then Result square val' else go grid' square' dir' val'

distance :: Square -> Int
distance (x, y) = (abs x) + (abs y)

p1 n = square $ spiral n (\grid _ -> M.size grid)

p2 n = value $ spiral n (\grid square -> sumNeighbors grid square) -- part 2

main = do
  file <- readFile "input.txt"
  let input = read file :: Int
  putStrLn . show $ spiralMemory input -- fast
  putStrLn . show $ distance (p1 input) -- slow
  putStrLn . show $ p2 input

  
