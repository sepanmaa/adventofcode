import qualified Data.Set as S
import Prelude hiding (Up, Down, Left, Right)

data Dir = Up | Down | Left | Right deriving (Show, Eq)

data Node = Clean | Weakened | Infected | Flagged

data State = State { nodes :: S.Set (Int, Int)
                   , pos :: (Int, Int)
                   , dir :: Dir
                   , infections :: Int
                   } deriving Show

parseNodes input len =
  let xs = zip input [ (x, y) | x <- [-len..len], y <- [-len..len] ]
  in map (\(_,(x,y)) -> (y, x)) $ filter (\x -> fst x == '#') xs

parseInput :: String -> State
parseInput input =
  let xs = concat $ lines input
      len = (length $ lines input)
  in State { nodes = S.fromList $ parseNodes xs (len `div` 2)
           , pos = (0, 0)
           , dir = Up
           , infections = 0 }
                              
turn s = let turnLeft = case dir s of
                          Up -> Left
                          Left -> Down
                          Down -> Right
                          Right -> Up
             turnRight = case dir s of
                           Up -> Right
                           Right -> Down
                           Down -> Left
                           Left -> Up
         in s { dir = if S.member (pos s) (nodes s)
                      then turnRight
                      else turnLeft }

infect s = let infected = S.member (pos s) (nodes s)
               s' = s { nodes = if infected
                                then S.delete (pos s) (nodes s)
                                else S.insert (pos s) (nodes s) }
           in s' { infections = if S.member (pos s) (nodes s)
                                then infections s
                                else infections s + 1 }

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
  putStrLn . show . infections $ virus s 10000
