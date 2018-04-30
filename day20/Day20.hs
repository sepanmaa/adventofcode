import Data.Maybe (catMaybes)
import Data.List (find, partition)

data Vector = V Int Int Int deriving (Show, Eq)

data Particle = P { pos :: Vector
                  , vel :: Vector
                  , acc :: Vector
                  } deriving Show

parseInput = catMaybes . map (parseParticle . parseLine) . lines

parseLine = let readInt x = read x :: Int
                filterNumbers = map (\x -> if x `elem` "pva=<>," then ' ' else x)
            in map readInt . words . filterNumbers
  
parseParticle :: [Int] -> Maybe Particle
parseParticle (x:y:z:x':y':z':x'':y'':z'':[]) =
  Just $ P (V x y z) (V x' y' z') (V x'' y'' z'')
parseParticle _ = Nothing

manhattan (P (V x y z) _ _) = abs x + abs y + abs z

simulate (P (V px py pz) (V vx vy vz) a@(V ax ay az)) =
  let (vx', vy', vz') = ((ax+vx), (ay+vy), (az+vz))
  in P (V (px+vx') (py+vy') (pz+vz')) (V vx' vy' vz') a

removeColliding [] = []
removeColliding (x:xs) =
  let (colliding, notColliding) = partition (\y -> pos x == pos y) xs
  in if length colliding > 0 
     then removeColliding notColliding
     else x : removeColliding xs

p1 input = let simulated = (iterate (map simulate) $ parseInput input) !! 500
           in snd . minimum $ zip (map manhattan simulated) [0..]

p2 input = length $ (iterate (map simulate . removeColliding) (parseInput input)) !! 100

main = do
  input <- readFile "input.txt"
  putStrLn . show $ p1 input
  putStrLn . show $ p2 input


