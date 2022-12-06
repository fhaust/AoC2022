import qualified Data.List as L
import qualified Data.List.Split as L

part l = fmap (+l) . L.findIndex (\x -> L.nub x == x) . L.divvy l 1

main = do
  f <- readFile "inputs/day06.txt"
  putStrLn $ "Part1: " ++ show (part 4 f)
  putStrLn $ "Part2: " ++ show (part 14 f)

