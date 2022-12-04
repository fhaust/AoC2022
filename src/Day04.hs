import qualified Data.List.Split as L
import           Numeric.Interval (Interval, (...))
import qualified Numeric.Interval as I

parseInput :: String -> [[Interval Integer]]
parseInput = map (map ((\[a,b] -> read a ... read b) . L.splitOn "-") . L.splitOn ",") . lines

part1 = sum . map (fromEnum . (\[a,b] -> a `I.contains` b || b `I.contains` a))
part2 = sum . map (fromEnum . not . I.null . (\[a,b] -> I.intersection a b))

main = do
  f <- parseInput <$> readFile "inputs/day04.txt"
  putStrLn $ "Part 1: " ++ show (part1 f)
  putStrLn $ "Part 2: " ++ show (part2 f)
