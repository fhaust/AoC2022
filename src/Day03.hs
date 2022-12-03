
import qualified Data.List as L
import qualified Data.List.Split as L

prio x = let x' = fromEnum x in if x' > 96 then x' - 96 else x' - 38

part1 = sum . map (prio . head . uncurry L.intersect . (\x -> splitAt (length x `div` 2) x))
part2 = sum . map (prio . head . foldl1 L.intersect) . L.chunksOf 3

main = do
  f <- lines <$> readFile "inputs/day03.txt"
  putStrLn $ "Part1: " ++ show (part1 f)
  putStrLn $ "Part2: " ++ show (part2 f)
