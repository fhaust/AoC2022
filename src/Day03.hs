
import qualified Data.List as L

prio x | x `elem` ['a'..'z'] = fromEnum x - fromEnum 'a' + 1
       | x `elem` ['A'..'Z'] = fromEnum x - fromEnum 'A' + 27

part1 = sum . map (prio . head . uncurry L.intersect . (\x -> splitAt (length x `div` 2) x))

by3 f [] = []
by3 f (a:b:c:xs) = f a b c : by3 f xs

part2 = sum . map (prio . head) . by3 (\a b c -> a `L.intersect` b `L.intersect` c)

main = do
  f <- lines <$> readFile "inputs/day03.txt"
  putStrLn $ "Part1: " ++ show (part1 f)
  putStrLn $ "Part2: " ++ show (part2 f)
