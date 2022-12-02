
import qualified Data.List as L
import qualified Data.List.Split as L

-- Parsing

parseInput = map (L.splitOn " ") . lines

-- Part 1

handPoints "X" = 1
handPoints "Y" = 2
handPoints "Z" = 3

winPoints "A" "Y" = 6
winPoints "B" "Z" = 6
winPoints "C" "X" = 6
winPoints "A" "X" = 3
winPoints "B" "Y" = 3
winPoints "C" "Z" = 3
winPoints _   _   = 0

stepPart1 ps [a,b] = ps + handPoints b + winPoints a b

-- Part 2

x2i "A" = 0
x2i "B" = 1
x2i "C" = 2

toBeat' a = (a + 1) `mod` 3
toDraw' a = a
toLose' a = (a - 1) `mod` 3

i2x 0 = "X"
i2x 1 = "Y"
i2x 2 = "Z"

toBeat = i2x . toBeat' . x2i
toDraw = i2x . toDraw' . x2i
toLose = i2x . toLose' . x2i

stepPart2 ps [a,"X"] = ps + 0 + handPoints (toLose a)
stepPart2 ps [a,"Y"] = ps + 3 + handPoints (toDraw a)
stepPart2 ps [a,"Z"] = ps + 6 + handPoints (toBeat a)

-- Main

main = do
  f <- parseInput <$> readFile "inputs/day02.txt"
  putStrLn $ "Part1: " ++ show (L.foldl' stepPart1 0 f)
  putStrLn $ "Part2: " ++ show (L.foldl' stepPart2 0 f)
