import Data.List (maximumBy, tails)
import Data.List.Extra (splitOn)

main = do
  input <- getContents
  print (solve input)

testInput = "7,1\n11,1\n11,7\n9,7\n9,5\n2,5\n2,3\n7,3"

parse input = map parseLine (lines input)

parseLine :: String -> (Int, Int)
parseLine line = (read x, read y)
  where
    [x, y] = splitOn "," line

area (x1, y1) (x2, y2) = (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)

areas list = [area a b | (a : rest) <- tails list, b <- rest]

solve input = maximum (areas (parse input))
