import Data.List.Extra (find, splitOn, (\\))
import Data.Maybe (isJust)

testInput = "3-5\n10-14\n16-20\n12-18\n\n1\n5\n8\n11\n17\n32\n"

parse :: String -> ([(Int, Int)], [Int])
parse input = (map parseRange (lines ranges), map read (lines ingredients))
  where
    [ranges, ingredients] = splitOn "\n\n" input

parseRange input = (read a, read b)
  where
    [a, b] = splitOn "-" input

solve input = length $ filter (isFresh freshRanges) ingredients
  where
    (freshRanges, ingredients) = parse input

isFresh :: [(Int, Int)] -> Int -> Bool
isFresh freshRanges ingr = isJust $ find (\(a, b) -> ingr >= a && ingr <= b) freshRanges

test = solve testInput == 3

main = do
  input <- getContents
  print (solve input)
