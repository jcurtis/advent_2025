import Data.List.Extra (find, foldl', sortBy, splitOn)
import Data.Maybe (isJust)
import Data.Ord (comparing)

main = do
  input <- getContents
  print (solve input)
  print (solve' input)

-- print (solve' input)

testInput = "3-5\n10-14\n16-20\n12-18\n\n1\n5\n8\n11\n17\n32\n"

parse :: String -> ([(Int, Int)], [Int])
parse input = (map parseRange (lines ranges), map read (lines ingredients))
  where
    [ranges, ingredients] = splitOn "\n\n" input

parseRange input = (read a, read b)
  where
    [a, b] = splitOn "-" input

-- part 1

solve input = length $ filter (isFresh freshRanges) ingredients
  where
    (freshRanges, ingredients) = parse input

isFresh :: [(Int, Int)] -> Int -> Bool
isFresh freshRanges ingr = isJust $ find (\(a, b) -> ingr >= a && ingr <= b) freshRanges

test = solve testInput == 3

-- part 2

solve' input = sum $ map rangeLength (mergeRanges ranges)
  where
    (ranges, _) = parse input

mergeRanges [] = []
mergeRanges ranges = foldl' merge [] sorted
  where
    sorted = sortBy (comparing fst) ranges
    merge [] range = [range]
    merge acc@((x, y) : rest) (a, b) = case mergeMaybe (a, b) (x, y) of
      Just result -> result : rest
      Nothing -> (a, b) : acc

mergeMaybe (a, b) (x, y)
  | y < a = Nothing
  | x > b = Nothing
  | otherwise = Just (min a x, max b y)

rangeLength (a, b) = b - a + 1

test' = solve' testInput == 14
