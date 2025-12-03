import Data.List.Split (chunksOf, splitOn)

testInput = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

parse :: String -> [[Int]]
parse input = map parseRange (splitOn "," input)

parseRange :: String -> [Int]
parseRange input = [(read a) .. (read b)]
  where
    [a, b] = splitOn "-" input

-- part 1

invalid :: Int -> Bool
invalid input = a == b
  where
    inputStr = show input
    (a, b) = splitAt (length inputStr `div` 2) inputStr

solve input = sum $ concatMap (filter invalid) (parse input)

test = solve testInput == 1227775554

-- part 2

invalid' :: Int -> Bool
invalid' input = invalidChunks (show input) 1

invalidChunks input chunkSize
  | chunkSize > (length input `div` 2) = False
  | allEqual chunks = True
  | otherwise = invalidChunks input (chunkSize + 1)
  where
    chunks = chunksOf chunkSize input

allEqual [] = True
allEqual (x : xs) = all (== x) xs

solve' input = sum $ concatMap (filter invalid') (parse input)

test' = solve' testInput == 4174379265

main = do
  input <- getContents
  print (solve input)
  print (solve' input)
