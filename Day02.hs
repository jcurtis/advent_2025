import Data.List.Split (splitOn)

testInput = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

parse :: String -> [[Int]]
parse input = map parseRange (splitOn "," input)

parseRange :: String -> [Int]
parseRange input = [(read a) .. (read b)]
  where
    [a, b] = splitOn "-" input

invalidRange = filter invalid

invalid :: Int -> Bool
invalid input = a == b
  where
    inputStr = show input
    (a, b) = splitAt (length inputStr `div` 2) inputStr

solve input = sum $ concatMap invalidRange (parse input)

test = solve testInput == 1227775554

main = do
  input <- getContents
  print (solve input)
