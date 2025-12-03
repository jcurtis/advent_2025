import Data.List.Split (splitOn)

testInput = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

-- parse :: String -> [[Integer]]
parse input = map parseRange (splitOn "," input)

parseRange :: String -> [Integer]
parseRange input = [(read a) .. (read b)]
  where
    [a, b] = splitOn "-" input

invalid input = a == b
  where
    (a, b) = splitAt (length input `div` 2) input

-- solve input = (filter invalid (parse input))
--
-- test = solve testInput == 1227775554
