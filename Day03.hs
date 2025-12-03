import Data.Char (digitToInt)
import Data.List (elemIndex, intercalate)
import Data.Maybe (fromJust)

testInput = "987654321111111\n811111111111119\n234234234234278\n818181911112111"

parse :: String -> [[Int]]
parse input = map (map digitToInt) (lines input)

-- part 1

turnOn battery = (maxJolt * 10) + nextMaxJolt
  where
    maxJolt = maximum (init battery)
    maxJoltPos = fromJust (elemIndex maxJolt battery)
    nextMaxJolt = maximum (drop (maxJoltPos + 1) battery)

solve input = sum $ map turnOn (parse input)

test = solve testInput == 357

-- part 2

join :: [Int] -> Int
join list = read (intercalate "" (map show list))

turnOn' :: Int -> [Int] -> Int
turnOn' _ [] = 0
turnOn' 0 battery = maximum battery
turnOn' 1 battery = maximum battery
turnOn' digitsLeft battery
  | length battery == digitsLeft = join battery
  | otherwise =
      (maxJolt * (10 ^ (digitsLeft - 1)))
        + turnOn' (digitsLeft - 1) (drop (maxJoltPos + 1) battery)
  where
    maxJolt = maximum (take (length battery - (digitsLeft - 1)) battery)
    maxJoltPos = fromJust (elemIndex maxJolt battery)

solve' input = sum $ map (turnOn' 12) (parse input)

test' = solve' testInput == 3121910778619

main = do
  input <- getContents
  print (solve input)
  print (solve' input)
