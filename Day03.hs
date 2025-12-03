import Data.Char (digitToInt)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

testInput = "987654321111111\n811111111111119\n234234234234278\n818181911112111"

parse :: String -> [[Int]]
parse input = map (map digitToInt) (lines input)

turnOn battery = (maxJolt * 10) + nextMaxJolt
  where
    maxJolt = maximum (init battery)
    maxJoltPos = fromJust (elemIndex maxJolt battery)
    nextMaxJolt = maximum (drop (maxJoltPos + 1) battery)

solve input = sum $ map turnOn (parse input)

test = solve testInput == 357

main = do
  input <- getContents
  print (solve input)
