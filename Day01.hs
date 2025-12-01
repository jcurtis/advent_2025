testInput = "L68\nL30\nR48\nL5\nR60\nL55\nL1\nL99\nR14\nL82"

parse input = map parseLine (lines input)

parseLine :: String -> Integer
parseLine ('L' : val) = negate (read val)
parseLine ('R' : val) = read val

-- part 1

solve input = foldl rotater (50, 0) (parse input)

rotate start val = (start + val) `mod` 100

rotater (start, count) val = (nextStart, if nextStart == 0 then count + 1 else count)
  where
    nextStart = rotate start val

test = solve testInput == (32, 3)

-- part 2

solve' input = foldl rotater' (50, 0) (parse input)

rotater' (start, count) val = (rotate start val, count + crossings start val)

crossings start 0 = 0
crossings 0 val = abs val `div` 100
crossings start val
  | val > 0 = val `div` 100 + if (start + (val `mod` 100)) >= 100 then 1 else 0
  | otherwise = abs val `div` 100 + if (start - abs val `mod` 100) <= 0 then 1 else 0

test' = solve' testInput == (32, 6)

main = do
  input <- getContents
  print (solve input)
  print (solve' input)
