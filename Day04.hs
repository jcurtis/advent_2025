import Data.List ((\\))
import Data.Maybe (mapMaybe)

testInput = "..@@.@@@@.\n@@@.@.@.@@\n@@@@@.@.@@\n@.@@@@..@.\n@@.@@@@.@@\n.@@@@@@@.@\n.@.@.@.@@@\n@.@@@.@@@@\n.@@@@@@@@.\n@.@.@@@.@."

parse input = concat $ zipWith parseLine [0 ..] (lines input)

parseLine y line =
  mapMaybe
    ( \(x, char) ->
        if char == '@' then Just (x, y) else Nothing
    )
    (zip [0 ..] line)

canAccess grid (x, y) = length (filter (`elem` grid) checks) < 4
  where
    checks = neighbours (x, y)

neighbours (x, y) =
  [ (x + a, y + b)
    | a <- [-1 .. 1],
      b <- [-1 .. 1],
      (a, b) /= (0, 0)
  ]

-- part 1

solve input = length $ filter (canAccess grid) grid
  where
    grid = parse input

test = solve testInput == 13

-- part 2

removeRolls :: [(Int, Int)] -> Int
removeRolls grid
  | toRemoveCount == 0 = 0
  | otherwise = toRemoveCount + removeRolls cleared
  where
    toRemove = filter (canAccess grid) grid
    toRemoveCount = length toRemove
    cleared = grid \\ toRemove

solve' input = removeRolls (parse input)

test' = solve' testInput == 43

main = do
  input <- getContents
  print (solve input)
  print (solve' input)
