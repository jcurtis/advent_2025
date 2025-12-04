import Data.Maybe (mapMaybe)

testInput = "..@@.@@@@.\n@@@.@.@.@@\n@@@@@.@.@@\n@.@@@@..@.\n@@.@@@@.@@\n.@@@@@@@.@\n.@.@.@.@@@\n@.@@@.@@@@\n.@@@@@@@@.\n@.@.@@@.@."

parse input = concat $ zipWith parseLine [0 ..] (lines input)

parseLine y line =
  mapMaybe
    ( \(x, char) ->
        if char == '@' then Just (x, y) else Nothing
    )
    (zip [0 ..] line)

canAccess (x, y) grid = length (filter (`elem` grid) checks) < 4
  where
    checks = neighbours (x, y)

neighbours (x, y) =
  [ (x + a, y + b)
    | a <- [-1 .. 1],
      b <- [-1 .. 1],
      (a, b) /= (0, 0)
  ]

solve input = length $ filter (\(x, y) -> canAccess (x, y) grid) grid
  where
    grid = parse input

test = solve testInput == 13

main = do
  input <- getContents
  print (solve input)
