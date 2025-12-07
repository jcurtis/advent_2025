import Data.List (elemIndex)
import Data.Maybe (fromJust, mapMaybe)

testInput = ".......S.......\n...............\n.......^.......\n...............\n......^.^......\n...............\n.....^.^.^.....\n...............\n....^.^...^....\n...............\n...^.^...^.^...\n...............\n..^...^.....^..\n...............\n.^.^.^.^.^...^.\n..............."

parse input = (start, splitters, levels)
  where
    start = (fromJust $ elemIndex 'S' input, 0)
    splitters = concat $ zipWith parseLine [0 ..] (lines input)
    levels = length (lines input)

parseLine y line =
  mapMaybe
    ( \(x, char) ->
        if char == '^' then Just (x, y) else Nothing
    )
    (zip [0 ..] line)

solve input = nextBeams start splitters
  where
    (start, splitters, levels) = parse input

nextBeams (x, y) splitters
  | (x, y + 1) `elem` splitters = [(x - 1, y + 1), (x + 1, y + 1)]
  | otherwise = [(x, y + 1)]
