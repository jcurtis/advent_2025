import Data.List (elemIndex)
import Data.Maybe (fromJust, mapMaybe)

testInput = ".......S.......\n...............\n.......^.......\n...............\n......^.^......\n...............\n.....^.^.^.....\n...............\n....^.^...^....\n...............\n...^.^...^.^...\n...............\n..^...^.....^..\n...............\n.^.^.^.^.^...^.\n..............."

parse input = (start, splitters)
  where
    start = (0, fromJust $ elemIndex 'S' input)
    splitters = concat $ zipWith parseLine [0 ..] (lines input)

parseLine y line =
  mapMaybe
    ( \(x, char) ->
        if char == '^' then Just (x, y) else Nothing
    )
    (zip [0 ..] line)

solve input = undefined
  where
    (start, splitters) = parse input
