import Data.List (elemIndex, elemIndices)
import Data.List.Extra (nub)
import Data.Maybe (fromJust)

main = do
  input <- getContents
  print (solve input)

testInput = ".......S.......\n...............\n.......^.......\n...............\n......^.^......\n...............\n.....^.^.^.....\n...............\n....^.^...^....\n...............\n...^.^...^.^...\n...............\n..^...^.....^..\n...............\n.^.^.^.^.^...^.\n..............."

parse input = (start, splitters)
  where
    start = fromJust $ elemIndex 'S' input
    splitters = map (elemIndices '^') (lines input)

solve input =
  snd $
    foldl
      ( \(beams, splits) splitterLine ->
          let (newBeams, newSplits) = teleport beams splitterLine
           in (newBeams, newSplits + splits)
      )
      ([start], 0)
      splitters
  where
    (start, splitters) = parse input

teleport beams [] = (beams, 0)
teleport [] _ = ([], 0)
teleport beams splitterLine = (nub $ concat splitMap, splits)
  where
    splitMap = map (splitBeam splitterLine) beams
    splits = length $ filter (\item -> length item > 1) splitMap

splitBeam splitterLine beam
  | beam `elem` splitterLine = [beam - 1, beam + 1]
  | otherwise = [beam]

test = solve testInput == 21
