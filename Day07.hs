import Data.List (elemIndex, elemIndices)
import Data.List.Extra (nub)
import Data.Maybe (fromJust)
import Data.MemoTrie (memo2)

main = do
  input <- getContents
  print (solve input)
  print (solve' input)

testInput = ".......S.......\n...............\n.......^.......\n...............\n......^.^......\n...............\n.....^.^.^.....\n...............\n....^.^...^....\n...............\n...^.^...^.^...\n...............\n..^...^.....^..\n...............\n.^.^.^.^.^...^.\n..............."

parse input = (start, splitters)
  where
    start = fromJust $ elemIndex 'S' input
    splitters = map (elemIndices '^') (lines input)

-- part 1

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

-- part 2

teleport' beams [] = length beams
teleport' [] _ = 0
teleport' beams (splitterLine : splitters) =
  sum $
    map
      (splitBeam'Memo (splitterLine : splitters))
      beams

splitBeam' :: [[Int]] -> Int -> Int
splitBeam' (splitterLine : splitters) beam
  | beam `elem` splitterLine = teleport' [beam - 1, beam + 1] splitters
  | otherwise = teleport' [beam] splitters

splitBeam'Memo = memo2 splitBeam'

solve' input = teleport' [start] splitters
  where
    (start, splitters) = parse input

test' = solve' testInput == 40
