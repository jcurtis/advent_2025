import Data.List (maximumBy, tails)
import Data.List.Extra (sortBy, splitOn)

main = do
  input <- getContents
  print (solve input)
  print (solve' input)

testInput = "7,1\n11,1\n11,7\n9,7\n9,5\n2,5\n2,3\n7,3"

parse input = map parseLine (lines input)

parseLine :: String -> (Int, Int)
parseLine line = (read x, read y)
  where
    [x, y] = splitOn "," line

-- part 1

area (x1, y1) (x2, y2) = (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)

areas list = [area a b | (a : rest) <- tails list, b <- rest]

solve input = maximum (areas (parse input))

test = solve testInput == 50

-- part 2
-- calcs are non inclusive

areas' list = [(a, b, area a b) | (a : rest) <- tails list, b <- rest]

inArea (x1, y1) (x2, y2) (a, b) = between a x1 x2 && between b y1 y2

between a x y = (a > min x y) && (a < max x y)

intersect rect [a] = False
intersect (a, b) ((p1, q1) : (p2, q2) : polygon) =
  ( (q1 == q2 && between q1 y1 y2)
      || (p1 == p2 && between p1 x1 x2)
  )
    || ((a, b) `intersect` ((p2, q2) : polygon))
  where
    (x1, y1) = a
    (x2, y2) = b

validate [] _ = error "No rectangles left"
validate ((a, b, _) : rects) polygon
  -- check if any red tiles inside rectangle
  | any (inArea a b) polygon = validate rects polygon
  -- check if any lines of the polygon intersect the rect
  | (a, b) `intersect` polygon = validate rects polygon
  -- if neither previous checks are true the rect is valid
  | otherwise = (a, b)

sortedRects polygon =
  sortBy
    (\(_, _, areaA) (_, _, areaB) -> compare areaB areaA)
    (areas' polygon)

solve' input = ((a, b), area a b)
  where
    polygon = parse input
    rects = sortedRects polygon
    (a, b) = validate rects polygon

-- 53025 too low

onSegment (x, y) (xp, yp) (xq, yq) = (x == xp && x == xq) || (y == yp && y == yq)

pointInPolygon _ [p] = False
pointInPolygon (x, y) (p : q : polygon)
  | onSegment (x, y) p q = True
  | (yp > y) /= (yq > y) = x < ((((y - yp) * (xq - xp)) `div` (yq - yp)) + xp)
  | otherwise = False
  where
    (xp, yp) = p
    (xq, yq) = q
