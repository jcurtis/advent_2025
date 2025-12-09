import Data.List (find, nub)
import Data.List.Extra (singleton, sortBy, splitOn, tails)
import Data.Maybe (fromJust)
import Data.Ord (Down (Down), comparing)

main = do
  input <- getContents
  print (solve input 1000)

testInput = "162,817,812\n57,618,57\n906,360,560\n592,479,940\n352,342,300\n466,668,158\n542,29,236\n431,825,988\n739,650,466\n52,470,668\n216,146,977\n819,987,18\n117,168,530\n805,96,715\n346,949,466\n970,615,88\n941,993,340\n862,61,35\n984,92,344\n425,690,689"

parse input = map parseLine (lines input)

type Pos = (Float, Float, Float)

type PosD = ((Pos, Pos), Float)

parseLine :: String -> Pos
parseLine line = (x, y, z)
  where
    [x, y, z] = map read (splitOn "," line)

distance :: (Pos, Pos) -> Float
distance ((a, b, c), (x, y, z)) =
  sqrt
    ( ((a - x) ^ 2)
        + ((b - y) ^ 2)
        + ((c - z) ^ 2)
    )

pairs :: [Pos] -> [PosD]
pairs list = [((a, b), distance (a, b)) | (a : rest) <- tails list, b <- rest]

sortPairs :: [PosD] -> [PosD]
sortPairs = sortBy (\(_, a) (_, b) -> compare a b)

mergeCircuits circuits juncA juncB =
  if circuitA == circuitB
    then
      Nothing
    else
      Just ((circuitA ++ circuitB) : without circuits circuitA circuitB)
  where
    circuitA = fromJust $ findCircuit juncA circuits
    circuitB = fromJust $ findCircuit juncB circuits

findCircuit junc = find (\circuit -> junc `elem` circuit)

without circuits a b = [x | x <- circuits, x /= a && x /= b]

makeConnections circuits _ 1 = circuits
makeConnections circuits ((a, b) : sorted) iteration =
  case mergeCircuits circuits a b of
    Just merged -> makeConnections merged sorted (iteration - 1)
    Nothing -> makeConnections circuits sorted iteration

solve input connections = product (take 3 (sortBy (comparing Data.Ord.Down) (map length endCircuits)))
  where
    posList = parse input
    circuits = map singleton posList
    sorted = map fst (sortPairs (pairs posList))
    endCircuits = makeConnections circuits sorted connections

test = solve testInput 10 == 40
