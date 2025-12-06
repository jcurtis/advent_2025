import Data.Char (isNumber, isSpace)
import Data.List (transpose)
import Data.List.Split (splitWhen)

main = do
  input <- readFile "./input.txt"
  print (solve input)
  print (solve' input)

-- print (solve' input)

testInput = "123 328  51 64 \n45 64  387 23 \n6 98  215 314\n*   +   *   + "

op "*" = product
op "+" = sum

-- part 1

parse input = transpose (map words (lines input))

solveProblem line = op (last line) (map read (init line))

solve input = sum $ map solveProblem (parse input)

test = solve testInput == 4277556

-- part 2

rotl :: [[x]] -> [[x]]
rotl = transpose . map reverse

parse' input = splitWhen (all isSpace) (rotl inputs)
  where
    inputs = lines input

solveLine :: [String] -> Int
solveLine line = operation nums
  where
    nums = map readNum line
    operation = if '*' `elem` concat line then product else sum

readNum :: String -> Int
readNum str = read (filter isNumber str)

solve' input = sum $ map solveLine (parse' input)

test' = solve' testInput == 3263827
