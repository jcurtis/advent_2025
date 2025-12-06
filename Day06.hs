import Data.List (transpose)

main = do
  input <- getContents
  print (solve input)

testInput = "123 328  51 64 \n45 64  387 23 \n6 98  215 314\n*   +   *   + "

op "*" = product
op "+" = sum

parse input = transpose (map words (lines input))

solveProblem line = op (last line) (map read (init line))

solve input = sum $ map solveProblem (parse input)

test = solve testInput == 4277556
