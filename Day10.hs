import Data.List.Extra (dropEnd, splitOn)
import Text.Regex.TDFA

testInput = "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}\n[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}\n[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"

type Lights = String

type Button = Int

type Buttons = [Button]

parse input = map parseLine (lines input)

wiringReg = "\\(([0-9,]+)\\)"

parseLine input = (lights, buttons)
  where
    lights = tail (takeWhile (/= ']') input)
    buttons = map parseWire (getAllTextMatches (input =~ wiringReg) :: [String])

parseWire :: Lights -> Buttons
parseWire input = map read (splitOn "," trimmed)
  where
    trimmed = dropEnd 1 (drop 1 input)

replace :: Int -> a -> [a] -> [a]
replace _ _ [] = []
replace 0 val list = val : drop 1 list
replace n val (x : xs) = x : replace (n - 1) val xs

toggle lights n
  | cur == '.' = replace n '#' lights
  | otherwise = replace n '.' lights
  where
    cur = lights !! n

press :: Lights -> Buttons -> Lights
press = foldl toggle
