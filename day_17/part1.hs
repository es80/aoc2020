import           Data.Array
import           Data.Maybe

type Size = Int
type Row = Int
type Col = Int
type Slice = Int
type Location = (Row, Col, Slice)
type Grid = Array Location Char

test = [".#.", "..#", "###"]
input =
  [ "##...#.#"
  , "####.#.#"
  , "#...####"
  , "..#.#.#."
  , "####.#.."
  , "#.#.#..#"
  , ".####.##"
  , "..#...##"
  ]

cycles :: Int
cycles = 6

maxSize :: Size -> Size
maxSize s = s + (2 * cycles)

getInputArray :: [[Char]] -> (Grid, Int)
getInputArray xs =
  let dim       = maximum (map length xs)
      bound     = div dim 2
      hBound    = if mod dim 2 == 1 then bound else bound - 1
      lowBound  = (,,) (-bound) (-bound) 0
      highBound = (,,) hBound hBound 0
  in  (listArray (lowBound, highBound) (concat xs), dim)

getStartingArray :: [[Char]] -> Grid
getStartingArray xs =
  let
    (inputArray, dim) = getInputArray xs
    bound             = div (maxSize dim) 2
    low               = (,,) (-bound) (-bound) (-bound)
    high              = (,,) bound bound bound
    blankArray = listArray (low, high) (replicate ((2 * bound + 1) ^ 3) '.')
  in
    blankArray // assocs inputArray

checkBounds :: Grid -> Location -> Bool
checkBounds arr (r, c, s) =
  let ((minR, minC, minS), (maxR, maxC, maxS)) = bounds arr
  in  r <= maxR && c <= maxC && s <= maxS && r >= minR && c >= minC && s >= minS

lookupChar :: Grid -> Location -> Maybe Char
lookupChar arr loc | checkBounds arr loc = Just $ arr ! loc
                   | otherwise           = Nothing

getAdjacents :: Grid -> Location -> [Char]
getAdjacents arr (r, c, s)
  | checkBounds arr (r, c, s) = mapMaybe
    (lookupChar arr)
    [ (r + i, c + j, s + k)
    | i <- [-1 .. 1]
    , j <- [-1 .. 1]
    , k <- [-1 .. 1]
    , not (i == 0 && j == 0 && k == 0)
    ]
  | otherwise = []

countHash :: [Char] -> Int
countHash s = length $ filter (== '#') s

changeFromActive :: Grid -> Location -> Char
changeFromActive arr loc =
  let hashes = countHash (getAdjacents arr loc)
  in  if hashes == 2 || hashes == 3 then '#' else '.'

changeFromInactive :: Grid -> Location -> Char
changeFromInactive arr loc =
  let hashes = countHash (getAdjacents arr loc)
  in  if hashes == 3 then '#' else '.'

action :: Char -> (Grid -> Location -> Char)
action '#' = changeFromActive
action '.' = changeFromInactive

singleCycle :: Grid -> Grid
singleCycle arr =
  let entries = assocs arr
      changed = map (\(location, char) -> (location, action char arr location))
  in  arr // changed entries

nCycles :: Int -> Grid -> Grid
nCycles 0 arr = arr
nCycles n arr = nCycles (n - 1) (singleCycle arr)

numActive :: Grid -> Int
numActive arr = length $ filter (== '#') $ elems arr

main :: IO ()
main = print $ numActive $ nCycles cycles $ getStartingArray input

