import           Data.Array
import           Data.Maybe
import           System.Environment
import           System.IO

type Row = Int
type Col = Int
type Location = (Row, Col)
type Grid = Array (Int, Int) Char

getBounds :: [[Char]] -> ((Int, Int), (Int, Int))
getBounds rows =
  let colLength = maximum $ map length rows
      rowLength = length rows
  in  ((1, 1), (rowLength, colLength))

mk2DArray :: [[Char]] -> Grid
mk2DArray rows = let bounds = getBounds rows in listArray bounds $ concat rows

checkBounds :: Grid -> Location -> Bool
checkBounds arr (r, c) =
  let ((minR, minC), (maxR, maxC)) = bounds arr in r <= maxR && c <= maxC && r >= minR && c >= minC

lookupChar :: Grid -> Location -> Maybe Char
lookupChar arr (r, c) | checkBounds arr (r, c) = Just $ arr ! (r, c)
                      | otherwise              = Nothing

isEmptySeat :: Grid -> Location -> Bool
isEmptySeat arr (r, c) = lookupChar arr (r, c) == Just 'L'

isOccupiedSeat :: Grid -> Location -> Bool
isOccupiedSeat arr (r, c) = lookupChar arr (r, c) == Just '#'

getAdjacents :: Grid -> Location -> [Char]
getAdjacents arr (r, c)
  | checkBounds arr (r, c) = mapMaybe
    (lookupChar arr)
    [ (r - 1, c)
    , (r + 1, c)
    , (r    , c - 1)
    , (r    , c + 1)
    , (r - 1, c - 1)
    , (r - 1, c + 1)
    , (r + 1, c - 1)
    , (r + 1, c + 1)
    ]
  | otherwise = []

canOccupy :: Grid -> Location -> Bool
canOccupy arr (r, c) = isEmptySeat arr (r, c) && not (null adj) && notElem '#' adj
  where adj = getAdjacents arr (r, c)

canEmpty :: Grid -> Location -> Bool
canEmpty arr (r, c) =
  let adj = getAdjacents arr (r, c)
  in  (lookupChar arr (r, c) == Just '#') && length (filter (== '#') adj) >= 4

seatsToOccupy :: Grid -> [Location] -> [(Location, Char)]
seatsToOccupy arr [] = []
seatsToOccupy arr (idx : idxs) =
  let rest = seatsToOccupy arr idxs in if canOccupy arr idx then (idx, '#') : rest else rest

seatsToEmpty :: Grid -> [Location] -> [(Location, Char)]
seatsToEmpty arr [] = []
seatsToEmpty arr (idx : idxs) =
  let rest = seatsToEmpty arr idxs in if canEmpty arr idx then (idx, 'L') : rest else rest

applyRules :: Grid -> (Bool, Grid)
applyRules arr =
  let allIndices = indices arr
      occupy     = seatsToOccupy arr allIndices
      newArr     = arr // occupy
      empty      = seatsToEmpty newArr allIndices
      newArr2    = newArr // empty
  in  (not (null occupy && null empty), newArr2)

applyAllRules :: Grid -> Grid
applyAllRules arr =
  let (changed, nextArr) = applyRules arr in if changed then applyAllRules nextArr else arr

countSeated :: Grid -> Int
countSeated arr =
  let ix    = indices arr
      chars = map (lookupChar arr) ix
  in  length (filter (== Just '#') chars)

main :: IO ()
main = do
  args     <- getArgs
  inHandle <- openFile (head args) ReadMode
  contents <- hGetContents inHandle
  print $ countSeated $ applyAllRules $ mk2DArray $ lines contents

