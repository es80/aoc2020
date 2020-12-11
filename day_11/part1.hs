import           Data.Array
import           Data.Maybe
import           System.Environment
import           System.IO

type Row = Int
type Col = Int
type CharMatrix = Array (Int, Int) Char

getBounds :: [[Char]] -> ((Int, Int), (Int, Int))
getBounds rows =
  let colLength = maximum $ map length rows
      rowLength = length rows
  in  ((1, 1), (rowLength, colLength))

mk2DArray :: [[Char]] -> CharMatrix
mk2DArray rows = let bounds = getBounds rows in listArray bounds $ concat rows

checkBounds :: CharMatrix -> (Row, Col) -> Bool
checkBounds arr (r, c) =
  let ((minR, minC), (maxR, maxC)) = bounds arr
  in  r <= maxR && c <= maxC && r >= minR && c >= minC

lookupChar :: CharMatrix -> (Row, Col) -> Maybe Char
lookupChar arr (r, c) | checkBounds arr (r, c) = Just $ arr ! (r, c)
                      | otherwise              = Nothing

getAdjacents :: CharMatrix -> (Row, Col) -> [Char]
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

canOccupy :: CharMatrix -> (Row, Col) -> Bool
canOccupy arr (r, c) =
  let adj = getAdjacents arr (r, c)
  in  (lookupChar arr (r, c) == Just 'L') && not (null adj) && notElem '#' adj

occupies :: CharMatrix -> [(Row, Col)] -> [((Row, Col), Char)]
occupies arr [] = []
occupies arr (idx : idxs) =
  let rest = occupies arr idxs
  in  if canOccupy arr idx then (idx, '#') : rest else rest

becomesEmpty :: CharMatrix -> (Row, Col) -> Bool
becomesEmpty arr (r, c) =
  let adj = getAdjacents arr (r, c)
  in  (lookupChar arr (r, c) == Just '#') && length (filter (== '#') adj) >= 4

empties :: CharMatrix -> [(Row, Col)] -> [((Row, Col), Char)]
empties arr [] = []
empties arr (idx : idxs) =
  let rest = empties arr idxs
  in  if becomesEmpty arr idx then (idx, 'L') : rest else rest

applyRules :: CharMatrix -> (Bool, CharMatrix)
applyRules arr =
  let ix     = indices arr
      occ    = occupies arr ix
      emp    = empties arr ix
      newArr = (arr // occ) // emp
  in  (not (null occ && null emp), newArr)

applyAllRules :: CharMatrix -> CharMatrix
applyAllRules arr =
  let (changed, nextArr) = applyRules arr
  in  if changed then applyAllRules nextArr else arr

countSeats :: CharMatrix -> Int
countSeats arr =
  let ix    = indices arr
      chars = map (lookupChar arr) ix
  in  length (filter (== Just '#') chars)

main :: IO ()
main = do
  args     <- getArgs
  inHandle <- openFile (head args) ReadMode
  contents <- hGetContents inHandle
  print $ countSeats $ applyAllRules $ mk2DArray $ lines contents

