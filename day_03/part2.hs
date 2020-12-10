import           System.Environment
import           System.IO

type Bounds = (Int, Int)
type Slope = (Int, Int)
type Slopes = [Slope]
type Index = (Int, Int)
type Tally = Int

nextIndex :: Bounds -> Slope -> Index -> Maybe Index
nextIndex (bx, by) (sx, sy) (x, y)
  | x + sx >= bx = Nothing
  | otherwise    = Just (x + sx, (y + sy) `mod` by)

lookupChar :: Index -> [String] -> Int
lookupChar (i, j) lines | (lines !! i) !! j == '#' = 1
                        | otherwise                = 0

traversal :: Bounds -> Slope -> Index -> [String] -> Tally
traversal b s i lines = case nextIndex b s i of
  Nothing    -> 0
  Just index -> lookupChar index lines + traversal b s index lines

getBounds :: [String] -> Bounds
getBounds lines = (length lines, length $ head lines)

findMultiple :: Slopes -> [String] -> Int
findMultiple slopes lines =
  let mapFunc slope = traversal (getBounds lines) slope (0, 0) lines
  in  product $ map mapFunc slopes

slopes :: Slopes
slopes = [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]

main = do
  args     <- getArgs
  inHandle <- openFile (head args) ReadMode
  contents <- hGetContents inHandle
  let inLines = lines contents
  print $ findMultiple slopes inLines
