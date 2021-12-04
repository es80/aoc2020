import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           System.Environment
import           System.IO

data Direction = East | West | NorthEast | NorthWest | SouthEast | SouthWest
  deriving Show

parseLine :: String -> [Direction]
parseLine [] = []
parseLine (x : xs) | x == 'e' = East : rest
                   | x == 'w' = West : rest
                   | null xs  = error $ "unable to parse line at " ++ [x] ++ " : " ++ xs
  where rest = parseLine xs
parseLine (x : y : ys) | x == 'n' && y == 'e' = NorthEast : rest
                       | x == 'n' && y == 'w' = NorthWest : rest
                       | x == 's' && y == 'e' = SouthEast : rest
                       | x == 's' && y == 'w' = SouthWest : rest
                       | otherwise = error $ "unable to parse line at " ++ [x, y] ++ " : " ++ ys
  where rest = parseLine ys

type Location = (Int, Int)
type BlackTiles = Set Location

moveToAdjacentLocation :: Location -> Direction -> Location
moveToAdjacentLocation (i, j) direction = case direction of
  East      -> (i + 2, j)
  West      -> (i - 2, j)
  NorthEast -> (i + 1, j + 1)
  NorthWest -> (i - 1, j + 1)
  SouthEast -> (i + 1, j - 1)
  SouthWest -> (i - 1, j - 1)

colourTile :: [Direction] -> Location -> BlackTiles -> BlackTiles
colourTile [] location tiles =
  if Set.member location tiles then Set.delete location tiles else Set.insert location tiles
colourTile (d : ds) location tiles = colourTile ds (moveToAdjacentLocation location d) tiles

colourAllTiles :: [[Direction]] -> BlackTiles
colourAllTiles = foldr (\d -> colourTile d (0, 0)) Set.empty

main :: IO ()
main = do
  args     <- getArgs
  inHandle <- openFile (head args) ReadMode
  contents <- hGetContents inHandle
  let dirs = map parseLine (lines contents)
  print $ Set.size (colourAllTiles dirs)

