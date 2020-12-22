import           Data.Maybe
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set

type Location = (Int, Int, Int)
type Grid = Map Location Char

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

rows :: [[Char]] -> [[(Int, Char)]]
rows = map (zip [0 ..])

cols :: [[(Int, Char)]] -> [(Int, [(Int, Char)])]
cols = zip [0 ..]

rowToKeyValues :: (Int, [(Int, Char)]) -> [(Location, Char)]
rowToKeyValues (a, xs) = map (\(i, c) -> ((a, i, 0), c)) xs

mkStartMap :: [[Char]] -> Grid
mkStartMap input =
  let indexRows = rows input
      indexCols = cols indexRows
      keyValues = concatMap rowToKeyValues indexCols
  in  Map.fromList (filter (\(k, v) -> v == '#') keyValues)

adjacents :: Location -> [Location]
adjacents (r, c, s) =
  [ (r + i, c + j, s + k)
  | i <- [-1 .. 1]
  , j <- [-1 .. 1]
  , k <- [-1 .. 1]
  , not (i == 0 && j == 0 && k == 0)
  ]

getAdjacentHashes :: Grid -> Location -> Int
getAdjacentHashes grid loc =
  length (mapMaybe (`Map.lookup` grid) (adjacents loc))

locationsToInactive :: Grid -> [Location]
locationsToInactive grid =
  let keys = Map.keys grid
      numHashes loc = getAdjacentHashes grid loc
  in  filter (\l -> numHashes l < 2 || numHashes l > 3) keys

locationsToActive :: Grid -> [Location]
locationsToActive grid =
  let keys              = Map.keys grid
      inactiveLocations = Set.toList (getInactiveKeys keys)
      currentlyInactive = filter (`Map.notMember` grid) inactiveLocations
      numHashes loc = getAdjacentHashes grid loc
  in  filter (\l -> numHashes l == 3) currentlyInactive

getInactiveKeys :: [Location] -> Set Location
getInactiveKeys = foldr (Set.union . Set.fromList . adjacents) Set.empty

singleCycle :: Grid -> Grid
singleCycle grid =
  let deactivate = locationsToInactive grid
      activate   = locationsToActive grid
      deleted    = foldr Map.delete grid deactivate
      added      = foldr (`Map.insert` '#') deleted activate
  in  added

nCycles :: Int -> Grid -> Grid
nCycles 0 grid = grid
nCycles n grid = nCycles (n - 1) (singleCycle grid)

numActive :: Grid -> Int
numActive grid = length (Map.keys grid)

cycles :: Int
cycles = 6

main :: IO ()
main = print $ numActive $ nCycles cycles $ mkStartMap input

