import           Control.Applicative     hiding ( many )
import           Data.Array
import           Data.Char
import           Data.List
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe
import           System.Environment
import           System.IO
import           Text.ParserCombinators.ReadP

type Row = Int
type Col = Int
type Index = (Row, Col)
type Tile = Array Index Char
type Tiles = Map Int Tile

-- Parsing --------------------------------------------------------------------

dim :: Int
dim = 10

parseInteger :: ReadP Int
parseInteger = read <$> many1 (satisfy isDigit)

parseId :: ReadP Int
parseId = string "Tile " *> parseInteger <* string ":\n"

parseRow :: ReadP [Char]
parseRow = count dim (char '#' <|> char '.') <* char '\n'

parseArray :: ReadP [[Char]]
parseArray = count dim parseRow

parseTiles :: ReadP [(Int, [[Char]])]
parseTiles =
  let parseTile = (,) <$> parseId <*> parseArray in sepBy parseTile (char '\n') <* char '\n' <* eof

parseFile :: String -> [(Int, [[Char]])]
parseFile fileContent = case readP_to_S parseTiles fileContent of
  [(extracted, "")] -> extracted
  _                 -> error "Parsing failed"

mkMap :: [(Int, [[Char]])] -> Tiles
mkMap [] = Map.empty
mkMap (x : xs) =
  let (key, arr) = x
      rest       = mkMap xs
  in  Map.insert key (listArray ((1, 1), (dim, dim)) (concat arr)) rest

-- Symmetries -----------------------------------------------------------------

transform :: (Index -> Index) -> Array Index Char -> Array Index Char
transform indexFunc arr =
  let pairs = assocs arr
  in  array (bounds arr) (map (\(index, value) -> (indexFunc index, value)) pairs)

identity :: Array Index Char -> Array Index Char
identity = transform id

reflectH :: Array Index Char -> Array Index Char
reflectH arr =
  let dim = snd $ snd $ bounds arr in transform (\(row, col) -> (dim - row + 1, col)) arr

reflectV :: Array Index Char -> Array Index Char
reflectV arr =
  let dim = snd $ snd $ bounds arr in transform (\(row, col) -> (row, dim - col + 1)) arr

reflectD1 :: Array Index Char -> Array Index Char
reflectD1 arr = let dim = snd $ snd $ bounds arr in transform (\(row, col) -> (col, row)) arr

reflectD2 :: Array Index Char -> Array Index Char
reflectD2 arr =
  let dim = snd $ snd $ bounds arr in transform (\(row, col) -> (dim - col + 1, dim - row + 1)) arr

rotate180 :: Array Index Char -> Array Index Char
rotate180 = reflectH . reflectV

rotate90 :: Array Index Char -> Array Index Char
rotate90 = reflectV . reflectD1

rotate270 :: Array Index Char -> Array Index Char
rotate270 = reflectV . reflectD2

getSymmetries :: Tile -> [Tile]
getSymmetries tile = map
  (\symmetry -> symmetry tile)
  [identity, reflectH, reflectV, reflectD1, reflectD2, rotate180, rotate90, rotate270]

-- Find matches ---------------------------------------------------------------

matchTopToBottom :: Tile -> Tile -> Bool
matchTopToBottom arr1 arr2 =
  let top    = (filter (\((row, col), entry) -> row == 1) $ assocs arr1)
      bottom = (filter (\((row, col), entry) -> row == dim) $ assocs arr2)
  in  map snd top == map snd bottom

matchBottomToTop :: Tile -> Tile -> Bool
matchBottomToTop arr1 arr2 = matchTopToBottom arr2 arr1

matchLeftToRight :: Tile -> Tile -> Bool
matchLeftToRight arr1 arr2 =
  let left  = (filter (\((row, col), entry) -> col == 1) $ assocs arr1)
      right = (filter (\((row, col), entry) -> col == dim) $ assocs arr2)
  in  map snd left == map snd right

matchRightToLeft :: Tile -> Tile -> Bool
matchRightToLeft arr1 arr2 = matchLeftToRight arr2 arr1

matchSingleTile :: (Tile -> Tile -> Bool) -> Tile -> Tile -> Maybe (Tile, Tile)
matchSingleTile matcher referenceTile tile =
  let symmetries = getSymmetries tile
  in  case filter (matcher referenceTile) symmetries of
        []  -> Nothing
        [t] -> Just (t, tile)
        _   -> error "Multiple matches found."

findMatch :: (Tile -> Tile -> Bool) -> Tile -> [Tile] -> Maybe (Tile, Tile)
findMatch matcher tile []       = Nothing
findMatch matcher tile (t : ts) = case matchSingleTile matcher tile t of
  Just (sym, tile) -> Just (sym, tile)
  Nothing          -> findMatch matcher tile ts

-- Assemble image -------------------------------------------------------------

orientateCorner :: [Tile] -> [Tile] -> Tile
orientateCorner []           tiles = error "Failed to orientate corner"
orientateCorner (sym : syms) tiles = case findMatch matchLeftToRight sym tiles of
  Just (_, _) -> orientateCorner syms tiles
  Nothing     -> case findMatch matchTopToBottom sym tiles of
    Just (_, _) -> orientateCorner syms tiles
    Nothing     -> sym

cornerIndex :: Int
cornerIndex = 2593

getCorner :: Tiles -> ([Tile], [Tile])
getCorner tiles =
  let cornerTile = tiles Map.! cornerIndex
      otherTiles = Map.elems (Map.delete cornerIndex tiles)
  in  ([orientateCorner (getSymmetries cornerTile) otherTiles], otherTiles)

fill :: [Tile] -> [Tile] -> [Tile]
fill soFar []   = soFar
fill soFar rest = if length soFar `mod` 12 == 0
  then case findMatch matchBottomToTop (soFar !! (length soFar - 12)) rest of
    Just (match, original) -> fill (soFar ++ [match]) (delete original rest)
  else case findMatch matchRightToLeft (last soFar) rest of
    Just (match, original) -> fill (soFar ++ [match]) (delete original rest)

-- Make image -----------------------------------------------------------------

mkImage :: Array Index Char
mkImage = listArray ((1, 1), (120, 120)) (cycle "x")

tileForImage :: Tile -> Int -> Int -> [(Index, Char)]
tileForImage tile bigRow bigCol =
  let associations = assocs tile
      mapped       = map (\((row, col), e) -> ((row + (bigRow * 10), col + (bigCol * 10)), e))
  in  mapped associations

updateImage :: [Tile] -> Array Index Char -> Array Index Char
updateImage tiles image =
  let zipped    = zip [0 ..] tiles
      newAssocs = map (\(i, tile) -> tileForImage tile (i `div` 12) (i `mod` 12)) zipped
  in  image // concat newAssocs

removeBorders :: Array Index Char -> Array Index Char
removeBorders image =
  let associations      = assocs image
      exclude           = [1, 11 .. 120] ++ [10, 20 .. 120]
      filtered = filter (\((row, col), e) -> row `notElem` exclude && col `notElem` exclude)
      remainingElements = map snd $ filtered associations
  in  listArray ((1, 1), (96, 96)) remainingElements

getImage :: Tiles -> Array Index Char
getImage tiles =
  let assembled = uncurry fill (getCorner tiles) in removeBorders $ updateImage assembled mkImage

-- Find sea monsters ----------------------------------------------------------

type SeaMonster = [((Int, Int), Char)]

getSeaMonster :: SeaMonster
getSeaMonster =
  let seaMonster   = ["                  # ", "#    ##    ##    ###", " #  #  #  #  #  #   "]
      rows         = length seaMonster
      cols         = length $ head seaMonster
      rowCols      = [ (row, col) | row <- [1 .. rows], col <- [1 .. cols] ]
      associations = zip rowCols $ concat seaMonster
  in  filter (\((row, col), e) -> e == '#') associations

checkBounds :: Index -> Array Index Char -> Bool
checkBounds (r, c) arr =
  let ((minR, minC), (maxR, maxC)) = bounds arr in r <= maxR && c <= maxC && r >= minR && c >= minC

lookForSeaMonster :: Index -> Array Index Char -> SeaMonster -> Maybe Index
lookForSeaMonster (r, c) image seaMonster =
  let indexMapper = \((row, col), e) -> ((row + r, col + c), e)
      lookup (i, e) = checkBounds i image && (image ! i == e)
  in  if all (lookup . indexMapper) seaMonster then Just (r, c) else Nothing

checkAllForSeaMonster :: Array Index Char -> SeaMonster -> [Index]
checkAllForSeaMonster image seaMonster =
  let ind = indices image in mapMaybe (\i -> lookForSeaMonster i image seaMonster) ind

getOrientation :: Array Index Char -> SeaMonster -> Array Index Char
getOrientation image seaMonster = let orientations = getSymmetries image in pick orientations
 where
  pick []       = error "failed"
  pick (x : xs) = if null $ checkAllForSeaMonster x seaMonster then pick xs else x

monsterMask :: SeaMonster -> SeaMonster
monsterMask = map (\(i, e) -> (i, '.'))

applyMonsterMask :: Array Index Char -> SeaMonster -> Array Index Char
applyMonsterMask image seaMonster =
  let startIndices = checkAllForSeaMonster image seaMonster
      indexMapper i = \((row, col), e) -> ((row + fst i, col + snd i), e)
      updates = map (\i -> map (indexMapper i) (monsterMask seaMonster)) startIndices
  in  image // concat updates

countHashs :: Array Index Char -> Int
countHashs image = length $ filter (== '#') $ elems image

main :: IO ()
main = do
  args     <- getArgs
  inHandle <- openFile (head args) ReadMode
  contents <- hGetContents inHandle
  let tiles         = mkMap $ parseFile contents
  let orientedImage = getOrientation (getImage tiles) getSeaMonster
  print $ countHashs (applyMonsterMask orientedImage getSeaMonster)

