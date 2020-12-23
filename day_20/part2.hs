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
  let parseTile = (,) <$> parseId <*> parseArray
  in  sepBy parseTile (char '\n') <* char '\n' <* eof

parseFile :: String -> [(Int, [[Char]])]
parseFile fileContent = case readP_to_S parseTiles fileContent of
  [(extracted, "")] -> extracted
  _                 -> error "Parsing failed"

-------------------------------------------------------------------------------

type Row = Int
type Col = Int
type Index = (Row, Col)
type Tiles = Map Int (Array Index Char)

-- Lookup small arrays by id -------------------------------------------------- 

mkMap :: [(Int, [[Char]])] -> Map Int (Array Index Char)
mkMap [] = Map.empty
mkMap (x : xs) =
  let (key, arr) = x
      rest       = mkMap xs
  in  Map.insert key (listArray arrBounds (concat arr)) rest

mkTilesMap :: String -> Map Int (Array Index Char)
mkTilesMap fileContent = mkMap (parseFile fileContent)

mkArrs :: [(Int, [[Char]])] -> [Array Index Char]
mkArrs [] = []
mkArrs (x : xs) =
  let (key, arr) = x
      rest       = mkArrs xs
  in  listArray arrBounds (concat arr) : rest

mkTilesList :: String -> [Array Index Char]
mkTilesList fileContent = mkArrs $ parseFile fileContent

-- Small Arrays ---------------------------------------------------------------

arrBounds :: ((Row, Col), (Row, Col))
arrBounds = ((1, 1), (dim, dim))

mkArray :: [[Char]] -> Array Index Char
mkArray chars = listArray ((1, 1), (dim, dim)) (concat chars)

-- Transformations ------------------------------------------------------------

transform :: (Index -> Index) -> Array Index Char -> Array Index Char
transform indexFunc arr =
  let pairs = assocs arr
  in  array (bounds arr)
            (map (\(index, value) -> (indexFunc index, value)) pairs)

identity :: Array Index Char -> Array Index Char
identity = transform id

reflectH :: Array Index Char -> Array Index Char
reflectH arr =
  let dim = snd $ snd $ bounds arr
  in  transform (\(row, col) -> (dim - row + 1, col)) arr

reflectV :: Array Index Char -> Array Index Char
reflectV arr =
  let dim = snd $ snd $ bounds arr
  in  transform (\(row, col) -> (row, dim - col + 1)) arr

reflectD1 :: Array Index Char -> Array Index Char
reflectD1 arr =
  let dim = snd $ snd $ bounds arr in transform (\(row, col) -> (col, row)) arr

reflectD2 :: Array Index Char -> Array Index Char
reflectD2 arr =
  let dim = snd $ snd $ bounds arr
  in  transform (\(row, col) -> (dim - col + 1, dim - row + 1)) arr

rotate180 :: Array Index Char -> Array Index Char
rotate180 = reflectH . reflectV

rotate90 :: Array Index Char -> Array Index Char
rotate90 = reflectV . reflectD1

rotate270 :: Array Index Char -> Array Index Char
rotate270 = reflectV . reflectD2

-- Matches --------------------------------------------------------------------

matchTopToBottom :: Array Index Char -> Array Index Char -> Bool
matchTopToBottom arr1 arr2 =
  let top    = (filter (\((row, col), entry) -> row == 1) $ assocs arr1)
      bottom = (filter (\((row, col), entry) -> row == dim) $ assocs arr2)
  in  (map snd top) == (map snd bottom)

matchBottomToTop :: Array Index Char -> Array Index Char -> Bool
matchBottomToTop arr1 arr2 = matchTopToBottom arr2 arr1

matchLeftToRight :: Array Index Char -> Array Index Char -> Bool
matchLeftToRight arr1 arr2 =
  let left  = (filter (\((row, col), entry) -> col == 1) $ assocs arr1)
      right = (filter (\((row, col), entry) -> col == dim) $ assocs arr2)
  in  (map snd left) == (map snd right)

matchRightToLeft :: Array Index Char -> Array Index Char -> Bool
matchRightToLeft arr1 arr2 = matchLeftToRight arr2 arr1

type Tile = Array Index Char

findMatch :: (Tile -> Tile -> Bool) -> Tile -> [Tile] -> Maybe (Tile, Tile)
findMatch matcher tile [] = Nothing
findMatch matcher tile (t : ts)
  | matcher tile t             = Just (t, t)
  | matcher tile (reflectH t)  = Just (reflectH t, t)
  | matcher tile (reflectV t)  = Just (reflectV t, t)
  | matcher tile (reflectD1 t) = Just (reflectD1 t, t)
  | matcher tile (reflectD2 t) = Just (reflectD2 t, t)
  | matcher tile (rotate180 t) = Just (rotate180 t, t)
  | matcher tile (rotate90 t)  = Just (rotate90 t, t)
  | matcher tile (rotate270 t) = Just (rotate270 t, t)
  | otherwise                  = findMatch matcher tile ts

findLeftMatch :: Tile -> [Tile] -> Maybe (Tile, Tile)
findLeftMatch = findMatch matchLeftToRight

findRightMatch :: Tile -> [Tile] -> Maybe (Tile, Tile)
findRightMatch = findMatch matchRightToLeft

findTopMatch :: Tile -> [Tile] -> Maybe (Tile, Tile)
findTopMatch = findMatch matchTopToBottom

findBottomMatch :: Tile -> [Tile] -> Maybe (Tile, Tile)
findBottomMatch = findMatch matchBottomToTop

corner = 2593
getCorner :: String -> Tile
getCorner fileContent = fromJust $ Map.lookup corner (mkTilesMap fileContent)

syms =
  [ identity
  , reflectH
  , reflectV
  , reflectD1
  , reflectD2
  , rotate180
  , rotate90
  , rotate270
  ]
getSyms :: Tile -> [Tile]
getSyms tile = map (\sym -> sym tile) syms

orientationCorner :: [Tile] -> [Tile] -> Tile
orientationCorner []           tiles = error "fail"
orientationCorner (sym : syms) tiles = case findLeftMatch sym tiles of
  Just (_, _) -> orientationCorner syms tiles
  Nothing     -> case findTopMatch sym tiles of
    Just (_, _) -> orientationCorner syms tiles
    Nothing     -> sym

buildRow :: ([Tile], [Tile]) -> ([Tile], [Tile])
buildRow (row, rest) = if length row == 12
  then (row, rest)
  else case findLeftMatch (head row) rest of
    Just (match, original) -> buildRow (match : row, delete original rest)
    Nothing                -> case findRightMatch (last row) rest of
      Just (match, original) -> buildRow (row ++ [match], delete original rest)
      Nothing                -> (row, rest)

fill :: [Tile] -> [Tile] -> [Tile]
fill soFar []   = soFar
fill soFar rest = if length soFar `mod` 12 == 0
  then case findBottomMatch (soFar !! (length soFar - 12)) rest of
    Just (match, original) -> fill (soFar ++ [match]) (delete original rest)
    Nothing                -> error "fail 1"
  else case findRightMatch (last soFar) rest of
    Just (match, original) -> fill (soFar ++ [match]) (delete original rest)
    Nothing                -> error "fail 2"

-- Big array ------------------------------------------------------------------

mkBigArray :: Array Index Char
mkBigArray = listArray ((1, 1), (120, 120)) (cycle "x")

tileForBigArray :: Tile -> Int -> Int -> [(Index, Char)]
tileForBigArray tile bigRow bigCol =
  let associations = assocs tile
      mapped =
        map
          (\((row, col), e) -> ((row + (bigRow * 10), col + (bigCol * 10)), e))
  in  mapped associations

updateBigArray :: [Tile] -> Array Index Char -> Array Index Char
updateBigArray tiles bigArr =
  let zipped    = zip [0 ..] tiles
      newAssocs = map
        (\(i, tile) -> tileForBigArray tile (i `div` 12) (i `mod` 12))
        zipped
  in  bigArr // concat newAssocs

removeBorders :: Array Index Char -> Array Index Char
removeBorders bigArr =
  let
    associations = assocs bigArr
    exclude      = [1, 11 .. 120] ++ [10, 20 .. 120]
    filtered     = filter
      (\((row, col), e) -> row `notElem` exclude && col `notElem` exclude)
    remainingElements = map snd $ filtered associations
  in
    listArray ((1, 1), (96, 96)) remainingElements

type SeaMonster = [((Int, Int), Char)]
getSeaMonster :: SeaMonster
getSeaMonster =
  let seaMonster =
        ["                  # ", "#    ##    ##    ###", " #  #  #  #  #  #   "]
      rows         = length seaMonster
      cols         = length $ head seaMonster
      rowCols      = [ (row, col) | row <- [1 .. rows], col <- [1 .. cols] ]
      associations = zip rowCols $ concat seaMonster
  in  filter (\((row, col), e) -> e == '#') associations

lookForSeaMonster :: Index -> Array Index Char -> SeaMonster -> Bool
lookForSeaMonster (r, c) bigArr seaMonster =
  let indexMapper = \((row, col), e) -> ((row + r, col + c), e)
      lookup (i, e) = (checkBounds i bigArr) && (bigArr ! i == e)
  in  all lookup (map indexMapper seaMonster)

checkAllForSeaMonster :: Array Index Char -> SeaMonster -> Bool
checkAllForSeaMonster bigArr seaMonster =
  let ind = indices bigArr
  in  any (== True) (map (\i -> lookForSeaMonster i bigArr seaMonster) ind)

checkBounds :: Index -> Array Index Char -> Bool
checkBounds (r, c) arr =
  let ((minR, minC), (maxR, maxC)) = bounds arr
  in  r <= maxR && c <= maxC && r >= minR && c >= minC

getOrientation :: Array Index Char -> SeaMonster -> Array Index Char
getOrientation bigArr seaMonster =
  let orientations = map (\fn -> fn bigArr) syms in pick orientations
 where
  pick []       = error "failed"
  pick (x : xs) = if checkAllForSeaMonster x seaMonster then x else pick xs

monsterMask :: SeaMonster -> SeaMonster
monsterMask = map (\(i, e) -> (i, '.'))

countHashs :: Array Index Char -> Int
countHashs bigArr = length $ filter (== '#') $ elems bigArr

applyMonsterMask :: Array Index Char -> SeaMonster -> Array Index Char
applyMonsterMask bigArr seaMonster =
  let
    startIndices = checkAllForSeaMonster2 bigArr seaMonster
    indexMapper i = \((row, col), e) -> ((row + fst i, col + snd i), e)
    updates =
      map (\i -> (map (indexMapper i) (monsterMask seaMonster))) startIndices
  in
    bigArr // concat updates

lookForSeaMonster2 :: Index -> Array Index Char -> SeaMonster -> Maybe Index
lookForSeaMonster2 (r, c) bigArr seaMonster =
  let indexMapper = \((row, col), e) -> ((row + r, col + c), e)
      lookup (i, e) = (checkBounds i bigArr) && (bigArr ! i == e)
  in  if all lookup (map indexMapper seaMonster) then Just (r, c) else Nothing

checkAllForSeaMonster2 :: Array Index Char -> SeaMonster -> [Index]
checkAllForSeaMonster2 bigArr seaMonster =
  let ind = indices bigArr
  in  catMaybes (map (\i -> lookForSeaMonster2 i bigArr seaMonster) ind)

main :: IO ()
main = do
  args     <- getArgs
  inHandle <- openFile (head args) ReadMode
  contents <- hGetContents inHandle
  let tiles     = mkTilesList contents
  let c         = getCorner contents
  let oc        = orientationCorner (getSyms c) (delete c tiles)
  let puzzle    = fill [oc] (delete c tiles)
  let big       = updateBigArray puzzle mkBigArray
  let noBorders = removeBorders big
  let oriented  = getOrientation noBorders getSeaMonster
  --print $ checkAllForSeaMonster2 oriented getSeaMonster
  let updated   = applyMonsterMask oriented getSeaMonster
  print $ countHashs updated

