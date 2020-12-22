import           Control.Applicative     hiding ( many )
import           Data.Array
import           Data.Char
import Data.List
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
  in  array arrBounds (map (\(index, value) -> (indexFunc index, value)) pairs)

identity :: Array Index Char -> Array Index Char
identity = transform id

reflectH :: Array Index Char -> Array Index Char
reflectH = transform (\(row, col) -> (dim - row + 1, col))

reflectV :: Array Index Char -> Array Index Char
reflectV = transform (\(row, col) -> (row, dim - col + 1))

reflectD1 :: Array Index Char -> Array Index Char
reflectD1 = transform (\(row, col) -> (col, row))

reflectD2 :: Array Index Char -> Array Index Char
reflectD2 = transform (\(row, col) -> (dim - col + 1, dim - row + 1))

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

---------

data Orientation = Identity | Rotate90 | Rotate180 | Rotate270 | ReflectH | ReflectV | ReflectD1 | ReflectD2

type Tile = Array Index Char

findMatch :: (Tile -> Tile -> Bool) -> Tile -> [Tile] -> Maybe (Tile, Tile)
findMatch matcher tile [] = Nothing
findMatch matcher tile (t:ts)
  | matcher tile t = Just (t, t)
  | matcher tile (reflectH t) = Just (reflectH t, t)
  | matcher tile (reflectV t)  = Just (reflectV t, t)
  | matcher tile (reflectD1 t)  = Just (reflectD1 t, t)
  | matcher tile (reflectD2 t)  = Just (reflectD2 t, t)
  | matcher tile (rotate180 t)  = Just (rotate180 t, t)
  | matcher tile (rotate90 t)  = Just (rotate90 t, t)
  | matcher tile (rotate270 t)  = Just (rotate270 t, t)
  | otherwise = findMatch matcher tile ts

findLeftMatch :: Tile -> [Tile] -> Maybe (Tile, Tile)
findLeftMatch = findMatch matchLeftToRight   

findRightMatch :: Tile -> [Tile] -> Maybe (Tile, Tile)
findRightMatch = findMatch matchRightToLeft   

findTopMatch :: Tile -> [Tile] -> Maybe (Tile, Tile)
findTopMatch = findMatch matchTopToBottom   

findBottomMatch :: Tile -> [Tile] -> Maybe (Tile, Tile)
findBottomMatch = findMatch matchBottomToTop   

  {--
startNextRow :: ([Tile] -> [Tile])-> ([Tile], [Tile])
startNextRow currentRow rest =
  case findBottomMatch (head currentRow) rest of
    Just (match, original) -> startNextRow 
--}

  {--
buildRows :: [Tile] -> [Tile -> Tile] -> [[Tile]] -> [[Tile]]
buildRows [] orientations rows = rows
buildRows (t:ts) [] rows = buildRows (ts ++ [t]) syms rows
buildRows (t:ts) orientations rows = 
  let (row, rest) = buildRow ([t], ts)
      len = length row
   in if len == 12 then buildRows rest orientations (row : rows) else buildRows ((head orientations t):ts) (tail orientations) rows 
--}

corner = 2593
getCorner :: String -> Tile
getCorner fileContent = fromJust $ Map.lookup corner (mkTilesMap fileContent)

syms = [identity, reflectH, reflectV, reflectD1, reflectD2, rotate180, rotate90, rotate270] 
getSyms :: Tile -> [Tile]
getSyms tile = map (\sym -> sym tile) syms

orientationCorner :: [Tile] -> [Tile] -> Tile
orientationCorner [] tiles = error "fail"
orientationCorner (sym:syms) tiles = 
   case findLeftMatch sym tiles of
     Just (_,_) -> orientationCorner syms tiles
     Nothing -> case findTopMatch sym tiles of
                  Just (_,_) -> orientationCorner syms tiles
                  Nothing -> sym


buildRow :: ([Tile],[Tile]) -> ([Tile], [Tile])
buildRow (row, rest) = 
  if length row == 12 then (row, rest) else
  case findLeftMatch (head row) rest of
      Just (match, original) -> buildRow (match : row, delete original rest) 
      Nothing -> case findRightMatch (last row) rest of
                   Just (match, original) -> buildRow (row ++ [match], delete original rest)
                   Nothing -> (row, rest)


fill :: [Tile] -> [Tile] -> [Tile]
fill soFar [] = soFar
fill soFar rest = 
  if length soFar `mod` 12 == 0 
     then 
       case findBottomMatch (soFar !! (length soFar - 12)) rest of
         Just (match, original) -> fill (soFar ++ [match]) (delete original rest)
         Nothing -> error "fail 1"
     else
       case findRightMatch (last soFar) rest of
         Just (match, original) -> fill (soFar ++ [match]) (delete original rest)
         Nothing -> error "fail 2"




-- Big array ------------------------------------------------------------------

mkBigArray :: Array Index Char
mkBigArray = listArray ((1, 1), (96, 96)) (cycle "x")

tileForBigArray :: Tile -> Int -> Int -> [(Index,Char)]
tileForBigArray tile bigRow bigCol = 
  let associations = assocs tile
      filtered = filter (\((row,col),e) -> row >= 2 && row <= 9 && col >=2 && col <= 9)
      mapped = map (\((row,col),e) -> (((row - 1) + (bigRow * 8), (col -1) + (bigCol * 8)),e))
   in (mapped . filtered) associations

updateBigArray :: [Tile] -> Array Index Char -> Array Index Char
updateBigArray tiles bigArr =
  let zipped = zip [1..] tiles
      newAssocs = map (\(i, tile) -> tileForBigArray tile (i `div` 12) (if (i `mod` 12) == 0 then 11 else (i `mod` 12) - 1 )) zipped

   in bigArr \\ zipped









main :: IO ()
main = do
  args     <- getArgs
  inHandle <- openFile (head args) ReadMode
  contents <- hGetContents inHandle
  --mapM_ print $ mkTilesList contents
  let tiles = mkTilesList contents
  --let row = buildRow ([head tiles], tail tiles)
  --mapM_ print $ buildRow ([head tiles], tail tiles)
  let c = getCorner contents
  print $ c
  let oc = orientationCorner (getSyms c) (delete c tiles)
  print $ fill [oc] (delete c tiles)




