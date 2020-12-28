import           Control.Applicative     hiding ( many )
import           Data.Char
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

data TileEdges = TileEdges
  { top     :: [Char]
  , bottom  :: [Char]
  , left    :: [Char]
  , right   :: [Char]
  , topR    :: [Char]
  , bottomR :: [Char]
  , leftR   :: [Char]
  , rightR  :: [Char]
  }
  deriving Show

data TileBorderShares = TileBorderShares
  { tileIndex    :: Int
  , sharesTop    :: [Int]
  , sharesBottom :: [Int]
  , sharesLeft   :: [Int]
  , sharesRight  :: [Int]
  }
  deriving Show

mkTileEdges :: [[Char]] -> TileEdges
mkTileEdges arr = TileEdges { top     = head arr
                            , bottom  = last arr
                            , left    = getLeft
                            , right   = getRight
                            , topR    = reverse $ head arr
                            , bottomR = reverse $ last arr
                            , leftR   = reverse getLeft
                            , rightR  = reverse getRight
                            }
 where
  getLeft  = [ head (arr !! i) | i <- [0 .. dim - 1] ]
  getRight = [ (arr !! i) !! (dim - 1) | i <- [0 .. dim - 1] ]

mkMap :: [(Int, [[Char]])] -> Map Int TileEdges
mkMap [] = Map.empty
mkMap (x : xs) =
  let (key, arr) = x
      rest       = mkMap xs
  in  Map.insert key (mkTileEdges arr) rest

findMatch :: [Char] -> (Int, TileEdges) -> Maybe Int
findMatch xs (index, tile) =
  if xs
       `elem` [ top tile
              , bottom tile
              , left tile
              , right tile
              , topR tile
              , bottomR tile
              , leftR tile
              , rightR tile
              ]
    then Just index
    else Nothing

numBordersMatch :: Int -> Map Int TileEdges -> TileBorderShares
numBordersMatch x tileEdges =
  let tile           = fromJust $ Map.lookup x tileEdges
      otherTiles     = Map.assocs (Map.delete x tileEdges)
      topsMatches    = mapMaybe (findMatch (top tile)) otherTiles
      bottomsMatches = mapMaybe (findMatch (bottom tile)) otherTiles
      leftsMatches   = mapMaybe (findMatch (left tile)) otherTiles
      rightsMatches  = mapMaybe (findMatch (right tile)) otherTiles
  in  TileBorderShares { tileIndex    = x
                       , sharesTop    = topsMatches
                       , sharesBottom = bottomsMatches
                       , sharesLeft   = leftsMatches
                       , sharesRight  = rightsMatches
                       }

numEdgesNoShares :: TileBorderShares -> Int
numEdgesNoShares t = length
  (filter
    (== True)
    [ null (sharesTop t)
    , null (sharesBottom t)
    , null (sharesLeft t)
    , null (sharesRight t)
    ]
  )

findCorners :: [TileBorderShares] -> [TileBorderShares]
findCorners = filter (\t -> numEdgesNoShares t == 2)

solution :: String -> Int
solution fileContent =
  let tileMap      = mkMap $ parseFile fileContent
      borderShares = map (`numBordersMatch` tileMap) $ Map.keys tileMap
      corners      = findCorners borderShares
  in  product $ map tileIndex corners

main :: IO ()
main = do
  args     <- getArgs
  inHandle <- openFile (head args) ReadMode
  contents <- hGetContents inHandle
  let tileMap = mkMap $ parseFile contents
  print $ solution contents

