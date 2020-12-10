import           Control.Applicative     hiding ( many )
import           Data.Char
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           System.Environment
import           System.IO
import           Text.ParserCombinators.ReadP

-- Parsing --------------------------------------------------------------------

parseSpace :: ReadP Char
parseSpace = satisfy isSpace

parseLetter :: ReadP Char
parseLetter = satisfy isLetter

parseColour :: ReadP String
parseColour = many1 $ parseLetter <|> parseSpace

parseSuffix :: ReadP String
parseSuffix = string " bags contain no other bags."

parseContain :: ReadP String
parseContain = string " bags contain "

parseInt :: ReadP String
parseInt = many1 $ satisfy isDigit

parseBagContents :: ReadP (String, Int)
parseBagContents = do
  num <- parseInt
  parseSpace
  colour <- parseColour
  parseSpace
  string "bag." <|> string "bags." <|> string "bags, " <|> string "bag, "
  return (colour, read num :: Int)

parseLine :: ReadP (String, [(String, Int)])
parseLine = do
  colour <- parseColour
  parseContain <|> parseSuffix
  contentBags <- many parseBagContents
  eof
  return (colour, contentBags)

-- Parse the file content into a data structure -------------------------------

type InnerColour = String
type OuterColour = String

extract :: String -> Map InnerColour [OuterColour]
extract line = case parsed of
  [((outerColour, innerBagsList), "")] ->
    Map.unions $ map (constructMaps outerColour) innerBagsList
  _ -> error $ "Parsing failed: " ++ line
 where
  parsed = readP_to_S parseLine line
  constructMaps outerColour =
    \(innerColour, count) -> Map.singleton innerColour [outerColour]

parseFileContent :: String -> Map InnerColour [OuterColour]
parseFileContent contents = Map.unionsWith (++) (map extract $ lines contents)

-- Solve the problem ----------------------------------------------------------

countOuter :: Map InnerColour [OuterColour] -> [OuterColour] -> Set String
countOuter coloursMap [] = Set.empty
countOuter coloursMap (x : xs) =
  let outerColours = Map.lookup x coloursMap
  in  case outerColours of
        Nothing -> countOuter coloursMap xs
        Just ys ->
          Set.union (countOuter coloursMap (xs ++ ys)) $ Set.fromList ys

main :: IO ()
main = do
  args     <- getArgs
  inHandle <- openFile (head args) ReadMode
  contents <- hGetContents inHandle
  let parsed = parseFileContent contents
  print $ Set.size $ countOuter parsed ["shiny gold"]
