import           Control.Applicative     hiding ( many )
import           Data.Char
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
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
type Count = Int

extractFromLine :: String -> Map OuterColour [(InnerColour, Count)]
extractFromLine line = case parsed of
  [((outerColour, innerBags), "")] -> Map.singleton outerColour innerBags
  _ -> error $ "Parsing failed: " ++ line
  where parsed = readP_to_S parseLine line

parseFileContent :: String -> [Map OuterColour [(InnerColour, Count)]]
parseFileContent contents = map extractFromLine $ lines contents

formSingleMap :: Ord k => [Map k v] -> Map k v
formSingleMap xs =
  let singleMap = Map.unions xs
  in  if length xs == Map.size singleMap
        then singleMap
        else error "Parsed duplicate bags."

-- Solve the problem ----------------------------------------------------------

type Colour = String
type Multiple = Int
type Bag = (Colour, Multiple)

getCount :: Map OuterColour [(InnerColour, Count)] -> [Bag] -> Int
getCount coloursMap [] = 0
getCount coloursMap ((colour, multiple) : restOfBags) =
  case Map.lookup colour coloursMap of
    Nothing       -> multiple + getCount coloursMap restOfBags
    Just moreBags -> multiple * numNewBags + getCount
      coloursMap
      (restOfBags ++ map multiplier moreBags)
     where
      numNewBags = sum (map snd moreBags)
      multiplier =
        (\(colour, currentMultiple) -> (colour, currentMultiple * multiple))

main :: IO ()
main = do
  args     <- getArgs
  inHandle <- openFile (head args) ReadMode
  contents <- hGetContents inHandle
  let parsed = parseFileContent contents
  print $ getCount (formSingleMap parsed) [("shiny gold", 1)]
