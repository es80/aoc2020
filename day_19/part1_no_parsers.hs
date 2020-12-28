import           Control.Applicative     hiding ( many )
import           Data.Char
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           System.Environment
import           System.IO
import           Text.ParserCombinators.ReadP

-- Parse Input File -----------------------------------------------------------

type Message = String
data Rule = Simple (Int, Char) | Combo (Int, [[Int]]) deriving Show

parseAB :: ReadP Char
parseAB = char 'a' <|> char 'b'

parseInt :: ReadP Int
parseInt = read <$> many1 (satisfy isDigit)

parseInts :: ReadP [Int]
parseInts = sepBy parseInt (char ' ')

parseGroups :: ReadP [[Int]]
parseGroups = sepBy parseInts (string " | ")

parseSimpleRule :: ReadP Rule
parseSimpleRule =
  Simple <$> ((,) <$> (parseInt <* string ": \"") <*> (parseAB <* char '"'))

parseComplexRule :: ReadP Rule
parseComplexRule =
  Combo <$> ((,) <$> (parseInt <* string ": ") <*> parseGroups)

parseMessage :: ReadP String
parseMessage = many parseAB

parseFile :: ReadP ([Rule], [Message])
parseFile = do
  rules <- sepBy (parseSimpleRule <|> parseComplexRule) (char '\n')
  char '\n'
  char '\n'
  messages <- sepBy parseMessage (char '\n')
  char '\n'
  eof
  return (rules, messages)

extract :: String -> ([Rule], [Message])
extract fileContent = case readP_to_S parseFile fileContent of
  [(extracted, "")] -> extracted
  _                 -> error "Parsing failed"

-- Find all valid messages ----------------------------------------------------

type Valid = Map Int [String]

mkValid :: [Rule] -> Valid -> Valid
mkValid []       valid = valid
mkValid (r : rs) valid = case r of
  Simple (num, 'a'  ) -> mkValid rs (Map.insert num ["a"] valid)
  Simple (num, 'b'  ) -> mkValid rs (Map.insert num ["b"] valid)
  Combo  (num, numss) -> case validLookups2 numss valid of
    Nothing -> mkValid (rs ++ [r]) valid
    Just ms -> mkValid rs (Map.insert num ms valid)

validLookups :: [Int] -> Valid -> Maybe [String]
validLookups xs valid =
  let listMaybes = map (`Map.lookup` valid) xs
  in  case sequence listMaybes of
        Nothing   -> Nothing
        Just strs -> Just (map concat (sequence strs))

validLookups2 :: [[Int]] -> Valid -> Maybe [String]
validLookups2 xss valid =
  let listMaybes = map (`validLookups` valid) xss
  in  case sequence listMaybes of
        Nothing   -> Nothing
        Just strs -> Just (foldl1 (++) strs)

counts :: [Rule] -> [Message] -> Int
counts rules messages =
  let valid = mkValid rules Map.empty Map.! 0
  in  length $ filter (`elem` valid) messages

-------------------------------------------------------------------------------

main :: IO ()
main = do
  args     <- getArgs
  inHandle <- openFile (head args) ReadMode
  contents <- hGetContents inHandle
  let (r, m) = extract contents
  print $ uncurry counts (extract contents)

