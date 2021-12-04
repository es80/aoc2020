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
parseSimpleRule = Simple <$> ((,) <$> (parseInt <* string ": \"") <*> (parseAB <* char '"'))

parseComplexRule :: ReadP Rule
parseComplexRule = Combo <$> ((,) <$> (parseInt <* string ": ") <*> parseGroups)

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

-- Construct A Tree -----------------------------------------------------------

data Tree a = LeafA | LeafB | Node [[Tree a]] deriving Show
type TreeMap a = Map Int (Tree a)

mkTreeMap :: [Rule] -> TreeMap a -> TreeMap a
mkTreeMap []       treeMap = treeMap
mkTreeMap (r : rs) treeMap = case r of
  Simple (num, 'a'  ) -> mkTreeMap rs (Map.insert num LeafA treeMap)
  Simple (num, 'b'  ) -> mkTreeMap rs (Map.insert num LeafB treeMap)
  Combo  (num, numss) -> case treeMapLookups2 numss treeMap of
    Nothing    -> mkTreeMap (rs ++ [r]) treeMap
    Just trees -> mkTreeMap rs (Map.insert num (Node trees) treeMap)

treeMapLookups :: [Int] -> TreeMap a -> Maybe [Tree a]
treeMapLookups xs treeMap = let listMaybes = map (`Map.lookup` treeMap) xs in sequence listMaybes
-- sequence :: t (m a) -> m (t a)
-- e.g. given a list of maybes produces a maybe list (just list if all members
-- justs, nothing if any member is a nothing

treeMapLookups2 :: [[Int]] -> TreeMap a -> Maybe [[Tree a]]
treeMapLookups2 xss treeMap =
  let listMaybes = map (`treeMapLookups` treeMap) xss in sequence listMaybes

-- Make A Parser --------------------------------------------------------------

mkParser :: Tree a -> ReadP String
mkParser LeafA    = string "a"
mkParser LeafB    = string "b"
mkParser (Node a) = let ps = map (map mkParser) a in combinerOr ps

combinerOr :: [[ReadP String]] -> ReadP String
combinerOr ps = choice (map combinerAnd ps)
-- succeeds if one of the parsers in the list succeeds
-- choice takes a list of parsers [p1, p2, p3 ..]
-- and produces a parser p1 <|> p2 <|> p3 ...
-- equivalent to foldr (<|>) pfail

combinerAnd :: [ReadP String] -> ReadP String
combinerAnd [x     ] = x
combinerAnd (x : xs) = (++) <$> x <*> combinerAnd xs
-- succeeds if all parsers in the list succeed in sequence

parserRule8 :: TreeMap a -> ReadP String
parserRule8 treeMap = concat <$> many1 (mkParser $ treeMap Map.! 42)

parserRule11 :: TreeMap a -> ReadP String
parserRule11 treeMap = do
  a <- many1 (mkParser $ treeMap Map.! 42)
  b <- count (length a) (mkParser $ treeMap Map.! 31)
  return (concat a ++ concat b)

getParser :: [Rule] -> ReadP String
getParser rs =
  let treeMap = mkTreeMap rs Map.empty in (++) <$> parserRule8 treeMap <*> parserRule11 treeMap

counts :: [Rule] -> [Message] -> Int
counts rules messages =
  let parser = getParser rules <* eof
      parsed message = case readP_to_S parser message of
        [(success, "")] -> 1
        _               -> 0
  in  sum (map parsed messages)

-------------------------------------------------------------------------------

main :: IO ()
main = do
  args     <- getArgs
  inHandle <- openFile (head args) ReadMode
  contents <- hGetContents inHandle
  let (r, m) = extract contents
  print $ uncurry counts (extract contents)

