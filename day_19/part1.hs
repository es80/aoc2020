import           Control.Applicative     hiding ( many )
import           Data.Char
import           Data.List
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           System.Environment
import           System.IO
import           Text.ParserCombinators.ReadP

-- Parsing Rules --------------------------------------------------------------

type Message = String
data Rule = Simple (Int, Char) | Combo (Int, [[Int]]) deriving Show

parseAB :: ReadP Char
parseAB = char 'a' <|> char 'b'

parseMessage :: ReadP String
parseMessage = many parseAB

parseInt :: ReadP Int
parseInt = read <$> (many1 $ satisfy isDigit)

parseInts :: ReadP [Int]
parseInts = sepBy parseInt (char ' ')

parseGroups :: ReadP [[Int]]
parseGroups = sepBy parseInts (string " | ")

parseSimpleRule :: ReadP Rule
parseSimpleRule = Simple <$> ((,) <$> (parseInt <* string ": \"") <*> (parseAB <* char '"'))

parseComplexRule :: ReadP Rule
parseComplexRule = Combo <$> ((,) <$> (parseInt <* string ": ") <*> parseGroups)

parseRules :: ReadP [Rule]
parseRules = sepBy (parseSimpleRule <|> parseComplexRule) (char '\n')

parseFile :: String -> ([Rule], [Message])
parseFile fileContent = case readP_to_S parser fileContent of 
                          [(extracted, "")] -> extracted
                          _ -> error "blah"

parser :: ReadP ([Rule],[Message])
parser = do
  rules <- parseRules
  many get
  char '\n'
  char '\n'
  messages <- many parseMessage
  eof
  return (rules,messages)


main :: IO ()
main = do
  args     <- getArgs
  inHandle <- openFile (head args) ReadMode
  contents <- hGetContents inHandle
  print $ readP_to_S parser contents
  --rules    <- (openFile "rules" ReadMode) >>= hGetContents
  --print $ length (filter (/= []) (counts rules contents))
  --print $ filter (/= []) (counts rules contents)


{--
parseLine :: String -> Maybe (Int, [[Int]])
parseLine s = case readP_to_S parseComplexRule s of
  [(ext, "")] -> Just ext
  otherwise   -> Nothing

parseLines :: String -> [(Int, [[Int]])]
parseLines s = mapMaybe parseLine $ lines s


parseFile :: ReadP ((Int, Char), (Int, Char))
parseFile = do
  skipMany parseComplexRule
  r1 <- parseSimpleRule
  skipMany parseComplexRule
  r2 <- parseSimpleRule
  skipMany parseComplexRule
  eof
  return (r1, r2)

parseRulesFile :: String -> ((Int, Char), (Int, Char))
parseRulesFile fileContent = case readP_to_S parseFile fileContent of
  [(extracted, "")] -> extracted
  _                 -> error "Parsing failed"

--}
-------------------------------------------------------------------------------
{--

data Tree a = LeafA | LeafB | Node [[Tree a]] deriving Show
type Rules = [(Int, [[Int]])]
type TreeMap a = Map Int (Tree a)

start :: Map Int (Tree a)
start = Map.insert 12 LeafA (Map.singleton 7 LeafB)

mkTree :: Rules -> TreeMap a -> (Rules, TreeMap a)
mkTree [] t = ([], t)
mkTree (x : xs) t =
  let (k, v) = x
  in  case lookups2 v t of
        Nothing -> mkTree (xs ++ [x]) t
        Just ls -> mkTree (xs) (Map.insert k (Node ls) t)

lookups :: [Int] -> TreeMap a -> Maybe [Tree a]
lookups xs tm = let lm = map (\x -> Map.lookup x tm) xs in sequence lm

lookups2 :: [[Int]] -> TreeMap a -> Maybe [[Tree a]]
lookups2 xs tm = let lm = map (\x -> lookups x tm) xs in sequence lm

getTree :: String -> Tree a
getTree s =
  let (r, t) = mkTree (parseLines s) start
  in  case r of
        [] -> fromJust (Map.lookup 0 t)
        _  -> error "fail"

mkParser :: Tree a -> ReadP String
mkParser LeafA    = string "a"
mkParser LeafB    = string "b"
mkParser (Node a) = let ps = map (map mkParser) a in list2ToP ps

listToP :: [ReadP String] -> ReadP String
listToP [x] = x
listToP (x : xs) =
  let p = listToP xs
  in  do
        s  <- x
        s2 <- p
        return (s ++ s2)

list2ToP :: [[ReadP String]] -> ReadP String
list2ToP x = choice (map listToP x)

getParser :: String -> ReadP String
getParser s =
  let m = mkParser (getTree s)
  in  do
        s <- m
        eof
        return s

counts :: String -> String -> [[(String, String)]]
counts r s = let l = lines s in map (readP_to_S (getParser r)) l

--}


