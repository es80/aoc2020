import           Control.Applicative     hiding ( many )
import           Data.Char
import           Data.List
import           Data.Maybe
import           System.Environment
import           System.IO
import           Text.ParserCombinators.ReadP

-- Parsing --------------------------------------------------------------------

parseInteger :: ReadP String
parseInteger = many1 $ satisfy isDigit

parseNumElement :: ReadP (Maybe Integer)
parseNumElement = parseInteger >>= (\num -> return $ Just (read num :: Integer))

parseX :: ReadP (Maybe Integer)
parseX = char 'x' >> return (Just 1)

parseElement :: ReadP (Maybe Integer)
parseElement = parseNumElement <|> parseX

parseElements :: ReadP [Maybe Integer]
parseElements = do
  nums <- sepBy parseElement (char ',')
  eof
  return nums

parseLine :: String -> [Maybe Integer]
parseLine line = case readP_to_S parseElements line of
  [(val, "")] -> val
  _           -> error $ "Failed to parse line: " ++ line

parseFile :: String -> [Integer]
parseFile fileContent =
  let pairLines = lines fileContent
  in  if length pairLines == 2
        then catMaybes $ parseLine $ last pairLines
        else error "Failed to parse file"

-- Solution -------------------------------------------------------------------

type Offset = Integer
type Modulus = Integer

findOffsets :: [Modulus] -> [(Offset, Modulus)]
findOffsets = zip [0 ..]

filterOnes :: [(Offset, Modulus)] -> [(Offset, Modulus)]
filterOnes = filter (\(offset, modulus) -> modulus /= 1)

sortModuli :: [(Offset, Modulus)] -> [(Offset, Modulus)]
sortModuli = sortBy (\(res1, mod1) (res2, mod2) -> compare mod2 mod1)

mkSieves :: [(Offset, Modulus)] -> [[Integer] -> [Integer]]
mkSieves = map (\(offset, modulus) -> filter (\x -> mod (x + offset) modulus == 0))

applySieves :: [[Integer] -> [Integer]] -> [Integer] -> [Integer]
applySieves [] nums = nums
applySieves (sieve : sieves) nums =
  let first : second : _ = sieve nums in applySieves sieves [first, second ..]

main :: IO ()
main = do
  args     <- getArgs
  inHandle <- openFile (head args) ReadMode
  contents <- hGetContents inHandle
  let offsetsAndModuli = sortModuli . filterOnes . findOffsets . parseFile
  print $ head $ applySieves (mkSieves $ offsetsAndModuli contents) [1 ..]

