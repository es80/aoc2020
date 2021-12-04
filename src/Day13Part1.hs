import           Control.Applicative     hiding ( many )
import           Data.Char
import           Data.Maybe
import           System.Environment
import           System.IO
import           Text.ParserCombinators.ReadP

parseInteger :: ReadP String
parseInteger = many1 $ satisfy isDigit

parseNumElement :: ReadP (Maybe Integer)
parseNumElement = parseInteger >>= (\num -> return $ Just (read num :: Integer))

parseX :: ReadP (Maybe Integer)
parseX = char 'x' >> return Nothing

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

parseFile :: String -> (Integer, [Integer])
parseFile fileContent =
  let pairLines = lines fileContent
  in  if length pairLines == 2
        then (read $ head pairLines :: Integer, catMaybes $ parseLine $ last pairLines)
        else error "Failed to parse file"

waitTimesById :: Integer -> [Integer] -> [(Integer, Integer)]
waitTimesById timeStamp = map (\id -> (id, id - timeStamp `mod` id))

getEarliest :: [(Integer, Integer)] -> (Integer, Integer)
getEarliest = foldl1 (\(a, b) (c, d) -> if b < d then (a, b) else (c, d))

main :: IO ()
main = do
  args     <- getArgs
  inHandle <- openFile (head args) ReadMode
  contents <- hGetContents inHandle
  let (timeStamp, ids) = parseFile contents
      earliest         = getEarliest $ waitTimesById timeStamp ids
  print $ uncurry (*) earliest

