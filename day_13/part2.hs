import           Control.Applicative     hiding ( many )
import           Data.Char
import           Data.Maybe
import           System.Environment
import           System.IO
import           Text.ParserCombinators.ReadP

parseInteger :: ReadP String
parseInteger = many1 $ satisfy isDigit

parseNumElement :: ReadP (Maybe Integer)
parseNumElement =
  parseInteger >>= (\num -> return $ Just (read num :: Integer))

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


-- add the remainders to the list of nums parsed from file
includeRemainders index []           = []
includeRemainders index (1   : nums) = includeRemainders (index + 1) nums
includeRemainders index (num : nums) = (num - index, num) : rest
  where rest = includeRemainders (index + 1) nums

-- Given r and r' find gcd and a minimum pair Bezout Coefficients 
extendedEuclidean
  :: (Integer, Integer, Integer)
  -> (Integer, Integer, Integer)
  -> (Integer, Integer, Integer)
extendedEuclidean (r, s, t) (r', s', t')
  | r' == 0   = (r, s, t)
  | otherwise = extendedEuclidean (r', s', t') (r'', s'', t'')
 where
  q   = r `div` r'
  r'' = r - (q * r')
  s'' = s - (q * s')
  t'' = t - (q * t')

-- If n1, n2 have gcd d then there are m1, m2 such that n1*m1 + n2*m2 = d
bezoutCoeffs :: (Integer, Integer) -> (Integer, Integer)
bezoutCoeffs (n1, n2) =
  let (_, m1, m2) = extendedEuclidean (n1, 1, 0) (n2, 0, 1) in (m1, m2)

{-- To solve 
   x === a1 mod n1 
   x === a2 mod n2 
   where n1,n2 coprime
   find Bezout coeffs m1,m2 so that m1*n1 + m2*n2 = 1, then 
   x = a1*m2*n2 + a2*m1*n1
--}
solvePair :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
solvePair (a1, n1) (a2, n2) =
  let (m1, m2) = bezoutCoeffs (n1, n2)
  in  ((a1 * m2 * n2) + (a2 * m1 * n1), n1 * n2)
-- this returns a pair a', n' such that x === a' mod n' and we can use this to
-- solve the next pair as x === a' mod n' and x === a3 mod n3.

convert :: (Integer, Integer) -> (Integer, Integer)
convert (a, b) | a >= 0 && a < b = (a, b)
               | otherwise       = (a - ((a `div` b) * b), b)

apply :: [(Integer, Integer)] -> (Integer, Integer)
apply = foldl1 solvePair

-- check gcd of list nums is 1

main :: IO ()
main = do
  args     <- getArgs
  inHandle <- openFile (head args) ReadMode
  contents <- hGetContents inHandle
  print $ fst $ convert $ apply $ includeRemainders 0 (parseFile contents)

