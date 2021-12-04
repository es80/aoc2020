import           Data.List
import           System.Environment
import           System.IO

data Choice = Lefty | Righty
 deriving Show

search :: Int -> [Int] -> [Choice] -> [Int]
search len xs []       = xs
search len xs (c : cs) = case c of
  Lefty  -> search newLen (take newLen xs) cs
  Righty -> search newLen (drop newLen xs) cs
  where newLen = div len 2

searcher :: String -> [Choice] -> [Int]
searcher string choices = let len = 2 ^ length string in search len [0 .. len - 1] choices

getNumber :: (Char, Char) -> String -> Maybe Int
getNumber (left, right) string =
  let choices = map (\c -> if c == right then Righty else Lefty) string
      found   = searcher string choices
  in  case found of
        [x] -> Just x
        _   -> Nothing

getID :: String -> Maybe Int
getID str =
  let row = getNumber ('F', 'B') $ take 7 str
      col = getNumber ('L', 'R') $ drop 7 str
  in  (+) <$> ((* 8) <$> row) <*> col

findMissing :: [Maybe Int] -> Maybe Int
findMissing []  = Nothing
findMissing [x] = Nothing
findMissing (x : y : ys) =
  let diff = (-) <$> y <*> x
  in  case diff of
        Just 2 -> succ <$> x
        _      -> findMissing (y : ys)

showAnswer :: [String] -> String
showAnswer lines = maybe "something went wrong" show $ (findMissing . sort . map getID) lines

main = do
  args     <- getArgs
  inHandle <- openFile (head args) ReadMode
  contents <- hGetContents inHandle
  let inLines = lines contents
  putStrLn $ showAnswer inLines

