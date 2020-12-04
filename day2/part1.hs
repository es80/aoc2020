import           System.Environment
import           System.IO

data Policy = Policy
  { minCount  :: Int    -- the minimum number of times a char can appear
  , maxCount  :: Int    -- the maximum number of times a char can appear
  , character :: Char   -- the char the policy applies to
  , password  :: String -- the password to check
  }
  deriving Show

countCharInString :: Char -> String -> Int
countCharInString ch [] = 0
countCharInString ch (c : s) | ch == c   = 1 + countCharInString ch s
                             | otherwise = countCharInString ch s

policyIsSatisfied :: Policy -> Bool
policyIsSatisfied p =
  let count = countCharInString (character p) (password p)
  in  count >= (minCount p) && count <= (maxCount p)

parseLineAsPolicy :: [String] -> Maybe Policy
parseLineAsPolicy (num1 : num2 : ch : pwd : _) =
  Just $ Policy (read num1 :: Int) (read num2 :: Int) (ch !! 0) pwd
parseLineAsPolicy _ = Nothing

parseLineAsWords :: String -> [String]
parseLineAsWords line =
  let addedSpaces = map (\c -> if c == '-' || c == ':' then ' ' else c)
  in  words $ addedSpaces line

parseLines :: [String] -> [Maybe Policy]
parseLines = map (parseLineAsPolicy . parseLineAsWords)

count :: Int -> [Maybe Policy] -> String
count tally []       = show tally
count tally (x : xs) = case x of
  Just policy ->
    if policyIsSatisfied policy then count (tally + 1) xs else count tally xs
  Nothing -> "Failed to parse a policy"

main = do
  args     <- getArgs
  inHandle <- openFile (args !! 0) ReadMode
  contents <- hGetContents inHandle
  let inLines     = lines contents
      parsedLines = parseLines inLines
  putStrLn $ count 0 parsedLines

