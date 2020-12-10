import           System.Environment
import           System.IO

data Policy = Policy
  { index1    :: Int    -- the first index we check 
  , index2    :: Int    -- the second index we check
  , character :: Char   -- the char the policy applies to
  , password  :: String -- the password to check
  }
  deriving Show

xor :: Bool -> Bool -> Bool
xor b1 b2 | b1 == b2  = False
          | otherwise = True

policyIsSatisfied :: Policy -> Bool
policyIsSatisfied p =
  let idx1   = index1 p - 1
      idx2   = index2 p - 1
      found1 = (password p !! idx1) == character p
      found2 = (password p !! idx2) == character p
  in  xor found1 found2

parseLineAsPolicy :: [String] -> Maybe Policy
parseLineAsPolicy (num1 : num2 : chStr : pwd : _) =
  Just $ Policy (read num1 :: Int) (read num2 :: Int) (head chStr) pwd
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
  inHandle <- openFile (head args) ReadMode
  contents <- hGetContents inHandle
  let inLines     = lines contents
      parsedLines = parseLines inLines
  putStrLn $ count 0 parsedLines
