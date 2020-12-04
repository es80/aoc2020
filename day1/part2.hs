import           Data.IntSet                    ( IntSet )
import qualified Data.IntSet                   as IntSet
import           System.Environment
import           System.IO

type Target = Int

maybeFindProduct :: Target -> [Int] -> IntSet -> Maybe Int
maybeFindProduct target []       set = Nothing
maybeFindProduct target (x : xs) set = if IntSet.member (target - x) set
  then Just $ x * (target - x)
  else maybeFindProduct target xs (IntSet.insert x set)

maybeFindTriple :: [Int] -> Maybe Int
maybeFindTriple []       = Nothing
maybeFindTriple (x : xs) = case maybeFindProduct (2020 - x) xs IntSet.empty of
  Nothing      -> maybeFindTriple xs
  Just product -> Just $ product * x

findProduct :: [Int] -> String
findProduct nums = case maybeFindTriple nums of
  Nothing -> "No solution found."
  Just x  -> show x

parseInputNums :: [String] -> [Int]
parseInputNums list = map (read :: String -> Int) list

main = do
  args     <- getArgs
  inHandle <- openFile (args !! 0) ReadMode
  contents <- hGetContents inHandle
  let inLines = lines contents
  putStrLn $ findProduct $ parseInputNums inLines

