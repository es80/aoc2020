import           Data.IntSet                    ( IntSet )
import qualified Data.IntSet                   as IntSet
import           System.Environment
import           System.IO

maybeFindProduct :: [Int] -> IntSet -> Maybe Int
maybeFindProduct []       set = Nothing
maybeFindProduct (x : xs) set = if IntSet.member (2020 - x) set
  then Just $ x * (2020 - x)
  else maybeFindProduct xs (IntSet.insert x set)

findProduct :: [Int] -> String
findProduct nums = case maybeFindProduct nums IntSet.empty of
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
