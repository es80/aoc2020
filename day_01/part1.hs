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
findProduct nums =
  maybe "No solution found." show (maybeFindProduct nums IntSet.empty)

parseInputNums :: [String] -> [Int]
parseInputNums = map (read :: String -> Int)

main = do
  args     <- getArgs
  inHandle <- openFile (head args) ReadMode
  contents <- hGetContents inHandle
  let inLines = lines contents
  putStrLn $ findProduct $ parseInputNums inLines
