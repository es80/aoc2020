import System.IO
import System.Environment
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

parseInputNums :: [String] -> [Int]
parseInputNums list = map (read :: String -> Int) list

checkForCompliment :: Int -> IntSet -> Maybe Int
checkForCompliment num set
  | IntSet.member (2020 - num) set = Just $ 2020 - num
  | otherwise = Nothing

maybeFindProduct :: [Int] -> IntSet -> Maybe Int
maybeFindProduct [] set = Nothing
maybeFindProduct (x:xs) set = case checkForCompliment x set of 
                        Nothing -> maybeFindProduct xs (IntSet.insert x set)
                        Just y -> Just $ x * y

findProduct :: [Int] -> String
findProduct nums = case maybeFindProduct nums IntSet.empty of
                     Nothing -> "No solution found."
                     Just x -> show x

main = do
  args <- getArgs
  inHandle <- openFile (args !! 0) ReadMode 
  contents <- hGetContents inHandle
  let inLines = lines contents
  putStrLn $ findProduct $ parseInputNums inLines

