import           Data.IntSet                    ( IntSet )
import qualified Data.IntSet                   as IntSet
import           System.Environment
import           System.IO

listOfPairs :: [Int] -> [(Int, Int)]
listOfPairs []       = []
listOfPairs (x : xs) = [ (x, y) | y <- xs ] ++ listOfPairs xs

setOfSums :: [(Int, Int)] -> IntSet
setOfSums pairs = IntSet.fromList $ map (uncurry (+)) pairs

processPartition :: ([Int], [Int]) -> Maybe Int
processPartition (nums, []) = Nothing
processPartition (nums, next : rest)
  | IntSet.member next set = processPartition (drop 1 nums ++ [next], rest)
  | otherwise              = Just next
  where set = setOfSums $ listOfPairs nums

getStartPartition :: [Int] -> ([Int], [Int])
getStartPartition = splitAt 25

findSolution :: [Int] -> String
findSolution nums = maybe "No solution found." show (processPartition $ getStartPartition nums)

parseInput :: String -> [Int]
parseInput fileContent = map (read :: String -> Int) $ lines fileContent

main = do
  args     <- getArgs
  inHandle <- openFile (head args) ReadMode
  contents <- hGetContents inHandle
  putStrLn $ findSolution $ parseInput contents
