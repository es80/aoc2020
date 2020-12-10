import           Data.Array
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

findSolution1 :: [Int] -> Int
findSolution1 nums = case processPartition $ getStartPartition nums of
  Just x  -> x
  Nothing -> error "No solution found part 1."

parseInput :: String -> [Int]
parseInput fileContent = map (read :: String -> Int) $ lines fileContent

-- Part 2 ---------------------------------------------------------------------

mkArray :: [Int] -> Array Int Int
mkArray nums = listArray (0, length nums - 1) nums

getSliceSum :: (Int, Int) -> Array Int Int -> Int
getSliceSum (low, high) arr =
  let indexes = [low .. high]
      nums    = [ arr ! i | i <- indexes ]
  in  sum nums

findSlice :: Int -> (Int, Int) -> Array Int Int -> [Int]
findSlice target (low, high) arr
  | sliceSum == target
  = let indexes = [low .. high] in [ arr ! i | i <- indexes ]
  | sliceSum > target
  = findSlice target (low + 1, high) arr
  | sliceSum < target
  = findSlice target (low, high + 1) arr
  where sliceSum = getSliceSum (low, high) arr

findSolution2 :: [Int] -> Int
findSolution2 nums =
  let part1 = findSolution1 nums
      slice = findSlice part1 (0, 2) (mkArray nums)
  in  minimum slice + maximum slice

main = do
  args     <- getArgs
  inHandle <- openFile (head args) ReadMode
  contents <- hGetContents inHandle
  print $ findSolution2 $ parseInput contents
