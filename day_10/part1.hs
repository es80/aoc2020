import           Data.List
import           System.Environment
import           System.IO

gaps :: [Int] -> [Int]
gaps []           = []
gaps [x         ] = []
gaps (x : y : ys) = y - x : gaps (y : ys)

getMultiple :: [Int] -> Int
getMultiple nums =
  let numGaps = gaps (0 : sort nums) ++ [3]
      counts  = \x -> length (filter (== x) numGaps)
  in  counts 1 * counts 3

parseInput :: String -> [Int]
parseInput fileContent = map (read :: String -> Int) $ lines fileContent

main :: IO ()
main = do
  args     <- getArgs
  inHandle <- openFile (head args) ReadMode
  contents <- hGetContents inHandle
  print $ getMultiple $ parseInput contents

