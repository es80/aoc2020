import           Data.List
import           System.Environment
import           System.IO

gaps :: [Int] -> [Int]
gaps []           = []
gaps [x         ] = []
gaps (x : y : ys) = y - x : gaps (y : ys)

parseInput :: String -> [Int]
parseInput fileContent = map (read :: String -> Int) $ lines fileContent

groupsOfOnes :: [Int] -> [Int]
groupsOfOnes xs =
  let ones = filter (\x -> head x == 1) in map length $ ones $ group xs

{--
   From the OEIS:
  
   Tribonacci numbers: a(n) = a(n-1) + a(n-2) + a(n-3) for n >= 3 with 
   a(0) = a(1) = 0 and a(2) = 1. 
   
   a(n) = number of compositions of n-2 with no part greater than 3. Example:
   a(5)=4 because we have 1+1+1 = 1+2 = 2+1 = 3. - Emeric Deutsch, Mar 10 2004
--}

tribonacci :: Int -> Int
tribonacci n = head (tribonaccis n)
 where
  tribonaccis 0 = [0]
  tribonaccis 1 = [0, 0]
  tribonaccis 2 = [1, 0, 0]
  tribonaccis n =
    let ts@(x : y : z : ys) = tribonaccis (n - 1) in (x + y + z : ts)

arrangements :: [Int] -> Int
arrangements = foldr (\x -> (*) (tribonacci (x + 2))) 1

inputToLengthsOfOnes :: [Int] -> [Int]
inputToLengthsOfOnes nums = groupsOfOnes $ gaps $ sort $ 0 : nums

main :: IO ()
main = do
  args     <- getArgs
  inHandle <- openFile (head args) ReadMode
  contents <- hGetContents inHandle
  print $ arrangements $ inputToLengthsOfOnes $ parseInput contents

