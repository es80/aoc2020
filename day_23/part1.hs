import           Data.List
import           System.Environment
import           System.IO

cups = [4, 7, 6, 1, 3, 8, 2, 5, 9] :: [Int]

pickUp :: [Int] -> ([Int], [Int])
pickUp xs =
  let pick = (take 3 . drop 1) xs
      rest = take 1 xs ++ drop 4 xs
  in  (pick, rest)

findInsertIndex :: Int -> [Int] -> Int
findInsertIndex x xs = if x == 0
  then findInsertIndex 9 xs
  else case elemIndex x xs of
    Just index -> index + 1
    Nothing    -> findInsertIndex (x - 1) xs

insertCups :: ([Int], [Int]) -> [Int]
insertCups (pickedUp, rest) =
  let index   = findInsertIndex (head rest - 1) rest
      putBack = take index rest ++ pickedUp ++ drop index rest
  in  tail putBack ++ [head putBack]

moves :: Int -> [Int] -> [Int]
moves 0 xs = xs
moves n xs = let next = (insertCups . pickUp) xs in moves (n - 1) next

getAnswer :: [Int] -> String
getAnswer xs = if head xs == 1
  then concatMap show $ tail xs
  else getAnswer $ tail xs ++ [head xs]

main = putStrLn $ getAnswer $ moves 100 cups

