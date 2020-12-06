import           Data.List
import           System.Environment
import           System.IO

splitAtAll :: (a -> Bool) -> [a] -> [[a]]
splitAtAll p [] = [[]]
splitAtAll p (x : xs) =
  let rest = splitAtAll p xs
  in  if p x then [] : rest else (x : head rest) : tail rest

getGroups :: String -> [[String]]
getGroups fileContent = splitAtAll (== "") $ lines fileContent

getSumUniques :: [[String]] -> Int
getSumUniques groups = sum $ map (length . nub . concat) groups

main = do
  args     <- getArgs
  inHandle <- openFile (head args) ReadMode
  contents <- hGetContents inHandle
  print $ getSumUniques $ getGroups contents
