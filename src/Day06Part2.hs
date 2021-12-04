import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           System.Environment
import           System.IO

type Answers = String
type GroupAnswers = [Answers]

splitAtAll :: (a -> Bool) -> [a] -> [[a]]
splitAtAll p [] = [[]]
splitAtAll p (x : xs) =
  let rest = splitAtAll p xs in if p x then [] : rest else (x : head rest) : tail rest

getListGroupAnswers :: String -> [GroupAnswers]
getListGroupAnswers fileContent = splitAtAll (== "") $ lines fileContent

formSetsFromGroup :: GroupAnswers -> [Set Char]
formSetsFromGroup = map Set.fromList

countGroupAllAnswered :: [Set Char] -> Int
countGroupAllAnswered sets = Set.size $ foldl1 Set.intersection sets

totalCount :: String -> Int
totalCount fileContent =
  sum $ map (countGroupAllAnswered . formSetsFromGroup) $ getListGroupAnswers fileContent

main :: IO ()
main = do
  args     <- getArgs
  inHandle <- openFile (head args) ReadMode
  contents <- hGetContents inHandle
  print $ totalCount contents
