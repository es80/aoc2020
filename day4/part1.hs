import           Data.List
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           System.Environment
import           System.IO

type Key = String
type Value = String
type Passport = [(Key, Value)]

splitAtAll :: (a -> Bool) -> [a] -> [[a]]
splitAtAll p [] = [[]]
splitAtAll p (x : xs) =
  let rest = splitAtAll p xs
  in  if p x then [] : rest else (x : head rest) : tail rest

splitAtFirst :: (a -> Bool) -> [a] -> ([a], [a])
splitAtFirst p [] = ([], [])
splitAtFirst p (x : xs) =
  let (first, second) = splitAtFirst p xs
  in  if p x then ([], xs) else (x : first, second)

getPassportStrings :: [String] -> [String]
getPassportStrings lines = map unwords $ splitAtAll (== "") lines

splitIntoKeyValuePairs :: String -> Passport
splitIntoKeyValuePairs string = map (splitAtFirst (== ':')) $ words string

keysSet :: Passport -> Set String
keysSet keyVals = Set.insert "cid" $ Set.fromList $ map fst keyVals

validKeys :: Set String
validKeys =
  Set.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"]

validSet :: Set String -> Bool
validSet keys = keys == validKeys

getValidPassports :: [String] -> Int
getValidPassports =
  length
    . filter validSet
    . map (keysSet . splitIntoKeyValuePairs)
    . getPassportStrings

main = do
  args     <- getArgs
  inHandle <- openFile (head args) ReadMode
  contents <- hGetContents inHandle
  let inLines = lines contents
  print $ getValidPassports inLines
