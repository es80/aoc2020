import           Data.List
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           System.Environment
import           System.IO
import           Text.Regex.Posix

-- Part 1 ---------------------------------------------------------------------

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
  in  if p x then ([], xs) else ((x : first), second)

getPassportStrings :: [String] -> [String]
getPassportStrings lines = map (intercalate " ") $ splitAtAll (== "") lines

splitIntoKeyValuePairs :: String -> Passport
splitIntoKeyValuePairs string = map (splitAtFirst (== ':')) $ words string

keysSet :: Passport -> Set String
keysSet keyVals = Set.insert "cid" $ Set.fromList $ map fst keyVals

validKeys :: Set String
validKeys =
  Set.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"]

validSet :: Set String -> Bool
validSet keys = keys == validKeys

-- Part 2 ---------------------------------------------------------------------

intervalContainsInt :: (Int, Int) -> Int -> Bool
intervalContainsInt (low, high) test = test >= low && test <= high

validYear :: (Int, Int) -> String -> Bool
validYear (low, high) string =
  intervalContainsInt (low, high) (read string :: Int)

validHeightHelper :: (Int, String) -> Bool
validHeightHelper (h, "cm") = intervalContainsInt (150, 193) h
validHeightHelper (h, "in") = intervalContainsInt (59, 76) h
validHeightHelper _         = False

validHeight :: String -> Bool
validHeight string =
  let reg                = "^([0-9]{2,3})(in|cm)$"
      (_, _, _, matches) = string =~ reg :: (String, String, String, [String])
      numMatches         = length matches
  in  numMatches
        == 2
        && (validHeightHelper (read $ matches !! 0 :: Int, matches !! 1))

validHairColour :: String -> Bool
validHairColour string = string =~ "^#[0-9a-f]{6}$"

validEyeColour :: String -> Bool
validEyeColour string = string =~ "^(amb|blu|brn|gry|grn|hzl|oth)$"

validPassportId :: String -> Bool
validPassportId string = string =~ "^[0-9]{9}$"

validPair :: (Key, Value) -> Bool
validPair (key, value) = case key of
  "byr"     -> validYear (1920, 2002) value
  "iyr"     -> validYear (2010, 2020) value
  "eyr"     -> validYear (2020, 2030) value
  "hgt"     -> validHeight value
  "hcl"     -> validHairColour value
  "ecl"     -> validEyeColour value
  "pid"     -> validPassportId value
  "cid"     -> True
  otherwise -> False

allValidPairs :: Passport -> Bool
allValidPairs passport = and $ map validPair passport

getValidPassports :: [String] -> Int
getValidPassports =
  length
    . filter validSet
    . map keysSet
    . filter allValidPairs
    . map splitIntoKeyValuePairs
    . getPassportStrings

main = do
  args     <- getArgs
  inHandle <- openFile (args !! 0) ReadMode
  contents <- hGetContents inHandle
  let inLines = lines contents
  putStrLn $ show $ getValidPassports inLines

