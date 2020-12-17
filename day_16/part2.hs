import           Data.Char
import           Data.Maybe
import           Data.List
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           System.Environment
import           System.IO
import           Text.ParserCombinators.ReadP

type Range = (Int, Int)
type Ticket = [Int]
type Rule = (String, Range, Range)

-- Parsing --------------------------------------------------------------------

parseInt :: ReadP String
parseInt = many1 $ satisfy isDigit

parseRange :: ReadP Range
parseRange = do
  low <- parseInt
  satisfy (== '-')
  high <- parseInt
  return (read low :: Int, read high :: Int)

parseRule :: ReadP Rule
parseRule = do
  name   <- manyTill get (string ": ")
  range1 <- parseRange
  string " or "
  range2 <- parseRange
  return (name, range1, range2)

parseTicket :: ReadP Ticket
parseTicket = do
  nums <- sepBy parseInt (char ',')
  return (map (\x -> read x :: Int) nums)

parseMyTicket :: ReadP Ticket
parseMyTicket = do
  string "your ticket:"
  satisfy (== '\n')
  ticket <- parseTicket
  satisfy (== '\n')
  return ticket

parseNearbyTickets :: ReadP [Ticket]
parseNearbyTickets = do
  string "nearby tickets:"
  satisfy (== '\n')
  sepBy parseTicket (satisfy (== '\n'))

parseAll :: ReadP ([Rule], Ticket, [Ticket])
parseAll = do
  rules <- sepBy parseRule (satisfy (== '\n'))
  satisfy (== '\n')
  satisfy (== '\n')
  mine <- parseMyTicket
  satisfy (== '\n')
  nearby <- parseNearbyTickets
  satisfy (== '\n')
  eof
  return (rules, mine, nearby)

parseFileContent :: String -> ([Rule], Ticket, [Ticket])
parseFileContent fileContent = case readP_to_S parseAll fileContent of
  [(extracted, "")] -> extracted
  _                 -> error "Parsing failed"

-- Problem --------------------------------------------------------------------

inRange :: Range -> Int -> Bool
inRange (low, high) num = num >= low && num <= high

checkValid :: Rule -> Int -> Bool
checkValid (filed, range1, range2) num =
  inRange range1 num || inRange range2 num

checkValidAnyRule :: [Rule] -> Int -> Bool
checkValidAnyRule rules num = any (`checkValid` num) rules

filterValid :: [Rule] -> Ticket -> Maybe Ticket
filterValid rules ticket =
  if all (checkValidAnyRule rules) ticket then Just ticket else Nothing

filterAllValid :: [Rule] -> [Ticket] -> [Ticket]
filterAllValid rules = mapMaybe (filterValid rules)

possibleFields :: [Rule] -> Int -> [String]
possibleFields [] num = []
possibleFields (rule : rules) num =
  let (field, _, _) = rule
      rest          = possibleFields rules num
  in  if checkValid rule num then field : rest else rest

ticketPossibleFields :: [Rule] -> Ticket -> [Set String]
ticketPossibleFields rules = map (Set.fromList . possibleFields rules)

combineTickets :: [Set String] -> [Set String] -> [Set String]
combineTickets []       []       = []
combineTickets (x : xs) (y : ys) = Set.intersection x y : combineTickets xs ys

combineAll :: [Ticket] -> [Rule] -> [Set String]
combineAll tickets rules =
  let allSets = map (ticketPossibleFields rules) tickets
  in  foldl1 combineTickets allSets

findSingles :: [Set String] -> [Set String]
findSingles = filter (\s -> Set.size s == 1)

elimination :: [Set String] -> [Set String]
elimination sets =
  let singles    = findSingles sets
      setSingles = Set.unions singles
  in  map (\s -> if Set.size s /= 1 then s Set.\\ setSingles else s) sets

fullElimination :: [Set String] -> [Set String]
fullElimination sets =
  if sets == elimination sets then sets else fullElimination (elimination sets)

checkAllSingle :: [Set String] -> ()
checkAllSingle [] = ()
checkAllSingle (x : xs) =
  if Set.size x /= 1 then error "Elimination failed" else checkAllSingle xs

getAnswer :: Ticket -> [Rule] -> [Ticket] -> Int
getAnswer myTicket rules nearbyTickets =
  let validTickets = filterAllValid rules nearbyTickets
      fields       = fullElimination $ combineAll validTickets rules
      _            = checkAllSingle fields
      list         = map (head . Set.toList) fields
      paired       = zip list myTicket
      filtered     = filter (\(a, b) -> "departure" `isPrefixOf` a) paired
  in  foldr (\a b -> snd a * b) 1 filtered

main :: IO ()
main = do
  args     <- getArgs
  inHandle <- openFile (head args) ReadMode
  contents <- hGetContents inHandle
  let (rules, myTicket, nearbyTickets) = parseFileContent contents
  print $ getAnswer myTicket rules nearbyTickets

