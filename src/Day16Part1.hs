import           Data.Char
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
checkValid (filed, range1, range2) num = inRange range1 num || inRange range2 num

checkValidAnyRule :: [Rule] -> Int -> Bool
checkValidAnyRule rules num = any (`checkValid` num) rules

filterInvalid :: [Rule] -> Ticket -> [Int]
filterInvalid rules = filter (not . checkValidAnyRule rules)

filterAllInvalid :: [Rule] -> [Ticket] -> [[Int]]
filterAllInvalid rules = map (filterInvalid rules)

getSum :: [[Int]] -> Int
getSum nums = sum (map sum nums)

main :: IO ()
main = do
  args     <- getArgs
  inHandle <- openFile (head args) ReadMode
  contents <- hGetContents inHandle
  let (rules, myTicket, nearbyTickets) = parseFileContent contents
  let invalid                          = filterAllInvalid rules nearbyTickets
  print $ getSum invalid

