import           Data.Char
import           System.Environment
import           System.IO
import           Text.ParserCombinators.ReadP

-- Parsing --------------------------------------------------------------------

parseLetter :: ReadP Char
parseLetter = satisfy isLetter

parseInt :: ReadP String
parseInt = many1 $ satisfy isDigit

parseLine :: ReadP (Char, Int)
parseLine = do
  action <- parseLetter
  value  <- parseInt
  eof
  return (action, read value :: Int)

type Instruction = (Char, Int)

extract :: String -> Instruction
extract line = case readP_to_S parseLine line of
  [((action, value), "")] -> (action, value)
  _                       -> error $ "Parsing failed: " ++ line

parseFileContent :: String -> [Instruction]
parseFileContent contents = map extract (lines contents)

-- Problem --------------------------------------------------------------------

data Position = Position
  { n :: Int
  , e :: Int
  , s :: Int
  , w :: Int
  }
  deriving Show
type Ship = Position
type Waypoint = Position

move :: (Ship, Waypoint) -> Int -> (Ship, Waypoint)
move (ship, waypoint) value =
  ( ship { n = n ship + (value * n waypoint)
         , e = e ship + (value * e waypoint)
         , s = s ship + (value * s waypoint)
         , w = w ship + (value * w waypoint)
         }
  , waypoint
  )

changeRight :: Waypoint -> Int -> Waypoint
changeRight Position { n = n, e = e, s = s, w = w } value = case value of
  90  -> Position w n e s
  180 -> Position s w n e
  270 -> Position e s w n

changeLeft :: Waypoint -> Int -> Waypoint
changeLeft Position { n = n, e = e, s = s, w = w } value = case value of
  90  -> Position e s w n
  180 -> Position s w n e
  270 -> Position w n e s

applyInstruction :: (Ship, Waypoint) -> Instruction -> (Ship, Waypoint)
applyInstruction (ship, waypoint) (action, value) = case action of
  'N' -> (ship, waypoint { n = n waypoint + value })
  'E' -> (ship, waypoint { e = e waypoint + value })
  'S' -> (ship, waypoint { s = s waypoint + value })
  'W' -> (ship, waypoint { w = w waypoint + value })
  'F' -> move (ship, waypoint) value
  'L' -> (ship, changeLeft waypoint value)
  'R' -> (ship, changeRight waypoint value)

applyInstructions :: (Ship, Waypoint) -> [Instruction] -> (Ship, Waypoint)
applyInstructions = foldl applyInstruction

initialPosition = (Position 0 0 0 0, Position 1 10 0 0)

getManhattan :: Position -> Int
getManhattan ship = abs (n ship - s ship) + abs (e ship - w ship)

main :: IO ()
main = do
  args     <- getArgs
  inHandle <- openFile (head args) ReadMode
  contents <- hGetContents inHandle
  let parsed = parseFileContent contents
  print $ getManhattan $ fst $ applyInstructions initialPosition parsed

