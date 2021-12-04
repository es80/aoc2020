import           Data.Char
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
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

data Direction = North | East | South | West deriving (Enum, Eq, Ord, Show)
data Ship = Ship
  { direction :: Direction
  , location  :: Map Direction Int
  }
  deriving Show

next :: Direction -> Direction
next direction = if direction == West then North else succ direction

prev :: Direction -> Direction
prev direction = if direction == North then West else pred direction

iter :: (a -> a) -> Int -> a -> a
iter f 0 a = a
iter f n a = iter f (n - 1) (f a)

changeRight :: Direction -> Int -> Direction
changeRight direction value = iter next (value `div` 90) direction

changeLeft :: Direction -> Int -> Direction
changeLeft direction value = iter prev (value `div` 90) direction

move :: Ship -> Int -> Ship
move ship value = ship { location = Map.insertWith (+) (direction ship) value (location ship) }

applyInstruction :: Ship -> Instruction -> Ship
applyInstruction ship (action, value) = case action of
  'N' -> ship { location = Map.insertWith (+) North value (location ship) }
  'E' -> ship { location = Map.insertWith (+) East value (location ship) }
  'S' -> ship { location = Map.insertWith (+) South value (location ship) }
  'W' -> ship { location = Map.insertWith (+) West value (location ship) }
  'R' -> ship { direction = changeRight (direction ship) value }
  'L' -> ship { direction = changeLeft (direction ship) value }
  'F' -> move ship value

applyInstructions :: Ship -> [Instruction] -> Ship
applyInstructions = foldl applyInstruction

initialLocation = Map.fromList [(North, 0), (East, 0), (South, 0), (West, 0)]
initialShip = Ship East initialLocation

getManhattan :: Ship -> Int
getManhattan ship =
  let loc = location ship
      n   = Map.findWithDefault 0 North loc
      e   = Map.findWithDefault 0 East loc
      s   = Map.findWithDefault 0 South loc
      w   = Map.findWithDefault 0 West loc
  in  abs (n - s) + abs (e - w)

main :: IO ()
main = do
  args     <- getArgs
  inHandle <- openFile (head args) ReadMode
  contents <- hGetContents inHandle
  let parsed = parseFileContent contents
  print $ getManhattan $ applyInstructions initialShip parsed

