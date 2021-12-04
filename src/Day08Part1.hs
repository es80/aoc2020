import           Control.Applicative     hiding ( many )
import           Data.Char
import           Data.IntSet                    ( IntSet )
import qualified Data.IntSet                   as IntSet
import           System.Environment
import           System.IO
import           Text.ParserCombinators.ReadP

-- Parsing --------------------------------------------------------------------

parseOperation :: ReadP String
parseOperation = string "acc" <|> string "jmp" <|> string "nop"

parseSpace :: ReadP Char
parseSpace = satisfy isSpace

parseSign :: ReadP Char
parseSign = satisfy (\char -> char == '+' || char == '-')

parseInt :: ReadP String
parseInt = many1 $ satisfy isDigit

parseLine :: ReadP (String, Char, Int)
parseLine = do
  op <- parseOperation
  parseSpace
  sign <- parseSign
  num  <- parseInt
  eof
  return (op, sign, read num :: Int)

-- Parse the file content -----------------------------------------------------

type Instruction = (String, Int)
type Program = [Instruction]

extractLine :: String -> Instruction
extractLine line = case readP_to_S parseLine line of
  [((op, sign, num), "")] ->
    let delta = case sign of
          '+' -> num
          '-' -> -num
    in  (op, delta)
  _ -> error $ "Failed to parse line " ++ line

-- Solve the problem ----------------------------------------------------------

data State = State
  { acc         :: Int
  , nextIndex   :: Int
  , prevIndices :: IntSet
  }
  deriving Show

initialState = State 0 0 IntSet.empty

applyInstruction :: Program -> State -> State
applyInstruction program state =
  let newPrevIndices     = IntSet.insert currIndex $ prevIndices state
      currAcc            = acc state
      currIndex          = nextIndex state
      (currentOp, delta) = program !! currIndex
  in  case currentOp of
        "acc" ->
          state { acc = currAcc + delta, nextIndex = currIndex + 1, prevIndices = newPrevIndices }
        "jmp" -> state { nextIndex = currIndex + delta, prevIndices = newPrevIndices }
        "nop" -> state { nextIndex = currIndex + 1, prevIndices = newPrevIndices }


applyAllInstructions :: Program -> State -> State
applyAllInstructions program state =
  let newState = applyInstruction program state
  in  if IntSet.member (nextIndex newState) (prevIndices newState)
        then newState
        else applyAllInstructions program newState

finalAccumulator :: String -> Int
finalAccumulator fileContent = acc finalState
 where
  program    = map extractLine $ lines fileContent
  finalState = applyAllInstructions program initialState

main :: IO ()
main = do
  args     <- getArgs
  inHandle <- openFile (head args) ReadMode
  contents <- hGetContents inHandle
  print $ finalAccumulator contents

