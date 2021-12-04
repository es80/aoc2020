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

data Status = Terminated | Looping | Running | RunningOneChange deriving Eq
data State = State
  { acc         :: Int
  , nextIndex   :: Int
  , prevIndices :: IntSet
  , status      :: Status
  }

initialState = State 0 0 IntSet.empty Running

applyAcc :: Instruction -> State -> State
applyAcc (_, delta) state = state
  { acc         = acc state + delta
  , nextIndex   = nextIndex state + 1
  , prevIndices = IntSet.insert (nextIndex state) (prevIndices state)
  }

applyJmp :: Instruction -> State -> State
applyJmp (_, delta) state = state
  { nextIndex   = nextIndex state + delta
  , prevIndices = IntSet.insert (nextIndex state) (prevIndices state)
  }

applyNop :: Instruction -> State -> State
applyNop (_, delta) state = state
  { nextIndex   = nextIndex state + 1
  , prevIndices = IntSet.insert (nextIndex state) (prevIndices state)
  }

allowChange :: Instruction -> State -> [State]
allowChange (op, delta) state = if status state == RunningOneChange
  then []
  else case op of
    "jmp" -> [applyNop (op, delta) (state { status = RunningOneChange })]
    "nop" -> [applyJmp (op, delta) (state { status = RunningOneChange })]

applyInstruction :: Program -> State -> [State]
applyInstruction program state =
  let currIndex   = nextIndex state
      (op, delta) = program !! currIndex
  in  case op of
        "acc" -> [applyAcc (op, delta) state]
        "jmp" -> applyJmp (op, delta) state : allowChange (op, delta) state
        "nop" -> applyNop (op, delta) state : allowChange (op, delta) state

setStatus :: Int -> State -> State
setStatus maxIndex state
  | nextIndex state == maxIndex = state { status = Terminated }
  | IntSet.member (nextIndex state) (prevIndices state) = state { status = Looping }
  | otherwise                   = state

applyAllInstructions :: Program -> [State] -> State
applyAllInstructions program (pop : rest) =
  let maxLength = length program
  in
    case status pop of
      Terminated -> pop
      Looping    -> applyAllInstructions program rest
      _          -> applyAllInstructions
        program
        (rest ++ map (setStatus maxLength) (applyInstruction program pop))

finalAccumulator :: String -> Int
finalAccumulator fileContent = acc finalState
 where
  program    = map extractLine $ lines fileContent
  finalState = applyAllInstructions program [initialState]

main :: IO ()
main = do
  args     <- getArgs
  inHandle <- openFile (head args) ReadMode
  contents <- hGetContents inHandle
  print $ finalAccumulator contents

