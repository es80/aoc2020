import           System.Environment
import           System.IO

type Tally = Int
type Index = Int

traversal :: Index -> Tally -> [String] -> Tally
traversal index tally [] = tally
traversal index tally (line : lines) =
  let newTally = if line !! index == '#' then tally + 1 else tally
      newIndex = mod (index + 3) (length line)
  in  traversal newIndex newTally lines

main = do
  args     <- getArgs
  inHandle <- openFile (args !! 0) ReadMode
  contents <- hGetContents inHandle
  let inLines = lines contents
      tally   = traversal 0 0 inLines
  print tally
