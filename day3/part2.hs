import           System.Environment
import           System.IO

type Bound = Int
type Slope = (Int, Int)
type Index = (Int, Int)
type Tally = Int

nextIndex :: Bound -> Slope -> Index -> Index 
nextIndex b (sx, sy) (x, y) = (x + sx, (y + sy) `mod` b)   



main = do
  args     <- getArgs
  inHandle <- openFile (args !! 0) ReadMode
  contents <- hGetContents inHandle
  let inLines     = lines contents
  print contents

