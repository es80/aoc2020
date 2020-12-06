import           System.Environment
import           System.IO

main :: IO ()
main = do
  args     <- getArgs
  inHandle <- openFile (head args) ReadMode
  contents <- hGetContents inHandle
  putStrLn "-- TODO --"
