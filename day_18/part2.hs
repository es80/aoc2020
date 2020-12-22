import           Data.Char
import           System.Environment
import           System.IO

parseLine :: String -> [String]
parseLine []         = []
parseLine (' ' : xs) = parseLine xs
parseLine (x   : xs) = [x] : parseLine xs

leftB :: String -> Bool
leftB s = s == "("

rightB :: String -> Bool
rightB s = s == ")"

bracket :: String -> Bool
bracket s = rightB s || leftB s

plus :: String -> Bool
plus s = s == "+"

mult :: String -> Bool
mult s = s == "*"

op :: String -> Bool
op s = plus s || mult s

num :: String -> Bool
num = all isDigit

evalAdd :: String -> String -> String
evalAdd s t = show ((read s :: Int) + (read t :: Int))

evalMult :: String -> String -> String
evalMult s t = show ((read s :: Int) * (read t :: Int))

evalOp :: String -> String -> String -> String
evalOp x "+" y = evalAdd x y
evalOp x "*" y = evalMult x y
evalOp x z   y = error (x ++ z ++ y)

reduce :: [String] -> [String]
reduce (x : y : z : zs) | num x && plus y && num z = reduce (evalAdd x z : zs)
                        | leftB x && num y && rightB z = reduce (y : zs)
                        | otherwise = x : reduce (y : z : zs)
reduce s = s

reduceMults :: [String] -> [String]
reduceMults [] = []
reduceMults (z : zs)
  | leftB z
  = let (ms, rest) = break bracket zs
    in  if rightB (head rest)
          then reduceMults (findMult ms ++ tail rest)
          else z : reduceMults zs
  | otherwise
  = z : reduceMults zs

findMult :: [String] -> [String]
findMult []  = []
findMult [x] = [x]
findMult (x : y : z : zs)
  | num x && mult y && num z = findMult (evalMult x z : zs)
  | otherwise                = error (concat (x : y : z : zs))
findMult s = s

reduceAll :: [String] -> [String]
reduceAll s =
  let t  = reduce s
      t' = reduceMults t
  in  if s == t' then s else reduceAll t'

finalCalc :: [String] -> [String]
finalCalc s =
  let t = reduceAll s in if any bracket t then error (concat t) else calc t
 where
  calc [x             ] = [x]
  calc (x : y : z : zs) = calc (evalOp x y z : zs)

main :: IO ()
main = do
  args     <- getArgs
  inHandle <- openFile (head args) ReadMode
  contents <- hGetContents inHandle
  let calcs = map (finalCalc . parseLine) (lines contents)
  --mapM_ (putStrLn . concat) calcs
  print $ sum $ map read $ concat calcs

