import           Control.Applicative     hiding ( many )
import           Data.Char
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           System.Environment
import           System.IO
import           Text.ParserCombinators.ReadP

-- Parsing --------------------------------------------------------------------

type Mask = [Char]
type Address = Integer
type Value = Integer

parseBit :: ReadP Char
parseBit = satisfy (== 'X') <|> satisfy (== '1') <|> satisfy (== '0')

parseMask :: ReadP Mask
parseMask = do
  string "mask = "
  many1 parseBit

parseInteger :: ReadP String
parseInteger = many1 $ satisfy isDigit

parseMem :: ReadP (Address, Value)
parseMem = do
  string "mem["
  address <- parseInteger
  string "] = "
  value <- parseInteger
  return (read address :: Address, read value :: Value)

parsePartProgram :: ReadP (Mask, [(Address, Value)])
parsePartProgram = do
  mask <- parseMask
  satisfy (== '\n')
  mems <- sepBy1 parseMem (satisfy (== '\n'))
  return (mask, mems)

parseProgram :: ReadP [(Mask, [(Address, Value)])]
parseProgram = do
  parts <- sepBy1 parsePartProgram (satisfy (== '\n'))
  satisfy (== '\n')
  eof
  return parts

parseFileContent :: String -> [(Mask, [(Address, Value)])]
parseFileContent fileContent = case readP_to_S parseProgram fileContent of
  [(extracted, "")] -> extracted
  _                 -> error "Parsing failed"

-- Solution -------------------------------------------------------------------

applyMask :: Mask -> [Integer] -> [Integer]
applyMask [] [] = []
applyMask (m : ms) (v : vs) =
  let result = applyMask ms vs
  in  case m of
        'X' -> v : result
        '1' -> 1 : result
        '0' -> 0 : result
applyMask _ _ = error "Mismatched list lengths"

bitsToNum :: [Integer] -> Integer
bitsToNum [] = 0
bitsToNum (b : bs) =
  let n = bitsToNum bs in if b == 1 then n + 2 ^ length bs else n

numToBits :: Integer -> [Integer]
numToBits 0 = []
numToBits x = numToBits (div x 2) ++ [rem x 2]

pad :: [Integer] -> [Integer]
pad bits = if length bits == 36 then bits else pad (0 : bits)

type Memory = Map Address Value

processPartProgram :: Memory -> (Mask, [(Address, Value)]) -> Memory
processPartProgram memory (mask, []) = memory
processPartProgram memory (mask, (address, value) : rest) =
  let bits     = pad $ numToBits value
      masked   = applyMask mask bits
      newValue = bitsToNum masked
  in  processPartProgram (Map.insert address newValue memory) (mask, rest)

processProgram :: Memory -> [(Mask, [(Address, Value)])] -> Memory
processProgram memory []       = memory
processProgram memory (s : ss) = processProgram newMem ss
  where newMem = processPartProgram memory s

total :: Memory -> Integer
total = Map.foldr (+) 0

main :: IO ()
main = do
  args     <- getArgs
  inHandle <- openFile (head args) ReadMode
  contents <- hGetContents inHandle
  print $ total $ processProgram Map.empty $ parseFileContent contents

