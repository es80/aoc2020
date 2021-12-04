import           Control.Applicative     hiding ( many )
import           Data.Char
import           System.Environment
import           System.IO
import           Text.ParserCombinators.ReadP

data Exp = Val Int | Mult Exp Exp | Add Exp Exp deriving Show

parseInt :: ReadP Exp
parseInt = do
  digits <- many1 $ satisfy isDigit
  return (Val (read digits))

parseAdd :: ReadP (Exp -> Exp -> Exp)
parseAdd = string "+" >> return Add

parseMult :: ReadP (Exp -> Exp -> Exp)
parseMult = string "*" >> return Mult

parseGroup :: ReadP Exp
parseGroup = parseInt <|> string "(" *> parseExp <* string ")"

parseExp :: ReadP Exp
parseExp = chainl1 parseGroup (parseAdd <|> parseMult)

eval :: Exp -> Int
eval (Val a   ) = a
eval (Mult a b) = eval a * eval b
eval (Add  a b) = eval a + eval b

runParser :: String -> Int
runParser s = case readP_to_S (parseExp <* eof) s of
  [(exp, "")] -> eval exp
  _           -> error "Parsing failed"

main :: IO ()
main = do
  args     <- getArgs
  inHandle <- openFile (head args) ReadMode
  contents <- hGetContents inHandle
  let l = map (concat . words) $ lines contents
  print $ sum (map runParser l)

