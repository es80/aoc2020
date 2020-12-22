import           Control.Applicative     hiding ( many )
import           Data.Char
import           Data.List
import           System.Environment
import           System.IO
import           Text.ParserCombinators.ReadP

-------------------------------------------------------------------------------


-- Parsing --------------------------------------------------------------------

data Token = LeftB | RightB | Plus | Mult | Digits Int deriving Show


parseLine :: String -> [Token]
parseLine [] = []
parseLine (x:xs) 
  | x == '(' = LeftB  : rest
  | x == ')' = RightB : rest
  | x == '+' = Plus   : rest
  | x == '*' = Mult   : rest
  | isDigit x = Digits (read [x]) : rest
  | isSpace x = rest
  | otherwise = error $ "unable to parse line at " ++ [x] ++ " : " ++ xs
  where rest = parseLine xs


{--

data Opt = String | Op | Bracket

parseInt :: ReadP Int
parseInt = do
  digits <- many1 $ satisfy isDigit
  return (read digits)


getOp " + " = Add
getOp " * " = Mult
parseOp :: ReadP Opt
parseOp = do
  op <- string " + " <|> string " * "
  return (getOp op)

getBrack '(' = LeftB
getBrack ')' = RightB
parseBrack :: ReadP Opt
parseBrack = do
  b <- satisfy (== '(') <|> satisfy (== ')')
  return (getBrack b)

parseLine :: ReadP Opt
parseLine = do
  o <- parseInt <|> parseOp <|> parseBrack
  return (o)

parseVal :: ReadP (Exp a)
parseVal = do
  v <- parseInt
  return $ (Val (read v))

parseExpOp :: ReadP (Exp a, Op)
parseExpOp = do
  l <- parseVal
  o <- parseOp
  return (l, o)

parseOpExp :: ReadP (Op, Exp a)
parseOpExp = do
  o <- parseOp
  r <- parseVal
  return (o, r)

parseExp :: ReadP (Exp a)
parseExp = do
  l  <- parseVal
  oe <- many parseOpExp
  return (foldl (\x (o, e) -> Exp x o e) l oe)

parseB :: ReadP (Exp a)
parseB = do
  satisfy (== '(')
  r <- parseExp
  satisfy (== ')')
  return r

parseNest :: ReadP (Op, Exp a)
parseNest = do
  o <- parseOp
  satisfy (== '(')
  r <- parseExp
  satisfy (== ')')
  return (o, r)

parseExp2 :: ReadP (Exp a)
parseExp2 = do
  l     <- parseExp
  opexp <- many parseNest
  return (foldl (\x (o, e) -> Exp x o e) l opexp)

evalString :: String -> Int
evalString s =
  let p = readP_to_S parseExp2 s
  in  case p of
        [] -> error "failed"
        x  -> let l = last x in eval (fst l)


--}









{--
-------------------------------------------------------------------------------

data Op = Add | Mult deriving (Show, Eq)
data Exp a = Val Int | Exp (Exp a) Op (Exp a) deriving Show

eval :: Exp a -> Int
eval (Val a       ) = a
eval (Exp e1 op e2) = case op of
  Add  -> eval e1 + eval e2
  Mult -> eval e1 * eval e2

test = "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"
test2 = "((5 * 9) * (((((7 * 3) * 3) + 9) * 3) + ((8 + 6) * 4)))"

a = Exp (Val 5) Mult (Val 9)
b = Exp (Val 7) Mult (Val 3)
c = Exp b Mult (Val 3)
d = Exp c Add (Val 9)
e = Exp d Mult (Val 3)
f = Exp (Val 8) Add (Val 6)
g = Exp f Mult (Val 4)
h = Exp e Add g
i = Exp a Mult h

-------------------------------------------------------------------------------

data Exp a = Val Int | Mult (Exp a) (Exp a) | Add (Exp a) (Exp a) deriving Show

eval :: Exp a -> Int
eval (Val a   ) = a
eval (Mult a b) = eval a * eval b
eval (Add  a b) = eval a + eval b

a = Mult (Val 5) (Val 9)
b = Mult (Val 7) (Val 3)
c = Mult b (Val 3)
d = Add c (Val 9)
e = Mult d (Val 3)
f = Add (Val 8) (Val 6)
g = Mult f (Val 4)
h = Add e g
i = Mult a h

test3 = "(9 * 3 + 4) * (7 + (2 + 3 + 6 * 2) + 3) * 4 * 5"

-------------------------------------------------------------------------------

expParser :: String -> [String]
expParser []       = []
expParser (x : xs) = case x of
  ' ' -> expParser xs
  _   -> [x] : expParser xs


parseSimple :: [String] -> Exp a
parseSimple [x] = Val (read x :: Int)
parseSimple xs =
  let (x : y : zs) = reverse xs
  in  if y == "+"
        then Add (parseSimple [x]) (parseSimple zs)
        else Mult (parseSimple [x]) (parseSimple zs)



expParser :: [(Exp a, Maybe Op)] -> String -> (Exp a, String)
expParser [(exp, op)] [] = case op of
  (Just o) -> error "gone wrong"
  Nothing  -> (exp, "")
expParser [(exp, op)] (x : xs) = case op of
  (Just o) -> error "gone wrong"
  Nothing  -> (exp, (x : xs))

expParser [] (x : xs)
  | isDigit x
  = expParser [(Val (read [x] :: Int), Nothing)] xs
  | x == '('
  = let (new, str) = expParser [] xs in expParser ([(new, Nothing)]) xs
  | otherwise
  = error "gone wrong1"

expParser ((exp, op) : es) (x : xs)
  | isDigit x = case op of
    (Just o) ->
      let new = Exp exp o (Val (read [x] :: Int))
      in  expParser ((new, Nothing) : es) xs
    Nothing ->
      let new = Val (read [x] :: Int) in expParser ((new, Nothing) : es) xs
  | x == '+' = case op of
    (Just o) -> error "gone wrong2"
    Nothing  -> expParser ((exp, Just Add) : es) xs
  | x == '*' = case op of
    (Just o) -> error "gone wrong3"
    Nothing  -> expParser ((exp, Just Mult) : es) xs
  | x == '(' = case op of
    (Just o) ->
      let (new, str) = expParser [] xs
      in  expParser ((new, Nothing) : (exp, op) : es) (str ++ xs)
                 --in if length str == 0 then (Exp exp o new, "") else error "gone wrong6"
    Nothing ->
      let (new, str) = expParser [] xs
      in  expParser ((new, Nothing) : (exp, op) : es) (str ++ xs)
  | x == ')' = case op of
    (Just o) -> error "gone wrong5"
    Nothing  -> case es of
      []                  -> (exp, xs)
      ((exp2, op2) : ess) -> case op2 of
        Just (o2) ->
          let newExp = Exp exp2 o2 exp
          in  expParser ((newExp, Nothing) : (tail es)) xs
        Nothing -> expParser ((exp, op) : (exp2, op2) : ess) xs


--}

main :: IO ()
main = do
  args     <- getArgs
  inHandle <- openFile (head args) ReadMode
  contents <- hGetContents inHandle
  let l  = map words $ lines contents
  mapM_ putStrLn (map (intercalate " ") l)
  -- print $ sum l3

