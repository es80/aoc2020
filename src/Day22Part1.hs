import           Data.List.Split
import           System.Environment
import           System.IO

maybeHead :: [a] -> Maybe a
maybeHead []       = Nothing
maybeHead (x : xs) = Just x

data Player = PlayerOne | PlayerTwo deriving Show
type Deck1 = [Int]
type Deck2 = [Int]

getStartingDecks :: String -> (Deck1, Deck2)
getStartingDecks fileContent =
  let split      = splitOn [""] (lines fileContent)
      numStrings = map tail split
      ints       = map (map read) numStrings
  in  (head ints, last ints)

playRounds :: (Deck1, Deck2) -> (Player, [Int])
playRounds (ply1, ply2) =
  let rest1 = tail ply1
      rest2 = tail ply2
  in  case maybeHead ply1 of
        Nothing -> (PlayerTwo, ply2)
        Just c1 -> case maybeHead ply2 of
          Nothing -> (PlayerOne, ply1)
          Just c2 -> if c1 > c2
            then playRounds (rest1 ++ [c1, c2], rest2)
            else playRounds (rest1, rest2 ++ [c2, c1])

getWinningDeck :: (Deck1, Deck2) -> [Int]
getWinningDeck startingDecks = snd $ playRounds startingDecks

getScore :: [Int] -> Int
getScore nums = let len = length nums in sum $ zipWith (*) [len, len - 1 ..] nums

main = do
  args     <- getArgs
  inHandle <- openFile (head args) ReadMode
  contents <- hGetContents inHandle
  print $ getScore $ getWinningDeck $ getStartingDecks contents

