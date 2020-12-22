import           Data.List.Split
import           System.Environment
import           System.IO
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set

maybeHead :: [a] -> Maybe a
maybeHead []       = Nothing
maybeHead (x : xs) = Just x

data Player = PlayerOne | PlayerTwo deriving Show
type DeckOne = [Int]
type DeckTwo = [Int]

getStartingDecks :: String -> (DeckOne, DeckTwo)
getStartingDecks fileContent =
  let split      = splitOn [""] (lines fileContent)
      numStrings = map tail split
      ints       = map (map read) numStrings
  in  (head ints, last ints)



playRounds
  :: (DeckOne, DeckTwo) -> (Set DeckOne, Set DeckTwo) -> (Player, [Int])
playRounds (ply1, ply2) (previous1, previous2) =
  if Set.member ply1 previous1 && Set.member ply2 previous2 then (PlayerOne, []) else
      let rest1       = tail ply1
          rest2       = tail ply2
          newPrevious = (Set.insert ply1 previous1, Set.insert ply2 previous2)
      in  case maybeHead ply1 of
            Nothing -> (PlayerTwo, ply2)
            Just c1 -> case maybeHead ply2 of
              Nothing -> (PlayerOne, ply1)
              Just c2 -> if length rest1 >= c1 && length rest2 >= c2 then
                  let (subWinner, _) = playRounds
                        (take c1 rest1, take c2 rest2)
                        (Set.empty    , Set.empty)
                  in  case subWinner of
                        PlayerOne -> plyOneWon
                        PlayerTwo -> plyTwoWon
                else if c1 > c2 then plyOneWon else plyTwoWon
                    where plyOneWon = playRounds (rest1, rest2 ++ [c2, c1]) newPrevious
                          plyTwoWon = playRounds (rest1 ++ [c1, c2], rest2) newPrevious


  {--
playRounds :: (DeckOne, DeckTwo) -> (Player, [Int])
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
            --}







getWinningDeck :: (DeckOne, DeckTwo) -> [Int]
getWinningDeck startingDecks =
  snd $ playRounds startingDecks (Set.empty, Set.empty)

getScore :: [Int] -> Int
getScore nums =
  let len = length nums in sum $ zipWith (*) [len, len - 1 ..] nums

main = do
  args     <- getArgs
  inHandle <- openFile (head args) ReadMode
  contents <- hGetContents inHandle
  print $ getScore $ getWinningDeck $ getStartingDecks contents





