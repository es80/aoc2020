import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IntMap
import           Data.List
import           System.Environment
import           System.IO

inputNums = [0, 20, 7, 16, 1, 18, 15] :: [Int]

type Game = IntMap Int
type Spoken = Int
type Turn = Int

takeTurn :: Game -> Spoken -> Turn -> (Game, Spoken)
takeTurn game prevSpoken currTurn = case IntMap.lookup prevSpoken game of
  Just i  -> (IntMap.insert prevSpoken currTurn game, currTurn - i)
  Nothing -> (IntMap.insert prevSpoken currTurn game, 0)

playGame :: [Int] -> Turn -> (Game, Spoken)
playGame nums t =
  let indices     = [length nums .. t]
      initialGame = IntMap.fromList $ zip nums [1 ..]
  in  foldl' (uncurry takeTurn) (initialGame, last nums) indices

main :: IO ()
main = print $ snd $ playGame inputNums (30000000 - 1)

