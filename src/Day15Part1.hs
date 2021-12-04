import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IntMap

input = [0, 20, 7, 16, 1, 18, 15] :: [Int]

type Game = IntMap Int
type Spoken = Int
type Turn = Int

setupGame :: [Int] -> Game
setupGame nums = IntMap.fromList $ zip nums [1 ..]

takeTurn :: Game -> Turn -> Spoken -> (Game, Spoken)
takeTurn game currTurn prevSpoken = case IntMap.lookup prevSpoken game of
  Just i  -> (IntMap.insert prevSpoken currTurn game, currTurn - i)
  Nothing -> (IntMap.insert prevSpoken currTurn game, 0)

takeNTurns :: [Int] -> Turn -> (Game, Spoken)
takeNTurns nums n | n < length nums = (setupGame nums, nums !! n)
                  | otherwise       = let (g, s) = takeNTurns nums (n - 1) in takeTurn g n s

main :: IO ()
main = print $ snd $ takeNTurns input (2020 - 1)

