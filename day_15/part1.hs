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
takeNTurns nums n
  | n < length nums = (setupGame nums, nums !! n)
  | otherwise       = let (g, s) = takeNTurns nums (n - 1) in takeTurn g n s

main :: IO ()
main = print $ snd $ takeNTurns input (2020 - 1)

{--
initIndices :: [Int] -> Int -> [Int]
initIndices nums n = [length nums .. n]

takeTurns :: [Int] -> Spoken -> Game -> ([Int], Spoken, Game)
takeTurns []       s g = ([], s, g)
takeTurns (x : xs) s g = let !(ga, sp) = takeTurn g x s in takeTurns xs sp ga

playGame :: [Int] -> Int -> ([Int], Spoken, Game)
playGame nums n = takeTurns (initIndices nums n) (last nums) (setupGame nums)
--}

