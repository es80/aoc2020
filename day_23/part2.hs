{-# LANGUAGE BangPatterns #-}

import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IntMap
import           System.Environment
import           System.IO

maxCup = 1000000 :: Int
cupNums = [4, 7, 6, 1, 3, 8, 2, 5, 9] ++ [10 .. maxCup] :: [Int]

type Cups = IntMap Int

mkMap :: Int -> [Int] -> Cups
mkMap head [x] = IntMap.insert x head IntMap.empty
mkMap head (x : y : ys) =
  let cups = mkMap head (y : ys) in IntMap.insert x y cups

pickUp :: Int -> Cups -> ([Int], Cups, Int)
pickUp ptr cups =
  let first  = cups IntMap.! ptr
      second = cups IntMap.! first
      third  = cups IntMap.! second
      newPtr = cups IntMap.! third
      picked = [first, second, third]
      cups'  = IntMap.insert ptr newPtr cups
  in  (picked, cups', newPtr)

findDest :: Int -> [Int] -> Int
findDest ptr picked =
  let pred = ptr - 1
  in  if pred == 0
        then findDest (maxCup + 1) picked
        else if pred `notElem` picked then pred else findDest pred picked

insert :: Int -> [Int] -> Cups -> Cups
insert dest picked cups =
  let !destNext = cups IntMap.! dest
      [x, y, z] = picked
  in  IntMap.insert dest x (IntMap.insert z destNext cups)

moves :: Int -> Int -> Cups -> (Int, Cups)
moves 0 ptr cups = (ptr, cups)
moves n ptr cups =
  let (picked, newCups, nextPtr) = pickUp ptr cups
      dest                       = findDest ptr picked
      !replaced                  = insert dest picked newCups
  in  moves (n - 1) nextPtr replaced

playGame :: Int -> [Int] -> Cups
playGame numMoves cupNums =
  let firstPtr       = head cupNums
      cups           = mkMap firstPtr cupNums
      (_, finalCups) = moves numMoves firstPtr cups
  in  finalCups

getAnswer :: Cups -> Int
getAnswer cups =
  let clockwise1 = cups IntMap.! 1
      clockwise2 = cups IntMap.! clockwise1
  in  clockwise1 * clockwise2

main = print $ getAnswer $ playGame 10000000 cupNums

