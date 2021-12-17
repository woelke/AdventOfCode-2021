{-# LANGUAGE TupleSections #-}

import AOCInputs
import Matrix1
import Data.Maybe

import Data.List

-- import qualified Data.Map as Map
-- import qualified Data.Set as Set

readInput :: [String] -> [[(Integer, Maybe Integer)]]
expand :: Integer -> Integer -> [[(Integer, Maybe Integer)]] -> [[(Integer, Maybe Integer)]]

dijkstra :: (Int, Int) -> [[(Integer, Maybe Integer)]] -> [[(Integer, Maybe Integer)]]
dijkstraInner :: [[(Integer, Maybe Integer)]] -> [(Integer, (Int, Int))]-> [[(Integer, Maybe Integer)]]

run1 :: IO ()

readInput xs = (map . map) (, Nothing) $ readSingleDigitMatrix xs

dijkstra (x, y) m = dijkstraInner m [(0,(x,y))]

dijkstraInner m [] = m
dijkstraInner m ((travelCostQueue, (x, y)):prioQueue) = if isNothing posTravelCost || newTravelCost < fromJust posTravelCost
                                                           then dijkstraInner mAfterProcessedPrioQueue updatedPrioQueue
                                                           else dijkstraInner m prioQueue
  where (posCost, posTravelCost) = value x y m
        newTravelCost = travelCostQueue + posCost
        notDiagonal x1 y1 = x1 == x || y1 == y
        surroundingPoses = surrounding notDiagonal x y m
        mAfterProcessedPrioQueue = set (posCost, Just newTravelCost) x y m
        updatedPrioQueue = foldl' (\acc (x1, y1) -> insert (newTravelCost, (x1, y1)) acc) prioQueue surroundingPoses

expand xCount yCount m = expandColumns yCount $ map (expandRows xCount) m
  where increaseTuple increaseBy (posCost, travelCost) = (((posCost + increaseBy - 1) `mod` 9) + 1, travelCost)
        expandRows 0 xs = xs
        expandRows count xs = expandRows (count - 1) xs ++ map (increaseTuple count) xs
        expandColumns 0 ys = ys
        expandColumns count ys = expandColumns (count - 1) ys ++ (map . map) (increaseTuple count) ys

run1 =
  do ls <- readLines "puzzle.txt"
     let m = readInput ls
     let mExpanded = expand 4 4 m
     let dijkstradMap = dijkstra (0,0) mExpanded
     let cost = fromJust $ snd $ last $ last dijkstradMap
     -- putStr $ showMatrix $ (map . map) fst dijkstradMap
     print $ cost - fst (value 0 0 m)

