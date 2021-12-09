import AOCInputs

import qualified Data.Set as Set
import qualified Data.List as List

readInput :: [String] -> [[Integer]]

width :: [[Integer]] -> Int
height :: [[Integer]] -> Int
value :: Int -> Int -> [[Integer]] -> Integer
validCord :: Int -> Int -> [[Integer]] -> Bool

surrounding :: (Int -> Int -> Bool) -> Int -> Int -> [[Integer]] -> [(Int, Int)]

localMinimums :: [[Integer]] -> [(Int, Int)]
basin :: (Int, Int) -> [[Integer]] -> Set.Set (Int,Int)

run1 :: IO ()

readInput = (map . map) (\x -> read [x]::Integer)

width m = length $ head m
height m = length m
value x y m = (m !! y) !! x
validCord x y m =    0 <= x && x < width m
                  && 0 <= y && y < height m

surrounding predicate xIn yIn m = [ (x, y) | x <- [xIn-1 .. xIn+1], y <- [yIn-1 .. yIn+1]
                                     , not (x == xIn && y == yIn)
                                       && validCord x y m
                                       && predicate x y]


localMinimums m = [(x, y) | x <- [0 .. (width m) - 1], y <- [0 .. (height m) - 1], isLocalMin x y]
  where isLocalMin xIn yIn = null $ surrounding (\x y -> (value xIn yIn m) >= (value x y m)) xIn yIn m

basin (xIn, yIn) m = (Set.fromList currentSurrounding) `Set.union` next `Set.union` Set.singleton (xIn, yIn)
  where isPartOfBasin x y = let testValue = value x y m
                                currentValue = value xIn yIn m
                                horizontalOrVertialAligend = x == xIn || y == yIn
                                in testValue /= 9 && horizontalOrVertialAligend && testValue > currentValue
        currentSurrounding = surrounding isPartOfBasin xIn yIn m
        next = foldl (\acc (x, y) -> basin (x, y) m `Set.union` acc) Set.empty currentSurrounding

run1 =
  do ls <- readLines "puzzle.txt"
     let heightMap = readInput ls
     let localMins = localMinimums heightMap
     let localMinsValues = map (\(x, y) -> value x y heightMap) localMins
     print $ "localMins: " ++ show localMins
     print $ "localMins sum: " ++ show (sum $ map (+1) localMinsValues)
     let basins = map (\localMin -> basin localMin heightMap) localMins
     let basinSizes = List.sort $ map Set.size basins
     print $ "basins count: " ++ show (length basins)
     print $ "basins sizes: " ++ show basinSizes
     print $ "three largest basins: " ++ show (take 3 $ reverse basinSizes)
     print $ "puzzle result: " ++ show (product $ take 3 $ reverse basinSizes)


