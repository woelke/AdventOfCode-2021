import AOCInputs

import Matrix1

run1 :: IO ()

-- import Data.Maybe
-- import Data.List

step :: [[Integer]] -> (Int, [[Integer]])

processFlashes :: [[Integer]] -> (Int, [[Integer]])

step energyMap = processFlashes $ increaseEnergy energyMap
  where increaseEnergy m = map (map (+1)) m

processFlashes energyMap = let flashReadyPoss = findFlashReadyPoss energyMap
                               processedMap = processFlashIteration flashReadyPoss energyMap
                               (nextFlashCount, nextMap) = processFlashes processedMap
                               in if null flashReadyPoss
                                     then (0, processedMap)
                                     else (length flashReadyPoss + nextFlashCount, nextMap)
  where findFlashReadyPoss m = [ (x, y)  | x <- [0 .. width m - 1], y <- [0 .. height m - 1], value x y m > 9]
        processFlash (x, y) m = set 0 x y $ foldl (\acc (xIn, yIn) -> setWith conditionalAdd 1 xIn yIn acc) m affectedPoss
          where conditionalAdd newV oldV = if oldV == 0 then 0 else newV + oldV
                notFlashed xIn yIn = value xIn yIn m /= 0
                affectedPoss = surrounding notFlashed x y m
        processFlashIteration flashPoss m = foldl (flip processFlash) m flashPoss

steps count energyMap = foldl (\(c, m) _ -> let (nextC, nextM) = step m in (c + nextC, nextM)) (0, energyMap) [1 .. count]

stepsUntilSync energyMap
  | sum (map sum energyMap) == 0 =(0, energyMap)
  | otherwise = let (nextC, nextM) = stepsUntilSync $ snd $ step energyMap
                 in (1 + nextC, nextM)


run1 =
  do ls <- readLines "puzzle.txt"
     let startMap = readSingleDigitMatrix ls
     putStrLn $ showMatrix startMap
     let stepX = stepsUntilSync startMap
     -- let stepX = steps 100 startMap
     print $ "flash count: " ++ show (fst stepX)
     putStrLn $ showMatrix $ snd stepX



