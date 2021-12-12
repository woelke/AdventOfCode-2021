-- Matrix helper functions

module Matrix1 where

import AOCInputs

readSingleDigitMatrix :: [String] -> [[Integer]]

width :: [[Integer]] -> Int
height :: [[Integer]] -> Int
value :: Int -> Int -> [[Integer]] -> Integer
validCord :: Int -> Int -> [[Integer]] -> Bool

surrounding :: (Int -> Int -> Bool) -> Int -> Int -> [[Integer]] -> [(Int, Int)]

setWith :: (Integer -> Integer -> Integer) -> Integer -> Int -> Int -> [[Integer]] -> [[Integer]]
set :: Integer -> Int -> Int -> [[Integer]] -> [[Integer]]

showMatrix :: [[Integer]] -> String

readSingleDigitMatrix = (map . map) (\x -> read [x]::Integer)

width m = length $ head m
height = length
value x y m = (m !! y) !! x
validCord x y m =    0 <= x && x < width m
                  && 0 <= y && y < height m

surrounding predicate xIn yIn m = [ (x, y) | x <- [xIn-1 .. xIn+1], y <- [yIn-1 .. yIn+1]
                                     , not (x == xIn && y == yIn)
                                       && validCord x y m
                                       && predicate x y]

setWith fun v x y m = let newLine = swapAt (fun v $ value x y m) x (m !! y)
                       in swapAt newLine y m
  where swapAt e pos ls = let (a,b) = splitAt pos ls
                           in a ++ (e : tail b)

set = setWith const

showMatrix [] = ""
showMatrix m = foldl (\acc l -> acc ++ showLine l ++ "\n") "" m
  where showLine (x:xs) = foldl (\acc xIn -> acc ++ (' ' : show xIn)) (show x) xs
        showLine [] = ""

