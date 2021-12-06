-- Day 1

import AOCInputs
import System.IO

countIncreased :: [Integer] -> Integer

countIncreased [] = 0
countIncreased [_] = 0
countIncreased (x:x':xs) =
  if x < x' then
    1 + countIncreased (x':xs)
  else
    countIncreased (x':xs)

map3 :: (a -> a -> a -> b) -> [a] -> [b]

map3 _ [] = []
map3 _ [_] = []
map3 _ [_, _] = []
map3 f [x, y, z] = [f x y z]
map3 f (x:y:z:ls) = f x y z : map3 f (y:z:ls)


run1 :: IO ()

run1 =
  do numList <- readIntegerList (puzzleFile 1)
     print "Sonar value increased by "
     print (countIncreased numList)

run2 :: IO ()

run2 =
  do numList <- readIntegerList (puzzleFile 1)
     print "Sonar value increased by "
     print (countIncreased (trippleCount numList))
  where trippleCount = map3 (\x y z -> x + y + z)
