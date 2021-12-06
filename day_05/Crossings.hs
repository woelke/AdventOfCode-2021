-- Day 4

import AOCInputs

import qualified Data.Map as Map

readPointPairs :: [String] -> [((Integer, Integer), (Integer, Integer))]
readPointPair :: String -> ((Integer, Integer), (Integer, Integer))
isHorizontal :: ((Integer, Integer), (Integer, Integer)) -> Bool
isVertical :: ((Integer, Integer), (Integer, Integer)) -> Bool
getPointsFromPointPair :: ((Integer, Integer), (Integer, Integer)) -> [(Integer,Integer)]
addPoints :: [(Integer,Integer)] -> Map.Map (Integer,Integer) Integer -> Map.Map (Integer,Integer) Integer
countCrossings :: Map.Map (Integer,Integer) Integer -> Int

readPointPairs = map readPointPair

readPointPair str = toPair (map strListToIntPair (toListList str))
  where toListList x = map (splitLine (atString ",")) $ splitLine (atString " -> ") x
        strListToIntPair xs =  (read (head xs)::Integer, read (last xs)::Integer)
        toPair [x, y] = (x, y)
        toPair _ = error "cannot be converted to a pair"

isHorizontal ((_,y1), (_,y2)) = y1 == y2
isVertical ((x1,_), (x2,_)) = x1 == x2

getPointsFromPointPair t
  | isHorizontal t = hPoints t
  | isVertical t = vPoints t
  | otherwise  = dPoints t
  where vPoints ((x1,y1), (_,y2)) = map (\v -> (x1, v)) [min y1 y2 .. max y1 y2]
        hPoints ((x1,y1), (x2,_)) = map (\v -> (v, y1)) [min x1 x2 .. max x1 x2]
        dPoints (p1, p2) = if p1 == p2 then [p1] else p1 : dPoints (p1 `pAdd` summand p1 p2, p2)
          where summand (x1,y1) (x2,y2) = (if x1 < x2 then 1 else (-1), if y1 < y2 then 1 else (-1))
                pAdd (a,b) (c,d) = (a + c, b + d)

addPoints ts worldMap = foldr addPoint worldMap ts
  where addPoint t m = case Map.lookup t m of
                          Just x -> Map.insert t (x + 1) m
                          Nothing -> Map.insert t 1 m

countCrossings m = length $ filter (> 1) (Map.elems m)

run1 :: IO ()

run1 =
  do ls <- readLines "puzzle.txt"
     let pointPairs = readPointPairs ls
     let points = foldl (\acc ppair -> acc ++ getPointsFromPointPair ppair) [] pointPairs
     let worldMap = addPoints points Map.empty
     let crossings = countCrossings worldMap
     -- print pointPairs
     -- print worldMap
     print crossings

