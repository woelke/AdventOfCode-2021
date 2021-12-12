import AOCInputs

import GHC.Unicode
import qualified Data.Set as Set

data Cave = Start [Cave]
          | End
          | P String [Cave] deriving Show

readPlan :: [String] -> [(String, String)]
findPaths :: String -> [(String, String)] -> ([String], [(String, String)])

trav :: Cave -> [[String]]
build :: [(String, String)] -> Cave

dropDeadEnds :: [[String]] -> [[String]]
dropDuplicates :: [[String]] -> [[String]]

run1 :: IO ()

readPlan = map (\x -> let [a, b] = splitLine (atString "-") x in (a,b))

findPaths _ [] = ([], [])
findPaths from ((a,b):xs)
  | from == a = (b:tos, reducedPlanIfSmallCave)
  | from == b = (a:tos, reducedPlanIfSmallCave)
  | otherwise = (tos, (a,b):remainingPlan)
  where (tos, remainingPlan) = findPaths from xs
        reducedPlanIfSmallCave = if isUpper (head from)
                                    then (a,b):remainingPlan
                                    else remainingPlan

trav cave =
  case cave of
    End -> [["end"]]
    Start [] -> error "A cave without tunnels"
    Start xs -> "start" `prependToEach` next xs
    P name [] -> [[name]]
    P name xs -> name `prependToEach` next xs
  where prependToEach p = map (p:)
        next xs = foldl (\acc x -> trav x ++ acc) [] xs

build plan = buildDepth "start" plan True
  where buildDepth name p keepOneCave
          | name == "start" = Start (next False keepOneCave)
          | name == "end" = End
          | otherwise = P name (if keepOneCave then next True False ++ next False True else next False False)
            where next keepCaveFind keepCaveNext = let (nextCaveNames, remainingPlan) = findPaths name p
                                                    in map (\nextCaveName -> buildDepth nextCaveName (if keepCaveFind then p else remainingPlan) keepCaveNext) nextCaveNames

dropDeadEnds = filter (\xs -> last xs == "end")
dropDuplicates paths = Set.toList (Set.fromList paths)


-- [[start, a, end], [start, b, end]]
-- >>>>
-- start [[a, end], [b, end]]
-- a merge [[end]], [b] merge [[end]]
--
-- test2 = Start [P "a" [End], P "b" [End]]


-- test = Start [P "A" [P "c" [], End], P "b" [P "d" [], End]]
-- test1 = Start [P "A" [End], P "b" [End]]

run1 =
  do ls <- readLines "puzzle.txt"
     let plan = readPlan ls
     let cave = build plan
     let allPaths = trav cave
     let noDeadEndPaths = dropDeadEnds allPaths
     let noDuplicatesDeadEnds = dropDuplicates noDeadEndPaths
     -- print plan
     -- print cave
     -- print noDuplicatesDeadEnds
     print $ show (length noDuplicatesDeadEnds)

