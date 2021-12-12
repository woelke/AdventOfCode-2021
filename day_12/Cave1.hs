import AOCInputs

import GHC.Unicode

data Cave = Start [Cave]
          | End
          | P String [Cave] deriving Show

readPlan :: [String] -> [(String, String)]
findPaths :: String -> [(String, String)] -> ([String], [(String, String)])

trav :: Cave -> [[String]]
build :: [(String, String)] -> Cave

dropDeadEnds :: [[String]] -> [[String]]

run1 :: IO ()

readPlan = map (\x -> let [a, b] = splitLine (atString "-") x in (a,b))

findPaths _ [] = ([], [])
findPaths from ((a,b):xs)
  | from == a = (b:tos, reducedPlanIfSmallCave)
  | from == b = (a:tos, reducedPlanIfSmallCave)
  | otherwise = (tos, (a,b):remainingPlan)
  where (tos, remainingPlan) = findPaths from xs
        reducedPlanIfSmallCave = if isUpper (head from) then (a,b):remainingPlan else remainingPlan

trav cave =
  case cave of
    End -> [["end"]]
    Start [] -> error "A cave without tunnels"
    Start xs -> "start" `prependToEach` next xs
    P name [] -> [[name]]
    P name xs -> name `prependToEach` next xs
  where prependToEach p = map (p:)
        next xs = foldl (\acc x -> trav x ++ acc) [] xs

build = buildDepth "start"
  where buildDepth name p
          | name == "start" = Start next
          | name == "end" = End
          | otherwise = P name next
            where next = let (nextCaveNames, remainingPlan) = findPaths name p
                          in map (`buildDepth` remainingPlan) nextCaveNames

dropDeadEnds = filter (\xs -> last xs == "end")


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
     let paths = trav cave
     let noDeadEndPaths = dropDeadEnds paths
     -- print plan
     -- print cave
     print noDeadEndPaths
     print $ show (length noDeadEndPaths)

