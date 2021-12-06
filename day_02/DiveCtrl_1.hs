-- Day 1

import AOCInputs

--import System.IO


data DiveControl = Forward | Down | Up deriving Show
data DiveCommand = Cmd DiveControl Integer deriving Show

data DivePosition = Pos Integer Integer deriving Show

readDiveCmd :: String -> DiveCommand
readDiveCtrl :: String -> DiveControl

changePos :: DivePosition -> DiveCommand-> DivePosition
puzzleResult :: DivePosition -> Integer


readDiveCtrl str
  | str == "forward" = Forward
  | str == "down" = Down
  | str == "up" = Up
  | otherwise = error "Unknown DiveCtrl"

readDiveCmd cmd = Cmd (readDiveCtrl a)  (read b::Integer)
  where [a, b] = words cmd

changePos (Pos h d) (Cmd Forward x) = Pos (h + x) d
changePos (Pos h d) (Cmd Down x) = Pos  h (d + x)
changePos (Pos h d) (Cmd Up x) = Pos h (d - x)

puzzleResult (Pos x y) = x * y

run1 :: IO ()

run1 =
  do cmds <- readLines (puzzleFile 1)
     let diveCmds = map readDiveCmd cmds
     let pos = foldl changePos (Pos 0 0) diveCmds
     print $ "current pos: " ++ show pos
     print $ "puzzle Result: " ++ show (puzzleResult pos)

     --print "Position is "
     --print (countIncreased numList)

--run2 :: IO ()

--run2 =
  --do numList <- readIntegerList (puzzleFile 1)
     --print "Sonar value increased by "
     --print (countIncreased (trippleCount numList))
  --where trippleCount = map3 (\x y z -> x + y + z)
