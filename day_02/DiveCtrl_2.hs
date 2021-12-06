-- Day 1

import AOCInputs

--import System.IO


data DiveControl = Forward | Down | Up deriving Show
data DiveCommand = Cmd DiveControl Integer deriving Show

data DivePosition = Pos Integer Integer Integer deriving Show

readDiveCmd :: String -> DiveCommand
readDiveCtrl :: String -> DiveControl

changePos :: DivePosition -> DiveCommand-> DivePosition
puzzleResult :: DivePosition -> Integer
startPos :: DivePosition


readDiveCtrl str
  | str == "forward" = Forward
  | str == "down" = Down
  | str == "up" = Up
  | otherwise = error "Unknown DiveCtrl"

readDiveCmd cmd = Cmd (readDiveCtrl a)  (read b::Integer)
  where [a, b] = words cmd

changePos (Pos h d a) (Cmd Down x) = Pos h d (a + x)
changePos (Pos h d a) (Cmd Up x) = Pos h d (a - x)
changePos (Pos h d a) (Cmd Forward x) = Pos (h + x) (d + (a*x)) a

puzzleResult (Pos h d _) = h * d

startPos = Pos 0 0 0

run2 :: IO ()

run2 =
  do cmds <- readLines (puzzleFile 1)
     let diveCmds = map readDiveCmd cmds
     let pos = foldl changePos startPos diveCmds
     print $ "current pos: " ++ show pos
     print $ "puzzle Result: " ++ show (puzzleResult pos)
