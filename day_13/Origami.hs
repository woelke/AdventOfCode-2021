import AOCInputs
import Matrix1

import Data.List
import qualified Data.Set as Set

data FoldCmd = Horizontal Integer
             | Vertical Integer deriving Show

readInput :: [String] -> (Set.Set (Integer, Integer), [FoldCmd])

widthPaper :: Set.Set (Integer, Integer) -> Integer
heightPaper :: Set.Set (Integer, Integer) -> Integer

resizePaper :: Integer -> Integer -> Set.Set (Integer, Integer) -> Set.Set (Integer, Integer)
inverseHorizontal :: Set.Set (Integer, Integer) -> Integer -> Set.Set (Integer, Integer)
inverseVertical :: Set.Set (Integer, Integer) -> Integer -> Set.Set (Integer, Integer)

showPaper :: Set.Set (Integer, Integer) -> String

foldPaper :: FoldCmd -> Set.Set (Integer, Integer) -> Set.Set (Integer, Integer)

readInput = foldl (flip parseLine) (Set.empty, [])
  where parseLine l (paper, cmds)
          | ',' `elem` l = (Set.insert (parseCordinates l) paper, cmds)
          | '=' `elem` l = (paper, cmds ++ [parseFoldCmd l])
          | otherwise = (paper, cmds)
        parseCordinates l = let [x, y] = splitLine (atString ",") l
                             in (read x::Integer, read y::Integer)
        parseFoldCmd l = let [cmd, p] = splitLine (atString "=") l
                          in if cmd == "fold along y"
                                then Horizontal (read p::Integer)
                                else Vertical (read p::Integer)

widthPaper p = 1 + Set.foldl (\acc (x, _) -> if x < acc then acc else x) 0 p
heightPaper p = 1 + Set.foldl (\acc (_, y) -> if y < acc then acc else y) 0 p

resizePaper newWidth newHeight paper = foldr Set.delete paper outOfScopeEntries
  where outOfScopeEntries = Set.foldl (\acc (x, y) -> if x >= newWidth || y >= newHeight then (x,y):acc else acc) [] paper

inverseHorizontal p w = Set.foldl (\acc (x, y) -> Set.insert (w-x, y) acc) Set.empty p

inverseVertical p h = Set.foldl (\acc (x, y) -> Set.insert (x, h-y) acc) Set.empty p

showPaper p = showMatrix $ Set.foldl (\acc (x,y) -> set 1 (fromIntegral x) (fromIntegral y) acc) emptyPaper p
  where emptyPaper = replicate (fromIntegral (heightPaper p)) (emptyLine (fromIntegral (widthPaper p)))
        emptyLine w = replicate w 0

foldPaper cmd paper =
  case cmd of
    Horizontal a -> resizePaper (widthPaper paper) a $ inverseVertical paper (2*a) `Set.union` paper
    Vertical a -> resizePaper a (heightPaper paper) $ inverseHorizontal paper (2*a) `Set.union` paper

processCmds cmds paper = foldl (flip foldPaper) paper cmds

run1 =
  do ls <- readLines "puzzle.txt"
     let (paper, cmds) = readInput ls
     let foldedPaper = processCmds cmds paper
     -- putStr $ showPaper paper
     print cmds
     putStr $ showPaper foldedPaper
     print $ show (Set.size foldedPaper)
