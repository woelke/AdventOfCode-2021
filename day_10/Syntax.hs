import AOCInputs

import Data.Maybe
import Data.List

parseSyntax :: String -> (Bool, Char, String)

parseSyntax [] = (True, 'e', [])
parseSyntax [c] = (False, c, [])
parseSyntax (c:cs)
  | isOpen c = if hasError
                  then if nextOrErrChar == 'e'
                          then (hasError, nextOrErrChar, c : remainingCs)
                          else (hasError, nextOrErrChar, remainingCs)
                  else if nextOrErrChar `isCounterPartof` c
                       then parseSyntax remainingCs
                       else (True, nextOrErrChar, remainingCs)
  | otherwise = (False, c, cs)
  where (hasError, nextOrErrChar , remainingCs) = parseSyntax cs
        isOpen x = x `elem` ['[', '{', '<', '(']
        isCounterPartof x y = y == (fromMaybe 'x' $ lookup x [(')', '('), (']','['), ('>', '<'), ('}', '{')])

parseSyntax1 [] = (True, 'e', [])
parseSyntax1 [c] = (False, c, [])
parseSyntax1 (c:cs)
  | isOpen c = if hasError
                  then if nextOrErrChar == 'e'
                          then (hasError, nextOrErrChar, c : remainingCs)
                          else (hasError, nextOrErrChar, c : remainingCs)
                  else if nextOrErrChar `isCounterPartof` c
                       then parseSyntax1 remainingCs
                       else (True, nextOrErrChar, c : nextOrErrChar : remainingCs)
  | otherwise = (False, c, cs)
  where (hasError, nextOrErrChar , remainingCs) = parseSyntax1 cs
        isOpen x = x `elem` ['[', '{', '<', '(']
        isCounterPartof x y = y == (fromMaybe 'x' $ lookup x [(')', '('), (']','['), ('>', '<'), ('}', '{')])

score1 (True, 'e', _) = 0
score1 (True, ')', _) = 3
score1 (True, ']', _) = 57
score1 (True, '}', _) = 1197
score1 (True, '>', _) = 25137
score1 _  = 0

score2 xs = foldl (\acc x -> acc*5 + score x) 0 xs
  where score c = fromJust $ lookup c [(')', 1), (']', 2), ('}', 3), ('>', 4)]

third (_,_,x) = x

inverseBrackets xs = map counterPart $ reverse xs
  where counterPart b = fromJust $ lookup b [('(', ')'), ('[',']'), ('<', '>'), ('{', '}')]

run1 =
  do ls <- readLines "puzzle.txt"
     let scores1 = map (score1 . parseSyntax)  ls
     let lsZeroScores = map snd $ filter (\(s, _) -> s == 0) $ zip scores1 ls
     print scores1
     print lsZeroScores

     let scores2 = map score2 $ map (inverseBrackets . third . parseSyntax1) lsZeroScores
     print scores2
     let sortedScores2 = sort scores2
     print sortedScores2
     print $ sortedScores2 !! ((length sortedScores2) `div` 2)


