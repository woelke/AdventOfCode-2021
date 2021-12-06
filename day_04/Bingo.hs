-- Day 4

import AOCInputs
import Matrix

readGame :: [String] -> ([Integer], [[[(Bool, Integer)]]])
readNumbers :: [String] -> ([Integer], [String])
readFields :: [String] -> [[[(Bool, Integer)]]]
showFields :: [[[(Bool, Integer)]]] -> String
markFields :: Integer -> [[[(Bool, Integer)]]] -> [[[(Bool, Integer)]]]

readField :: [String] -> ([[(Bool, Integer)]], [String])
showField :: [[(Bool, Integer)]] -> String
markField :: Integer -> [[(Bool, Integer)]] -> [[(Bool, Integer)]]

isFieldaWin :: [[(Bool, Integer)]] -> Bool

markFieldsUntilFirstWon :: [Integer] -> [[[(Bool,Integer)]]] -> ([[(Bool,Integer)]], Integer)
markFieldsUntilLastWon :: [Integer] -> [[[(Bool,Integer)]]] -> ([[(Bool,Integer)]], Integer)
puzzleResult :: [[(Bool, Integer)]] -> Integer -> Integer

run1 :: IO ()
run2 :: IO ()

readGame xs = (numbers, fields)
  where (numbers, _:nextLines) = readNumbers xs
        fields = readFields nextLines

readNumbers [] = error "no numbers found"
readNumbers (x:xs) = (toIntList $ toStrList x, xs)
  where toStrList = splitLine (atChar ',')
        toIntList = map (\y -> read y::Integer)

readFields [] = []
readFields ("":xss) = readFields xss
readFields xss = field : readFields next
  where (field, next) = readField xss

showFields f = concat $ strFieldList f
  where strFieldList = map showField
markFields n = map (markField n)

readField [] = ([], [])
readField (x:xs)
  | not (null x) = (row : fieldParts, remainingXs)
  | otherwise = ([], xs)
    where row = map (\y -> (False, read y::Integer)) (splitLine atSpace x)
          (fieldParts, remainingXs) = readField xs

showField [] = "\n"
showField (c:cs) = showLine c ++ showField cs
  where showLine [] = "\n"
        showLine (t:ts) = showTuple t ++ showLine ts
        showTuple (True, value) = "<" ++ show value ++ "> "
        showTuple (False, value) = show value ++ " "

markField n = map . map $ mark
  where mark (marked, value)
          | value == n =  (True, n)
          | otherwise = (marked, value)

isFieldaWin m = rowWin || colWin
  where rowWin =  rowCheck m
        colWin = rowCheck $ transposeM m
        rowCheck matrix = any (all isMarked) matrix
        isMarked (marked, _) = marked

markFieldsUntilFirstWon [] _ = error "no win found"
markFieldsUntilFirstWon (n:numbers) fields =
  case getWonField markedFields of
    Just x -> (x, n)
    Nothing -> markFieldsUntilFirstWon numbers markedFields
  where markedFields = markFields n fields
        getWonField [] = Nothing
        getWonField (f:fs) = if isFieldaWin f then Just f else getWonField fs

markFieldsUntilLastWon _ [] = error "out of fields"
markFieldsUntilLastWon [] _ = error "no win Found"
markFieldsUntilLastWon (n:numbers) fields =
  case getWonFields markedFields of
    [] -> markFieldsUntilLastWon numbers markedFields
    [x] -> if length fields == 1 then (x, n) else markFieldsUntilLastWon numbers (erease [x] markedFields)
    (x:xs) -> markFieldsUntilLastWon numbers (erease (x:xs) markedFields)
  where markedFields = markFields n fields
        getWonFields fs = foldl (\acc x ->  if isFieldaWin x then x : acc else acc) [] fs
        erease [] xs = xs
        erease (e:es) xs = erease es $ filter (/= e) xs

puzzleResult successMatrix successNumber = sumMatrix successMatrix * successNumber
  where sumRow row = sum $ map (\(marked, value) -> if not marked then value else 0) row
        sumMatrix m = foldl (\acc row -> sumRow row + acc) 0 m

run1 =
  do gameLines <- readLines "puzzle.txt"
     let (numbers, fields) = readGame gameLines
     let (successField, successNumber) = markFieldsUntilFirstWon numbers fields
     putStr $ "numbers: " ++ show numbers ++ "\n"
     putStr $ "win: \n" ++ showField successField
     print $ "successNumber: " ++ show successNumber
     print $ "puzzleResult: " ++ show (puzzleResult successField successNumber)

run2 =
  do gameLines <- readLines "puzzle.txt"
     let (numbers, fields) = readGame gameLines
     let (successField, successNumber) = markFieldsUntilLastWon numbers fields
     putStr $ "numbers: " ++ show numbers ++ "\n"
     putStr $ "win: \n" ++ showField successField
     print $ "successNumber: " ++ show successNumber
     print $ "puzzleResult: " ++ show (puzzleResult successField successNumber)
