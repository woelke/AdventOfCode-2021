-- puzzle input helper

module AOCInputs where

import System.IO

splitLine :: (String -> (Bool, String)) -> String -> [String]
atString :: String -> String -> (Bool, String)
atSpace :: String -> (Bool, String)

readLines :: FilePath -> IO [String]
readIntegerListFromSingleLineFile :: (String -> (Bool, String)) -> FilePath -> IO [Integer]
readIntegerListFromMultiLineFile :: FilePath -> IO [Integer]

readLines file =
  do fh <- openFile file ReadMode
     res <- hReadList fh
     hClose fh
     return res

readIntegerListFromSingleLineFile sepFun file =
  do line <- readLines file
     return $ map (\x -> read x::Integer) (splitLine sepFun $ head line)

readIntegerListFromMultiLineFile file =
  do l <- readLines file
     return $ map (\x -> read x::Integer) l

splitLine sepFun = splitLineInner sepFun [] []

atString [] xs = (True, xs)
atString _ [] = (False, [])
atString (c:cs) (x:xs)
  | c == x = (separate, if separate then newXs else x:xs)
  | otherwise = (False, x:xs)
  where (separate, newXs) = atString cs xs

-- a space can have an arbitary length
atSpace = atString " "

-----------------------------------------
------------- private -------------------
-----------------------------------------

hReadList :: Handle -> IO [String]

hReadList fh =
  do ended <- hIsEOF fh
     if ended then
        return []
     else
        do line <- hGetLine fh
           next <- hReadList fh
           return (line : next)

splitLineInner :: (String -> (Bool, String)) -> [String] -> String -> String -> [String]

splitLineInner _ res [] [] = res
splitLineInner _ res tempToKeep [] = res ++ [tempToKeep]
splitLineInner sepFun res tempToKeep (x:xs) =
  if separate
     then splitLineInner sepFun (addElemIfNotEmpty res tempToKeep) [] newXs
     else splitLineInner sepFun res (tempToKeep ++ [x]) xs
  where (separate, newXs) = sepFun (x:xs)
        addElemIfNotEmpty es e = if null e then es else es ++ [e]
