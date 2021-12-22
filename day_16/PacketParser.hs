{-# LANGUAGE TupleSections #-}

import AOCInputs
import Misc
-- import Matrix1
import Data.Maybe

-- import Data.List

-- import qualified Data.Map as Map
-- import qualified Data.Set as Set

data OperatorType = Sum | Prod | Min | Max | Value | Gthan | Lthan | Equal deriving (Eq, Show, Enum)
data Content = Val Integer
             | Op OperatorType [Packet] deriving Show
data Packet = Ver Integer Content deriving Show

type HexChar = Char
type HexString = String
type BinChar = Char
type BinString = String



hexChar2BinStr :: HexChar -> Maybe BinString
hexString2BinStr :: HexString -> Maybe BinString
binStr2Integer :: BinString -> Maybe Integer
binChar2Bool :: BinChar -> Maybe Bool

parsePacket :: BinString -> (Packet, BinString)
parseContent :: BinString -> (Content, BinString)
parseLiteralValue :: BinString -> (Content, BinString)
parseSubPackets :: OperatorType -> BinString -> (Content, BinString)

versionsToList :: Packet -> [Integer]
calcPacketValue :: Packet -> Integer

run1 :: IO ()

hexChar2BinStr hexChar = lookup hexChar conversionTable
  where conversionTable = [('0', "0000"),
                           ('1', "0001"),
                           ('2', "0010"),
                           ('3', "0011"),
                           ('4', "0100"),
                           ('5', "0101"),
                           ('6', "0110"),
                           ('7', "0111"),
                           ('8', "1000"),
                           ('9', "1001"),
                           ('A', "1010"),
                           ('B', "1011"),
                           ('C', "1100"),
                           ('D', "1101"),
                           ('E', "1110"),
                           ('F', "1111")]

hexString2BinStr hexStr = let binLists = map hexChar2BinStr hexStr
                           in if any isNothing binLists
                                 then Nothing
                                 else Just $ concatMap fromJust binLists

binStr2Integer binStr = snd $ foldr accumulateChunks (0, Just 0) binStr
  where accumulateChunks c (pos, acc) =
          let val = binCharToVal c
           in if any isNothing [val, acc]
                 then (pos + 1, Nothing)
                 else (pos + 1, Just $ fromJust acc + fromJust val * 2^pos)
        binCharToVal c
          | c == '1' = Just 1
          | c == '0' = Just 0
          | otherwise = Nothing

binChar2Bool binChar
  | binChar == '0' = Just False
  | binChar == '1' = Just True
  | otherwise = Nothing


parseLiteralValue binStr = let (binStrVal, remainder) = parseGroups (fromJust $ binChar2Bool $ head binStr) (drop 1 binStr)
                        in (Val $ fromJust $ binStr2Integer binStrVal, remainder)
  where parseGroups False xs = splitAt 4 xs
        parseGroups True xs = let binStrVal = take 4 xs
                                  (nextBinStrVal, remainder) = parseGroups (fromJust $ binChar2Bool $ xs !! 4) (drop 5 xs)
                               in (binStrVal ++ nextBinStrVal, remainder)

parseContent binStr = let (opType, remainder) = parseOperatorType binStr
                       in dispatch opType remainder
  where parseOperatorType ys = (toEnum $ fromIntegral $ fromJust $ binStr2Integer $ take 3 ys, drop 3 ys)
        dispatch opType ys
          | opType == Value = parseLiteralValue ys
          | otherwise = parseSubPackets opType ys

parseSubPackets opType binStr = dispatchFunByLengthTypeId $ tail binStr
  where dispatchFunByLengthTypeId
          | head binStr == '0' = parseByLength
          | head binStr == '1' = parseByCount
          | otherwise = error "unkown char in binary string"
        parseByLength xs = let len = fromIntegral $ fromJust $ binStr2Integer $ take 15 xs
                               wholeRemainder = drop 15 xs
                               toParseRemainder = take len wholeRemainder
                               notParsedRemainder = drop len wholeRemainder
                               (packets, _) = foldlWhile
                                                (\(_, r) _ -> not $ null r)
                                                (\(ps, r) _ -> let (p, r1) = parsePacket r in (ps ++ [p], r1))
                                                ([] , toParseRemainder) [(1::Integer)..]
                            in (Op opType packets, notParsedRemainder)
        parseByCount xs = let count = fromJust $ binStr2Integer $ take 11 xs
                              wholeRemainder = drop 11 xs
                              (packets, notParsedRemainder) = foldl
                                                                (\(ps, r) _ -> let (p, r1) = parsePacket r in (ps ++ [p], r1))
                                                                ([], wholeRemainder) [1..count]
                           in (Op opType packets, notParsedRemainder)

parsePacket binStr = let (version, remainder) = parseVersion binStr
                         (content, remainder1) = parseContent remainder
                      in (version content, remainder1)
  where parseVersion xs = (Ver $ fromJust $ binStr2Integer $ take 3 xs, drop 3 xs)

versionsToList (Ver ver content) = ver : concat ( versfromContent content)
  where versfromContent (Val _) =  []
        versfromContent (Op _ packets) = map versionsToList packets

calcPacketValue (Ver _ content) = value content
  where values ps = map calcPacketValue ps
        value (Val x) = x
        value (Op Sum ps) = sum $ values ps
        value (Op Prod ps) = product $ values ps
        value (Op Min ps) = minimum $ values ps
        value (Op Max ps) = maximum $ values ps
        value (Op Gthan ps) = let [a,b] = values ps in fromIntegral $ fromEnum (a > b)
        value (Op Lthan ps) = let [a,b] = values ps in fromIntegral $ fromEnum (a < b)
        value (Op Equal ps) = let [a,b] = values ps in fromIntegral $ fromEnum (a == b)
        value _ = error "unknown operator"

run1 =
  do ls <- readLines "puzzle.txt"
     let p = parsePacket $ fromJust $ hexString2BinStr $ head ls
     print p
     -- print $ sum $ versionsToList $ fst p
     print $ calcPacketValue $ fst p

