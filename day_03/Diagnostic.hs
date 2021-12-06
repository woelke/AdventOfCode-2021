-- Day 1

import AOCInputs
import Data.Maybe

showBools :: [Bool] -> String
readBools :: String -> [Bool]
boolsToInteger :: [Bool] -> Integer
invertBools :: [Bool] -> [Bool]

showBoolMatrix :: [[Bool]] -> String
readBoolMatrix :: [String] -> [[Bool]]
transposeBoolMatrix :: [[Bool]] -> [[Bool]]

headBM :: [[Bool]] -> [Bool]
tailBM :: [[Bool]] -> [[Bool]]
prependBM :: [Bool] -> [[Bool]] -> [[Bool]]
appendBM :: [[Bool]] -> [Bool] -> [[Bool]]
coloumnLengthBM :: [[Bool]] -> Int

mostCommonTF :: [Bool] -> Maybe Bool
leastCommonTF :: [Bool] -> Maybe Bool

calcGammaRate :: [[Bool]] -> [Bool]
calcEpsilonRate :: [[Bool]] -> [Bool]

calcRating :: ([Bool] -> Bool) -> [[Bool]] -> [Bool]

calcOxygenRating :: [[Bool]] -> [Bool]
calcCO2Rating :: [[Bool]] -> [Bool]



puzzleResult1 :: [Bool] -> [Bool] -> Integer
puzzleResult2 :: [Bool] -> [Bool] -> Integer

run1 :: IO ()
run2 :: IO ()

showBools = map (\x -> if x then '1' else '0')
readBools = map (== '1')
boolsToInteger xs = snd $ foldr (\x (pos,value) -> if x then (pos + 1, 2^pos + value) else (pos + 1, value)) (0::Integer,0::Integer) xs
invertBools = map not

showBoolMatrix = foldr (\xs acc -> showBools xs ++ "\n" ++ acc) []
readBoolMatrix = map readBools

transposeBoolMatrix = foldr (\row acc -> rowToColoumn row +++ acc) []
  where rowToColoumn = map (: [])
        (+++) [] [] = []
        (+++) xss [] = xss
        (+++) [] yss = yss
        (+++) (xs:xss) (ys:yss) = (xs ++ ys) : xss +++ yss

headBM m = head $ transposeBoolMatrix m
tailBM = transposeBoolMatrix . tail . transposeBoolMatrix
prependBM h t = transposeBoolMatrix (h : transposeBoolMatrix t)
appendBM t h = transposeBoolMatrix (transposeBoolMatrix t ++ [h])
coloumnLengthBM m = length (head m)

mostCommonTF bs = res $ countTF bs
  where countTF = foldl (\(t, f) x -> if x then (t + 1, f) else (t, f + 1)) (0::Integer,0::Integer)
        res (t, f)
          | t < f = Just False
          | t > f = Just True
          | otherwise = Nothing

leastCommonTF bs =
  case mostCommonTF bs of
    Just x -> Just (not x)
    Nothing -> Nothing

calcGammaRate m = map (fromJust . mostCommonTF) $ transposeBoolMatrix m
calcEpsilonRate bs = invertBools $ calcGammaRate bs

calcRating bitToKeep m = head $ fixRotation (filterBM m (coloumnLengthBM m))
  where removeLines [] [] _ = []
        removeLines (x:xs) (ys:yys) bit = if x == bit
                                     then ys : removeLines xs yys bit
                                     else removeLines xs yys bit
        removeLines _ _ _ = error "length missmatch"
        rotate x = tailBM x `appendBM` headBM x
        fixRotation (x, n)
          | n < 0 = error "fixRation failed x < 0"
          | n == 0 = x
          | otherwise = fixRotation (rotate x, n - 1)
        filterBM [xs] n = ([xs], n)
        filterBM xss n = filterBM (rotate (newM xss)) (n - 1)
          where newM x = removeLines (headBM x) x (bitToKeep (headBM x))

calcOxygenRating = calcRating (fromMaybe True . mostCommonTF )
calcCO2Rating = calcRating (fromMaybe False . leastCommonTF)

puzzleResult1 g e = boolsToInteger g * boolsToInteger e
puzzleResult2 o c = boolsToInteger o * boolsToInteger c

run1 =
  do diagLines <- readLines (puzzleFile 1)
     let diagnostic = readBoolMatrix diagLines
     let gammaRate = calcGammaRate diagnostic
     let epsilonRate = calcEpsilonRate diagnostic
     print $ "gammaRate: " ++ show (boolsToInteger gammaRate)
     print $ "epsilonRate: " ++ show (boolsToInteger epsilonRate)
     print $ "puzzle Result: " ++ show (puzzleResult1 gammaRate epsilonRate)

run2 =
  do diagLines <- readLines (puzzleFile 1)
     let diagnostic = readBoolMatrix diagLines
     let oxygen = calcOxygenRating diagnostic
     let co2 = calcCO2Rating diagnostic
     print $ "oxygen rating: " ++ show (boolsToInteger oxygen)
     print $ "co2: " ++ show (boolsToInteger co2)
     print $ "puzzle Result: " ++ show (puzzleResult2 oxygen co2)

-- test =
  -- do diagLines <- readLines (testFile 1)
     -- let diagnostic = readBoolMatrix diagLines
     -- let oxy = calcOxygenRating diagnostic
     -- putStr $ "-- diagnostic: \n" ++ showBoolMatrix diagnostic
     -- print $ "-- oxy:" ++ showBools oxy
