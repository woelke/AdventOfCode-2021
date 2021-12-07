import AOCInputs

import GHC.Integer

run1 :: IO ()

costToAlignAtHeight :: Integer -> [Integer] -> Integer
bestOption :: [Integer] -> (Integer, Integer)

bestOption hs = findLowestOption options
  where options = map (\h -> (h, costToAlignAtHeight h hs)) [(minimum hs) ..]
        findLowestOption ((h1, f1):(h2,f2):xs)
          | f1 > f2 = findLowestOption ((h2,f2):xs)
          | otherwise = (h1, f1)
        findLowestOption _ = error "unreachable pattern"

costToAlignAtHeight h hs = sum $ map (\x -> naturalSum $ absInteger (h - x)) hs
  where naturalSum x = (x * (x + 1)) `div` 2

run1 =
  do heights <- readIntegerListFromSingleLineFile (atString ",") "puzzle.txt"
     print $ "bestOptions: " ++ show (bestOption heights)


