-- Day 6

import AOCInputs

nullGeneration :: [Integer]
readGeneration :: String -> [Integer]
addValueAtIndex :: Integer -> Integer -> [Integer] -> [Integer]
processGeneration :: [Integer] -> [Integer]

run1 :: IO ()

nullGeneration = replicate 9 0
addValueAtIndex value index = zipWith (\i x -> if index == i then value + x else x) [0 ..]

readGeneration str = foldr (addValueAtIndex 1) nullGeneration getIntList
  where getIntList = map (\x -> read x::Integer) (splitLine (atString ",") str)

processGeneration g =  addValueAtIndex (head g) 8 $ addValueAtIndex (head g) 6 (shiftLeft g)
  where shiftLeft xs = tail xs ++ [0]



run1 =
  do ls <- readLines "puzzle.txt"
     let gen0 = readGeneration (head ls)
     let genx = last $ take (256 + 1) $iterate processGeneration gen0
     print gen0
     print genx
     print $ sum genx

