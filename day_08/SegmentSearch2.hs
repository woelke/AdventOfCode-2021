import AOCInputs

import Data.Maybe
import qualified Data.List as List
import qualified Data.Set as Set

data Note = Note { signals :: [Set.Set Char]
                 , outputs :: [Set.Set Char] } deriving Show

readInput :: [String] -> [Note]
refineSignals :: [Set.Set Char] -> [Set.Set Char]
matchOutputs :: [Set.Set Char] -> [Set.Set Char] -> Integer

checkRequriements :: [Set.Set Char] -> Bool

run1 :: IO ()

readInput strs = map parseNote strs
  where parseNote str = let note = splitLine (atString " | ") str
                        in Note { signals=(map Set.fromList $ splitLine atSpace $ head note)
                                , outputs=(map Set.fromList $ splitLine atSpace $ last note)}

refineSignals sigs =  head [p | p <- List.permutations sigs, checkRequriements p]

checkRequriements [s0, s1, s2, s3, s4, s5, s6, s7, s8, s9] =
     checkSizes
  && check0 && check6 && check9
  && check2 && check3 && check5
  where isSubOf a b = a `Set.isSubsetOf` b
        checkSizes =
          Set.size s0 == 6 &&
          Set.size s1 == 2 &&
          Set.size s2 == 5 &&
          Set.size s3 == 5 &&
          Set.size s4 == 4 &&
          Set.size s5 == 5 &&
          Set.size s6 == 6 &&
          Set.size s7 == 3 &&
          Set.size s8 == 7 &&
          Set.size s9 == 6

        check0 = not (s4 `isSubOf` s0) && (s1 `isSubOf` s0)
        check6 = not (s4 `isSubOf` s6) && not (s1 `isSubOf` s6)
        check9 = (s4 `isSubOf` s9) && (s1 `isSubOf` s9)

        check2 = not (s1 `isSubOf` s2) && not (s2 `isSubOf` s9)
        check3 = s1 `isSubOf` s3
        check5 = not (s1 `isSubOf` s5) && (s5 `isSubOf` s9)

checkRequriements _ = False

matchOutputs refinedSigs outs = maybeIntList2Value $ map indexOf outs
  where indexOf x = foldl (\acc (i, sig) -> if isJust acc
                                               then acc
                                               else if x == sig
                                                 then Just i
                                                 else Nothing ) Nothing $ zip [0 ..] refinedSigs
        maybeIntList2Value xs = foldl (\acc (pos, x) -> acc + (10^pos * (fromJust x))) 0 $ zip [3,2,1,0] xs

run1 =
  do ls <- readLines "puzzle.txt"
     let notes = readInput ls
     let res = map getOutputValue notes
     print res
     print $ sum res

     -- let refinedSignals = refineSignals $ signals $ head notes
     -- let matchedOutputs = matchOutputs refinedSignals $ outputs $ head notes
  where getOutputValue note = matchOutputs (refineSignals $ signals note) (outputs note)


