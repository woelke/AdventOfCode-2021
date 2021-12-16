import AOCInputs

import qualified Data.Map as Map
import qualified Data.Set as Set

readInput :: [String] -> (String, Map.Map String String)

step :: String -> Map.Map String String -> String
stepX :: Integer -> String -> Map.Map String String -> String

letterStatistics :: String -> [(Char, Int)]

puzzleResult :: String -> Integer

readInput = foldl (flip parseLine) ("", Map.empty)
  where parseLine l (template, rules)
          | '-' `elem` l = let [k, v] = splitLine (atString " -> ") l
                             in (template, Map.insert k v rules)
          | null l = (template, rules)
          | otherwise = (l, rules)


step template rules = stepInner template rules
  where stepInner [x] _ = [x]
        stepInner [] _ = []
        stepInner (a:b:ts) rules = applyRules ++ step (b:ts) rules
          where applyRules = let newV = Map.lookup (a:[b]) rules
                              in case newV of
                                   Nothing -> error "applyRule pattern unreachable"
                                   Just x -> a:x

stepX count template rules = last $ take (fromIntegral (count + 1)) $ iterate callStep template
  where callStep t = step t rules

letterStatistics polymer = map (\l -> (l, length $ filter (==l) polymer)) letters
  where letters = Set.toList $ Set.fromList polymer

puzzleResult polymer = fromIntegral $ maxCount - minCount
  where stats = letterStatistics polymer
        maxCount = foldl (\acc (_, count) -> max acc count) 0 stats
        minCount = foldl (\acc (_, count) -> min acc count) (snd $ head stats) stats

run1 =
  do ls <- readLines "test.txt"
     let (template, rules) = readInput ls
     let polymer = stepX 20 template rules
     let statistics = letterStatistics polymer
     -- print polymer
     print statistics
     print $ puzzleResult polymer
