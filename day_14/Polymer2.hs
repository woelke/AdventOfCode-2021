import AOCInputs

import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

readInput :: [String] -> (String, Map.Map String String)

step :: String -> Map.Map String String -> String
stepX :: Integer -> String -> Map.Map String String -> String
stepXBufferdStats :: Integer -> String -> Map.Map String String -> [(Char, Integer)]
getStatsForPair :: Char -> Char -> Integer -> Map.Map String String -> Map.Map (Char, Char) [(Char, Integer)] -> ([(Char, Integer)], Map.Map (Char, Char) [(Char, Integer)])

letterStatistics :: String -> [(Char, Integer)]
unionStats :: [(Char, Integer)] -> [(Char, Integer)] -> [(Char, Integer)]

puzzleResult :: [(Char, Integer)] -> Integer

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

stepXBufferdStats count template rules = accumulatedStats `unionStats` [(last template, 1)]
  where acc = ([], Map.empty)
        zipTemplate = zip template (tail template)
        accumulatedStats = fst $ foldl (\(stats, buffer) (a,b) -> let (newStats, newBuffer) = getStatsForPair a b count rules buffer
                                                                   in (stats `unionStats` newStats `unionStats` [(b, -1)], newBuffer)) acc zipTemplate

getStatsForPair a b count rules buffer =
  case Map.lookup (a,b) buffer of
    Just x -> (x, buffer)
    Nothing -> let stats = letterStatistics $ stepX count (a:[b]) rules
                in (stats, Map.insert (a,b) stats buffer)

letterStatistics polymer = map (\l -> (l, fromIntegral $ length $ filter (==l) polymer)) letters
  where letters = Set.toList $ Set.fromList polymer

unionStats = foldl (flip addStat)
  where addStat (c,v) stats =
          case lookup c stats of
            Nothing -> (c,v):stats
            Just x -> (c, x + v): delete (c,x) stats

puzzleResult stats = fromIntegral $ maxCount - minCount
  where maxCount = foldl (\acc (_, count) -> max acc count) 0 stats
        minCount = foldl (\acc (_, count) -> min acc count) (snd $ head stats) stats

run1 =
  do ls <- readLines "puzzle.txt"
     let (template, rules) = readInput ls
     let polymer20 = stepX 20 template rules
     -- print $ letterStatistics polymer20
     let stats40 = stepXBufferdStats 20 polymer20 rules
     print stats40
     print $ puzzleResult stats40
