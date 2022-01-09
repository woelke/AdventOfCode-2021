{-# LANGUAGE TupleSections #-}

import AOCInputs
import Misc
-- import Matrix1
import Data.Maybe

-- import Data.List

import qualified Data.Map as Map
-- import qualified Data.Set as Set

data Stree = StreePair (Stree, Stree)
           | StreeVal Integer deriving Show

data Entry = Val Integer
           | Ref Integer deriving Show

data SearchDirection = Up
                     | Down

type Pair = (Entry, Entry)

type SnailNum = Map.Map Integer Pair

readStree :: String -> Stree
readSnailNumFromStree :: Stree -> SnailNum
readSnailNum :: String -> SnailNum
showSnailNum :: SnailNum -> String

isSnailNumEqualTo :: SnailNum -> SnailNum -> Bool
add :: SnailNum -> SnailNum -> SnailNum
explode :: SnailNum -> SnailNum
split :: SnailNum -> SnailNum
reduce :: SnailNum -> SnailNum
reduceLog :: SnailNum -> [(String,SnailNum)]
addReduce :: SnailNum -> SnailNum -> SnailNum
addsReduce :: [SnailNum] -> SnailNum

toListSN :: SnailNum -> [(Integer, Pair)]

showLog :: [(String, SnailNum)] -> String

isRef :: Entry -> Bool
getRef :: Entry -> Integer
isVal :: Entry -> Bool
getVal :: Entry -> Integer

isRef x =
  case x of
    Ref _ -> True
    Val _ -> False

getRef x =
  case x of
    Ref a -> a
    Val _ -> error "not a Ref"

isVal = not . isRef

getVal x =
  case x of
    Ref _ -> error "not a Val"
    Val a -> a

readStree str = fst $ parse str
    where getNumber xs (y:ys)
            | y `elem` [ ']', ','] = (read xs::Integer, y:ys)
            | otherwise = getNumber (xs ++ [y]) ys
          parse (x:xs)
            | x == '[' = let (a, _:ra) = parse xs
                             (b, _:rb) = parse ra
                          in (StreePair (a,b), rb)
            | otherwise = let (n, r) = getNumber [x] xs
                           in (StreeVal n, r)

readSnailNumFromStree t = snd $ readSnailInner 0 [1..] Map.empty t
  where readSnailInner toUseRef refGen snailNum (StreePair (StreeVal l, StreeVal r)) =
          let nextSnailNum = Map.insert toUseRef (Val l, Val r) snailNum
           in (refGen, nextSnailNum)
        readSnailInner toUseRef refGen snailNum (StreePair (StreeVal l, StreePair r)) =
          let (ref, refGen1) = (head refGen, tail refGen)
              (refGen2, snailNum1) = readSnailInner ref refGen1 snailNum (StreePair r)
              snailNum2 = Map.insert toUseRef (Val l, Ref ref) snailNum1
           in (refGen2, snailNum2)
        readSnailInner toUseRef refGen snailNum (StreePair (StreePair l, StreeVal r)) =
          let (ref, refGen1) = (head refGen, tail refGen)
              (refGen2, snailNum1) = readSnailInner ref refGen1 snailNum (StreePair l)
              snailNum2 = Map.insert toUseRef (Ref ref, Val r) snailNum1
           in (refGen2, snailNum2)
        readSnailInner toUseRef refGen snailNum (StreePair (l, r)) =
          let (ref1, ref2, refGen1) = (head refGen, head $ tail refGen, tail $ tail refGen)
              (refGen2, snailNum1) = readSnailInner ref1 refGen1 snailNum l
              (refGen3, snailNum2) = readSnailInner ref2 refGen2 snailNum1 r
              snailNum3 = Map.insert toUseRef (Ref ref1, Ref ref2) snailNum2
           in (refGen3, snailNum3)

readSnailNum = readSnailNumFromStree . readStree

showSnailNum snailNum = showSnailNumInner $ fromJust $ Map.lookup 0 snailNum
  where part (Val a) = show a
        part (Ref a) = showSnailNumInner $ fromJust $ Map.lookup a snailNum
        showSnailNumInner (l, r) = "[" ++ part l ++ "," ++ part r ++ "]"

add a b =  let newFirstRefOfB = maxKey a + 2
               newA = increaseRefs (Map.toList a) 1
               newB = increaseRefs (Map.toList b) newFirstRefOfB
               newHead = (0 ,(Ref 1, Ref newFirstRefOfB))
            in Map.fromList $ newHead : newA ++ newB
  where maxKey x = maximum $ Map.keys x
        increaseRefs x by = map updateKeyVal x
          where updateRef (Ref r) = Ref (r + by)
                updateRef v = v
                updateKeyVal (k ,(l, r)) = (k + by, (updateRef l, updateRef r))

isSnailNumEqualTo sn1 sn2 = showSnailNum sn1 == showSnailNum sn2

toListSN sn = inner 0 (fromJust $ Map.lookup 0 sn)
  where inner k (Ref l, Val r) = inner l (fromJust $ Map.lookup l sn) ++ [(k, (Ref l, Val r))]
        inner k (Val l, Ref r) = (k, (Val l, Ref r)) : inner r (fromJust $ Map.lookup r sn)
        inner k (Ref l, Ref r) = inner l (fromJust $ Map.lookup l sn) ++ inner r (fromJust $ Map.lookup r sn)
        inner k entry = [(k, entry)]


----------------------------------------
  -- explode related functions
----------------------------------------
rotateAllPairs :: SnailNum -> SnailNum
findKeyInUse :: Integer -> SnailNum -> Maybe Integer
updateRight :: Integer -> Integer -> Integer -> SearchDirection -> SnailNum -> SnailNum
updateLeft :: Integer -> Integer -> Integer -> SnailNum -> SnailNum
updateParent :: Integer -> SnailNum -> SnailNum
findKeyToExplode :: SnailNum -> Maybe Integer

explode snailNum = let keyToExplode = findKeyToExplode snailNum
                       (l, r) = fromJust $ Map.lookup (fromJust keyToExplode) snailNum
                       snailNum1 = updateRight (fromJust keyToExplode) (fromJust keyToExplode) (getVal r) Up snailNum
                       snailNum2 = updateLeft (fromJust keyToExplode) (fromJust keyToExplode) (getVal l) snailNum1
                       snailNum3 = updateParent (fromJust keyToExplode) snailNum2
                    in if isNothing keyToExplode
                          then snailNum
                          else snailNum3

rotateAllPairs = Map.map (\(l, r) -> (r, l))

findKeyInUse keyToFind sn = search 0
  where search k = let pair = Map.lookup k sn
                       resList = map (\x -> if isRef x
                                            then if getRef x == keyToFind
                                                    then Just k
                                                    else search (getRef x)
                                            else Nothing) [fst $ fromJust pair, snd $ fromJust pair]
                     in if isNothing pair
                           then Nothing
                           else listToMaybe $ catMaybes resList

findKeyToExplode snailNum = inner (0::Integer) (0::Integer) (fromJust $ Map.lookup 0 snailNum)
  where callInner depth key = inner (depth + 1) key (fromJust $ Map.lookup key snailNum)
        inner depth key (Val _, Val _) = if depth > 3 then Just key else Nothing
        inner depth _ (Ref l, Val _) = callInner depth l
        inner depth _ (Val _, Ref r) = callInner depth r
        inner depth _ (Ref l, Ref r) = let lKey = callInner depth l
                                           rKey = callInner depth r
                                        in maybe rKey Just lKey

updateRight key lastKey val Up sn = let (l, r) = fromJust $ Map.lookup key sn
                                        parent = findKeyInUse key sn
                                     in if isRef l
                                           then if isRef r
                                                   then if getRef l == lastKey
                                                           then updateRight (getRef r) key val Down sn
                                                           else if isNothing parent
                                                                   then sn
                                                                   else updateRight (fromJust parent) key val Up sn
                                                   else Map.insert key (l, Val (getVal r + val)) sn
                                           else if isNothing parent
                                                   then sn
                                                   else updateRight (fromJust parent) key val Up sn
updateRight key _ val Down sn = let (l, r) = fromJust $ Map.lookup key sn
                               in if isVal l
                                     then Map.insert key (Val (getVal l + val), r) sn
                                     else updateRight (getRef l) key val Down sn
updateLeft key lastKey val =  rotateAllPairs . updateRight key lastKey val Up . rotateAllPairs
updateParent key sn = let parentKey = fromJust $ findKeyInUse key sn
                          (l, r) = fromJust $ Map.lookup parentKey sn
                       in Map.delete key $ Map.insert parentKey (if isRef l && getRef l == key then (Val 0, r) else (l, Val 0)) sn

explodeTestData = [("[[[[[9,8],1],2],3],4]","[[[[0,9],2],3],4]")
                  ,("[7,[6,[5,[4,[3,2]]]]]","[7,[6,[5,[7,0]]]]")
                  ,("[[6,[5,[4,[3,2]]]],1]","[[6,[5,[7,0]]],3]")
                  ,("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]","[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")
                  ,("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]","[[3,[2,[8,0]]],[9,[5,[7,0]]]]")
                  ,("[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]", "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")
                  ]

testExplode = all ((== True) . test) explodeTestData
  where test (input, expectedResult) = explode (readSnailNum input) `isSnailNumEqualTo` readSnailNum expectedResult


-- a = fst $ last explodeTestData
-- a1 = readSnailNum a

----------------------------------------
  -- split related functions
----------------------------------------
split snailNum = let key = findValToSplit snailNum
                  in if isNothing key
                        then snailNum
                        else doSplit (fromJust key) snailNum
  where maxAllowedValue = 9
        findValToSplit sn = foldr (\(k , (l, r)) acc -> if any (> maxAllowedValue) (getVals [l, r]) then Just k else acc) Nothing (toListSN sn)
          where getVals xs = mapMaybe (\x -> if isVal x then Just (getVal x) else Nothing) xs
        doSplit key sn = let newKey = 1 + maximum (Map.keys sn)
                             newPair val = let v = val `div` 2 in (Val v, Val $ if v * 2 < val then v+1 else v)
                             snailNum1 val = Map.insert newKey (newPair val) sn
                             (l, r) = fromJust $ Map.lookup key sn
                          in if isVal l && getVal l > maxAllowedValue
                                then Map.insert key (Ref newKey, r) (snailNum1 $ getVal l)
                                else Map.insert key (l, Ref newKey) (snailNum1 $ getVal r)

splitTestData = [("[[[[0,7],4],[15,[0,13]]],[1,1]]","[[[[0,7],4],[[7,8],[0,13]]],[1,1]]")
                  ,("[[[[0,7],4],[[7,8],[0,13]]],[1,1]]","[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]")
                  ]

testSplit = all ((== True) . test) splitTestData
  where test (input, expectedResult) = split (readSnailNum input) `isSnailNumEqualTo` readSnailNum expectedResult

----------------------------------------
  -- reduce related functions
----------------------------------------
reduce snailNum = fst $ foldlWhile changed (advance doReduceIteration) (doReduceIteration snailNum, snailNum) [1..]
  where changed (a, b) _ = not $ a `isSnailNumEqualTo` b
        advance fun (current, _) _ = (fun current, current)
        doReduceIteration = split . explodes
        explodes sn = fst $ foldlWhile changed (advance explode) (explode sn, sn) [1..]

reduceLog snailNum = fst $ foldlWhile (\(log, lastLogLength) _ -> length log /= lastLogLength)
                                      (\(log, _) _ -> (advance doReduceIterationLog log 1 ,length log))
                                      (doReduceIterationLog start, length start) [1..]
  where start = [("start        : ", snailNum)]
        fstSN log = snd $ head log
        sndSN log = snd $ head $ tail log
        changed log _ = if length log == 1
                           then True
                           else not $ fstSN log `isSnailNumEqualTo` sndSN log
        advance fun log _ = fun log
        doReduceIterationLog = splitLog . explodesLog
        splitLog log = let current = fstSN log
                           res = split current
                        in if current `isSnailNumEqualTo` res then log else ("after split  : ", res):log
        explodeLog log = ("after explode: ", explode $ fstSN log):log
        explodesLog log = tail $ foldlWhile changed (advance explodeLog) log [1..]

showLog log = showMyList $ map (\(description, sn) -> description ++ showSnailNum sn) (reverse log)

reduceTestData = [("[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]", "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")
                 ]

testReduce = all ((== True) . test) reduceTestData
  where test (input, expectedResult) = reduce (readSnailNum input) `isSnailNumEqualTo` readSnailNum expectedResult

reduceAddTestData = [ ("[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]", "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]","[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]")
                    , ("[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]","[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]","[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]")
                    , ("[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]","[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]","[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]")
                    , ("[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]","[7,[5,[[3,8],[1,4]]]]","[[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]")
                    , ("[[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]","[[2,[2,2]],[8,[8,1]]]","[[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]")
                    , ("[[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]","[2,9]","[[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]")
                    , ("[[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]","[1,[[[9,3],9],[[9,0],[0,7]]]]","[[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]")
                    , ("[[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]","[[[5,[7,4]],7],1]","[[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]")
                    , ("[[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]","[[[[4,2],2],6],[8,7]]","[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]")
                    ]

testReduceAdd = all ((== True) . test) reduceAddTestData
  where test (a, b, expectedResult) = reduce (readSnailNum a `add` readSnailNum b) `isSnailNumEqualTo` readSnailNum expectedResult

----------------------------------------
  -- addReduce related functions
----------------------------------------
addReduce a b = reduce (a `add` b)
addsReduce = foldl1 addReduce

----------------------------------------
  -- magnitude related functions
----------------------------------------
magnitude sn = inner (fromJust $ Map.lookup 0 sn)
  where mag (Val x) = x
        mag (Ref x) = inner (fromJust $ Map.lookup x sn)
        inner (l, r) = 3 * mag l + 2 * mag r

magnitudeTestData = [ ("[[1,2],[[3,4],5]]", 143)
                    , ("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]", 1384)
                    , ("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]", 3488)
                    ]


testMagnitude = all ((== True) . test) magnitudeTestData
  where test (a, expectedResult) = magnitude (readSnailNum a) == expectedResult

maxMagnitude xs = maximum [ magnitude (addReduce a b) | a <- xs, b <- xs, not (a `isSnailNumEqualTo` b) ]

----------------------------------------
  -- the main
----------------------------------------
runTest :: IO ()

runTest =
  do ls <- readLines "testLarge.txt"
     let res = readSnailNum $ head ls
     let ls1 = drop 2 ls
     print ls1
     let sum = addsReduce $ map readSnailNum ls1
     print $ showSnailNum sum
     print $ showSnailNum res
     print $ "result is correct: " ++ show (res `isSnailNumEqualTo` sum)


run1 :: IO ()
run2 :: IO ()

run1 =
  do ls <- readLines "puzzle.txt"
     let sum = addsReduce $ map readSnailNum ls
     let mag = magnitude sum
     print $ showSnailNum sum
     print mag

run2 =
  do ls <- readLines "puzzle.txt"
     print $ maxMagnitude $ map readSnailNum ls
