{-# LANGUAGE TupleSections #-}

import AOCInputs
import Misc
-- import Matrix1
import Data.Maybe

import Data.List

import qualified Data.Map as Map
import qualified Data.Set as Set

type Pos = [Integer]
type Offset = Pos

type Sign = Integer
type OriginIndex = Int
type ScannerId = Integer
type Orientation = [(Sign, OriginIndex)]
type OverlapInfo = (Offset, Orientation)
type Overlappses = Map.Map ScannerId [(ScannerId, OverlapInfo)]

type ScannerData = [Pos]
type Scanners = Map.Map ScannerId ScannerData

addP :: Pos -> Pos -> Pos
subP :: Pos -> Pos -> Pos

readScanners :: [String] -> Scanners
readScannerData :: [String] -> ScannerData -> ([String], ScannerData)

turnPos :: Pos -> Orientation -> Pos
turnScannerData :: ScannerData -> Orientation -> ScannerData
allOrientations :: Int -> [Orientation]

relativeDistance :: Pos -> ScannerData -> ScannerData
calcOverlapp :: ScannerData -> ScannerData -> Maybe OverlapInfo

addScannerData :: ScannerData -> ScannerData -> OverlapInfo -> ScannerData

allOverlappses :: Scanners -> Overlappses
mergeOverlappses :: ScannerId -> Overlappses -> Scanners -> ScannerData

readScannerData [] res = ([], res)
readScannerData (x:xs) res
  | null x = (xs, res)
  | "--- scanner " `isPrefixOf` x = readScannerData xs res
  | otherwise = readScannerData xs (res ++ [parseCoordinates x])
  where parseCoordinates ts = map (\t -> read t::Integer) (splitLine (atString ",") ts)

readScanners xs = fst $ foldlWhile
                          (\(_, r) _ -> not $ null r)
                          (\(res, r) scannerId -> let (r1, beacons) = readScannerData r []
                                                 in (Map.insert scannerId beacons res, r1))
                          (Map.empty, xs) [0..]

addP = zipWith (+)
subP = zipWith (-)

relativeDistance pos = map (`subP` pos)

turnPos pos = map (\(sign, index) -> sign * (pos !! index))
turnScannerData sd orientation = map (`turnPos` orientation) sd

allOrientations dimensions = [zip signs indexs | signs <- signsPermutations, indexs <- indexPermutations]
  where indexPermutations = permutations [0..(dimensions-1)]
        signsPermutations = map (map (\bin -> if bin == 0 then 1 else (-1))) $ map toBin [0..(2^dimensions) -1]
          where toBin x = fillWithLeadingZeros $ toBinInner x
                fillWithLeadingZeros l = let zerosToAdd = dimensions - length l
                                          in replicate zerosToAdd 0 ++ l
                toBinInner 0 = [0]
                toBinInner 1 = [1]
                toBinInner n
                    | even n = toBinInner (n `div` 2) ++ [0]
                    | otherwise = toBinInner (n `div` 2) ++ [1]

calcOverlapp a b = let overlapps = findOverlappings a b
                       (x, (y, orientationY)) = head overlapps
                    in if null overlapps
                          then Nothing
                          else Just (x `subP` turnPos y orientationY, orientationY)
  where relDists xs = [ (origin, relativeDistance origin xs) | origin <- xs]
        relDistOrientations xs = [ (origin, orientation, turnScannerData dists orientation)
                                    | (origin, dists) <- relDists xs
                                    , orientation <- allOrientations (length origin)]
        findOverlappings x y = [ (originX, (originY, orientationY))
                                  | (originX, distsX) <- relDists x
                                  , (originY, orientationY, distsY) <- relDistOrientations y
                                  , isOverlapping distsX distsY]
        overlapThreshold = 12
        isOverlapping aRel bRel = overlapThreshold <= Set.size (Set.fromList aRel `Set.intersection` Set.fromList bRel)


addScannerData a [] _ = a
addScannerData a b (offset, orientation) = Set.toList $ Set.fromList (a ++ modB)
  where modB = map (`addP` offset) (turnScannerData b orientation)

allOverlappses scanners = foldl' addOverlapsInfo Map.empty scannerIdPairs
  where scannerIdPairs = [(a, b) | a <- Map.keys scanners, b <- Map.keys scanners, a /= b]
        scannerData x = fromJust $ Map.lookup x scanners
        addOverlapsInfo res (a, b) =  let overlapInfo = calcOverlapp (scannerData a) (scannerData b)
                                          resInfos = Map.lookup a res
                                       in if isNothing overlapInfo
                                             then res
                                             else if isNothing resInfos
                                                     then Map.insert a [(b, fromJust overlapInfo)] res
                                                     else Map.insert a ((b, fromJust overlapInfo):fromJust resInfos) res

mergeOverlappses startId os scanners
  = let overlappsesFromStartId = Map.lookup startId os
        newOs = Map.delete startId os
     in if isNothing overlappsesFromStartId
           then []
           else mergeNext (fromJust $ Map.lookup startId scanners) (fromJust $ overlappsesFromStartId) newOs
    where mergeNext startIdScannerData overlappsesFromStartId currentOs
            = foldl' (\acc (id, oInfo) -> let sumScannerData = mergeOverlappses id currentOs scanners
                                           in addScannerData acc sumScannerData oInfo)
                     startIdScannerData overlappsesFromStartId

run1 :: IO ()
-- test :: IO ()

run1 =
  do ls <- readLines "puzzle.txt"
     let scanners = readScanners ls
     let allos = allOverlappses scanners
     let world = mergeOverlappses 0 allos scanners
     putStr $ showMyList (Map.toList allos)
     print "----------------"
     putStr $ showMyList world
     print $ length world

-- test =
  -- do ls <- readLines "test.txt"
     -- let scanners = readScanners ls
     -- let s0 = fromJust $ Map.lookup 0 scanners
     -- let s1 = fromJust $ Map.lookup 1 scanners
     -- let overlapInfo = fromJust $ calcOverlapp s0 s1
     -- print $  "overlapInfo: " ++ show overlapInfo
     -- let mergedS0S1 = addScannerData s0 s1 overlapInfo
