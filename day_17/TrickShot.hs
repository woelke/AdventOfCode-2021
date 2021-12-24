{-# LANGUAGE TupleSections #-}

import AOCInputs
import Misc
-- import Matrix1
-- import Data.Maybe

import Data.List

-- import qualified Data.Map as Map
import qualified Data.Set as Set

-- type Pos = (Integer, Integer)
-- type Velocity = (Integer, Integer)

type Pos = (Integer, Integer)
type Vel = (Integer, Integer)
data Probe = Probe { pos :: Pos,
                     vel :: Vel
                   } deriving (Eq, Ord, Show)

type Target = ((Integer, Integer),(Integer, Integer))

testTarget :: Target
puzzleTarget :: Target

step :: Probe -> Probe
stepX :: Integer -> Probe -> Probe
inTarget :: Probe -> Target -> Bool
missedTarget :: Probe -> Target -> Bool

analyzeProbeHitTarget :: Probe -> Target -> (Bool, [Probe])
findHighestProbeShot :: Target -> [Probe]
findAllPossibleProbeShotsCount :: Target -> Int

testTarget = ((20,30), (-10,-5))
puzzleTarget = ((14,50), (-267,-225))

step (Probe {pos = (xP, yP), vel = (xV, yV)}) =
  Probe {pos = (xP + xV,  yP + yV), vel = (newXV , yV - 1)}
    where newXV
            | xV > 0 = xV - 1
            | xV < 0 = xV + 1
            | otherwise = 0

stepX count probe = foldl (\acc _ -> step acc) probe [1..count]

inTarget (Probe {pos = (xP, yP), vel = _}) (xR, yR) =
  xP `between` xR && yP `between` yR
    where between p (a, b) = min a b <= p && p <= max a b

missedTarget (Probe {pos = (xP, yP), vel = _}) ((x1, x2), (y1, y2)) =
   xP > max x1 x2 || yP < min y1 y2

analyzeProbeHitTarget probe target =
  foldlWhile
    (\(hit, ps) _ -> not hit && not (missedTarget (head ps) target))
    (\(hit, ps) _ -> let nextP = step (head ps) in (if hit then hit else inTarget nextP target, nextP : ps))
    (False, [probe]) [1..]

findHighestProbeShot target = last $sortOn (maximum . map (snd . pos)) successShots
  where ((x1, x2), (y1, y2)) = target
        successShots = let tries = [ analyzeProbeHitTarget (Probe {pos = (0,0), vel = (xVel,yVel)}) target
                                      | xVel <- [0..300]
                                      , yVel <- [0..300] ]
                        in map snd (filter fst tries)

findAllPossibleProbeShotsCount target = Set.size $ Set.fromList $ map (vel . last) successShots
  where ((x1, x2), (y1, y2)) = target
        successShots = let tries = [ analyzeProbeHitTarget (Probe {pos = (0,0), vel = (xVel,yVel)}) target
                                      | xVel <- [(-300)..300]
                                      , yVel <- [(-300)..300] ]
                        in map snd (filter fst tries)

run1 =
  do ls <- readLines "test.txt"
     -- let hp = findHighestProbeShot puzzleTarget
     -- print hp
     -- print $ maximum $ map (snd . pos) hp
     let count  = findAllPossibleProbeShotsCount puzzleTarget
     print count

