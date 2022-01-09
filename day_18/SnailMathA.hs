{-# LANGUAGE TupleSections #-}

import AOCInputs
import Misc
-- import Matrix1
import Data.Maybe

-- import Data.List

-- import qualified Data.Map as Map
-- import qualified Data.Set as Set

data Stree = Pair (Stree, Stree)
           | Val Integer deriving (Eq)

data Actions = AddToMostLeftLeaveOfRigthBranch Integer
             | AddToMostRightLeaveOfLeftBrnach Integer deriving Show

instance Show Stree where
    show (Pair (l,r)) = "[" ++ show l ++ "," ++ show r ++  "]"
    show (Val x) = show x

instance Read Stree where
  readsPrec _ str = [(fst $ parse str,"")]
    where getNumber xs (y:ys)
            | y `elem` [ ']', ','] = (read xs::Integer, y:ys)
            | otherwise = getNumber (y:xs) ys
          parse (x:xs)
            | x == '[' = let (a, _:ra) = parse xs
                             (b, _:rb) = parse ra
                          in (Pair (a,b), rb)
            | otherwise = let (n, r) = getNumber [x] xs
                           in (Val n, r)


add :: Stree -> Stree -> Stree
addR :: Integer -> Stree -> Stree -> Stree
addL :: Integer -> Stree -> Stree -> Stree

explode :: Stree -> Stree
-- split :: Stree -> Stree
-- magnitude :: Stree -> Integer

add a b = Pair (a,b)

addL v Pair (l, _) = v `addL` l
addL v (Val x) = Val (v + x)

addR v Pair (_, r) = v `addR` r
addR v (Val x) = Val (v + x)

explode t = walk 0 t
  where walk depth (Pair (Val l, Val r))
          | depth >= 4 = (Val 0, [AddR l, AddL r]) -- explode
          | otherwise = (Pair (Val l, Val r), []) -- everything is ok
        walk depth (Pair (l, r))
          | null lActions && null rActions = (Pair (lResTree, rRestTree), [])
          | not (null lActions) = Pair  (Val 0, 
          | otherwise =
            where (lResTree, lActions) = walk (depth + 1) l
                  (rResTree, rActions) = walk (depth + 1) r





-- run1 :: IO ()

-- run1 =
  -- do ls <- readLines "puzzle.txt"
     -- let p = parsePacket $ fromJust $ hexString2BinStr $ head ls
     -- print p
     -- -- print $ sum $ versionsToList $ fst p
     -- print $ calcPacketValue $ fst p

