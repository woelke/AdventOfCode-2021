-- Matrix helper functions

module Matrix where

-- Matrix representation
-- ab => [[a,b], [c,d]]
-- cd

-- ab => ac
-- cd    bd
transposeM :: [[a]] -> [[a]]

-- abc => head [a,d] and Tail bc
-- def                        ef
headM :: [[a]] -> [a]
tailM :: [[a]] -> [[a]]

-- [c,f] prepend to ab => abc
--                  de    bef
prependM :: [a] -> [[a]] -> [[a]]

-- [a,d] append to bc => abc
--                 ef    def
appendM :: [[a]] -> [a] -> [[a]]

coloumnLengthM :: [[a]] -> Int

transposeM = foldr (\row acc -> rowToColoumn row +++ acc) []
  where rowToColoumn = map (: [])
        (+++) [] [] = []
        (+++) xss [] = xss
        (+++) [] yss = yss
        (+++) (xs:xss) (ys:yss) = (xs ++ ys) : xss +++ yss

headM m = head $ transposeM m
tailM = transposeM . tail . transposeM

prependM h t = transposeM (h : transposeM t)
appendM t h = transposeM (transposeM t ++ [h])

coloumnLengthM m = length (head m)
