-- puzzle input helper

module Misc where

-- predicate acc x
-- fun acc x

foldlWhile :: (t1 -> t2 -> Bool) -> (t1 -> t2 -> t1) -> t1 -> [t2] -> t1

foldlWhile _ _ acc [] = acc
foldlWhile predicate fun acc (x:xs) =
  if predicate acc x
     then foldlWhile predicate fun (fun acc x) xs
     else acc

showMyList :: Show a => [a] -> String

showMyList xs =  foldl (\acc x -> acc ++ show x ++ "\n") (show (head xs) ++ "\n") (tail xs)


get1of3 :: (a,b,c) -> a
get2of3 :: (a,b,c) -> b
get3of3 :: (a,b,c) -> c

get1of3 (x,_,_) = x
get2of3 (_,x,_) = x
get3of3 (_,_,x) = x
