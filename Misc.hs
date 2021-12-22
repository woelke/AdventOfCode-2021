-- puzzle input helper

module Misc where

-- predicate acc x
-- fun acc x

foldlWhile _ _ acc [] = acc
foldlWhile predicate fun acc (x:xs) =
  if predicate acc x
     then foldlWhile predicate fun (fun acc x) xs
     else acc


