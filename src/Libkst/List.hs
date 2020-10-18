module Libkst.List (
  splitWhen
) where

-- | Split list based on predicate.
--
-- @
-- splitWhen (=='a') "axyaxy" == ["a", "xy", "a", "xy"]
-- @
splitWhen
  :: (a -> Bool) -- ^ predicate
  -> [a]         -- ^ list to be splitted
  -> [[a]]       -- ^ result, where delimiters are included
splitWhen _ [] = []
splitWhen p xs =
  case break p xs of
    (x, [])    -> [x]
    ([], y:ys) -> [y] : splitWhen p ys
    (x, y:ys)  ->  x  : [y] : splitWhen p ys
