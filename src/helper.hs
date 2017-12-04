module Helper where

import Enum

removeElement :: (a -> Bool) -> [a] -> [a]
-- ^Removes 0 or 1 elements from a list meeting the condition
removeElement _ [] = []
removeElement p (a:as) = if p a then as else a : removeElement p as
