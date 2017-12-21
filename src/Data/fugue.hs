module Fugue
    ( replace
    ) where

--import Enum.Enum
import Data.List (deleteFirstsBy)

startsWith :: Eq a => [a] -> [a] -> Bool
startsWith _ [] = True   --It is always the case that [a, b, c] starts with []
startsWith [] _ = False  --It is never the case that [] starts with anything but [], which case we've already covered
startsWith (s:ss) (t:ts) = s == t && startsWith ss ts --Recursive case

minus :: Eq a => [a] -> [a] -> [a]
minus = deleteFirstsBy (==)
--minus p [] = p
--minus (s:ss) r@(t:ts) = if s == t then minus ss ts else minus ss r

--or
--minus = Data.List.deleteFirstsBy (==)

replace :: Eq a => ([a], [a]) -> [a] -> [a]
replace _ [] = []
replace p@(pat, rep) i@(s:ss) =
    if i `startsWith` pat
    then rep ++ replace p (i `minus` pat)
    else s : replace p ss
--removeElement :: (a -> Bool) -> [a] -> [a]
---- ^Removes 0 or 1 elements from a list meeting the condition
--removeElement _ [] = []
--removeElement p (a:as) = if p a then as else a : removeElement p as
