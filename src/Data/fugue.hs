module Data.Fugue
    ( replace
    , replaceAll
    , replaceFP
    , replaceAllFP
    , padList
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

--replace (pat, rep) i is the function that replaces every occurance of pat
--with rep in i and returns that new string
replace :: Eq a => ([a], [a]) -> [a] -> [a]
-- ^replace (pat, rep) i is the function that replaces every occurance of pat
--with rep in i and returns that new string
replace _ [] = []
replace p@(pat, rep) i@(s:ss) =
    if i `startsWith` pat
    then rep ++ replace p (i `minus` pat)
    else s : replace p ss

replaceFP :: Eq a => ([a], [a]) -> [a] -> [a]
-- ^ The fixed point of replace
replaceFP p i =
    if i == i' then i' else replaceFP p i' where i' = replace p i

replaceAll :: Eq a => [a] -> [([a], [a])] -> [a]
-- ^Fold over replace
replaceAll = foldr replace

replaceAllFP :: Eq a => [a] -> [([a], [a])] -> [a]
-- ^Fixed point of replaceAll
replaceAllFP i ps =
    if i == i' then i' else replaceAllFP i' ps where i' = replaceAll i ps

--Returns a list that is always size i or greater,
--padded by "Nothings" if its too small
padList :: [a] -> Int -> [Maybe a]
padList [] i = if i > 0 then Nothing : padList [] (i-1) else []
padList (x:xs) i = Just x : padList xs (i - 1)
--removeElement :: (a -> Bool) -> [a] -> [a]
---- ^Removes 0 or 1 elements from a list meeting the condition
--removeElement _ [] = []
--removeElement p (a:as) = if p a then as else a : removeElement p as
