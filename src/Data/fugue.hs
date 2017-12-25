module Data.Fugue
    ( replace
    , replaceAll
    , replaceFP
    , replaceAllFP
    , padList
    , isSubsetOf
    ) where
-- ^
-- import Data.Fugue()
-- -- insert the desired functions inside of the parentheses

--import Enum.Enum
import Data.List (delete, deleteFirstsBy)

startsWith :: Eq a => [a] -> [a] -> Bool
startsWith _ [] = True   --It is always the case that [a, b, c] starts with []
startsWith [] _ = False  --It is never the case that [] starts with anything but [], which case we've already covered
startsWith (s:ss) (t:ts) = s == t && startsWith ss ts --Recursive case

minus :: Eq a => [a] -> [a] -> [a]
minus = deleteFirstsBy (==)

replace :: Eq a => ([a], [a]) -> [a] -> [a]
-- ^replace (pat, rep) i is the string that is obtained by
-- replacing pat with rep in every occurance of i
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

padList :: [a] -> Int -> [Maybe a]
-- ^Returns a list that is always size i or greater,
--padded by "Nothings" if its too small
padList [] i = if i > 0 then Nothing : padList [] (i-1) else []
padList (x:xs) i = Just x : padList xs (i - 1)

isSubsetOf :: Eq a => [a] -> [a] -> Bool
isSubsetOf [] _ = True
isSubsetOf (x:xs) ys =
    if elem x ys
    then isSubsetOf xs (delete x ys)
    else False
